# =============================================================================
# CORRESPONDENCE ANALYSIS - MULTIPLE VISUALISATIONS WITH INTERPRETIVE TABLES
# =============================================================================
# Author: Gianmarco Alberti
# Date: January 2025
# Version: 1.2.0 - Added symmetric plot, geometric interpretation, adjusted residuals
# =============================================================================
# References:
#   Greenacre, M. (2017). Correspondence Analysis in Practice (3rd ed.)
#   Beh, E.J., Lombardo, R., & Alberti, G. (2018). Correspondence analysis and
#     the Freeman-Tukey statistic. Computational Statistics & Data Analysis.
#   Borg, I., & Groenen, P.J.F. (2005). Modern Multidimensional Scaling.
#   Yelland, P. (2010). An Introduction to Correspondence Analysis.
#   Orton, C., & Tyers, P. (1991). A technique for reducing the size of sparse
#     contingency tables.
# =============================================================================

#' @export
caClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "caClass",
    inherit = caBase,
    
    private = list(
        
        # =====================================================================
        # CACHED RESULTS
        # =====================================================================
        .contingencyTable = NULL,
        .caResult = NULL,
        .caResultAdj = NULL,
        .srdResultRows = NULL,
        .srdResultCols = NULL,
        
        # =====================================================================
        # INIT - Set up table structure to prevent flickering
        # =====================================================================
        .init = function() {
            
            # Check if we have enough variables specified
            if (is.null(self$options$rows) || is.null(self$options$cols)) {
                return()
            }
            
            # -----------------------------------------------------------------
            # Pre-build observed contingency table structure
            # -----------------------------------------------------------------
            # Get levels from the data to build stable column structure
            data <- self$data
            rowVar <- self$options$rows
            colVar <- self$options$cols
            
            rowLevels <- levels(data[[rowVar]])
            colLevels <- levels(data[[colVar]])
            
            if (is.null(rowLevels) || is.null(colLevels)) return()
            
            I <- length(rowLevels)
            J <- length(colLevels)
            
            table <- self$results$observedTable
            table$setTitle(paste0("Observed Contingency Table: ", rowVar, " \u00d7 ", colVar))
            
            # Add row name column
            table$addColumn(name = "rowname", title = rowVar, type = "text")
            
            # Add columns for each column category
            for (j in seq_len(J)) {
                table$addColumn(
                    name = paste0("col", j),
                    title = colLevels[j],
                    type = "integer",
                    superTitle = colVar
                )
            }
            
            # Add row total column
            table$addColumn(name = "rowtotal", title = "Total", type = "integer")
            
            # Pre-populate rows with placeholders
            for (i in seq_len(I)) {
                values <- list(rowname = rowLevels[i])
                for (j in seq_len(J)) {
                    values[[paste0("col", j)]] <- NA
                }
                values$rowtotal <- NA
                table$addRow(rowKey = paste0("row", i), values = values)
            }
            
            # Add totals row placeholder
            total_values <- list(rowname = "Total")
            for (j in seq_len(J)) {
                total_values[[paste0("col", j)]] <- NA
            }
            total_values$rowtotal <- NA
            table$addRow(rowKey = "total", values = total_values)
            
            # Format the totals row
            table$addFormat(
                rowKey = "total",
                col = "rowname",
                format = jmvcore::Cell.BEGIN_GROUP
            )
            
            # Pre-populate summary table with placeholder
            if (self$options$showSummary) {
                table <- self$results$summaryTable
                table$setRow(rowNo = 1, values = list(
                    statistic = NA, df = NA, pvalue = NA,
                    totalInertia = NA, nRows = NA, nCols = NA
                ))
            }
            
            # Pre-populate inertia table
            if (self$options$showInertia) {
                table <- self$results$inertiaTable
                nAxes <- self$options$nAxes
                for (k in 1:nAxes) {
                    table$addRow(rowKey = k, values = list(
                        dim = k, eigenvalue = NA, percent = NA, cumPercent = NA,
                        malinChi = NA, malinDf = NA, malinP = NA
                    ))
                }
            }
            
            # Pre-populate symmetric coordinates table columns
            if (self$options$showSymmetricCoords) {
                table <- self$results$symmetricCoordsTable
                nAxes <- self$options$nAxes
                for (k in 1:nAxes) {
                    table$addColumn(
                        name = paste0("dim", k),
                        title = paste0("Dim ", k),
                        type = "number",
                        format = "zto"
                    )
                }
            }
            
            # Pre-populate seriated table structure (columns built in .run)
            # We just ensure it's visible if requested
            if (self$options$showSeriatedTable) {
                # Structure will be built dynamically in .populateSeriatedTable
                # based on the sorted column levels
            }
            
            # Pre-populate contribution biplot coordinates table columns
            if (self$options$showContribCoords) {
                table <- self$results$contribCoordsTable
                nAxes <- self$options$nAxes
                for (k in 1:nAxes) {
                    table$addColumn(
                        name = paste0("dim", k),
                        title = paste0("Dim ", k),
                        type = "number",
                        format = "zto"
                    )
                }
            }
            
            # Pre-populate quality & contributions table columns
            if (self$options$showQualityContrib) {
                table <- self$results$qualityContribTable
                nAxes <- self$options$nAxes
                for (k in 1:nAxes) {
                    table$addColumn(
                        name = paste0("cos2_", k),
                        title = paste0("Dim ", k),
                        superTitle = "Quality (cos\u00b2)",
                        type = "number",
                        format = "zto"
                    )
                    table$addColumn(
                        name = paste0("contrib_", k),
                        title = paste0("Dim ", k),
                        superTitle = "Contribution %",
                        type = "number",
                        format = "zto"
                    )
                }
                table$addColumn(
                    name = "totalQuality",
                    title = "Total Quality",
                    type = "number",
                    format = "zto"
                )
            }
            
            # Pre-populate dimension definition table (structure only)
            if (self$options$showDimDef) {
                table <- self$results$dimDefTable
                table$addRow(rowKey = 1, values = list(
                    dimension = "Dim 1", category = "...",
                    contrib = NA, cumulContrib = NA, direction = ""
                ))
            }
            
            # Pre-populate dimension summary table
            if (self$options$showDimSummary) {
                table <- self$results$dimSummaryTable
                nAxes <- self$options$nAxes
                for (k in 1:nAxes) {
                    table$addRow(rowKey = k, values = list(
                        dimension = paste("Dim", k), variance = NA,
                        negativePole = "...", positivePole = "..."
                    ))
                }
            }
            
            # Pre-populate profiles table columns (dynamic based on contribFocus)
            if (self$options$showProfiles) {
                table <- self$results$profilesTable
                table$addColumn(name = "category", title = "Category", type = "text")
                table$addColumn(name = "mass", title = "Mass", type = "number", format = "zto")
            }
            
            # Pre-populate residual matrix table (structure only - columns built in .run)
            if (self$options$showCellContrib) {
                # Table structure will be built dynamically in .populateResidualMatrixTable
                # based on the column variable levels
            }
            
            # Pre-populate correlations table columns
            if (self$options$showCorrelations) {
                table <- self$results$correlTable
                nAxes <- self$options$nAxes
                
                table$addColumn(name = "category", title = "Category", type = "text")
                for (k in 1:nAxes) {
                    table$addColumn(
                        name = paste0("corr", k),
                        title = paste0("Dim ", k),
                        superTitle = "Correlation (\u00b1\u221acos\u00b2)",
                        type = "number",
                        format = "zto"
                    )
                }
                table$addColumn(name = "primaryDim", title = "Primary", type = "text")
                table$addColumn(name = "quality", title = "Quality", type = "number", format = "zto")
            }
            
            # Pre-populate interpretation table (structure only)
            if (self$options$showInterpretation) {
                table <- self$results$interpTable
                table$addRow(rowKey = 1, values = list(
                    category = "...", association = "..."
                ))
            }
        },
        
        # =====================================================================
        # RUN - Main analysis
        # =====================================================================
        .run = function() {
            
            # Early exit if variables not specified
            if (is.null(self$options$rows) || is.null(self$options$cols)) {
                return()
            }
            
            # Clear cached results at start of each run to prevent stale data
            private$.contingencyTable <- NULL
            private$.caResult <- NULL
            private$.caResultAdj <- NULL
            private$.srdResultRows <- NULL
            private$.srdResultCols <- NULL
            
            # Build contingency table
            private$.contingencyTable <- private$.buildContingencyTable()
            if (is.null(private$.contingencyTable)) return()
            
            # Run CA based on selected metric and residual type
            private$.caResult <- private$.runCA(private$.contingencyTable)
            if (is.null(private$.caResult)) return()
            
            # Run SRD clustering for both row and column categories
            private$.srdResultRows <- private$.runSRDForVariable("rows")
            private$.srdResultCols <- private$.runSRDForVariable("cols")
            
            # Populate observed table first (always shown)
            private$.populateObservedTable()
            
            # Populate tables
            if (self$options$showSummary) private$.populateSummaryTable()
            if (self$options$showInertia) private$.populateInertiaTable()
            if (self$options$showSymmetricCoords) private$.populateSymmetricCoordsTable()
            if (self$options$showContribCoords) private$.populateContribCoordsTable()
            if (self$options$showQualityContrib) private$.populateQualityContribTable()
            if (self$options$showDimDef) private$.populateDimDefTable()
            if (self$options$showDimSummary) private$.populateDimSummaryTable()
            if (self$options$showProfiles) private$.populateProfilesTable()
            if (self$options$showCellContrib) private$.populateResidualMatrixTable()
            if (self$options$showCorrelations) private$.populateCorrelTable()
            if (self$options$showInterpretation) private$.populateInterpTable()
            if (self$options$showSRDTable) private$.populateSRDTable()
            if (self$options$showSeriatedTable) private$.populateSeriatedTable()
            
            # Populate method information if requested
            if (self$options$showMethodInfo) private$.populateMethodInfo()
        },
        
        # =====================================================================
        # BUILD CONTINGENCY TABLE
        # =====================================================================
        .buildContingencyTable = function() {
            
            row_var <- self$options$rows
            col_var <- self$options$cols
            count_var <- self$options$counts
            data <- self$data
            
            if (nrow(data) == 0) {
                return(NULL)
            }
            
            data[[row_var]] <- as.factor(data[[row_var]])
            data[[col_var]] <- as.factor(data[[col_var]])
            
            complete_cases <- complete.cases(data[[row_var]], data[[col_var]])
            if (!is.null(count_var)) {
                complete_cases <- complete_cases & complete.cases(data[[count_var]])
            }
            data <- data[complete_cases, , drop = FALSE]
            
            if (nrow(data) == 0) {
                return(NULL)
            }
            
            if (is.null(count_var)) {
                cont_table <- table(data[[row_var]], data[[col_var]])
            } else {
                counts <- data[[count_var]]
                if (any(counts < 0, na.rm = TRUE)) {
                    jmvcore::reject("Counts variable contains negative values")
                    return(NULL)
                }
                formula_obj <- stats::as.formula(paste0("`", count_var, "` ~ `", 
                                                        row_var, "` + `", col_var, "`"))
                cont_table <- stats::xtabs(formula_obj, data = data)
            }
            
            if (nrow(cont_table) < 2 || ncol(cont_table) < 2) {
                return(NULL)
            }
            
            row_sums <- rowSums(cont_table)
            col_sums <- colSums(cont_table)
            
            if (any(row_sums == 0) || any(col_sums == 0)) {
                return(NULL)
            }
            
            return(as.matrix(cont_table))
        },
        
        # =====================================================================
        # CORRESPONDENCE ANALYSIS
        # =====================================================================
        .runCA = function(N) {
            
            n_axes <- self$options$nAxes
            distance_metric <- self$options$distanceMetric
            residual_type <- self$options$residualType
            
            I <- nrow(N)
            J <- ncol(N)
            max_axes <- min(I, J) - 1
            if (n_axes > max_axes) n_axes <- max_axes
            
            grand_total <- sum(N)
            P <- N / grand_total
            r <- rowSums(P)
            c <- colSums(P)
            
            # Expected proportions under independence
            r_mat <- matrix(r, nrow = I, ncol = J, byrow = FALSE)
            c_mat <- matrix(c, nrow = I, ncol = J, byrow = TRUE)
            E <- r_mat * c_mat
            
            # Compute residuals based on selected metric and type
            if (distance_metric == "freemanTukey") {
                # Freeman-Tukey residuals: 2 * (sqrt(P) - sqrt(E))
                # These yield Hellinger distances (Beh, Lombardo & Alberti, 2018)
                S <- 2 * (sqrt(P) - sqrt(E))
                stat_name <- "T\u00b2"
                distance_type <- "hellinger"
                resid_label <- "Freeman-Tukey"
            } else if (residual_type == "adjusted") {
                # Adjusted standardised residuals
                S <- private$.computeAdjustedResiduals(P, E, r, c, I, J)
                stat_name <- "\u03c7\u00b2 (adj)"
                distance_type <- "chisq_adj"
                resid_label <- "Adjusted standardised"
            } else {
                # Pearson (standardised) residuals: (P - E) / sqrt(E)
                S <- (P - E) / sqrt(E + 1e-16)
                stat_name <- "\u03c7\u00b2"
                distance_type <- "chisq"
                resid_label <- "Standardised"
            }
            
            # SVD
            svd_result <- svd(S, nu = min(I, n_axes + 1), nv = min(J, n_axes + 1))
            d_all <- svd_result$d
            eigenvalues_all <- d_all^2
            
            # Determine starting index for axes
            # For Pearson residuals, the doubly-centred matrix has rank M* = min(I,J)-1,
            # yielding a trivial first singular value ≈ 1.
            # For Freeman-Tukey residuals, the matrix is NOT doubly-centred, so no trivial axis.
            if (distance_metric == "freemanTukey") {
                start_idx <- 1
            } else {
                if (length(d_all) > 0 && (abs(d_all[1] - 1) < 0.001 || d_all[1] > 0.999)) {
                    start_idx <- 2
                } else {
                    start_idx <- 1
                }
            }
            
            axes_idx <- start_idx:(start_idx + n_axes - 1)
            axes_idx <- axes_idx[axes_idx <= length(d_all)]
            
            d <- d_all[axes_idx]
            U <- svd_result$u[, axes_idx, drop = FALSE]
            V <- svd_result$v[, axes_idx, drop = FALSE]
            n_axes_actual <- length(d)
            eigenvalues <- d^2
            
            # Coordinates - recovery requires mass weighting for ALL methods
            # Beh et al. (2018), equations 10 and 13:
            #   f_im = (a_im / sqrt(p_i.)) * lambda_m
            #   g_jm = (b_jm / sqrt(p_.j)) * lambda_m
            
            Dr_inv_sqrt <- diag(1 / sqrt(r))
            Dc_inv_sqrt <- diag(1 / sqrt(c))
            
            # Standard coordinates: a_im / sqrt(p_i.) and b_jm / sqrt(p_.j)
            row_std <- Dr_inv_sqrt %*% U
            col_std <- Dc_inv_sqrt %*% V
            
            # Principal coordinates: standard coords × singular values
            row_princ <- row_std %*% diag(d, nrow = length(d))
            col_princ <- col_std %*% diag(d, nrow = length(d))
            
            rownames(row_std) <- rownames(row_princ) <- rownames(N)
            rownames(col_std) <- rownames(col_princ) <- colnames(N)
            colnames(row_std) <- colnames(row_princ) <- paste0("Dim", 1:n_axes_actual)
            colnames(col_std) <- colnames(col_princ) <- paste0("Dim", 1:n_axes_actual)
            
            # Total inertia
            # For all methods, use only min(I-1, J-1) eigenvalues (following Beh & Lombardo)
            max_axes_for_inertia <- min(I - 1, J - 1)
            
            if (start_idx == 2 && length(eigenvalues_all) > 1) {
                # Pearson: skip trivial axis, take next min(I-1,J-1) eigenvalues
                end_idx <- min(start_idx + max_axes_for_inertia - 1, length(eigenvalues_all))
                all_nontrivial <- eigenvalues_all[start_idx:end_idx]
            } else {
                # Freeman-Tukey: take first min(I-1,J-1) eigenvalues
                end_idx <- min(max_axes_for_inertia, length(eigenvalues_all))
                all_nontrivial <- eigenvalues_all[1:end_idx]
            }
            inertia_total <- sum(all_nontrivial)
            inertia_proportion <- eigenvalues / inertia_total
            inertia_cumulative <- cumsum(inertia_proportion)
            
            # Test statistic (both X² and T² are asymptotically chi-squared)
            test_statistic <- inertia_total * grand_total
            df <- (I - 1) * (J - 1)
            p_value <- stats::pchisq(test_statistic, df, lower.tail = FALSE)
            
            # Contributions (%) - different calculation for Freeman-Tukey
            row_contrib <- matrix(0, nrow = I, ncol = n_axes_actual)
            col_contrib <- matrix(0, nrow = J, ncol = n_axes_actual)
            
            if (distance_metric == "freemanTukey") {
                # For Freeman-Tukey: contributions are based on squared SVD components
                # Following Beh & Lombardo's framework where U and V are orthonormal
                # Contribution = u_im^2 * 100 (sums to 100% per dimension)
                for (k in 1:n_axes_actual) {
                    row_contrib[, k] <- (U[, k]^2) * 100
                    col_contrib[, k] <- (V[, k]^2) * 100
                }
            } else {
                # Classical Pearson formula: contribution = (mass × coord²) / eigenvalue
                for (k in 1:n_axes_actual) {
                    row_contrib[, k] <- (r * row_princ[, k]^2) / eigenvalues[k] * 100
                    col_contrib[, k] <- (c * col_princ[, k]^2) / eigenvalues[k] * 100
                }
            }
            
            rownames(row_contrib) <- rownames(N)
            rownames(col_contrib) <- colnames(N)
            colnames(row_contrib) <- colnames(col_contrib) <- paste0("Dim", 1:n_axes_actual)
            
            # Quality (cos²) - computed using appropriate distance
            if (distance_type == "hellinger") {
                # Hellinger distance: d²_H = 4 * sum[(sqrt(p_i) - sqrt(p_j))²]
                row_dist_sq <- numeric(I)
                for (i in 1:I) {
                    row_profile <- P[i, ] / r[i]
                    row_dist_sq[i] <- 4 * sum((sqrt(row_profile) - sqrt(c))^2)
                }
                
                col_dist_sq <- numeric(J)
                for (j in 1:J) {
                    col_profile <- P[, j] / c[j]
                    col_dist_sq[j] <- 4 * sum((sqrt(col_profile) - sqrt(r))^2)
                }
            } else {
                # Chi-squared distance (standard or adjusted)
                row_dist_sq <- numeric(I)
                for (i in 1:I) {
                    row_profile <- P[i, ] / r[i]
                    row_dist_sq[i] <- sum((row_profile - c)^2 / c)
                }
                
                col_dist_sq <- numeric(J)
                for (j in 1:J) {
                    col_profile <- P[, j] / c[j]
                    col_dist_sq[j] <- sum((col_profile - r)^2 / r)
                }
            }
            
            # Cos² = (principal coordinate)² / (distance from centroid)²
            row_cos2 <- matrix(0, nrow = I, ncol = n_axes_actual)
            col_cos2 <- matrix(0, nrow = J, ncol = n_axes_actual)
            
            for (i in 1:I) {
                if (row_dist_sq[i] > 1e-10) {
                    for (k in 1:n_axes_actual) {
                        row_cos2[i, k] <- (row_princ[i, k]^2) / row_dist_sq[i]
                    }
                }
            }
            
            for (j in 1:J) {
                if (col_dist_sq[j] > 1e-10) {
                    for (k in 1:n_axes_actual) {
                        col_cos2[j, k] <- (col_princ[j, k]^2) / col_dist_sq[j]
                    }
                }
            }
            
            rownames(row_cos2) <- rownames(N)
            rownames(col_cos2) <- colnames(N)
            colnames(row_cos2) <- colnames(col_cos2) <- paste0("Dim", 1:n_axes_actual)
            
            # Contribution coordinates (for biplot) - different for Freeman-Tukey
            if (distance_metric == "freemanTukey") {
                # For Freeman-Tukey: use U and V directly (already orthonormal)
                # The "contribution coordinate" concept maps to the SVD vectors
                row_contrib_coord <- U
                col_contrib_coord <- V
            } else {
                # Classical: contribution coordinates = std_coord * sqrt(mass)
                row_contrib_coord <- row_std * sqrt(r)
                col_contrib_coord <- col_std * sqrt(c)
            }
            rownames(row_contrib_coord) <- rownames(N)
            rownames(col_contrib_coord) <- colnames(N)
            colnames(row_contrib_coord) <- colnames(col_contrib_coord) <- paste0("Dim", 1:n_axes_actual)
            
            # Malinvaud's test
            malinvaud <- private$.computeMalinvaud(eigenvalues_all, start_idx, I, J, grand_total, n_axes_actual)
            
            # Row and column profiles (for profiles table)
            row_profiles <- P / r  # Each row divided by its marginal
            col_profiles <- t(t(P) / c)  # Each column divided by its marginal
            
            list(
                N = N, n_rows = I, n_cols = J, grand_total = grand_total,
                row_mass = r, col_mass = c,
                row_profiles = row_profiles, col_profiles = col_profiles,
                row_std = row_std, col_std = col_std,
                row_princ = row_princ, col_princ = col_princ,
                row_contrib_coord = row_contrib_coord, col_contrib_coord = col_contrib_coord,
                eigenvalues = eigenvalues, singular_values = d,
                inertia_total = inertia_total,
                inertia_proportion = inertia_proportion, inertia_cumulative = inertia_cumulative,
                n_axes = n_axes_actual,
                test_statistic = test_statistic, stat_name = stat_name,
                distance_type = distance_type, resid_label = resid_label,
                df = df, p_value = p_value,
                row_contrib = row_contrib, col_contrib = col_contrib,
                row_cos2 = row_cos2, col_cos2 = col_cos2,
                malinvaud = malinvaud
            )
        },
        
        # =====================================================================
        # COMPUTE ADJUSTED RESIDUALS FOR CA (Beh, 2012; Beh & Lombardo)
        # =====================================================================
        .computeAdjustedResiduals = function(P, E, r, c, I, J) {
            # Adjusted residuals for CA following Beh & Lombardo's code:
            # adj.x <- dIh %*% solve(sqrt(I - dI)) %*% y %*% solve(sqrt(I - dJ)) %*% dJh
            #
            # This is equivalent to:
            # r_tilde_ij = (p_ij - p_i * p_j) / (sqrt(p_i * p_j) * sqrt((1-p_i)*(1-p_j)))
            #            = pearson_resid / sqrt((1-p_i)*(1-p_j))
            
            adj_resid <- matrix(0, nrow = I, ncol = J)
            
            for (i in 1:I) {
                for (j in 1:J) {
                    # Standard Pearson residual: (p_ij - p_i*p_j) / sqrt(p_i*p_j)
                    pearson_resid <- (P[i, j] - r[i] * c[j]) / sqrt(r[i] * c[j] + 1e-16)
                    
                    # Divide by sqrt((1-p_i)*(1-p_j)) to get adjusted residual
                    variance_factor <- sqrt((1 - r[i]) * (1 - c[j]))
                    
                    adj_resid[i, j] <- pearson_resid / (variance_factor + 1e-16)
                }
            }
            
            adj_resid
        },
        
        # =====================================================================
        # MALINVAUD'S TEST
        # =====================================================================
        .computeMalinvaud = function(eigenvalues_all, start_idx, I, J, grand_total, n_axes) {
            
            if (start_idx == 2) {
                eig <- eigenvalues_all[2:length(eigenvalues_all)]
            } else {
                eig <- eigenvalues_all
            }
            
            n_eig <- min(length(eig), n_axes)
            malin_results <- data.frame(
                dim = integer(n_eig), stat = numeric(n_eig),
                df = integer(n_eig), p_value = numeric(n_eig)
            )
            
            for (k in 1:n_eig) {
                remaining_inertia <- sum(eig[k:length(eig)])
                stat_k <- grand_total * remaining_inertia
                df_k <- (I - k) * (J - k)
                malin_results$dim[k] <- k
                malin_results$stat[k] <- stat_k
                malin_results$df[k] <- df_k
                malin_results$p_value[k] <- if (df_k > 0) {
                    stats::pchisq(stat_k, df_k, lower.tail = FALSE)
                } else {
                    NA
                }
            }
            return(malin_results)
        },
        
        # =====================================================================
        # SRD CLUSTERING (for contribution biplot - uses contribFocus)
        # =====================================================================
        .runSRD = function() {
            # This maintains backward compatibility for the contribution biplot
            # which clusters the "interpreted" variable (opposite of focus)
            focus <- self$options$contribFocus
            
            if (focus == "cols") {
                return(private$.srdResultRows)
            } else {
                return(private$.srdResultCols)
            }
        },
        
        # =====================================================================
        # SRD CLUSTERING FOR SPECIFIC VARIABLE
        # =====================================================================
        .runSRDForVariable = function(target) {
            
            ca <- private$.caResult
            alpha <- self$options$clusterAlpha
            distance_type <- ca$distance_type
            
            # Get the target variable's data
            if (target == "rows") {
                mat <- ca$N  # Rows as items, columns as variables
                names_target <- rownames(ca$N)
            } else {
                mat <- t(ca$N)  # Columns as items, rows as variables
                names_target <- colnames(ca$N)
            }
            
            # Perform SRD clustering with appropriate distance metric
            srd_result <- private$.performSRD(mat, alpha = alpha, distance_type = distance_type)
            srd_result$names_target <- names_target
            
            return(srd_result)
        },
        
        .performSRD = function(mat, alpha = 0.05, distance_type = "chisq") {
            
            n_items <- nrow(mat)
            original_names <- rownames(mat)
            if (is.null(original_names)) {
                original_names <- paste0("R", seq_len(n_items))
                rownames(mat) <- original_names
            }
            
            if (n_items < 2) {
                return(list(
                    groups = list(list(members = original_names, merge_p = NA)),
                    merge_history = NULL,
                    singletons = character(0)
                ))
            }
            
            current_labels <- original_names
            
            merge_history <- data.frame(
                step = integer(), item1 = character(), item2 = character(),
                pValue = numeric(), decision = character(),
                stringsAsFactors = FALSE
            )
            
            group_membership <- as.list(current_labels)
            names(group_membership) <- current_labels
            group_merge_p <- setNames(rep(NA_real_, length(current_labels)), current_labels)
            
            step <- 0
            
            while (nrow(mat) > 1) {
                
                n_current <- nrow(mat)
                best_pair <- NULL
                best_p_value <- 0
                
                for (i in 1:(n_current - 1)) {
                    for (j in (i + 1):n_current) {
                        
                        weight_i <- sum(mat[i, ])
                        weight_j <- sum(mat[j, ])
                        
                        if (weight_i == 0 || weight_j == 0) next
                        
                        profile_i <- mat[i, ] / weight_i
                        profile_j <- mat[j, ] / weight_j
                        col_masses <- colSums(mat) / sum(mat)
                        
                        valid_cols <- col_masses > 0
                        if (sum(valid_cols) < 2) next
                        
                        # Compute distance based on selected metric
                        if (distance_type == "hellinger") {
                            # Hellinger distance (squared)
                            d_squared <- 4 * sum((sqrt(profile_i[valid_cols]) - 
                                                  sqrt(profile_j[valid_cols]))^2)
                        } else {
                            # Chi-squared distance (squared) - applies to both standard and adjusted
                            d_squared <- sum(((profile_i[valid_cols] - profile_j[valid_cols])^2) / 
                                               col_masses[valid_cols])
                        }
                        
                        if (is.na(d_squared) || is.nan(d_squared)) next
                        
                        # Weighted distance for testing
                        weighted_dist <- d_squared * (weight_i * weight_j) / (weight_i + weight_j)
                        
                        if (is.na(weighted_dist) || is.nan(weighted_dist)) next
                        
                        # Both X² and T² are asymptotically chi-squared distributed
                        df_test <- sum(valid_cols) - 1
                        p_val <- 1 - stats::pchisq(weighted_dist, df = df_test)
                        
                        if (p_val > best_p_value) {
                            best_p_value <- p_val
                            best_pair <- c(i, j)
                        }
                    }
                }
                
                if (is.null(best_pair)) break
                
                step <- step + 1
                
                if (best_p_value > alpha) {
                    idx1 <- best_pair[1]
                    idx2 <- best_pair[2]
                    
                    label1 <- rownames(mat)[idx1]
                    label2 <- rownames(mat)[idx2]
                    
                    merged_row <- mat[idx1, , drop = FALSE] + mat[idx2, , drop = FALSE]
                    new_label <- paste0(label1, "+", label2)
                    rownames(merged_row) <- new_label
                    
                    members1 <- if (label1 %in% names(group_membership)) {
                        group_membership[[label1]]
                    } else {
                        unlist(strsplit(label1, "\\+"))
                    }
                    members2 <- if (label2 %in% names(group_membership)) {
                        group_membership[[label2]]
                    } else {
                        unlist(strsplit(label2, "\\+"))
                    }
                    group_membership[[new_label]] <- c(members1, members2)
                    group_merge_p[[new_label]] <- best_p_value
                    
                    mat <- mat[-c(idx1, idx2), , drop = FALSE]
                    mat <- rbind(mat, merged_row)
                    
                    merge_history <- rbind(merge_history, data.frame(
                        step = step, item1 = label1, item2 = label2,
                        pValue = best_p_value, decision = "Merged",
                        stringsAsFactors = FALSE
                    ))
                } else {
                    merge_history <- rbind(merge_history, data.frame(
                        step = step,
                        item1 = rownames(mat)[best_pair[1]],
                        item2 = rownames(mat)[best_pair[2]],
                        pValue = best_p_value, decision = "Stopped",
                        stringsAsFactors = FALSE
                    ))
                    break
                }
            }
            
            final_labels <- rownames(mat)
            groups <- list()
            singletons <- character(0)
            
            for (lab in final_labels) {
                members <- if (lab %in% names(group_membership)) {
                    group_membership[[lab]]
                } else {
                    lab
                }
                merge_p <- if (lab %in% names(group_merge_p)) {
                    group_merge_p[[lab]]
                } else {
                    NA
                }
                
                if (length(members) >= 2) {
                    groups[[length(groups) + 1]] <- list(members = members, merge_p = merge_p)
                } else {
                    singletons <- c(singletons, members)
                }
            }
            
            list(groups = groups, merge_history = merge_history, singletons = singletons)
        },
        
        # =====================================================================
        # POPULATE SUMMARY TABLE
        # =====================================================================
        .populateSummaryTable = function() {
            ca <- private$.caResult
            table <- self$results$summaryTable
            
            table$setRow(rowNo = 1, values = list(
                statistic = ca$test_statistic, 
                df = ca$df, 
                pvalue = ca$p_value,
                totalInertia = ca$inertia_total, 
                nRows = ca$n_rows, 
                nCols = ca$n_cols
            ))
            
            # Set column title based on statistic type
            table$getColumn("statistic")$setTitle(ca$stat_name)
            
            # Add note about distance/residual type
            if (ca$distance_type == "hellinger") {
                table$setNote("distNote", 
                    "Freeman-Tukey residuals (Hellinger distance). T\u00b2 is asymptotically \u03c7\u00b2-distributed.")
            } else if (ca$distance_type == "chisq_adj") {
                table$setNote("distNote", 
                    "Adjusted standardised residuals (\u03c7\u00b2 distance with marginal correction).")
            } else {
                table$setNote("distNote", 
                    "Pearson residuals (\u03c7\u00b2 distance).")
            }
        },
        
        # =====================================================================
        # POPULATE OBSERVED CONTINGENCY TABLE (with marginals)
        # =====================================================================
        .populateObservedTable = function() {
            
            N <- private$.contingencyTable
            if (is.null(N)) return()
            
            table <- self$results$observedTable
            
            row_names <- rownames(N)
            col_names <- colnames(N)
            I <- nrow(N)
            J <- ncol(N)
            
            # Calculate marginals
            row_totals <- rowSums(N)
            col_totals <- colSums(N)
            grand_total <- sum(N)
            
            # Update data rows using setRow (structure already built in .init)
            for (i in seq_len(I)) {
                values <- list(rowname = row_names[i])
                for (j in seq_len(J)) {
                    values[[paste0("col", j)]] <- as.integer(N[i, j])
                }
                values$rowtotal <- as.integer(row_totals[i])
                
                table$setRow(rowKey = paste0("row", i), values = values)
            }
            
            # Update column totals row
            total_values <- list(rowname = "Total")
            for (j in seq_len(J)) {
                total_values[[paste0("col", j)]] <- as.integer(col_totals[j])
            }
            total_values$rowtotal <- as.integer(grand_total)
            
            table$setRow(rowKey = "total", values = total_values)
        },

        # =====================================================================
        # POPULATE SERIATED CONTINGENCY TABLE
        # =====================================================================
        .populateSeriatedTable = function() {
            
            ca <- private$.caResult
            N <- private$.contingencyTable
            if (is.null(ca) || is.null(N)) return()
            
            table <- self$results$seriatedTable
            
            # Get the dimensions from symmetric plot settings
            dim1 <- self$options$dim1  # X-axis (primary sort)
            dim2 <- self$options$dim2  # Y-axis (secondary sort)
            
            # Validate dimensions exist
            if (dim1 > ca$n_axes || dim2 > ca$n_axes) {
                maxDim <- max(dim1, dim2)
                table$setNote("dimError", 
                              paste0("Dimension ", maxDim, " not available. Only ", 
                                     ca$n_axes, " dimensions extracted."))
                return()
            }
            
            # Extract principal coordinates
            rowCoords1 <- ca$row_princ[, dim1]
            rowCoords2 <- ca$row_princ[, dim2]
            colCoords1 <- ca$col_princ[, dim1]
            colCoords2 <- ca$col_princ[, dim2]
            
            # Get row and column names
            rowNames <- rownames(ca$row_princ)
            colNames <- rownames(ca$col_princ)
            
            I <- length(rowNames)
            J <- length(colNames)
            
            # ---------------------------------------------------------------------
            # Sort by Dim1 (X-axis) coordinates only, ascending (negative to positive)
            # ---------------------------------------------------------------------
            rowOrder <- order(rowCoords1)
            colOrder <- order(colCoords1)
            
            # Reorder the contingency table
            seriatedData <- N[rowOrder, colOrder, drop = FALSE]
            
            # Sorted coordinate values and names
            sortedRowCoords1 <- rowCoords1[rowOrder]
            sortedRowCoords2 <- rowCoords2[rowOrder]
            sortedColCoords1 <- colCoords1[colOrder]
            sortedColCoords2 <- colCoords2[colOrder]
            sortedRowNames <- rowNames[rowOrder]
            sortedColNames <- colNames[colOrder]
            
            # Get variable names for labelling
            rowVar <- self$options$rows
            colVar <- self$options$cols
            
            # Build table columns dynamically
            # First column: Row category
            table$addColumn(
                name = "rowCategory",
                title = rowVar,
                type = "text"
            )
            
            # Add columns for each (sorted) column category - use NUMBER type for flexibility
            for (j in seq_along(sortedColNames)) {
                table$addColumn(
                    name = paste0("col_", j),
                    title = sortedColNames[j],
                    type = "number",
                    superTitle = colVar
                )
            }
            
            # Add row coordinate columns (marginals) - rightmost
            table$addColumn(
                name = "rowCoord1",
                title = paste0("Dim ", dim1),
                type = "number",
                format = "zto"
            )
            table$addColumn(
                name = "rowCoord2",
                title = paste0("Dim ", dim2),
                type = "number",
                format = "zto"
            )
            
            # Populate data rows
            for (i in seq_along(sortedRowNames)) {
                rowData <- list(
                    rowCategory = sortedRowNames[i]
                )
                
                # Add cell values (as integers)
                for (j in seq_along(sortedColNames)) {
                    rowData[[paste0("col_", j)]] <- as.integer(seriatedData[i, j])
                }
                
                # Add row coordinates (marginals)
                rowData$rowCoord1 <- sortedRowCoords1[i]
                rowData$rowCoord2 <- sortedRowCoords2[i]
                
                table$addRow(rowKey = i, values = rowData)
            }
            
            # Add column coordinates rows (marginals) - bottom rows
            # First: Dim 1 coordinates
            coordRow1Data <- list(
                rowCategory = paste0("Dim ", dim1)
            )
            for (j in seq_along(sortedColNames)) {
                coordRow1Data[[paste0("col_", j)]] <- sortedColCoords1[j]
            }
            coordRow1Data$rowCoord1 <- NA
            coordRow1Data$rowCoord2 <- NA
            
            table$addRow(rowKey = I + 1, values = coordRow1Data)
            
            # Format the first coordinates row to stand out
            table$addFormat(
                rowKey = I + 1,
                col = "rowCategory",
                format = jmvcore::Cell.BEGIN_GROUP
            )
            
            # Second: Dim 2 coordinates
            coordRow2Data <- list(
                rowCategory = paste0("Dim ", dim2)
            )
            for (j in seq_along(sortedColNames)) {
                coordRow2Data[[paste0("col_", j)]] <- sortedColCoords2[j]
            }
            coordRow2Data$rowCoord1 <- NA
            coordRow2Data$rowCoord2 <- NA
            
            table$addRow(rowKey = I + 2, values = coordRow2Data)
            
            # Add footnote explaining the ordering
            table$setNote(
                "seriation",
                paste0("Rows and columns ordered by Dimension ", dim1, 
                       " (X-axis) coordinates, from most negative to most positive. ",
                       "Marginals show principal coordinates for Dimensions ", dim1, " and ", dim2, ".")
            )
            
            # Set table title
            table$setTitle(paste0("Seriated Contingency Table: ", rowVar, " \u00d7 ", colVar))
        },
        
        # =====================================================================
        # POPULATE INERTIA TABLE
        # =====================================================================
        .populateInertiaTable = function() {
            ca <- private$.caResult
            table <- self$results$inertiaTable
            table$deleteRows()
            for (k in 1:ca$n_axes) {
                table$addRow(rowKey = k, values = list(
                    dim = k, eigenvalue = ca$eigenvalues[k],
                    percent = ca$inertia_proportion[k] * 100,
                    cumPercent = ca$inertia_cumulative[k] * 100,
                    malinChi = ca$malinvaud$stat[k],
                    malinDf = ca$malinvaud$df[k],
                    malinP = ca$malinvaud$p_value[k]
                ))
            }
        },
        
        # =====================================================================
        # POPULATE SYMMETRIC COORDINATES TABLE
        # =====================================================================
        .populateSymmetricCoordsTable = function() {
            ca <- private$.caResult
            table <- self$results$symmetricCoordsTable
            nAxes <- self$options$nAxes
            
            # Row categories - principal coordinates
            rowNames <- rownames(ca$row_princ)
            rowKey <- 1
            for (i in seq_along(rowNames)) {
                values <- list(
                    category = rowNames[i],
                    type = "Row"
                )
                for (k in 1:nAxes) {
                    values[[paste0("dim", k)]] <- ca$row_princ[i, k]
                }
                table$addRow(rowKey = rowKey, values = values)
                rowKey <- rowKey + 1
            }
            
            # Column categories - principal coordinates
            colNames <- rownames(ca$col_princ)
            for (i in seq_along(colNames)) {
                values <- list(
                    category = colNames[i],
                    type = "Column"
                )
                for (k in 1:nAxes) {
                    values[[paste0("dim", k)]] <- ca$col_princ[i, k]
                }
                table$addRow(rowKey = rowKey, values = values)
                rowKey <- rowKey + 1
            }
        },
        
        # =====================================================================
        # POPULATE CONTRIBUTION BIPLOT COORDINATES TABLE
        # =====================================================================
        .populateContribCoordsTable = function() {
            ca <- private$.caResult
            table <- self$results$contribCoordsTable
            nAxes <- self$options$nAxes
            focusOn <- self$options$contribFocus
            
            # Determine which variable defines the plane vs is interpreted
            if (focusOn == "cols") {
                # Columns define plane (standard coords), rows interpreted (principal coords)
                focusCoords <- ca$col_std
                focusNames <- rownames(ca$col_std)
                focusType <- "Column"
                focusCoordType <- "Standard"
                
                interpCoords <- ca$row_princ
                interpNames <- rownames(ca$row_princ)
                interpType <- "Row"
                interpCoordType <- "Principal"
            } else {
                # Rows define plane (standard coords), columns interpreted (principal coords)
                focusCoords <- ca$row_std
                focusNames <- rownames(ca$row_std)
                focusType <- "Row"
                focusCoordType <- "Standard"
                
                interpCoords <- ca$col_princ
                interpNames <- rownames(ca$col_princ)
                interpType <- "Column"
                interpCoordType <- "Principal"
            }
            
            # Set table note explaining coordinate types
            table$setNote(
                "coordNote",
                paste0(
                    focusType, " categories (plane-defining): ", focusCoordType, " coordinates; ",
                    interpType, " categories (interpreted): ", interpCoordType, " coordinates"
                )
            )
            
            rowKey <- 1
            
            # Plane-defining categories (standard coordinates)
            for (i in seq_along(focusNames)) {
                values <- list(
                    category = focusNames[i],
                    type = focusType,
                    coordType = focusCoordType
                )
                for (k in 1:nAxes) {
                    values[[paste0("dim", k)]] <- focusCoords[i, k]
                }
                table$addRow(rowKey = rowKey, values = values)
                rowKey <- rowKey + 1
            }
            
            # Interpreted categories (principal coordinates)
            for (i in seq_along(interpNames)) {
                values <- list(
                    category = interpNames[i],
                    type = interpType,
                    coordType = interpCoordType
                )
                for (k in 1:nAxes) {
                    values[[paste0("dim", k)]] <- interpCoords[i, k]
                }
                table$addRow(rowKey = rowKey, values = values)
                rowKey <- rowKey + 1
            }
        },
        
        # =====================================================================
        # POPULATE QUALITY & CONTRIBUTIONS TABLE
        # =====================================================================
        .populateQualityContribTable = function() {
            ca <- private$.caResult
            table <- self$results$qualityContribTable
            
            nAxes <- ca$n_axes
            
            # Combine row and column data
            row_names <- rownames(ca$N)
            col_names <- colnames(ca$N)
            
            n_rows <- length(row_names)
            n_cols <- length(col_names)
            
            # Build data frame with all categories
            all_data <- data.frame(
                category = c(row_names, col_names),
                type = c(rep("Row", n_rows), rep("Column", n_cols)),
                stringsAsFactors = FALSE
            )
            
            # Add cos² and contributions for each dimension
            for (k in 1:nAxes) {
                all_data[[paste0("cos2_", k)]] <- c(ca$row_cos2[, k], ca$col_cos2[, k])
                all_data[[paste0("contrib_", k)]] <- c(ca$row_contrib[, k], ca$col_contrib[, k])
            }
            
            # Compute total quality (sum of cos² across displayed dimensions)
            all_data$totalQuality <- rowSums(all_data[, grep("^cos2_", names(all_data)), drop = FALSE])
            
            # Sort by Type (Row first, then Column), then by Total Quality descending within each type
            all_data <- all_data[order(all_data$type, -all_data$totalQuality), ]
            
            # Populate table
            for (i in seq_len(nrow(all_data))) {
                values <- list(
                    category = all_data$category[i],
                    type = all_data$type[i]
                )
                
                for (k in 1:nAxes) {
                    values[[paste0("cos2_", k)]] <- all_data[[paste0("cos2_", k)]][i]
                    values[[paste0("contrib_", k)]] <- all_data[[paste0("contrib_", k)]][i]
                }
                
                values$totalQuality <- all_data$totalQuality[i]
                
                table$addRow(rowKey = i, values = values)
            }
            
            table$setNote(
                "qualityNote",
                paste0(
                    "Quality (cos\u00b2) indicates how well a category is represented in each dimension. ",
                    "Contribution % indicates how much a category contributes to defining each dimension. ",
                    "Sorted by Total Quality (descending)."
                )
            )
        },
        
        # =====================================================================
        # POPULATE DIMENSION DEFINITION TABLE (all contributors, sorted)
        # =====================================================================
        .populateDimDefTable = function() {
            ca <- private$.caResult
            table <- self$results$dimDefTable
            table$deleteRows()
            
            focus <- self$options$contribFocus
            
            if (focus == "cols") {
                contrib <- ca$col_contrib
                princ <- ca$col_princ
                names_vec <- colnames(ca$N)
                n_cats <- ca$n_cols
            } else {
                contrib <- ca$row_contrib
                princ <- ca$row_princ
                names_vec <- rownames(ca$N)
                n_cats <- ca$n_rows
            }
            
            avg_contrib <- 100 / n_cats
            row_key <- 0
            
            for (dim_idx in 1:ca$n_axes) {
                
                dim_contrib <- contrib[, dim_idx]
                dim_coord <- princ[, dim_idx]
                
                # Sort ALL categories by contribution (decreasing)
                sorted_idx <- order(dim_contrib, decreasing = TRUE)
                
                cumul <- 0
                for (idx in sorted_idx) {
                    row_key <- row_key + 1
                    cumul <- cumul + dim_contrib[idx]
                    
                    direction <- if (dim_coord[idx] > 0) "+" else "\u2212"
                    
                    table$addRow(rowKey = row_key, values = list(
                        dimension = paste("Dim", dim_idx),
                        category = names_vec[idx],
                        contrib = dim_contrib[idx],
                        cumulContrib = cumul,
                        direction = direction
                    ))
                    
                    # Highlight above-average contributors (guard against NA)
                    if (!is.na(dim_contrib[idx]) && dim_contrib[idx] >= avg_contrib) {
                        table$addFormat(
                            rowKey = row_key,
                            col = "contrib",
                            format = jmvcore::Cell.NEGATIVE
                        )
                    }
                }
            }
            
            # Add note with average contribution threshold and variable clarification
            focus_label <- if (focus == "cols") "column" else "row"
            table$setNote('avgNote', sprintf(
                "Showing %s categories (plane-defining variable in Contribution Biplot). Contributions \u2265 average (%.2f%%) are highlighted.",
                focus_label, avg_contrib
            ))
        },
        
        # =====================================================================
        # POPULATE DIMENSION SUMMARY TABLE (pole-centric)
        # =====================================================================
        .populateDimSummaryTable = function() {
            ca <- private$.caResult
            table <- self$results$dimSummaryTable
            table$deleteRows()
            
            focus <- self$options$contribFocus
            
            if (focus == "cols") {
                contrib <- ca$col_contrib
                princ <- ca$col_princ
                names_vec <- colnames(ca$N)
                n_cats <- ca$n_cols
            } else {
                contrib <- ca$row_contrib
                princ <- ca$row_princ
                names_vec <- rownames(ca$N)
                n_cats <- ca$n_rows
            }
            
            avg_contrib <- 100 / n_cats
            
            for (dim_idx in 1:ca$n_axes) {
                
                dim_contrib <- contrib[, dim_idx]
                dim_coord <- princ[, dim_idx]
                
                # Identify above-average contributors
                above_avg <- !is.na(dim_contrib) & (dim_contrib >= avg_contrib)
                
                # Split by sign
                pos_idx <- which(above_avg & dim_coord > 0)
                neg_idx <- which(above_avg & dim_coord < 0)
                
                # Sort each group by contribution (descending)
                pos_idx <- pos_idx[order(dim_contrib[pos_idx], decreasing = TRUE)]
                neg_idx <- neg_idx[order(dim_contrib[neg_idx], decreasing = TRUE)]
                
                # Build strings with contributions
                pos_str <- if (length(pos_idx) > 0) {
                    paste(sapply(pos_idx, function(i) {
                        sprintf("%s (%.1f%%)", names_vec[i], dim_contrib[i])
                    }), collapse = ", ")
                } else {
                    "\u2014"
                }
                
                neg_str <- if (length(neg_idx) > 0) {
                    paste(sapply(neg_idx, function(i) {
                        sprintf("%s (%.1f%%)", names_vec[i], dim_contrib[i])
                    }), collapse = ", ")
                } else {
                    "\u2014"
                }
                
                table$addRow(rowKey = dim_idx, values = list(
                    dimension = paste("Dim", dim_idx),
                    variance = ca$inertia_proportion[dim_idx] * 100,
                    negativePole = neg_str,
                    positivePole = pos_str
                ))
            }
            
            table$setNote('avgNote', sprintf(
                "Only categories with contributions \u2265 average (%.2f%%) are shown. This table supports Contribution Biplot interpretation.",
                avg_contrib
            ))
        },
        
        # =====================================================================
        # POPULATE PROFILES TABLE (interpreted categories)
        # =====================================================================
        .populateProfilesTable = function() {
            ca <- private$.caResult
            table <- self$results$profilesTable
            
            focus <- self$options$contribFocus
            
            # Get the INTERPRETED variable's profiles (opposite of focus)
            if (focus == "cols") {
                profiles <- ca$row_profiles
                masses <- ca$row_mass
                names_vec <- rownames(ca$N)
                col_names <- colnames(ca$N)
                n_cats <- ca$n_rows
                table_title <- "Row Profiles (Interpreted Categories)"
            } else {
                profiles <- t(ca$col_profiles)
                masses <- ca$col_mass
                names_vec <- colnames(ca$N)
                col_names <- rownames(ca$N)
                n_cats <- ca$n_cols
                table_title <- "Column Profiles (Interpreted Categories)"
            }
            
            table$setTitle(table_title)
            
            for (j in seq_along(col_names)) {
                table$addColumn(
                    name = paste0("prof", j),
                    title = col_names[j],
                    type = "number",
                    format = "zto"
                )
            }
            
            for (i in seq_len(n_cats)) {
                values <- list(
                    category = names_vec[i],
                    mass = masses[i]
                )
                
                for (j in seq_along(col_names)) {
                    values[[paste0("prof", j)]] <- profiles[i, j]
                }
                
                table$addRow(rowKey = i, values = values)
            }
            
            if (focus == "cols") {
                marginal <- ca$col_mass
            } else {
                marginal <- ca$row_mass
            }
            
            marginal_values <- list(
                category = "Average (marginal)",
                mass = NA
            )
            for (j in seq_along(col_names)) {
                marginal_values[[paste0("prof", j)]] <- marginal[j]
            }
            
            table$addRow(rowKey = n_cats + 1, values = marginal_values)
            
            table$addFormat(
                rowKey = n_cats + 1,
                col = "category",
                format = jmvcore::Cell.BEGIN_GROUP
            )
            
            table$setNote('profileNote', 
                "Profiles show conditional distributions. Average (marginal) profile is the centroid.")
        },
        
        # =====================================================================
        # POPULATE RESIDUAL MATRIX TABLE
        # =====================================================================
        .populateResidualMatrixTable = function() {
            
            ca <- private$.caResult
            table <- self$results$residualMatrixTable
            
            N <- ca$N
            n <- ca$grand_total
            I <- ca$n_rows
            J <- ca$n_cols
            r <- ca$row_mass
            c <- ca$col_mass
            
            row_names <- rownames(N)
            col_names <- colnames(N)
            row_var <- self$options$rows
            col_var <- self$options$cols
            
            threshold <- self$options$residualThreshold
            
            # Determine residual type and statistic label
            distance_metric <- self$options$distanceMetric
            residual_type <- self$options$residualType
            
            if (distance_metric == "freemanTukey") {
                stat_label <- "% of Ť²"
                residual_label <- "Freeman-Tukey Residuals"
            } else if (residual_type == "adjusted") {
                stat_label <- "% of χ²ₐ"
                residual_label <- "Adjusted Standardised Residuals"
            } else {
                stat_label <- "% of χ²"
                residual_label <- "Standardised Residuals"
            }
            
            # Set table title
            table$setTitle(paste0(residual_label, ": ", row_var, " × ", col_var))
            
            # Build columns dynamically
            table$addColumn(name = "rowname", title = row_var, type = "text")
            
            for (j in seq_len(J)) {
                table$addColumn(
                    name = paste0("col", j),
                    title = col_names[j],
                    type = "number",
                    format = "zto",
                    superTitle = col_var
                )
            }
            
            # Add row contribution column
            table$addColumn(name = "rowPct", title = stat_label, type = "number", format = "zto")
            
            # Compute residuals based on selected method
            residual_matrix <- matrix(0, nrow = I, ncol = J)
            cell_contrib_matrix <- matrix(0, nrow = I, ncol = J)
            
            # Compute expected values
            expected <- outer(r, c) * n
            
            if (distance_metric == "freemanTukey") {
                # Standardised Freeman-Tukey residuals for CA (Beh, Lombardo & Alberti, 2018)
                # Formula: sqrt(n) * 2 * (sqrt(p_ij) - sqrt(p_i. * p_.j))
                # where p_ij = n_ij / n are proportions
                P <- N / n  # Convert to proportions
                for (i in 1:I) {
                    for (j in 1:J) {
                        residual_matrix[i, j] <- sqrt(n) * 2 * (sqrt(P[i, j]) - sqrt(r[i] * c[j]))
                        cell_contrib_matrix[i, j] <- residual_matrix[i, j]^2
                    }
                }
                total_stat <- sum(cell_contrib_matrix)
            } else if (residual_type == "adjusted") {
                # Adjusted standardised residuals
                for (i in 1:I) {
                    for (j in 1:J) {
                        obs <- N[i, j]
                        exp <- expected[i, j]
                        if (exp > 0) {
                            adjustment <- sqrt((1 - r[i]) * (1 - c[j]))
                            residual_matrix[i, j] <- (obs - exp) / (sqrt(exp) * adjustment)
                            cell_contrib_matrix[i, j] <- residual_matrix[i, j]^2
                        }
                    }
                }
                total_stat <- sum(cell_contrib_matrix)
            } else {
                # Standard Pearson residuals
                for (i in 1:I) {
                    for (j in 1:J) {
                        obs <- N[i, j]
                        exp <- expected[i, j]
                        if (exp > 0) {
                            residual_matrix[i, j] <- (obs - exp) / sqrt(exp)
                            cell_contrib_matrix[i, j] <- (obs - exp)^2 / exp
                        }
                    }
                }
                total_stat <- ca$test_statistic
            }
            
            # Compute row and column percentages
            row_pct <- rowSums(cell_contrib_matrix) / total_stat * 100
            col_pct <- colSums(cell_contrib_matrix) / total_stat * 100
            
            # Compute average contributions for highlighting
            avg_row_pct <- 100 / I
            avg_col_pct <- 100 / J
            
            # Add data rows
            for (i in seq_len(I)) {
                values <- list(rowname = row_names[i])
                
                for (j in seq_len(J)) {
                    values[[paste0("col", j)]] <- residual_matrix[i, j]
                }
                
                values$rowPct <- row_pct[i]
                
                table$addRow(rowKey = paste0("row", i), values = values)
                
                # Highlight residuals exceeding threshold
                if (threshold > 0) {
                    for (j in seq_len(J)) {
                        if (abs(residual_matrix[i, j]) > threshold) {
                            table$addFormat(
                                rowKey = paste0("row", i),
                                col = paste0("col", j),
                                format = jmvcore::Cell.NEGATIVE
                            )
                        }
                    }
                }
                
                # Highlight above-average row contributions
                if (row_pct[i] > avg_row_pct) {
                    table$addFormat(
                        rowKey = paste0("row", i),
                        col = "rowPct",
                        format = jmvcore::Cell.NEGATIVE
                    )
                }
            }
            
            # Add column percentages row
            col_pct_values <- list(rowname = stat_label)
            for (j in seq_len(J)) {
                col_pct_values[[paste0("col", j)]] <- col_pct[j]
            }
            col_pct_values$rowPct <- 100.0
            
            table$addRow(rowKey = "colPct", values = col_pct_values)
            
            # Format the percentage row to stand out
            table$addFormat(
                rowKey = "colPct",
                col = "rowname",
                format = jmvcore::Cell.BEGIN_GROUP
            )
            
            # Highlight above-average column contributions
            for (j in seq_len(J)) {
                if (col_pct[j] > avg_col_pct) {
                    table$addFormat(
                        rowKey = "colPct",
                        col = paste0("col", j),
                        format = jmvcore::Cell.NEGATIVE
                    )
                }
            }
            
            # Build explanatory note
            note_parts <- c()
            
            if (threshold > 0) {
                note_parts <- c(note_parts, sprintf(
                    "Residuals with |z| > %.2f are highlighted.", threshold
                ))
            }
            
            note_parts <- c(note_parts, sprintf(
                "Margins show %% contribution to total statistic; values above average (rows: %.2f%%, cols: %.2f%%) are highlighted.",
                avg_row_pct, avg_col_pct
            ))
            
            if (distance_metric == "freemanTukey") {
                note_parts <- c(note_parts, 
                                "Freeman-Tukey residuals are robust to overdispersion (Beh, Lombardo & Alberti, 2018).")
            } else if (residual_type == "adjusted") {
                note_parts <- c(note_parts,
                                "Adjusted residuals account for marginal constraints.")
            }
            
            table$setNote('residualNote', paste(note_parts, collapse = " "))
        },
        
        # =====================================================================
        # POPULATE CORRELATIONS TABLE
        # =====================================================================
        .populateCorrelTable = function() {
            ca <- private$.caResult
            table <- self$results$correlTable
            
            focus <- self$options$contribFocus
            quality_threshold <- self$options$qualityThreshold
            
            if (focus == "cols") {
                princ <- ca$row_princ
                cos2 <- ca$row_cos2
                names_vec <- rownames(ca$N)
                n_cats <- ca$n_rows
            } else {
                princ <- ca$col_princ
                cos2 <- ca$col_cos2
                names_vec <- colnames(ca$N)
                n_cats <- ca$n_cols
            }
            
            n_shown <- 0
            
            for (i in seq_len(n_cats)) {
                total_quality <- sum(cos2[i, ])
                
                if (total_quality < quality_threshold) {
                    next
                }
                
                n_shown <- n_shown + 1
                
                values <- list(category = names_vec[i])
                
                max_abs_corr <- 0
                primary_dim <- 1
                primary_sign <- "+"
                
                for (k in 1:ca$n_axes) {
                    cos2_val <- cos2[i, k]
                    coord_sign <- sign(princ[i, k])
                    if (coord_sign == 0) coord_sign <- 1
                    signed_corr <- coord_sign * sqrt(cos2_val)
                    values[[paste0("corr", k)]] <- signed_corr
                    
                    if (abs(signed_corr) > max_abs_corr) {
                        max_abs_corr <- abs(signed_corr)
                        primary_dim <- k
                        primary_sign <- if (coord_sign >= 0) "(+)" else "(\u2212)"
                    }
                }
                
                values$primaryDim <- paste("Dim", primary_dim, primary_sign)
                values$quality <- total_quality
                
                table$addRow(rowKey = i, values = values)
                
                table$addFormat(
                    rowKey = i,
                    col = paste0("corr", primary_dim),
                    format = jmvcore::Cell.NEGATIVE
                )
            }
            
            correl_explanation <- "Correlation = \u00b1\u221acos\u00b2, where sign reflects coordinate direction. Quality = sum of cos\u00b2 across all extracted dimensions (overall representation quality)."
            
            if (quality_threshold > 0) {
                n_filtered <- n_cats - n_shown
                if (n_filtered > 0) {
                    table$setNote('filterNote', sprintf(
                        "%d categories with quality < %.2f are hidden. %s This table supports Contribution Biplot interpretation.", 
                        n_filtered, quality_threshold, correl_explanation
                    ))
                } else {
                    table$setNote('filterNote', paste(correl_explanation, "This table supports Contribution Biplot interpretation."))
                }
            } else {
                table$setNote('interpNote', paste(correl_explanation, "This table supports Contribution Biplot interpretation."))
            }
        },
        
        # =====================================================================
        # POPULATE INTERPRETATION SUMMARY TABLE
        # =====================================================================
        .populateInterpTable = function() {
            ca <- private$.caResult
            table <- self$results$interpTable
            table$deleteRows()
            
            focus <- self$options$contribFocus
            
            if (focus == "cols") {
                focus_contrib <- ca$col_contrib
                focus_princ <- ca$col_princ
                focus_names <- colnames(ca$N)
                n_focus <- ca$n_cols
                
                interp_princ <- ca$row_princ
                interp_cos2 <- ca$row_cos2
                interp_names <- rownames(ca$N)
            } else {
                focus_contrib <- ca$row_contrib
                focus_princ <- ca$row_princ
                focus_names <- rownames(ca$N)
                n_focus <- ca$n_rows
                
                interp_princ <- ca$col_princ
                interp_cos2 <- ca$col_cos2
                interp_names <- colnames(ca$N)
            }
            
            avg_contrib <- 100 / n_focus
            
            interp_primary <- data.frame(
                name = interp_names,
                primary_dim = integer(length(interp_names)),
                primary_sign = integer(length(interp_names)),
                stringsAsFactors = FALSE
            )
            
            for (i in seq_along(interp_names)) {
                max_corr <- 0
                primary_dim <- 1
                primary_sign <- 1
                
                for (k in 1:ca$n_axes) {
                    corr <- sqrt(interp_cos2[i, k]) * sign(interp_princ[i, k])
                    if (abs(corr) > abs(max_corr)) {
                        max_corr <- corr
                        primary_dim <- k
                        primary_sign <- sign(interp_princ[i, k])
                        if (primary_sign == 0) primary_sign <- 1
                    }
                }
                
                interp_primary$primary_dim[i] <- primary_dim
                interp_primary$primary_sign[i] <- primary_sign
            }
            
            definers <- data.frame(
                name = character(),
                dim = integer(),
                sign = integer(),
                contrib = numeric(),
                stringsAsFactors = FALSE
            )
            
            for (k in 1:ca$n_axes) {
                for (i in seq_along(focus_names)) {
                    contrib_val <- focus_contrib[i, k]
                    if (!is.na(contrib_val) && contrib_val >= avg_contrib) {
                        definers <- rbind(definers, data.frame(
                            name = focus_names[i],
                            dim = k,
                            sign = sign(focus_princ[i, k]),
                            contrib = contrib_val,
                            stringsAsFactors = FALSE
                        ))
                    }
                }
            }
            
            if (nrow(definers) > 0) {
                definers <- definers[order(definers$dim, -definers$contrib), ]
            }
            
            row_key <- 0
            
            for (d in seq_len(nrow(definers))) {
                def_name <- definers$name[d]
                def_dim <- definers$dim[d]
                def_sign <- definers$sign[d]
                
                associated <- interp_primary$name[
                    interp_primary$primary_dim == def_dim & 
                    interp_primary$primary_sign == def_sign
                ]
                
                if (length(associated) > 0) {
                    assoc_str <- paste(associated, collapse = ", ")
                } else {
                    assoc_str <- "\u2014"
                }
                
                sign_str <- if (def_sign > 0) "+" else "\u2212"
                direction_str <- paste0("Dim ", def_dim, " (", sign_str, ")")
                
                row_key <- row_key + 1
                table$addRow(rowKey = row_key, values = list(
                    category = def_name,
                    direction = direction_str,
                    association = assoc_str
                ))
            }
            
            orphans <- interp_primary$name[
                !sapply(seq_len(nrow(interp_primary)), function(i) {
                    any(definers$dim == interp_primary$primary_dim[i] & 
                        definers$sign == interp_primary$primary_sign[i])
                })
            ]
            
            if (length(orphans) > 0) {
                orphan_groups <- list()
                for (i in seq_along(orphans)) {
                    idx <- which(interp_primary$name == orphans[i])
                    key <- paste0(interp_primary$primary_dim[idx], "_", interp_primary$primary_sign[idx])
                    if (is.null(orphan_groups[[key]])) {
                        orphan_groups[[key]] <- list(
                            dim = interp_primary$primary_dim[idx],
                            sign = interp_primary$primary_sign[idx],
                            names = c()
                        )
                    }
                    orphan_groups[[key]]$names <- c(orphan_groups[[key]]$names, orphans[i])
                }
                
                for (grp in orphan_groups) {
                    sign_str <- if (grp$sign > 0) "+" else "\u2212"
                    direction_str <- paste0("Dim ", grp$dim, " (", sign_str, ")")
                    pole_label <- paste0("Dim ", grp$dim, " (", sign_str, ") pole")
                    
                    row_key <- row_key + 1
                    table$addRow(rowKey = row_key, values = list(
                        category = pole_label,
                        direction = direction_str,
                        association = paste(grp$names, collapse = ", ")
                    ))
                }
            }
            
            avg_pct <- sprintf("%.2f%%", avg_contrib)
            table$setNote(
                "contribNote",
                paste0(
                    "Only defining categories with contributions \u2265 average (",
                    avg_pct,
                    ") are shown. Low-contributing categories are omitted for clarity of interpretation. ",
                    "This table supports Contribution Biplot interpretation."
                )
            )
        },
        
        # =====================================================================
        # POPULATE SRD TABLE - Reports clusters for BOTH rows and columns
        # =====================================================================
        .populateSRDTable = function() {
            ca <- private$.caResult
            srd_rows <- private$.srdResultRows
            srd_cols <- private$.srdResultCols
            table <- self$results$srdTable
            
            row_var <- self$options$rows
            col_var <- self$options$cols
            
            row_key <- 0
            
            # -----------------------------------------------------------------
            # Row clusters section
            # -----------------------------------------------------------------
            if (!is.null(srd_rows) && length(srd_rows$groups) > 0) {
                # Add header row for row clusters
                row_key <- row_key + 1
                table$addRow(rowKey = row_key, values = list(
                    cluster = "",
                    members = paste0("Row variable: ", row_var),
                    nMembers = NA,
                    mergePvalue = NA
                ))
                table$addFormat(rowKey = row_key, col = "members", 
                                format = jmvcore::Cell.BEGIN_GROUP)
                
                for (i in seq_along(srd_rows$groups)) {
                    group <- srd_rows$groups[[i]]
                    row_key <- row_key + 1
                    table$addRow(rowKey = row_key, values = list(
                        cluster = i,
                        members = paste(group$members, collapse = ", "),
                        nMembers = length(group$members),
                        mergePvalue = group$merge_p
                    ))
                }
            }
            
            # -----------------------------------------------------------------
            # Column clusters section
            # -----------------------------------------------------------------
            if (!is.null(srd_cols) && length(srd_cols$groups) > 0) {
                # Add header row for column clusters
                row_key <- row_key + 1
                table$addRow(rowKey = row_key, values = list(
                    cluster = "",
                    members = paste0("Column variable: ", col_var),
                    nMembers = NA,
                    mergePvalue = NA
                ))
                table$addFormat(rowKey = row_key, col = "members", 
                                format = jmvcore::Cell.BEGIN_GROUP)
                
                for (i in seq_along(srd_cols$groups)) {
                    group <- srd_cols$groups[[i]]
                    row_key <- row_key + 1
                    table$addRow(rowKey = row_key, values = list(
                        cluster = i,
                        members = paste(group$members, collapse = ", "),
                        nMembers = length(group$members),
                        mergePvalue = group$merge_p
                    ))
                }
            }
            
            # -----------------------------------------------------------------
            # Handle case where neither variable has clusters
            # -----------------------------------------------------------------
            if (row_key == 0) {
                table$addRow(rowKey = 1, values = list(
                    cluster = NA,
                    members = "No clusters found (all categories are singletons)",
                    nMembers = NA,
                    mergePvalue = NA
                ))
            }
            
            # -----------------------------------------------------------------
            # Build explanatory note
            # -----------------------------------------------------------------
            note_parts <- c()
            
            # Row singletons
            if (!is.null(srd_rows) && length(srd_rows$singletons) > 0) {
                note_parts <- c(note_parts, paste0(
                    "Row singletons: ", 
                    paste(srd_rows$singletons, collapse = ", "),
                    "."
                ))
            }
            
            # Column singletons
            if (!is.null(srd_cols) && length(srd_cols$singletons) > 0) {
                note_parts <- c(note_parts, paste0(
                    "Column singletons: ", 
                    paste(srd_cols$singletons, collapse = ", "),
                    "."
                ))
            }
            
            distance_label <- if (ca$distance_type == "hellinger") {
                "Hellinger distance"
            } else {
                "\u03c7\u00b2 distance"
            }
            
            note_parts <- c(note_parts, paste0(
                "Categories with statistically indistinguishable profiles (", 
                distance_label, ", p > ",
                self$options$clusterAlpha, ") are grouped together."
            ))
            
            table$setNote('srdNote', paste(note_parts, collapse = " "))
        },
        
        # =====================================================================
        # POPULATE METHOD INFORMATION (HTML)
        # =====================================================================
        .populateMethodInfo = function() {
            
            if (!self$options$showMethodInfo) return()
            
            focus <- self$options$contribFocus
            distance_metric <- self$options$distanceMetric
            residual_type <- self$options$residualType
            
            html <- "<div style='font-family: sans-serif; line-height: 1.6; font-size: 0.9em;'>"
            
            # -----------------------------------------------------------------
            # Section 1: Introduction to Correspondence Analysis
            # -----------------------------------------------------------------
            html <- paste0(html, "
              <h3 style='color: #2874A6; margin-top: 1em;'>Correspondence Analysis: Overview</h3>
              <p>Correspondence Analysis (CA) is a multivariate technique for visualising the association 
              structure between the row and column categories of a contingency table. It transforms the 
              distances between category profiles into a low-dimensional Euclidean space, 
              allowing the relationships to be displayed graphically. Categories that are associated 
              (i.e., occur together more often than expected under independence) appear close together 
              in the plot, whilst those that repel each other appear far apart.</p>
              
              <p><strong>Key output:</strong> The primary output is the <em>contribution biplot</em>, 
              which simultaneously displays both row and column categories. One set of categories 
              defines the dimensions of the space (shown with lines to the origin), whilst the other 
              set is projected onto this space for interpretation. The distances between points have 
              geometric meaning related to their statistical similarity.</p>
              
              <p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>See: 
              Greenacre 2017; Beh & Lombardo 2014.</em></p>
            ")
            
            # -----------------------------------------------------------------
            # Section 2: Distance Metrics and Residual Types
            # -----------------------------------------------------------------
            html <- paste0(html, "
              <h3 style='color: #2874A6; margin-top: 1.5em;'>Distance Metrics and Residual Types</h3>
              
              <p>This module supports two distance metrics for CA. The choice affects how deviations 
              from independence are measured and how distances between categories are calculated.</p>
              
              <h4 style='color: #5D6D7E; margin-top: 1em;'>Pearson Residuals (\u03c7\u00b2 Distance)</h4>
              
              <p>The classical approach decomposes the matrix of <em>Pearson standardised residuals</em>:</p>
              
              <p style='text-align: center; font-style: italic;'>
                r<sub>ij</sub> = (p<sub>ij</sub> \u2212 p<sub>i\u00b7</sub>p<sub>\u00b7j</sub>) / \u221a(p<sub>i\u00b7</sub>p<sub>\u00b7j</sub>)
              </p>
              
              <p>The sum of squared Pearson residuals equals Pearson's chi-squared statistic divided by n. 
              The distances between row (or column) categories in the optimal CA space are <em>chi-squared distances</em>:</p>
              
              <p style='text-align: center; font-style: italic;'>
                d<sub>\u03c7\u00b2</sub>(i, i') = \u221a[\u03a3<sub>j</sub> (p<sub>ij</sub>/p<sub>i\u00b7</sub> \u2212 p<sub>i'j</sub>/p<sub>i'\u00b7</sub>)\u00b2 / p<sub>\u00b7j</sub>]
              </p>
              
              <p>This approach assumes that cell frequencies follow a Poisson distribution, implying that the 
              mean and variance of cell counts are equal. In practice, contingency tables often exhibit 
              <em>overdispersion</em> (variance exceeding the mean), which can affect the reliability of 
              inferences based on Pearson residuals.</p>
              
              <p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>See: 
              Greenacre 2017; Beh & Lombardo 2014.</em></p>
              
              <h4 style='color: #5D6D7E; margin-top: 1em;'>Adjusted Standardised Residuals</h4>
              <p>Following Beh (2012), the adjusted residuals account for the variance of Pearson residuals:
              \u0072\u0303<sub>ij</sub> = r<sub>ij</sub> \u00d7 \u221a((1\u2212p<sub>i\u00b7</sub>)(1\u2212p<sub>\u00b7j</sub>)).</p>
              <p>This adjustment has two key effects:</p>
              <ul style='margin-left: 1.5em;'>
                <li><strong>Stretching:</strong> Profile coordinates move further from the origin, 
                with the degree of stretching depending on marginal proportions. Categories with 
                small marginals experience greater stretching.</li>
                <li><strong>Enhanced detection:</strong> Cells that deviate significantly from 
                independence may be more readily identified, as the adjusted chi-squared statistic 
                \u0058\u0303\u00b2 is typically larger than X\u00b2.</li>
              </ul>
              <p>The relationship between standard and adjusted coordinates involves the stretching factor 
              \u03c9<sub>m</sub> for each dimension, such that \u0066\u0303<sub>im</sub> = f<sub>im</sub> \u00d7 
              \u221a(\u03c9<sub>m</sub> / (p<sub>i\u00b7</sub>(1\u2212p<sub>i\u00b7</sub>))).</p>
              
              <p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>See: 
              Beh 2012; Haberman 1973.</em></p>
              
              <h4 style='color: #5D6D7E; margin-top: 1em;'>Freeman-Tukey Residuals (Hellinger Distance)</h4>
              
              <p>An alternative approach decomposes the matrix of <em>Freeman-Tukey residuals</em> 
              (Beh, Lombardo & Alberti, 2018):</p>
              
              <p style='text-align: center; font-style: italic;'>
                r\u0303<sub>ij</sub> = 2(\u221ap<sub>ij</sub> \u2212 \u221a(p<sub>i\u00b7</sub>p<sub>\u00b7j</sub>))
              </p>
              
              <p>The Freeman-Tukey transformation stabilises variance, addressing the overdispersion problem 
              common in sparse contingency tables. The sum of squared Freeman-Tukey residuals yields the 
              Freeman-Tukey statistic T\u00b2, which (like \u03c7\u00b2) is asymptotically chi-squared distributed 
              with (I\u22121)(J\u22121) degrees of freedom.</p>
              
              <p>The distances between categories in this framework are <em>Hellinger distances</em>:</p>
              
              <p style='text-align: center; font-style: italic;'>
                d<sub>H</sub>(i, i') = 2\u221a[\u03a3<sub>j</sub> (\u221a(p<sub>ij</sub>/p<sub>i\u00b7</sub>) \u2212 \u221a(p<sub>i'j</sub>/p<sub>i'\u00b7</sub>))\u00b2]
              </p>
              
              <p><strong>Advantages of Freeman-Tukey residuals:</strong></p>
              <ul style='margin-left: 2em;'>
                <li>More robust to overdispersion in sparse tables</li>
                <li>Reconstituted cell frequencies are always non-negative</li>
                <li>Interpretation of distances and contributions proceeds analogously to classical CA</li>
              </ul>
              
              <p><strong>Recommendation:</strong> Use Pearson residuals as the default for well-behaved tables. 
              If the overdispersion diagnostic plot shows non-random patterns, consider switching to 
              Freeman-Tukey residuals.</p>
              
              <p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>See: 
              Beh, Lombardo & Alberti 2018; Bishop, Fienberg & Holland 2007.</em></p>
            ")
            
            # -----------------------------------------------------------------
            # Section 3: Visualisation Types
            # -----------------------------------------------------------------
            html <- paste0(html, "
              <h3 style='color: #2874A6; margin-top: 1.5em;'>Visualisation Types</h3>
              
              <h4 style='color: #5D6D7E;'>Symmetric CA Plot</h4>
              <p>The classic biplot showing both row and column categories in principal coordinates.
              Categories close together have similar profiles; those far apart differ substantially.
              The origin represents the average profile (complete independence).</p>
              
              <p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>See: 
              Greenacre 2017.</em></p>
              
              <h4 style='color: #5D6D7E; margin-top: 1em;'>Geometric Interpretation Plot</h4>
              <p>This visualisation, following Borg & Groenen (2005) and Yelland (2010), provides an 
              intuitive method for reading relative frequencies directly from the plot:</p>
              <ul style='margin-left: 1.5em;'>
                <li>A line is drawn through the origin and a selected <em>reference category</em>.</li>
                <li>Perpendicular projections are dropped from each category to this reference line.</li>
                <li><strong>Same side as reference:</strong> Categories whose projections fall on the 
                same side of the origin as the reference category occur <em>more frequently</em> than 
                average in association with the reference. The further from the origin, the higher 
                the relative frequency.</li>
                <li><strong>Opposite side:</strong> Categories projecting on the opposite side occur 
                <em>less frequently</em> than average. The further from the origin, the lower the 
                relative frequency.</li>
              </ul>
              <p><strong>Limitations:</strong> This method complements but does not replace standard 
              CA interpretation. It is most effective for small-to-medium tables and is particularly 
              useful for communicating results to non-expert audiences.</p>
              
              <p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>See: 
              Borg & Groenen 2005; Yelland 2010.</em></p>
              
              <h4 style='color: #5D6D7E; margin-top: 1em;'>Contribution Biplot</h4>
              
              <p>The contribution biplot (Greenacre, 2017) is a refinement of the standard symmetric biplot 
              that facilitates interpretation. It distinguishes between:</p>
              
              <ul style='margin-left: 2em;'>
                <li><strong>Plane-defining categories</strong> (shown in blue with lines to origin): These define 
                the dimensions of the space. Their positions show their contributions to each axis, with longer 
                lines indicating larger contributions.</li>
                <li><strong>Interpreted categories</strong> (shown in red as triangles): These are projected 
                onto the plane defined by the other variable. Their positions can be interpreted relative 
                to the dimension-defining categories.</li>
              </ul>
            ")
            
            if (focus == "cols") {
                html <- paste0(html, "
                  <p><strong>Current selection:</strong> Column categories define the plane; row categories are interpreted.</p>
                ")
            } else {
                html <- paste0(html, "
                  <p><strong>Current selection:</strong> Row categories define the plane; column categories are interpreted.</p>
                ")
            }
            
            html <- paste0(html, "
              <p><strong>Interpreting the biplot:</strong></p>
              <ul style='margin-left: 2em;'>
                <li>The <em>angle</em> between two plane-defining categories reflects their similarity 
                (small angle = similar profiles, 90\u00b0 = uncorrelated, 180\u00b0 = opposite profiles)</li>
                <li>The <em>length</em> of the line from origin to a plane-defining category reflects its 
                contribution to the displayed dimensions (longer = more important)</li>
                <li>Interpreted categories (triangles) that fall close to a plane-defining category are 
                strongly associated with it</li>
                <li>Above-average contributors are shown with solid symbols; below-average with hollow symbols</li>
              </ul>
              
              <p><strong>Dimension definition:</strong> The average contribution is 100%/k, where k is the 
              number of plane-defining categories. Categories contributing more than average are considered 
              <em>above-average contributors</em> and are the primary definers of that dimension.</p>
              
              <p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>See: 
              Greenacre 2017.</em></p>
            ")
            
            # -----------------------------------------------------------------
            # Section 4: Understanding the Output Tables
            # -----------------------------------------------------------------
            html <- paste0(html, "
              <h3 style='color: #2874A6; margin-top: 1.5em;'>Understanding the Output Tables</h3>
              
              <h4 style='color: #5D6D7E; margin-top: 1em;'>Dimension Definition Table</h4>
              
              <p>This table shows which plane-defining categories contribute most to each dimension. 
              For each dimension, categories are listed in descending order of contribution. The columns show:</p>
              <ul style='margin-left: 2em;'>
                <li><strong>Contribution %:</strong> The percentage of the dimension's inertia explained by this category</li>
                <li><strong>Cumulative %:</strong> Running total of contributions</li>
                <li><strong>Direction:</strong> Whether the category falls on the positive (+) or negative (\u2212) side of the axis</li>
              </ul>
              <p>Categories contributing above the average (100%/k) are the primary definers of that dimension. 
              The table helps identify the substantive meaning of each axis.</p>
              
              <h4 style='color: #5D6D7E; margin-top: 1em;'>Dimension Summary Table</h4>
              
              <p>This compact table summarises each dimension by showing only the above-average contributors, 
              grouped by their pole (positive or negative). It provides a quick interpretive label for each 
              dimension. For example, if Dimension 1 has 'Heavy, Chain' on the positive pole and 'None, Light' 
              on the negative pole, this dimension contrasts heavy/chain smokers against non/light smokers.</p>
              
              <h4 style='color: #5D6D7E; margin-top: 1em;'>Correlations with Dimensions Table</h4>
              
              <p>This table shows how well each <em>interpreted</em> category is represented by each dimension. 
              The values are signed correlations (\u00b1\u221acos\u00b2), where:</p>
              <ul style='margin-left: 2em;'>
                <li>The <strong>magnitude</strong> (0 to 1) indicates how well the dimension captures the category's 
                deviation from the centroid</li>
                <li>The <strong>sign</strong> indicates whether the category falls on the positive or negative side 
                of the axis</li>
                <li><strong>Primary:</strong> The dimension with the largest absolute correlation for this category</li>
                <li><strong>Quality:</strong> The sum of cos\u00b2 across all retained dimensions; values near 1 indicate 
                excellent representation in the reduced space</li>
              </ul>
              <p>Categories with low quality should be interpreted cautiously as their positions may be misleading.</p>
              
              <h4 style='color: #5D6D7E; margin-top: 1em;'>Interpretation Summary Table</h4>
              
              <p>This table directly links plane-defining categories (above-average contributors) to the 
              interpreted categories that are most strongly associated with them. For each defining category, 
              it lists the interpreted categories that fall on the same side of the relevant dimension, 
              making the association patterns explicit. This is the key table for substantive interpretation.</p>
              
              <p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>See: 
              Greenacre 2017.</em></p>
            ")
            
            # -----------------------------------------------------------------
            # Section 4b: Residual matrix
            # -----------------------------------------------------------------
            html <- paste0(html, "
              <h4 style='color: #5D6D7E; margin-top: 1em;'>Residual Matrix Table</h4>
              
              <p>This table displays the residuals for each cell of the contingency table in their natural 
              matrix layout, preserving the row × column structure. The type of residual depends on the 
              selected distance metric and residual type:</p>
              
              <ul style='margin-left: 2em;'>
                <li><strong>Standardised residuals:</strong> (O − E) / √E, where O is observed and E is expected count</li>
                <li><strong>Adjusted standardised residuals:</strong> Standardised residuals divided by 
                √[(1 − p<sub>i·</sub>)(1 − p<sub>·j</sub>)], accounting for marginal constraints</li>
                <li><strong>Freeman-Tukey residuals:</strong> √n × 2 × (√p<sub>ij</sub> − √(p<sub>i·</sub> × p<sub>·j</sub>)), 
                robust to overdispersion in sparse tables</li>
              </ul>
              
              <p><strong>Reading the table:</strong></p>
              <ul style='margin-left: 2em;'>
                <li><strong>Cell values:</strong> Residuals indicate departure from independence. Positive values 
                mean the cell occurs <em>more often</em> than expected; negative values mean <em>less often</em> 
                than expected.</li>
                <li><strong>Highlighted cells:</strong> Residuals with |z| > threshold (default 1.96) are highlighted, 
                indicating statistically noteworthy departures from independence at approximately p < 0.05.</li>
                <li><strong>Row margins (% of statistic):</strong> The rightmost column shows each row's percentage 
                contribution to the total test statistic (χ², χ²<sub>a</sub>, or Ť²). This identifies which row 
                categories contribute most to the overall association.</li>
                <li><strong>Column margins (% of statistic):</strong> The bottom row shows each column's percentage 
                contribution. Together with row margins, this reveals the structure of the association.</li>
                <li><strong>Highlighted margins:</strong> Marginal percentages exceeding the average contribution 
                (100%/I for rows, 100%/J for columns) are highlighted, identifying categories that contribute 
                disproportionately to the total association strength.</li>
              </ul>
              
              <p><strong>Interpretation strategy:</strong> First, identify highlighted marginal percentages to find 
              the most influential rows and columns. Then examine the highlighted cells within those rows/columns 
              to understand the specific associations driving the overall pattern. Positive residuals indicate 
              attraction (categories co-occur more than expected); negative residuals indicate repulsion.</p>
              
              <p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>See: 
              Beh, Lombardo & Alberti 2018 (Table 2); Greenacre 2017.</em></p>
            ")
            
            # -----------------------------------------------------------------
            # Section 5: Quality of Representation
            # -----------------------------------------------------------------
            html <- paste0(html, "
              <h3 style='color: #2874A6; margin-top: 1.5em;'>Quality of Representation (cos\u00b2)</h3>
              
              <p>The <em>quality</em> (cos\u00b2) of a category's representation measures how well its position 
              in the reduced-dimension plot captures its full profile distance from the centroid. It is 
              calculated as:</p>
              
              <p style='text-align: center; font-style: italic;'>
                cos\u00b2<sub>k</sub> = (principal coordinate on axis k)\u00b2 / (total distance from centroid)\u00b2
              </p>
              
              <p>The sum of cos\u00b2 values across all retained axes gives the <em>total quality</em> for that 
              category. A quality of 0.8, for example, means 80% of the category's distance from the centroid 
              is captured in the displayed plot. Categories with low quality may be poorly represented and 
              should be interpreted cautiously.</p>
              
              <p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>See: 
              Greenacre 2017; Beh & Lombardo 2014.</em></p>
            ")
            
            # -----------------------------------------------------------------
            # Section 6: SRD Clustering
            # -----------------------------------------------------------------
            html <- paste0(html, "
              <h3 style='color: #2874A6; margin-top: 1.5em;'>SRD Clustering</h3>
              
              <p>Simultaneous Reduction of Dimension (SRD; Orton & Tyers, 1991) is a hierarchical clustering 
              technique based on the same distance metric used in CA. It groups categories whose profiles 
              are statistically indistinguishable, reducing the complexity of sparse contingency tables.</p>
              
              <p><strong>Purpose:</strong> SRD identifies which interpreted categories have statistically 
              similar profiles and can therefore be considered equivalent in terms of their associations 
              with the plane-defining variable. This is particularly useful for:</p>
              <ul style='margin-left: 2em;'>
                <li>Simplifying complex tables with many categories</li>
                <li>Identifying substantive groupings among categories</li>
                <li>Reducing noise from sampling variability in sparse tables</li>
              </ul>
              
              <p><strong>Algorithm:</strong></p>
              <ol style='margin-left: 2em;'>
                <li>Calculate the weighted chi-squared distance between every pair of category profiles</li>
                <li>Find the pair with the smallest weighted distance</li>
                <li>Test whether this distance is statistically significant (using chi-squared test with 
                k\u22121 degrees of freedom, where k is the number of columns in the profile)</li>
                <li>If p > \u03b1, merge the pair and repeat from step 1</li>
                <li>Stop when all remaining pairs have p \u2264 \u03b1</li>
              </ol>
              
              <p>The weighted distance formula is:</p>
              <p style='text-align: center; font-style: italic;'>
                d\u00b2<sub>weighted</sub> = d\u00b2 \u00d7 (n<sub>i</sub> \u00d7 n<sub>j</sub>) / (n<sub>i</sub> + n<sub>j</sub>)
              </p>
              
              <p><strong>Interpretation:</strong> Categories within the same cluster have profiles that are 
              not statistically distinguishable at the chosen \u03b1 level. Categories that remain as 
              <em>singletons</em> (not merged with any others) have profiles that are statistically 
              distinct from all other categories. In the biplot, clusters are shown as dashed convex 
              hulls around their members when the 'Show SRD clusters' option is enabled.</p>
              
              <p><strong>SRD Clusters Table:</strong> Lists each cluster with its member categories, 
              number of members, and the p-value at which the final merge occurred. Clusters with 
              high merge p-values contain categories with very similar profiles.</p>
              
              <p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>See: 
              Orton & Tyers 1991.</em></p>
            ")
            
            # -----------------------------------------------------------------
            # Section 7: Overdispersion Diagnostic Plot
            # -----------------------------------------------------------------
            html <- paste0(html, "
              <h3 style='color: #2874A6; margin-top: 1.5em;'>Overdispersion Diagnostic Plot</h3>
              
              <p>The overdispersion diagnostic plot (Beh, Lombardo & Alberti, 2018, Figure 1) helps 
              assess whether the data exhibit overdispersion, which would suggest that Freeman-Tukey 
              residuals are more appropriate than Pearson residuals for the correspondence analysis.</p>
              
              <p><strong>Axes:</strong></p>
              <ul style='margin-left: 2em;'>
                <li><strong>X-axis:</strong> Standard deviation under independence, \u221a(n \u00d7 p<sub>i\u00b7</sub> \u00d7 p<sub>\u00b7j</sub>). 
                Cells with small expected counts appear on the left; cells with large expected counts appear on the right.</li>
                <li><strong>Y-axis:</strong> Pearson residual, (n<sub>ij</sub> \u2212 e<sub>ij</sub>) / \u221ae<sub>ij</sub>. 
                This measures how much each cell deviates from independence.</li>
              </ul>
              
              <p><strong>How to interpret the plot:</strong></p>
              <ul style='margin-left: 2em;'>
                <li><strong>Random scatter (no overdispersion):</strong> If points are randomly distributed 
                across the plot with no systematic pattern, the Poisson assumption is reasonable and Pearson 
                residuals are appropriate for the CA.</li>
                <li><strong>Problematic patterns (overdispersion present):</strong> The following patterns 
                suggest overdispersion:
                  <ul style='margin-left: 1.5em; margin-top: 0.3em;'>
                    <li><em>Clustering to the left:</em> Most points have small expected SD, indicating 
                    many sparse cells</li>
                    <li><em>Diagonal banding:</em> Points form parallel diagonal lines (common in sparse 
                    tables with many small counts)</li>
                    <li><em>Fan shape:</em> The spread of residuals increases or decreases systematically 
                    with expected SD</li>
                  </ul>
                If any of these patterns are present (as in Beh et al., 2018, Figure 1), Freeman-Tukey 
                residuals provide more reliable results.</li>
              </ul>
              
              <p><strong>Why overdispersion matters:</strong> Pearson residuals assume that cell frequencies 
              follow a Poisson distribution where the mean equals the variance. When this assumption is 
              violated (overdispersion), the variance exceeds the mean, and Pearson-based inferences become 
              unreliable. The Freeman-Tukey transformation stabilises the variance, making it constant 
              (equal to 1/4) regardless of the expected count. This is why Freeman-Tukey residuals are 
              recommended when overdispersion is detected.</p>
              
              <p><strong>Recommendation:</strong> If the diagnostic plot shows clustering to the left, 
              switch the Distance Metric option from 'Pearson (\u03c7\u00b2 distance)' to 'Freeman-Tukey 
              (Hellinger distance)' to obtain more robust results.</p>
              
              <p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>See: 
              Beh, Lombardo & Alberti 2018.</em></p>
            ")
            
            # -----------------------------------------------------------------
            # Section 8: Inertia and Malinvaud's Test
            # -----------------------------------------------------------------
            html <- paste0(html, "
              <h3 style='color: #2874A6; margin-top: 1.5em;'>Inertia and Malinvaud's Test</h3>
              
              <p><em>Total inertia</em> is the sum of squared singular values (eigenvalues) from the SVD 
              decomposition. It equals the test statistic (\u03c7\u00b2 or T\u00b2) divided by the sample size n, and 
              measures the total association strength in the table. Each dimension captures a proportion 
              of this total inertia, shown in the inertia decomposition table.</p>
              
              <p><strong>Malinvaud's test</strong> (Saporta, 2006) assesses the statistical significance of the residual 
              inertia after extracting k dimensions. It tests whether the remaining dimensions contain 
              significant structure beyond what has been captured. A non-significant result (p > 0.05) 
              suggests that k dimensions adequately represent the data; a significant result suggests 
              additional dimensions may be informative.</p>
              
              <p>The test statistic is: n \u00d7 (inertia in dimensions k+1 through M), where M = min(I,J)\u22121. 
              This is compared to a chi-squared distribution with (I\u2212k)(J\u2212k) degrees of freedom.</p>
              
              <p style='font-size: 0.85em; color: #666; margin-top: 8px; margin-bottom: 12px;'><em>See: 
              Saporta 2006.</em></p>
            ")
            
            # -----------------------------------------------------------------
            # Section 9: Seriated Contingency Table
            # -----------------------------------------------------------------
            html <- paste0(html, "
              <h3 style='color: #2874A6; margin-top: 1.5em;'>Seriated Contingency Table</h3>
              
              <p>The seriated contingency table reorders rows and columns based on their principal 
              coordinates from the correspondence analysis, revealing gradient structures in the data. 
              This technique, also known as CA-based ordination or matrix seriation, is widely used in 
              ecology, archaeology, and other fields where detecting underlying gradients is important.</p>
              
              <h4 style='color: #5D6D7E; margin-top: 1em;'>Sorting Algorithm</h4>
              
              <p>Both rows and columns are sorted by their principal coordinates on the X-axis dimension 
              (as selected in Analysis Settings), ordered from most negative to most positive. This 
              directly reflects the horizontal arrangement of categories in the symmetric CA plot.</p>
              
              <p>The X-axis typically displays the first principal dimension, which captures the largest 
              proportion of inertia (association) in the data. Sorting by this dimension therefore 
              reveals the primary gradient structure. To explore gradients along other dimensions, 
              change the 'Dimension for X-axis' setting in Analysis Settings.</p>
              
              <h4 style='color: #5D6D7E; margin-top: 1em;'>Reading the Table</h4>
              
              <ul style='margin-left: 2em;'>
                <li><strong>Cell values:</strong> The observed frequencies, reordered to reveal diagonal 
                or gradient patterns. If a strong gradient exists, high values should tend to cluster 
                along the diagonal.</li>
                <li><strong>Row marginals (rightmost columns):</strong> The principal coordinates for each 
                row category on the X-axis and Y-axis dimensions. These show the exact position of each 
                row in the CA space.</li>
                <li><strong>Column marginals (bottom rows):</strong> The principal coordinates for each 
                column category on the X-axis and Y-axis dimensions. These show the exact position of 
                each column in the CA space.</li>
              </ul>
              
              <h4 style='color: #5D6D7E; margin-top: 1em;'>Interpretation</h4>
              
              <p>A well-seriated table reveals the underlying structure of the data:</p>
              
              <ul style='margin-left: 2em;'>
                <li><strong>Diagonal pattern:</strong> If high frequencies concentrate along the diagonal 
                from top-left to bottom-right (or vice versa), this indicates a strong gradient where 
                certain row categories consistently co-occur with certain column categories. In 
                archaeological contexts, this often reflects chronological change; in ecological data, 
                it may reveal environmental gradients.</li>
                <li><strong>Block structure:</strong> Clusters of high values in rectangular blocks suggest 
                groups of row and column categories that are mutually associated.</li>
                <li><strong>Scattered pattern:</strong> If no clear pattern emerges, the association 
                structure may be complex or weak, or the primary gradient may lie along a different 
                dimension.</li>
              </ul>
              
              <p><strong>Relationship to the plot:</strong> The table ordering directly corresponds to 
              the horizontal (X-axis) arrangement in the symmetric CA plot. Categories appearing on the 
              left side of the plot (negative coordinates) appear first in the table; those on the right 
              (positive coordinates) appear last.</p>
              
              <p><strong>Changing the sorting dimension:</strong> The seriated table sorts by whichever 
              dimension is selected as the X-axis in Analysis Settings. To seriate by a different 
              dimension, change the 'Dimension for X-axis' value. The Y-axis dimension coordinates are 
              still displayed in the marginals for reference, but do not affect the sort order.</p>
            ")
            
            html <- paste0(html, "</div>")
            
            self$results$methodInfo$setContent(html)
        },
        
        # =====================================================================
        # PLOT: SCREE
        # =====================================================================
        .plotScree = function(image, ggtheme, theme, ...) {
            
            if (is.null(private$.caResult)) return(FALSE)
            if (!self$options$showScreePlot) return(FALSE)
            
            ca <- private$.caResult
            
            scree_df <- data.frame(
                dim = factor(1:ca$n_axes),
                percent = ca$inertia_proportion * 100,
                cumulative = ca$inertia_cumulative * 100
            )
            
            p <- ggplot2::ggplot(scree_df, ggplot2::aes(x = dim))
            p <- p + ggplot2::geom_bar(
                ggplot2::aes(y = percent),
                stat = "identity",
                fill = "#2171B5",
                colour = NA,
                width = 0.7
            )
            p <- p + ggplot2::geom_line(
                ggplot2::aes(y = cumulative, group = 1),
                colour = "#CB181D",
                linewidth = 1
            )
            p <- p + ggplot2::geom_point(
                ggplot2::aes(y = cumulative),
                colour = "#CB181D",
                size = 3
            )
            p <- p + ggplot2::geom_text(
                ggplot2::aes(y = cumulative, label = sprintf("%.1f%%", cumulative)),
                vjust = -0.5, size = 3, colour = "#CB181D"
            )
            
            p <- p + ggplot2::labs(
                title = paste0("Scree Plot (", ca$resid_label, " residuals)"),
                x = "Dimension",
                y = "% Inertia"
            )
            
            p <- p + ggplot2::theme(
                panel.background = ggplot2::element_rect(fill = "white", colour = NA),
                panel.border = ggplot2::element_rect(fill = NA, colour = "grey60", linewidth = 0.5),
                panel.grid.major.y = ggplot2::element_line(colour = "grey90"),
                panel.grid.minor = ggplot2::element_blank(),
                panel.grid.major.x = ggplot2::element_blank(),
                axis.title = ggplot2::element_text(size = 10, colour = "grey30"),
                axis.text = ggplot2::element_text(size = 9, colour = "grey40"),
                plot.title = ggplot2::element_text(size = 11, face = "bold", colour = "grey20", hjust = 0.5)
            )
            
            print(p)
            return(TRUE)
        },
        
        # =====================================================================
        # PLOT: SYMMETRIC CA
        # =====================================================================
        .plotSymmetric = function(image, ggtheme, theme, ...) {
            
            # Early exit if data not ready
            if (is.null(self$options$rows) || is.null(self$options$cols)) return(FALSE)
            if (is.null(private$.contingencyTable)) return(FALSE)
            if (is.null(private$.caResult)) return(FALSE)
            if (!self$options$showSymmetricPlot) return(FALSE)
            
            ca <- private$.caResult
            
            # Additional validation - ensure we have valid coordinates
            if (is.null(ca$row_princ) || is.null(ca$col_princ)) return(FALSE)
            if (nrow(ca$row_princ) == 0 || nrow(ca$col_princ) == 0) return(FALSE)
            
            dim1 <- self$options$dim1
            dim2 <- self$options$dim2
            dotSize <- self$options$dotSize
            labelSize <- self$options$labelSize
            showClusters <- self$options$symmetricShowClusters
            clusterTarget <- self$options$symmetricClusterTarget
            showContrib <- self$options$symmetricShowContrib
            showQuality <- self$options$symmetricShowQuality
            showPoints <- self$options$symmetricShowPoints
            
            # Determine which point types to display
            showRowPoints <- showPoints %in% c("both", "rows")
            showColPoints <- showPoints %in% c("both", "cols")
            
            # Check dimension validity
            if (dim1 > ca$n_axes || dim2 > ca$n_axes) {
                return(FALSE)
            }
            
            # Contribution to the displayed plane (sum of contributions to dim1 and dim2)
            row_plane_contrib <- rowSums(ca$row_contrib[, c(dim1, dim2), drop = FALSE])
            col_plane_contrib <- rowSums(ca$col_contrib[, c(dim1, dim2), drop = FALSE])
            
            # Quality on the displayed plane (sum of cos² for dim1 and dim2)
            row_plane_quality <- rowSums(ca$row_cos2[, c(dim1, dim2), drop = FALSE])
            col_plane_quality <- rowSums(ca$col_cos2[, c(dim1, dim2), drop = FALSE])
            
            # Prepare data frames
            row_df <- data.frame(
                x = ca$row_princ[, dim1],
                y = ca$row_princ[, dim2],
                label = rownames(ca$N),
                type = "Row",
                contrib = row_plane_contrib,
                quality = row_plane_quality,
                stringsAsFactors = FALSE
            )
            
            col_df <- data.frame(
                x = ca$col_princ[, dim1],
                y = ca$col_princ[, dim2],
                label = colnames(ca$N),
                type = "Column",
                contrib = col_plane_contrib,
                quality = col_plane_quality,
                stringsAsFactors = FALSE
            )
            
            # Axis labels
            xlab <- sprintf("Dimension %d (%.1f%%)", dim1, ca$inertia_proportion[dim1] * 100)
            ylab <- sprintf("Dimension %d (%.1f%%)", dim2, ca$inertia_proportion[dim2] * 100)
            title <- paste0("Symmetric CA Plot (", ca$resid_label, " residuals)")
            
            # Build plot
            p <- ggplot2::ggplot()
            
            # Reference lines
            p <- p + ggplot2::geom_hline(yintercept = 0, linewidth = 0.5, colour = "grey70", linetype = "dashed")
            p <- p + ggplot2::geom_vline(xintercept = 0, linewidth = 0.5, colour = "grey70", linetype = "dashed")
            
            # SRD clusters (if requested) - only draw for visible point types
            if (showClusters) {
                row_colours <- c("#E41A1C", "#FB6A4A", "#FC9272", "#FCBBA1")
                col_colours <- c("#2171B5", "#4292C6", "#6BAED6", "#9ECAE1")
                
                draw_clusters <- function(p, srd_result, point_df, colours, linetype = "dashed") {
                    if (is.null(srd_result) || length(srd_result$groups) == 0) return(p)
                    
                    for (g in seq_along(srd_result$groups)) {
                        members <- srd_result$groups[[g]]$members
                        member_idx <- which(point_df$label %in% members)
                        cluster_colour <- colours[(g - 1) %% length(colours) + 1]
                        
                        if (length(member_idx) >= 3) {
                            hull_pts <- point_df[member_idx, c("x", "y")]
                            hull_order <- grDevices::chull(hull_pts$x, hull_pts$y)
                            hull_df <- hull_pts[c(hull_order, hull_order[1]), ]
                            
                            p <- p + ggplot2::geom_polygon(
                                data = hull_df,
                                ggplot2::aes(x = x, y = y),
                                fill = cluster_colour,
                                alpha = 0.15,
                                colour = cluster_colour,
                                linetype = linetype,
                                linewidth = 0.5
                            )
                        } else if (length(member_idx) == 2) {
                            line_pts <- point_df[member_idx, c("x", "y")]
                            
                            p <- p + ggplot2::geom_segment(
                                data = data.frame(
                                    x = line_pts$x[1], y = line_pts$y[1],
                                    xend = line_pts$x[2], yend = line_pts$y[2]
                                ),
                                ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
                                colour = cluster_colour,
                                linetype = linetype,
                                linewidth = 0.8
                            )
                        }
                    }
                    return(p)
                }
                
                # Only draw clusters for point types that are visible
                if (showRowPoints && (clusterTarget == "rows" || clusterTarget == "both")) {
                    p <- draw_clusters(p, private$.srdResultRows, row_df, row_colours, "dashed")
                }
                if (showColPoints && (clusterTarget == "cols" || clusterTarget == "both")) {
                    p <- draw_clusters(p, private$.srdResultCols, col_df, col_colours, "dotted")
                }
            }
            
            # Determine which aesthetics to map
            # Note: contribution has no "both" option
            # Only apply aesthetics if the corresponding points are visible
            useContribRows <- showContrib == "rows" && showRowPoints
            useContribCols <- showContrib == "cols" && showColPoints
            useQualityRows <- showQuality %in% c("rows", "both") && showRowPoints
            useQualityCols <- showQuality %in% c("cols", "both") && showColPoints
            
            # -----------------------------------------------------------------
            # ROW POINTS (only if visible)
            # -----------------------------------------------------------------
            if (showRowPoints) {
                if (useQualityRows && useContribRows) {
                    # Both quality (colour) and contribution (size)
                    p <- p + ggplot2::geom_point(
                        data = row_df,
                        ggplot2::aes(x = x, y = y, fill = quality, size = contrib),
                        colour = "#67000D", shape = 21, stroke = 0.6
                    )
                } else if (useQualityRows) {
                    # Quality only (colour)
                    p <- p + ggplot2::geom_point(
                        data = row_df,
                        ggplot2::aes(x = x, y = y, fill = quality),
                        colour = "#67000D", shape = 21, size = dotSize, stroke = 0.6
                    )
                } else if (useContribRows) {
                    # Contribution only (size)
                    p <- p + ggplot2::geom_point(
                        data = row_df,
                        ggplot2::aes(x = x, y = y, size = contrib),
                        colour = "#CB181D", fill = "white", shape = 21, stroke = 0.95
                    )
                } else {
                    # Neither - default appearance
                    p <- p + ggplot2::geom_point(
                        data = row_df,
                        ggplot2::aes(x = x, y = y),
                        colour = "#CB181D", fill = "white", shape = 21, size = dotSize, stroke = 0.95
                    )
                }
            }
            
            # -----------------------------------------------------------------
            # COLUMN POINTS (only if visible)
            # -----------------------------------------------------------------
            if (showColPoints) {
                if (useQualityCols && useContribCols) {
                    # Both quality (colour) and contribution (size)
                    p <- p + ggplot2::geom_point(
                        data = col_df,
                        ggplot2::aes(x = x, y = y, colour = quality, size = contrib),
                        shape = 17
                    )
                } else if (useQualityCols) {
                    # Quality only (colour)
                    p <- p + ggplot2::geom_point(
                        data = col_df,
                        ggplot2::aes(x = x, y = y, colour = quality),
                        shape = 17, size = dotSize
                    )
                } else if (useContribCols) {
                    # Contribution only (size)
                    p <- p + ggplot2::geom_point(
                        data = col_df,
                        ggplot2::aes(x = x, y = y, size = contrib),
                        colour = "#2171B5", shape = 17
                    )
                } else {
                    # Neither - default appearance
                    p <- p + ggplot2::geom_point(
                        data = col_df,
                        ggplot2::aes(x = x, y = y),
                        colour = "#2171B5", shape = 17, size = dotSize
                    )
                }
            }
            
            # -----------------------------------------------------------------
            # SCALES
            # -----------------------------------------------------------------
            # Size scale (contribution)
            if (useContribRows || useContribCols) {
                # Determine range based on which categories are shown
                all_contribs <- c()
                if (useContribRows) all_contribs <- c(all_contribs, row_df$contrib)
                if (useContribCols) all_contribs <- c(all_contribs, col_df$contrib)
                
                p <- p + ggplot2::scale_size_continuous(
                    name = "Contribution (%)",
                    range = c(dotSize * 0.5, dotSize * 2.5),
                    limits = c(0, max(all_contribs, na.rm = TRUE))
                )
            }
            
            # Colour/fill scales (quality)
            # We need separate handling for fill (rows) and colour (columns)
            if (useQualityRows && useQualityCols) {
                # Both - use fill for rows, colour for columns
                p <- p + ggplot2::scale_fill_gradient(
                    low = "#FCBBA1", high = "#67000D",
                    name = "Row quality (cos\u00b2)",
                    limits = c(0, 1)
                )
                p <- p + ggplot2::scale_colour_gradient(
                    low = "#C6DBEF", high = "#08519C",
                    name = "Col quality (cos\u00b2)",
                    limits = c(0, 1)
                )
            } else if (useQualityRows) {
                # Rows only - red gradient
                p <- p + ggplot2::scale_fill_gradient(
                    low = "#FCBBA1", high = "#67000D",
                    name = "Quality (cos\u00b2)",
                    limits = c(0, 1)
                )
            } else if (useQualityCols) {
                # Columns only - blue gradient
                p <- p + ggplot2::scale_colour_gradient(
                    low = "#C6DBEF", high = "#08519C",
                    name = "Quality (cos\u00b2)",
                    limits = c(0, 1)
                )
            }
            
            # -----------------------------------------------------------------
            # LABELS (only for visible point types)
            # -----------------------------------------------------------------
            row_df$label_colour <- "#CB181D"
            col_df$label_colour <- "#2171B5"
            
            # Build combined labels only from visible point types
            label_parts <- list()
            if (showRowPoints) {
                label_parts[[length(label_parts) + 1]] <- row_df[, c("x", "y", "label", "label_colour"), drop = FALSE]
            }
            if (showColPoints) {
                label_parts[[length(label_parts) + 1]] <- col_df[, c("x", "y", "label", "label_colour"), drop = FALSE]
            }
            
            if (length(label_parts) > 0) {
                combined_labels <- do.call(rbind, label_parts)
                
                p <- p + ggrepel::geom_text_repel(
                    data = combined_labels,
                    ggplot2::aes(x = x, y = y, label = label),
                    colour = combined_labels$label_colour,
                    size = labelSize,
                    max.overlaps = 30,
                    box.padding = 0.4,
                    point.padding = 0.3,
                    force = 2,
                    force_pull = 0.3,
                    min.segment.length = 0.5,
                    segment.colour = "grey50",
                    segment.size = 0.4,
                    seed = 42
                )
            }
            
            # -----------------------------------------------------------------
            # THEME
            # -----------------------------------------------------------------
            p <- p + ggplot2::labs(title = title, x = xlab, y = ylab)
            
            # Determine legend position based on what's shown
            hasLegend <- (useContribRows || useContribCols || useQualityRows || useQualityCols)
            
            p <- p + ggplot2::theme(
                panel.background = ggplot2::element_rect(fill = "white", colour = NA),
                panel.border = ggplot2::element_rect(fill = NA, colour = "grey60", linewidth = 0.5),
                panel.grid = ggplot2::element_blank(),
                axis.title = ggplot2::element_text(size = 10, colour = "grey30"),
                axis.text = ggplot2::element_text(size = 9, colour = "grey40"),
                plot.title = ggplot2::element_text(size = 11, face = "bold", colour = "grey20", hjust = 0.5),
                legend.position = if (hasLegend) "right" else "none",
                legend.background = ggplot2::element_rect(fill = "white", colour = NA),
                legend.title = ggplot2::element_text(size = 9),
                legend.text = ggplot2::element_text(size = 8)
            )
            
            if (self$options$equalScale) {
                p <- p + ggplot2::coord_equal()
            }
            
            # -----------------------------------------------------------------
            # CAPTION (dynamically reflects which points are shown)
            # -----------------------------------------------------------------
            if (showRowPoints && showColPoints) {
                caption_text <- "Red (\u25cb) = Row categories; Blue (\u25b2) = Column categories"
            } else if (showRowPoints) {
                caption_text <- "Red (\u25cb) = Row categories"
            } else {
                caption_text <- "Blue (\u25b2) = Column categories"
            }
            
            if (showClusters) {
                # Only mention clusters for visible point types
                cluster_parts <- c()
                if (showRowPoints && (clusterTarget == "rows" || clusterTarget == "both")) {
                    cluster_parts <- c(cluster_parts, "Dashed red = Row clusters")
                }
                if (showColPoints && (clusterTarget == "cols" || clusterTarget == "both")) {
                    cluster_parts <- c(cluster_parts, "Dotted blue = Column clusters")
                }
                if (length(cluster_parts) > 0) {
                    caption_text <- paste0(caption_text, "\n", paste(cluster_parts, collapse = "; "))
                }
            }
            
            p <- p + ggplot2::labs(caption = caption_text)
            p <- p + ggplot2::theme(
                plot.caption = ggplot2::element_text(size = 8, colour = "grey50", hjust = 0)
            )
            
            # Wrap print in tryCatch to handle ggrepel viewport errors
            tryCatch({
                print(p)
                return(TRUE)
            }, error = function(e) {
                # Silently fail if viewport error occurs during initial render
                return(FALSE)
            })
        },
        
        # =====================================================================
        # PLOT: GEOMETRIC INTERPRETATION
        # =====================================================================
        .plotGeometric = function(image, ggtheme, theme, ...) {
            
            if (is.null(private$.caResult)) return(FALSE)
            if (!self$options$showGeometricPlot) return(FALSE)
            
            ca <- private$.caResult
            
            dim1 <- self$options$dim1
            dim2 <- self$options$dim2
            dotSize <- self$options$dotSize
            labelSize <- self$options$labelSize
            refVariable <- self$options$geometricRefVariable
            
            # Get reference category based on which variable is selected
            if (refVariable == "rows") {
                refCategory <- self$options$geometricRefCategoryRow
            } else {
                refCategory <- self$options$geometricRefCategoryCol
            }
            
            # Check dimension validity
            if (dim1 > ca$n_axes || dim2 > ca$n_axes) {
                return(FALSE)
            }
            
            # Validate reference category
            if (is.null(refCategory) || refCategory == "") {
                # Default to first category of the selected variable
                if (refVariable == "rows") {
                    refCategory <- rownames(ca$N)[1]
                } else {
                    refCategory <- colnames(ca$N)[1]
                }
            }
            
            # Get coordinates based on reference variable
            if (refVariable == "rows") {
                ref_coord <- ca$row_princ
                other_coord <- ca$col_princ
                ref_names <- rownames(ca$N)
                other_names <- colnames(ca$N)
                ref_label <- "Row"
                other_label <- "Column"
                ref_colour <- "#CB181D"
                other_colour <- "#2171B5"
            } else {
                ref_coord <- ca$col_princ
                other_coord <- ca$row_princ
                ref_names <- colnames(ca$N)
                other_names <- rownames(ca$N)
                ref_label <- "Column"
                other_label <- "Row"
                ref_colour <- "#2171B5"
                other_colour <- "#CB181D"
            }
            
            # Check if reference category exists
            if (!(refCategory %in% ref_names)) {
                # Fall back to first category
                refCategory <- ref_names[1]
            }
            
            ref_idx <- which(ref_names == refCategory)
            
            # Reference category coordinates
            ref_x <- ref_coord[ref_idx, dim1]
            ref_y <- ref_coord[ref_idx, dim2]
            
            # Calculate angle theta for the reference line
            theta <- atan2(ref_y, ref_x)
            
            # Prepare data frames
            ref_df <- data.frame(
                x = ref_coord[, dim1],
                y = ref_coord[, dim2],
                label = ref_names,
                is_reference = ref_names == refCategory,
                stringsAsFactors = FALSE
            )
            
            other_df <- data.frame(
                x = other_coord[, dim1],
                y = other_coord[, dim2],
                label = other_names,
                stringsAsFactors = FALSE
            )
            
            # Rotate other coordinates to find projections
            other_df$rot_x <- cos(-theta) * other_df$x - sin(-theta) * other_df$y
            other_df$rot_y <- sin(-theta) * other_df$x + cos(-theta) * other_df$y
            
            # Project back to original coordinate system
            other_df$proj_x <- cos(theta) * other_df$rot_x
            other_df$proj_y <- sin(theta) * other_df$rot_x
            
            # Determine if projection is on same side as reference (positive rot_x)
            other_df$same_side <- other_df$rot_x > 0
            
            # Create segments data frame
            segments_df <- data.frame(
                x = other_df$x,
                y = other_df$y,
                xend = other_df$proj_x,
                yend = other_df$proj_y,
                same_side = other_df$same_side,
                stringsAsFactors = FALSE
            )
            
            # Axis labels
            xlab <- sprintf("Dimension %d (%.1f%%)", dim1, ca$inertia_proportion[dim1] * 100)
            ylab <- sprintf("Dimension %d (%.1f%%)", dim2, ca$inertia_proportion[dim2] * 100)
            title <- paste0("Geometric Interpretation (ref: ", refCategory, ")")
            
            # Build plot
            p <- ggplot2::ggplot()
            
            # Reference lines (axes)
            p <- p + ggplot2::geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey70", linetype = "dashed")
            p <- p + ggplot2::geom_vline(xintercept = 0, linewidth = 0.3, colour = "grey70", linetype = "dashed")
            
            # Line through origin and reference category
            p <- p + ggplot2::geom_abline(
                slope = tan(theta),
                intercept = 0,
                colour = "black",
                linewidth = 0.5
            )
            
            # Projection segments - solid for same side, dashed for opposite
            segments_same <- segments_df[segments_df$same_side, ]
            segments_opp <- segments_df[!segments_df$same_side, ]
            
            if (nrow(segments_same) > 0) {
                p <- p + ggplot2::geom_segment(
                    data = segments_same,
                    ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
                    linetype = "solid",
                    linewidth = 0.4,
                    colour = "grey50"
                )
            }
            
            if (nrow(segments_opp) > 0) {
                p <- p + ggplot2::geom_segment(
                    data = segments_opp,
                    ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
                    linetype = "dotted",
                    linewidth = 0.4,
                    colour = "grey50"
                )
            }
            
            # Reference variable categories - white-filled circles with outline
            # Reference category highlighted
            ref_other <- ref_df[!ref_df$is_reference, ]
            ref_main <- ref_df[ref_df$is_reference, ]
            
            # Other categories from reference variable (greyed out)
            if (nrow(ref_other) > 0) {
                p <- p + ggplot2::geom_point(
                    data = ref_other,
                    ggplot2::aes(x = x, y = y),
                    colour = "grey60", fill = "white", shape = 21, size = dotSize, stroke = 0.8
                )
            }
            
            # Reference category itself (highlighted)
            p <- p + ggplot2::geom_point(
                data = ref_main,
                ggplot2::aes(x = x, y = y),
                colour = ref_colour, fill = "white", shape = 21, size = dotSize * 1.3, stroke = 1.2
            )
            
            # Other variable categories (triangles)
            p <- p + ggplot2::geom_point(
                data = other_df,
                ggplot2::aes(x = x, y = y),
                colour = other_colour, shape = 17, size = dotSize
            )
            
            # Combine all labels for unified repulsion
            label_parts <- list()
            if (nrow(ref_other) > 0) {
                ref_other$colour <- "grey60"
                ref_other$fontface <- "plain"
                label_parts[[length(label_parts) + 1]] <- ref_other[, c("x", "y", "label", "colour", "fontface"), drop = FALSE]
            }
            ref_main$colour <- ref_colour
            ref_main$fontface <- "bold"
            label_parts[[length(label_parts) + 1]] <- ref_main[, c("x", "y", "label", "colour", "fontface"), drop = FALSE]
            other_df$colour <- other_colour
            other_df$fontface <- "plain"
            label_parts[[length(label_parts) + 1]] <- other_df[, c("x", "y", "label", "colour", "fontface"), drop = FALSE]
            
            combined_labels <- do.call(rbind, label_parts)
            
            p <- p + ggrepel::geom_text_repel(
                data = combined_labels,
                ggplot2::aes(x = x, y = y, label = label, colour = colour, fontface = fontface),
                size = labelSize,
                max.overlaps = 30,
                box.padding = 0.4,
                point.padding = 0.3,
                force = 2,
                force_pull = 0.3,
                min.segment.length = 0.5,
                segment.colour = "grey50",
                segment.size = 0.4,
                seed = 42
            )
            p <- p + ggplot2::scale_colour_identity()
            
            # Theme
            p <- p + ggplot2::labs(title = title, x = xlab, y = ylab)
            p <- p + ggplot2::theme(
                panel.background = ggplot2::element_rect(fill = "white", colour = NA),
                panel.border = ggplot2::element_rect(fill = NA, colour = "grey60", linewidth = 0.5),
                panel.grid = ggplot2::element_blank(),
                axis.title = ggplot2::element_text(size = 10, colour = "grey30"),
                axis.text = ggplot2::element_text(size = 9, colour = "grey40"),
                plot.title = ggplot2::element_text(size = 11, face = "bold", colour = "grey20", hjust = 0.5),
                legend.position = "none"
            )
            
            if (self$options$equalScale) {
                p <- p + ggplot2::coord_equal()
            }
            
            # Caption
            caption_text <- paste0(
                "Solid segments: above-average association with ", refCategory, "\n",
                "Dotted segments: below-average association with ", refCategory
            )
            p <- p + ggplot2::labs(caption = caption_text)
            p <- p + ggplot2::theme(
                plot.caption = ggplot2::element_text(size = 8, colour = "grey50", hjust = 0)
            )
            
            print(p)
            return(TRUE)
        },
        
        # =====================================================================
        # PLOT: CONTRIBUTION BIPLOT
        # =====================================================================
        .plotContribBiplot = function(image, ggtheme, theme, ...) {
            
            if (is.null(private$.caResult)) return(FALSE)
            if (!self$options$showContribPlot) return(FALSE)
            
            ca <- private$.caResult
            srd <- private$.runSRD()
            
            dim1 <- self$options$dim1
            dim2 <- self$options$dim2
            dotSize <- self$options$dotSize
            labelSize <- self$options$labelSize
            focus <- self$options$contribFocus
            showClusters <- self$options$contribShowClusters
            
            # Check dimension validity
            if (dim1 > ca$n_axes || dim2 > ca$n_axes) {
                return(FALSE)
            }
            
            # Determine which variable defines the plane
            if (focus == "cols") {
                # Columns define the plane, rows are interpreted
                focus_contrib <- ca$col_contrib_coord
                focus_mass <- ca$col_mass
                focus_contrib_pct <- ca$col_contrib
                focus_names <- colnames(ca$N)
                n_focus <- ca$n_cols
                focus_label <- "Column"
                
                interp_princ <- ca$row_princ
                interp_cos2 <- ca$row_cos2
                interp_names <- rownames(ca$N)
                interp_label <- "Row"
            } else {
                # Rows define the plane, columns are interpreted
                focus_contrib <- ca$row_contrib_coord
                focus_mass <- ca$row_mass
                focus_contrib_pct <- ca$row_contrib
                focus_names <- rownames(ca$N)
                n_focus <- ca$n_rows
                focus_label <- "Row"
                
                interp_princ <- ca$col_princ
                interp_cos2 <- ca$col_cos2
                interp_names <- colnames(ca$N)
                interp_label <- "Column"
            }
            
            avg_contrib <- 100 / n_focus
            
            # Build data frames
            focus_df <- data.frame(
                x = focus_contrib[, dim1],
                y = focus_contrib[, dim2],
                label = focus_names,
                contrib_d1 = focus_contrib_pct[, dim1],
                contrib_d2 = focus_contrib_pct[, dim2],
                stringsAsFactors = FALSE
            )
            
            focus_df$above_avg <- (!is.na(focus_df$contrib_d1) & focus_df$contrib_d1 >= avg_contrib) | 
                (!is.na(focus_df$contrib_d2) & focus_df$contrib_d2 >= avg_contrib)
            # Handle potential NA values defensively
            focus_df$above_avg[is.na(focus_df$above_avg)] <- FALSE
            focus_df$alpha <- ifelse(focus_df$above_avg, 1.0, 0.4)
            focus_df$fontface <- ifelse(focus_df$above_avg, "bold", "plain")
            
            interp_df <- data.frame(
                x = interp_princ[, dim1],
                y = interp_princ[, dim2],
                label = interp_names,
                quality = rowSums(interp_cos2[, c(dim1, dim2), drop = FALSE]),
                stringsAsFactors = FALSE
            )
            interp_df$alpha <- 1.0
            
            # Axis labels
            xlab <- sprintf("Dimension %d (%.1f%%)", dim1, ca$inertia_proportion[dim1] * 100)
            ylab <- sprintf("Dimension %d (%.1f%%)", dim2, ca$inertia_proportion[dim2] * 100)
            title <- paste0("Contribution Biplot (", ca$resid_label, " residuals)")
            
            # Build plot
            p <- ggplot2::ggplot()
            
            # Reference lines
            p <- p + ggplot2::geom_hline(yintercept = 0, linewidth = 0.5, colour = "grey70", linetype = "dashed")
            p <- p + ggplot2::geom_vline(xintercept = 0, linewidth = 0.5, colour = "grey70", linetype = "dashed")
            
            # SRD cluster hulls (if requested)
            if (showClusters && !is.null(srd) && length(srd$groups) > 0) {
                cluster_colours <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", 
                                     "#FF7F00", "#FFFF33", "#A65628", "#F781BF")
                
                for (g in seq_along(srd$groups)) {
                    members <- srd$groups[[g]]$members
                    member_idx <- which(interp_df$label %in% members)
                    cluster_colour <- cluster_colours[(g - 1) %% length(cluster_colours) + 1]
                    
                    if (length(member_idx) >= 3) {
                        # Draw convex hull polygon for 3+ members
                        hull_pts <- interp_df[member_idx, c("x", "y")]
                        hull_order <- grDevices::chull(hull_pts$x, hull_pts$y)
                        hull_df <- hull_pts[c(hull_order, hull_order[1]), ]
                        
                        p <- p + ggplot2::geom_polygon(
                            data = hull_df,
                            ggplot2::aes(x = x, y = y),
                            fill = cluster_colour,
                            alpha = 0.15,
                            colour = cluster_colour,
                            linetype = "dashed",
                            linewidth = 0.5
                        )
                    } else if (length(member_idx) == 2) {
                        # Draw connecting line for 2-member clusters
                        line_pts <- interp_df[member_idx, c("x", "y")]
                        
                        p <- p + ggplot2::geom_segment(
                            data = data.frame(
                                x = line_pts$x[1], y = line_pts$y[1],
                                xend = line_pts$x[2], yend = line_pts$y[2]
                            ),
                            ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
                            colour = cluster_colour,
                            linetype = "dashed",
                            linewidth = 0.8
                        )
                    }
                }
            }
            
            # Lines from origin are drawn after splitting focus_df (below)
            
            # Plane-defining points - split into above-average and below-average
            # to ensure proper fill opacity (white fill must be fully opaque)
            focus_above <- focus_df[focus_df$above_avg, , drop = FALSE]
            focus_below <- focus_df[!focus_df$above_avg, , drop = FALSE]
            
            # Lines from origin to plane-defining categories
            # Below-average: dashed, lighter colour
            if (nrow(focus_below) > 0) {
                p <- p + ggplot2::geom_segment(
                    data = focus_below,
                    ggplot2::aes(x = 0, y = 0, xend = x, yend = y),
                    colour = "#9ECAE1",
                    linewidth = 0.4,
                    linetype = "dashed"
                )
            }
            
            # Above-average: solid, darker colour
            if (nrow(focus_above) > 0) {
                p <- p + ggplot2::geom_segment(
                    data = focus_above,
                    ggplot2::aes(x = 0, y = 0, xend = x, yend = y),
                    colour = "#2171B5",
                    linewidth = 0.5,
                    linetype = "solid"
                )
            }
            
            # Below-average contributors: lighter outline, solid white fill
            if (nrow(focus_below) > 0) {
                p <- p + ggplot2::geom_point(
                    data = focus_below,
                    ggplot2::aes(x = x, y = y),
                    colour = "#9ECAE1", fill = "white", shape = 21, size = dotSize, stroke = 0.8
                )
            }
            
            # Above-average contributors: solid outline, solid white fill
            if (nrow(focus_above) > 0) {
                p <- p + ggplot2::geom_point(
                    data = focus_above,
                    ggplot2::aes(x = x, y = y),
                    colour = "#2171B5", fill = "white", shape = 21, size = dotSize, stroke = 1.1
                )
            }
            
            # Interpreted variable - optionally coloured by quality
            if (self$options$showQuality) {
                p <- p + ggplot2::geom_point(
                    data = interp_df,
                    ggplot2::aes(x = x, y = y, colour = quality),
                    shape = 17, size = dotSize
                )
                p <- p + ggplot2::scale_colour_gradient(
                    low = "#FCBBA1", high = "#67000D",
                    name = "Quality (cos\u00b2)"
                )
            } else {
                p <- p + ggplot2::geom_point(
                    data = interp_df,
                    ggplot2::aes(x = x, y = y, alpha = alpha),
                    colour = "#CB181D", shape = 17, size = dotSize
                )
            }
            
            p <- p + ggplot2::scale_alpha_identity()
            
            # Labels
            if (self$options$hideWeakContrib) {
                focus_show <- focus_df[focus_df$above_avg, , drop = FALSE]
            } else {
                focus_show <- focus_df
            }
            
            # Combine both data frames for unified label repulsion
            label_parts <- list()
            if (nrow(focus_show) > 0) {
                focus_show$colour <- "#2171B5"
                label_parts[[1]] <- focus_show[, c("x", "y", "label", "colour", "fontface"), drop = FALSE]
            }
            interp_df$colour <- "#CB181D"
            interp_df$fontface <- "plain"
            label_parts[[length(label_parts) + 1]] <- interp_df[, c("x", "y", "label", "colour", "fontface"), drop = FALSE]
            
            combined_labels <- do.call(rbind, label_parts)
            
            if (nrow(combined_labels) > 0) {
                if (self$options$showQuality) {
                    # When showing quality gradient, use manual scale for labels
                    p <- p + ggrepel::geom_text_repel(
                        data = combined_labels,
                        ggplot2::aes(x = x, y = y, label = label, fontface = fontface),
                        colour = combined_labels$colour,
                        size = labelSize,
                        max.overlaps = 30,
                        box.padding = 0.4,
                        point.padding = 0.3,
                        force = 2,
                        force_pull = 0.3,
                        min.segment.length = 0.5,
                        segment.colour = "grey50",
                        segment.size = 0.4,
                        seed = 42
                    )
                } else {
                    p <- p + ggrepel::geom_text_repel(
                        data = combined_labels,
                        ggplot2::aes(x = x, y = y, label = label, colour = colour, fontface = fontface),
                        size = labelSize,
                        max.overlaps = 30,
                        box.padding = 0.4,
                        point.padding = 0.3,
                        force = 2,
                        force_pull = 0.3,
                        min.segment.length = 0.5,
                        segment.colour = "grey50",
                        segment.size = 0.4,
                        seed = 42
                    )
                    p <- p + ggplot2::scale_colour_identity()
                }
            }
            
            # Theme
            p <- p + ggplot2::labs(title = title, x = xlab, y = ylab)
            p <- p + ggplot2::theme(
                panel.background = ggplot2::element_rect(fill = "white", colour = NA),
                panel.border = ggplot2::element_rect(fill = NA, colour = "grey60", linewidth = 0.5),
                panel.grid = ggplot2::element_blank(),
                axis.title = ggplot2::element_text(size = 10, colour = "grey30"),
                axis.text = ggplot2::element_text(size = 9, colour = "grey40"),
                plot.title = ggplot2::element_text(size = 11, face = "bold", colour = "grey20", hjust = 0.5),
                legend.position = if (self$options$showQuality) "right" else "none",
                legend.background = ggplot2::element_rect(fill = "white", colour = NA)
            )
            
            if (self$options$equalScale) {
                p <- p + ggplot2::coord_equal()
            }
            
            # Caption
            caption_text <- paste0(
                "Blue (\u25cf) = ", focus_label, " categories (solid = above-average contributor)\n",
                "Red (\u25b2) = ", interp_label, " categories"
            )
            if (showClusters && !is.null(srd) && length(srd$groups) > 0) {
                caption_text <- paste0(caption_text, "\nDashed regions = SRD clusters")
            }
            
            p <- p + ggplot2::labs(caption = caption_text)
            p <- p + ggplot2::theme(
                plot.caption = ggplot2::element_text(size = 8, colour = "grey50", hjust = 0)
            )
            
            print(p)
            return(TRUE)
        },
        
        # =====================================================================
        # PLOT: OVERDISPERSION DIAGNOSTIC
        # =====================================================================
        .plotOverdispersion = function(image, ggtheme, theme, ...) {
            
            if (is.null(private$.contingencyTable)) return(FALSE)
            if (!self$options$showOverdispersionPlot) return(FALSE)
            
            N <- private$.contingencyTable
            n <- sum(N)
            I <- nrow(N)
            J <- ncol(N)
            
            # Row and column marginal proportions
            r <- rowSums(N) / n
            c <- colSums(N) / n
            
            # Compute for each cell
            sd_indep <- numeric(I * J)
            pearson_resid <- numeric(I * J)
            
            idx <- 0
            for (i in 1:I) {
                for (j in 1:J) {
                    idx <- idx + 1
                    
                    e_ij <- n * r[i] * c[j]
                    sd_indep[idx] <- sqrt(n * r[i] * c[j])
                    
                    if (e_ij > 0) {
                        pearson_resid[idx] <- (N[i, j] - e_ij) / sqrt(e_ij)
                    } else {
                        pearson_resid[idx] <- 0
                    }
                }
            }
            
            plot_df <- data.frame(
                sd_indep = sd_indep,
                pearson_resid = pearson_resid,
                stringsAsFactors = FALSE
            )
            
            p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = sd_indep, y = pearson_resid))
            
            p <- p + ggplot2::geom_hline(yintercept = 0, linewidth = 0.5, colour = "grey60", linetype = "dashed")
            
            p <- p + ggplot2::geom_point(
                colour = "#2171B5",
                size = 2.5,
                alpha = 0.7
            )
            
            p <- p + ggplot2::labs(
                title = "Overdispersion Diagnostic Plot",
                x = expression(paste("SD under independence: ", sqrt(n %.% p[i.] %.% p[.j]))),
                y = expression(paste("Pearson residual: ", (n[ij] - e[ij]) / sqrt(e[ij])))
            )
            
            p <- p + ggplot2::theme(
                panel.background = ggplot2::element_rect(fill = "white", colour = NA),
                panel.border = ggplot2::element_rect(fill = NA, colour = "grey60", linewidth = 0.5),
                panel.grid.major = ggplot2::element_line(colour = "grey90"),
                panel.grid.minor = ggplot2::element_blank(),
                axis.title = ggplot2::element_text(size = 10, colour = "grey30"),
                axis.text = ggplot2::element_text(size = 9, colour = "grey40"),
                plot.title = ggplot2::element_text(size = 11, face = "bold", colour = "grey20", hjust = 0.5),
                plot.caption = ggplot2::element_text(size = 8, colour = "grey50", hjust = 0)
            )
            
            p <- p + ggplot2::labs(
                caption = "Random scatter suggests no overdispersion. Problematic patterns include:\nclustering to the left, diagonal banding, or fan-shaped spread.\nIf present, consider using Freeman-Tukey residuals."
            )
            
            print(p)
            return(TRUE)
        }
    )
)
