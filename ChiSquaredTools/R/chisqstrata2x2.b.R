# This file is a generated template, your changes will not be overwritten

#' @importFrom stats chisq.test mantelhaen.test pchisq xtabs as.formula
#' @importFrom ggplot2 ggplot aes geom_point geom_errorbarh geom_hline geom_vline geom_segment geom_rect geom_text labs theme_minimal theme element_text element_blank scale_x_continuous scale_y_discrete coord_cartesian annotate
#' @export
chisqstrata2x2Class <- R6::R6Class(
  "chisqstrata2x2Class",
  inherit = chisqstrata2x2Base,
  private = list(
    
    .run = function() {
      
      # ═══════════════════════════════════════════════════════════════════════════
      # 1a. Input validation
      # ═══════════════════════════════════════════════════════════════════════════
      
      if (is.null(self$options$rows) || 
          is.null(self$options$cols) || 
          is.null(self$options$strata)) {
        return()
      }
      
      rowVar <- self$options$rows
      colVar <- self$options$cols
      strataVar <- self$options$strata
      
      # Get the selected level names from LevelSelector
      rowRefLevel <- self$options$rowRef
      colRefLevel <- self$options$colRef
      
      data <- self$data
      
      # Ensure variables are factors
      if (!is.factor(data[[rowVar]])) {
        data[[rowVar]] <- as.factor(data[[rowVar]])
      }
      if (!is.factor(data[[colVar]])) {
        data[[colVar]] <- as.factor(data[[colVar]])
      }
      if (!is.factor(data[[strataVar]])) {
        data[[strataVar]] <- as.factor(data[[strataVar]])
      }
      
      # Validate 2x2 requirement
      nRowLevels <- nlevels(data[[rowVar]])
      nColLevels <- nlevels(data[[colVar]])
      nStrataLevels <- nlevels(data[[strataVar]])
      
      if (nRowLevels != 2) {
        jmvcore::reject(
          paste0("Row variable '", rowVar, "' must have exactly 2 levels (has ", 
                 nRowLevels, ")"),
          code = "invalid_row_levels"
        )
      }
      
      if (nColLevels != 2) {
        jmvcore::reject(
          paste0("Column variable '", colVar, "' must have exactly 2 levels (has ", 
                 nColLevels, ")"),
          code = "invalid_col_levels"
        )
      }
      
      if (nStrataLevels < 2) {
        jmvcore::reject(
          paste0("Stratifying variable '", strataVar, "' must have at least 2 levels"),
          code = "invalid_strata_levels"
        )
      }
      
      # ═══════════════════════════════════════════════════════════════════════════
      # 1b. Determine reference category positions from level names
      # ═══════════════════════════════════════════════════════════════════════════
      
      # Get factor levels
      rowLevels <- levels(data[[rowVar]])
      colLevels <- levels(data[[colVar]])
      
      # Determine which position (1 or 2) corresponds to selected level
      # Default to position 2 for row and position 1 for column if not specified
      if (is.null(rowRefLevel) || !(rowRefLevel %in% rowLevels)) {
        rowRefPos <- 2
        rowRefLevel <- rowLevels[2]
      } else {
        rowRefPos <- which(rowLevels == rowRefLevel)
      }
      
      if (is.null(colRefLevel) || !(colRefLevel %in% colLevels)) {
        colRefPos <- 1
        colRefLevel <- colLevels[1]
      } else {
        colRefPos <- which(colLevels == colRefLevel)
      }
      
      # ═══════════════════════════════════════════════════════════════════════════
      # 2. Build the 3D array of partial tables
      # ═══════════════════════════════════════════════════════════════════════════
      
      array3D <- private$.buildStratifiedArray(data, rowVar, colVar, strataVar)
      strataNames <- dimnames(array3D)[[3]]
      K <- dim(array3D)[3]
      
      # Convert to list of 2x2 matrices for internal processing
      listOfTables <- lapply(1:K, function(i) array3D[,,i])
      
      # ═══════════════════════════════════════════════════════════════════════════
      # 3. Compute marginal table (population happens later, after OR calculation)
      # ═══════════════════════════════════════════════════════════════════════════
      
      marginalTable <- Reduce("+", listOfTables)
      dimnames(marginalTable) <- dimnames(array3D)[1:2]
      
      # ═══════════════════════════════════════════════════════════════════════════
      # 4. Compute odds ratios and confidence intervals
      # ═══════════════════════════════════════════════════════════════════════════
      
      orsAndCIs <- lapply(listOfTables, private$.calculateORandCI, rowRefPos, colRefPos)
      marginalORandCI <- private$.calculateORandCI(marginalTable, rowRefPos, colRefPos)
      
      # ═══════════════════════════════════════════════════════════════════════════
      # 5. Populate tables (must be after OR calculation for annotations)
      # ═══════════════════════════════════════════════════════════════════════════
      
      # Populate partial tables with OR annotations
      private$.populatePartialTables(array3D, strataNames, rowVar, colVar, strataVar,
                                     orsAndCIs, rowRefPos, colRefPos)
      
      # Populate marginal table with OR annotation
      private$.populateMarginalTable(marginalTable, rowVar, colVar,
                                     marginalORandCI, rowRefPos, colRefPos)
      
      # Add section header for marginal table
      self$results$marginalTableHeader$setContent(
        "<h2 style='color: #3E6D9C; font-size: 1.2em; margin-top: 1.5em; margin-bottom: 0.5em;'>Marginal Table</h2>"
      )
      
      # Add section header for analysis results
      self$results$analysisResultsHeader$setContent(
        "<h2 style='color: #3E6D9C; font-size: 1.2em; margin-top: 1.5em; margin-bottom: 0.5em;'>Statistical Tests and Summary Measures</h2>"
      )
      
      # Add section header for interpretation guide
      if (self$options$showInterpretation) {
        self$results$interpretationGuideHeader$setContent(
          "<h2 style='color: #3E6D9C; font-size: 1.2em; margin-top: 1.5em; margin-bottom: 0.5em;'>Interpretation Guide</h2>"
        )
      }
      
      # ═══════════════════════════════════════════════════════════════════════════
      # 6. Run statistical tests
      # ═══════════════════════════════════════════════════════════════════════════
      
      # Suppress warnings for chi-squared tests with small expected counts
      suppressWarnings({
        
        # Chi-squared tests for each partial table
        chiSqResults <- lapply(listOfTables, function(tbl) {
          test <- stats::chisq.test(tbl, correct = FALSE)
          list(
            statistic = as.numeric(test$statistic),
            df = as.integer(test$parameter),
            pvalue = test$p.value
          )
        })
        
        # Chi-squared test for marginal table
        marginalChiSq <- stats::chisq.test(marginalTable, correct = FALSE)
        
        # Cochran-Mantel-Haenszel test
        cmhTest <- stats::mantelhaen.test(array3D, correct = FALSE)
        
      })
      
      # Extract common OR and CI, adjusting for reference category choices
      # R's mantelhaen.test uses standard cell layout (a*d)/(b*c)
      # We need to transform if user selected different references
      commonOR_raw <- as.numeric(cmhTest$estimate)
      commonCI_raw <- as.numeric(cmhTest$conf.int)
      
      # Determine if we need to invert based on reference selections
      # Default R calculation assumes row1=outcome, col1=exposure giving (a*d)/(b*c)
      # Our default (rowRefPos=2, colRefPos=1) is the reciprocal
      needsInvert <- (rowRefPos == 2 && colRefPos == 1) ||
        (rowRefPos == 1 && colRefPos == 2)
      
      if (needsInvert) {
        commonOR <- 1 / commonOR_raw
        commonCI <- 1 / rev(commonCI_raw)
      } else {
        commonOR <- commonOR_raw
        commonCI <- commonCI_raw
      }
      
      # Mantel-Haenszel test for homogeneity
      mhResult <- private$.mhHomogeneityTest(listOfTables, rowRefPos, colRefPos)
      
      # Breslow-Day-Tarone test
      bdtResult <- private$.breslowDayTaroneTest(array3D)
      
      # ═══════════════════════════════════════════════════════════════════════════
      # 7. Populate results tables
      # ═══════════════════════════════════════════════════════════════════════════
      
      private$.populateOddsRatioTable(orsAndCIs, marginalORandCI, commonOR, 
                                      commonCI, strataNames)
      
      # ═══════════════════════════════════════════════════════════════════════════
      # 8.  Prepare plot states
      # ═══════════════════════════════════════════════════════════════════════════
      
      # Forest plot state
      if (self$options$showForestPlot) {
        forestData <- private$.prepareForestData(orsAndCIs, marginalORandCI, commonOR, 
                                                 commonCI, strataNames, K)
        image <- self$results$forestPlot
        image$setState(forestData)
        # Dynamic height: 200 base + 60 per stratum + 80 for marginal/common
        image$setSize(700, 200 + (K * 60) + 80)
      }
      
      # Diagnostic tree state
      if (self$options$showDiagnosticTree) {
        treeData <- private$.prepareDiagnosticTreeData(cmhTest$p.value, 
                                                       mhResult$pvalue,
                                                       bdtResult$pvalue)
        image <- self$results$diagnosticTree
        image$setState(treeData)
      }
      
      # Trajectory plot state (only if strata marked as ordered)
      if (self$options$showTrajectoryPlot && self$options$strataOrdered) {
        trajectoryData <- private$.prepareTrajectoryData(orsAndCIs, strataNames)
        image <- self$results$trajectoryPlot
        image$setState(trajectoryData)
      }
      
      if (self$options$showPartialChiSq) {
        private$.populatePartialChiSqTable(chiSqResults, marginalChiSq, strataNames)
      }
      
      private$.populateCMHTable(cmhTest, commonOR, commonCI)
      private$.populateHomogeneityTable(mhResult, bdtResult, K)
      
      # ═══════════════════════════════════════════════════════════════════════════
      # 9. Generate interpretation
      # ═══════════════════════════════════════════════════════════════════════════
      
      if (self$options$showInterpretation) {
        private$.populateInterpretation(
          chiSqResults, marginalChiSq, cmhTest, mhResult, bdtResult,
          orsAndCIs, commonOR, commonCI, strataNames,
          rowVar, colVar, strataVar
        )
      }
      
      # ═══════════════════════════════════════════════════════════════════════════
      # 10. Populate method information and references
      # ═══════════════════════════════════════════════════════════════════════════
      
      private$.populateMethodInfo()
      private$.populateReferences()
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Helper: Build 3D array from data
    # ═══════════════════════════════════════════════════════════════════════════
    
    .buildStratifiedArray = function(data, rowVar, colVar, strataVar) {
      
      if (is.null(self$options$counts)) {
        # Long format: one row per observation
        fullTable <- table(data[[rowVar]], data[[colVar]], data[[strataVar]])
      } else {
        # Wide format: aggregated data with counts variable
        countsVar <- self$options$counts
        formula_str <- paste0(countsVar, " ~ ", rowVar, " + ", colVar, " + ", strataVar)
        fullTable <- stats::xtabs(stats::as.formula(formula_str), data = data)
      }
      
      # Ensure it is a proper 3D array
      fullTable <- as.array(fullTable)
      
      return(fullTable)
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Helper: Calculate OR and 95% CI with Haldane-Anscombe correction
    # ═══════════════════════════════════════════════════════════════════════════
    
    .calculateORandCI = function(tbl, rowRefPos, colRefPos) {
      
      # Apply Haldane-Anscombe correction if any diagonal product is zero
      if (tbl[1,1] * tbl[2,2] == 0 || tbl[1,2] * tbl[2,1] == 0) {
        tbl <- tbl + 0.5
      }
      
      # Cell references based on user-selected reference categories
      # rowRefPos: 1 means row 1 is outcome; 2 means row 2 is outcome
      # colRefPos: 1 means col 1 is exposure; 2 means col 2 is exposure
      #
      # Standard OR = (outcome_exposed * non-outcome_unexposed) / 
      #               (outcome_unexposed * non-outcome_exposed)
      
      if (rowRefPos == 2 && colRefPos == 1) {
        # Outcome = row 2, Exposure = col 1 (default)
        # OR = (row2_col1 * row1_col2) / (row2_col2 * row1_col1)
        or <- (tbl[2,1] * tbl[1,2]) / (tbl[2,2] * tbl[1,1])
      } else if (rowRefPos == 2 && colRefPos == 2) {
        # Outcome = row 2, Exposure = col 2
        # OR = (row2_col2 * row1_col1) / (row2_col1 * row1_col2)
        or <- (tbl[2,2] * tbl[1,1]) / (tbl[2,1] * tbl[1,2])
      } else if (rowRefPos == 1 && colRefPos == 1) {
        # Outcome = row 1, Exposure = col 1
        # OR = (row1_col1 * row2_col2) / (row1_col2 * row2_col1)
        or <- (tbl[1,1] * tbl[2,2]) / (tbl[1,2] * tbl[2,1])
      } else {
        # rowRefPos == 1 && colRefPos == 2
        # Outcome = row 1, Exposure = col 2
        # OR = (row1_col2 * row2_col1) / (row1_col1 * row2_col2)
        or <- (tbl[1,2] * tbl[2,1]) / (tbl[1,1] * tbl[2,2])
      }
      
      # Standard error of log OR (same regardless of direction)
      a <- tbl[1,1]; b <- tbl[1,2]; c <- tbl[2,1]; d <- tbl[2,2]
      se_log_or <- sqrt(1/a + 1/b + 1/c + 1/d)
      ci_lower <- exp(log(or) - 1.96 * se_log_or)
      ci_upper <- exp(log(or) + 1.96 * se_log_or)
      
      return(list(or = or, ci_lower = ci_lower, ci_upper = ci_upper))
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Helper: Generate OR annotation text
    # ═══════════════════════════════════════════════════════════════════════════
    
    .generateORAnnotation = function(or, rowLevels, colLevels, rowRefPos, colRefPos) {
      
      # Determine which level is outcome and which is exposure based on user selection
      if (rowRefPos == 2) {
        outcomeName <- rowLevels[2]
      } else {
        outcomeName <- rowLevels[1]
      }
      
      if (colRefPos == 1) {
        exposureName <- colLevels[1]
        referenceName <- colLevels[2]
      } else {
        exposureName <- colLevels[2]
        referenceName <- colLevels[1]
      }
      
      # Format OR value
      orFormatted <- sprintf("%.2f", or)
      
      # Build interpretation text - always use the actual OR value
      if (or >= 1) {
        # OR >= 1: express as "X times the odds" or "X times higher odds"
        if (or == 1) {
          annotation <- paste0(
            "<em>", exposureName, "</em> and <em>", referenceName, 
            "</em> have similar odds of being <em>", outcomeName, "</em>."
          )
        } else {
          annotation <- paste0(
            "<em>", exposureName, "</em> have ", orFormatted, 
            " times the odds of being <em>", outcomeName,
            "</em> compared to <em>", referenceName, "</em>."
          )
        }
      } else {
        # OR < 1: express using actual OR value with percentage interpretation
        pctLower <- sprintf("%.0f", (1 - or) * 100)
        annotation <- paste0(
          "<em>", exposureName, "</em> have ", orFormatted, 
          " times the odds of being <em>", outcomeName,
          "</em> compared to <em>", referenceName, 
          "</em> (i.e., ", pctLower, "% lower odds)."
        )
      }
      
      # Wrap in styled div
      html <- paste0(
        "<div style='font-size: 0.9em; color: #2c3e50; margin-top: 0.3em; ",
        "margin-bottom: 1em; padding: 0.4em 0.6em; background-color: #f8f9fa; ",
        "border-left: 3px solid #3E6D9C; font-style: italic;'>",
        "Interpretation (OR = ", orFormatted, "): ", annotation,
        " For statistical significance, refer to the tables below.",
        "</div>"
      )
      
      return(html)
    },
    
    .generateORAnnotationText = function(or, rowLevels, colLevels, rowRefPos, colRefPos) {
      
      # Determine which level is outcome and which is exposure
      if (rowRefPos == 2) {
        outcomeName <- rowLevels[2]
      } else {
        outcomeName <- rowLevels[1]
      }
      
      if (colRefPos == 1) {
        exposureName <- colLevels[1]
        referenceName <- colLevels[2]
      } else {
        exposureName <- colLevels[2]
        referenceName <- colLevels[1]
      }
      
      # Format OR value
      orFormatted <- sprintf("%.2f", or)
      
      # Build plain text interpretation
      if (or >= 1) {
        if (or == 1) {
          annotation <- paste0(
            exposureName, " and ", referenceName, 
            " have similar odds of being ", outcomeName, "."
          )
        } else {
          annotation <- paste0(
            "Interpretation (OR = ", orFormatted, "): ",
            exposureName, " have ", orFormatted, 
            " times the odds of being ", outcomeName,
            " compared to ", referenceName, ". ",
            "See tables below for confidence intervals and significance tests."
          )
        }
      } else {
        pctLower <- sprintf("%.0f", (1 - or) * 100)
        annotation <- paste0(
          "Interpretation (OR = ", orFormatted, "): ",
          exposureName, " have ", orFormatted, 
          " times the odds of being ", outcomeName,
          " compared to ", referenceName, 
          " (i.e., ", pctLower, "% lower odds). ",
          "See tables below for confidence intervals and significance tests."
        )
      }
      
      return(annotation)
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Helper: Mantel-Haenszel test for homogeneity of odds ratios
    # ═══════════════════════════════════════════════════════════════════════════
    
    .mhHomogeneityTest = function(listOfTables, rowRefPos, colRefPos) {
      
      # Function to calculate MH weights
      mhCalculateWeights <- function(tbl) {
        # Apply Haldane-Anscombe correction if needed
        if (tbl[1,1] * tbl[2,2] == 0 || tbl[1,2] * tbl[2,1] == 0) {
          tbl <- tbl + 0.5
        }
        a <- tbl[1,1]; b <- tbl[1,2]; c <- tbl[2,1]; d <- tbl[2,2]
        mh_weight <- 1 / (1/a + 1/b + 1/c + 1/d)
        return(mh_weight)
      }
      
      # Calculate weights for each partial table
      mhWeights <- sapply(listOfTables, mhCalculateWeights)
      
      # Calculate log OR for each partial table
      mhLogOR <- log(sapply(listOfTables, function(tbl) {
        private$.calculateORandCI(tbl, rowRefPos, colRefPos)$or
      }))
      
      # Compute the weighted average of the log ORs
      Y_bar <- sum(mhWeights * mhLogOR) / sum(mhWeights)
      
      # Compute the MH test statistic
      mhStatistic <- sum(mhWeights * (mhLogOR - Y_bar)^2)
      
      # Degrees of freedom
      df <- length(listOfTables) - 1
      
      # P-value
      pvalue <- stats::pchisq(mhStatistic, df, lower.tail = FALSE)
      
      return(list(statistic = mhStatistic, df = df, pvalue = pvalue))
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Helper: Breslow-Day-Tarone test for homogeneity
    # (Implementation based on Hoehle 2000, equations from Lachin 2000)
    # ═══════════════════════════════════════════════════════════════════════════
    
    .breslowDayTaroneTest = function(x) {
      
      # Get the common OR based on Mantel-Haenszel
      or_hat_mh <- as.numeric(stats::mantelhaen.test(x)$estimate)
      
      # Number of strata
      K <- dim(x)[3]
      
      # Initialise
      X2_HBD <- 0
      a <- tildea <- Var_a <- numeric(K)
      
      for (j in 1:K) {
        # Find marginals of table j
        mj <- apply(x[,,j], MARGIN = 1, sum)
        nj <- apply(x[,,j], MARGIN = 2, sum)
        
        # Solve for tilde(a)_j using quadratic formula
        coef <- c(-mj[1] * nj[1] * or_hat_mh, 
                  nj[2] - mj[1] + or_hat_mh * (nj[1] + mj[1]),
                  1 - or_hat_mh)
        sols <- Re(polyroot(coef))
        
        # Take the root which fulfills 0 < tilde(a)_j <= min(n1_j, m1_j)
        tildeaj <- sols[(0 < sols) & (sols <= min(nj[1], mj[1]))]
        
        # Handle edge case where no valid root is found
        if (length(tildeaj) == 0) {
          tildeaj <- min(nj[1], mj[1]) / 2
        } else {
          tildeaj <- tildeaj[1]
        }
        
        # Observed value
        aj <- x[1,1,j]
        
        # Determine other expected cell entries
        tildebj <- mj[1] - tildeaj
        tildecj <- nj[1] - tildeaj
        tildedj <- mj[2] - tildecj
        
        # Compute variance estimate
        Var_aj <- (1/tildeaj + 1/tildebj + 1/tildecj + 1/tildedj)^(-1)
        
        # Compute contribution to statistic
        X2_HBD <- X2_HBD + as.numeric((aj - tildeaj)^2 / Var_aj)
        
        # Store values
        a[j] <- aj
        tildea[j] <- tildeaj
        Var_a[j] <- Var_aj
      }
      
      # Compute Tarone corrected test
      X2_HBDT <- as.numeric(X2_HBD - (sum(a) - sum(tildea))^2 / sum(Var_a))
      
      # Compute p-value based on the Tarone corrected test
      pvalue <- 1 - stats::pchisq(X2_HBDT, df = K - 1)
      
      return(list(statistic = X2_HBDT, df = K - 1, pvalue = pvalue))
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Populate: Partial tables (one per stratum)
    # ═══════════════════════════════════════════════════════════════════════════
    
    .populatePartialTables = function(array3D, strataNames, rowVar, colVar, strataVar,
                                      orsAndCIs, rowRefPos, colRefPos) {
      
      K <- dim(array3D)[3]
      rowNames <- dimnames(array3D)[[1]]
      colNames <- dimnames(array3D)[[2]]
      
      partialTablesGroup <- self$results$partialTablesGroup
      
      for (k in 1:K) {
        
        # Get the partial table
        partialTab <- array3D[,,k]
        
        # Add table to array
        partialTablesGroup$addItem(key = k)
        table <- partialTablesGroup$get(key = k)
        
        # Set title (this creates the clean inline header)
        table$setTitle(paste0(strataVar, " = ", strataNames[k]))
        
        # Add row name column
        table$addColumn(
          name = 'rowname',
          title = rowVar,
          type = 'text'
        )
        
        # Add data columns
        for (j in 1:2) {
          table$addColumn(
            name = paste0("col", j),
            title = colNames[j],
            type = 'integer',
            superTitle = colVar
          )
        }
        
        # Add row total column
        table$addColumn(
          name = 'rowtotal',
          title = 'Total',
          type = 'integer'
        )
        
        # Populate rows
        for (i in 1:2) {
          rowValues <- list(rowname = rowNames[i])
          for (j in 1:2) {
            rowValues[[paste0("col", j)]] <- partialTab[i, j]
          }
          rowValues[['rowtotal']] <- sum(partialTab[i,])
          table$addRow(rowKey = i, values = rowValues)
        }
        
        # Add column totals row
        totalRowValues <- list(rowname = 'Total')
        for (j in 1:2) {
          totalRowValues[[paste0("col", j)]] <- sum(partialTab[,j])
        }
        totalRowValues[['rowtotal']] <- sum(partialTab)
        table$addRow(rowKey = 'total', values = totalRowValues)
        
        # Add OR annotation as table footnote if enabled
        if (self$options$showORAnnotation) {
          annotationText <- private$.generateORAnnotationText(
            or = orsAndCIs[[k]]$or,
            rowLevels = rowNames,
            colLevels = colNames,
            rowRefPos = rowRefPos,
            colRefPos = colRefPos
          )
          table$setNote('orNote', annotationText)
        }
      }
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Populate: Marginal table
    # ═══════════════════════════════════════════════════════════════════════════
    
    .populateMarginalTable = function(marginalTable, rowVar, colVar,
                                      marginalORandCI, rowRefPos, colRefPos) {
      
      table <- self$results$marginalTable
      table$setTitle("Marginal Table (collapsed across strata)")
      rowNames <- rownames(marginalTable)
      colNames <- colnames(marginalTable)
      
      # Add row name column
      table$addColumn(
        name = 'rowname',
        title = rowVar,
        type = 'text'
      )
      
      # Add data columns
      for (j in 1:2) {
        table$addColumn(
          name = paste0("col", j),
          title = colNames[j],
          type = 'integer',
          superTitle = colVar
        )
      }
      
      # Add row total column
      table$addColumn(
        name = 'rowtotal',
        title = 'Total',
        type = 'integer'
      )
      
      # Populate rows
      for (i in 1:2) {
        rowValues <- list(rowname = rowNames[i])
        for (j in 1:2) {
          rowValues[[paste0("col", j)]] <- marginalTable[i, j]
        }
        rowValues[['rowtotal']] <- sum(marginalTable[i,])
        table$addRow(rowKey = i, values = rowValues)
      }
      
      # Add column totals row
      totalRowValues <- list(rowname = 'Total')
      for (j in 1:2) {
        totalRowValues[[paste0("col", j)]] <- sum(marginalTable[,j])
      }
      totalRowValues[['rowtotal']] <- sum(marginalTable)
      table$addRow(rowKey = 'total', values = totalRowValues)
      
      # Add OR annotation if enabled
      if (self$options$showORAnnotation) {
        annotationText <- private$.generateORAnnotationText(
          or = marginalORandCI$or,
          rowLevels = rowNames,
          colLevels = colNames,
          rowRefPos = rowRefPos,
          colRefPos = colRefPos
        )
        table$setNote('orNote', annotationText)
      }
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Populate: Odds ratio table
    # ═══════════════════════════════════════════════════════════════════════════
    
    .populateOddsRatioTable = function(orsAndCIs, marginalORandCI, commonOR, 
                                        commonCI, strataNames) {
      
      table <- self$results$oddsRatioTable
      K <- length(orsAndCIs)
      
      # Add rows for each partial table
      for (k in 1:K) {
        table$addRow(rowKey = k, values = list(
          stratum = paste0("Partial Table ", k, " (", strataNames[k], ")"),
          or = orsAndCIs[[k]]$or,
          ciLower = orsAndCIs[[k]]$ci_lower,
          ciUpper = orsAndCIs[[k]]$ci_upper
        ))
      }
      
      # Add marginal table row
      table$addRow(rowKey = 'marginal', values = list(
        stratum = "Marginal Table",
        or = marginalORandCI$or,
        ciLower = marginalORandCI$ci_lower,
        ciUpper = marginalORandCI$ci_upper
      ))
      
      # Add MH common OR row
      table$addRow(rowKey = 'common', values = list(
        stratum = "MH Common OR",
        or = commonOR,
        ciLower = commonCI[1],
        ciUpper = commonCI[2]
      ))
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Populate: Partial chi-squared table
    # ═══════════════════════════════════════════════════════════════════════════
    
    .populatePartialChiSqTable = function(chiSqResults, marginalChiSq, strataNames) {
      
      table <- self$results$partialChiSqTable
      K <- length(chiSqResults)
      
      # Add rows for each partial table
      for (k in 1:K) {
        table$addRow(rowKey = k, values = list(
          stratum = paste0("Partial Table ", k, " (", strataNames[k], ")"),
          chisq = chiSqResults[[k]]$statistic,
          df = chiSqResults[[k]]$df,
          pvalue = chiSqResults[[k]]$pvalue
        ))
      }
      
      # Add marginal table row
      table$addRow(rowKey = 'marginal', values = list(
        stratum = "Marginal Table",
        chisq = as.numeric(marginalChiSq$statistic),
        df = as.integer(marginalChiSq$parameter),
        pvalue = marginalChiSq$p.value
      ))
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Populate: CMH test table
    # ═══════════════════════════════════════════════════════════════════════════
    
    .populateCMHTable = function(cmhTest, commonOR, commonCI) {
      
      table <- self$results$cmhTestTable
      
      table$addRow(rowKey = 1, values = list(
        test = "Cochran-Mantel-Haenszel",
        statistic = as.numeric(cmhTest$statistic),
        df = as.integer(cmhTest$parameter),
        pvalue = cmhTest$p.value,
        commonOR = commonOR,
        commonCILower = commonCI[1],
        commonCIUpper = commonCI[2]
      ))
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Populate: Homogeneity tests table
    # ═══════════════════════════════════════════════════════════════════════════
    
    .populateHomogeneityTable = function(mhResult, bdtResult, K) {
      
      table <- self$results$homogeneityTable
      
      # Mantel-Haenszel homogeneity test
      table$addRow(rowKey = 1, values = list(
        test = "Mantel-Haenszel",
        statistic = mhResult$statistic,
        df = mhResult$df,
        pvalue = mhResult$pvalue
      ))
      
      # Breslow-Day-Tarone test
      table$addRow(rowKey = 2, values = list(
        test = "Breslow-Day-Tarone",
        statistic = bdtResult$statistic,
        df = bdtResult$df,
        pvalue = bdtResult$pvalue
      ))
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Populate: Interpretation
    # ═══════════════════════════════════════════════════════════════════════════
    
    .populateInterpretation = function(chiSqResults, marginalChiSq, cmhTest, 
                                        mhResult, bdtResult, orsAndCIs, 
                                        commonOR, commonCI, strataNames,
                                        rowVar, colVar, strataVar) {
      
      # Guard against invalid test results
      if (is.null(cmhTest$p.value) || is.na(cmhTest$p.value) ||
          is.null(mhResult$pvalue) || is.na(mhResult$pvalue) ||
          is.null(bdtResult$pvalue) || is.na(bdtResult$pvalue)) {
        self$results$interpretationNote$setContent(
          "<p style='color: #856404; background-color: #fff3cd; padding: 10px; border-radius: 4px;'>
          <strong>Note:</strong> Interpretation could not be generated because the 
          statistical tests returned invalid results. If using aggregated data, 
          please assign the counts variable.</p>"
        )
        return()
      }
      
      K <- length(chiSqResults)
      
      html <- "<div style='font-family: sans-serif; line-height: 1.6; font-size: 0.95em;'>"
      
      # ─────────────────────────────────────────────────────────────────────────
      # (A) Chi-squared test results
      # ─────────────────────────────────────────────────────────────────────────
      
      html <- paste0(html, "<p><strong>(A) Chi-squared test results:</strong></p>")
      html <- paste0(html, "<ul style='margin-left: 0; padding-left: 1.5em;'>")
      
      for (k in 1:K) {
        sigText <- if (chiSqResults[[k]]$pvalue < 0.05) "significant" else "not significant"
        html <- paste0(html, "<li>Partial Table ", k, " (", strataNames[k], "): ",
                       "\u03C7\u00B2 = ", sprintf("%.2f", chiSqResults[[k]]$statistic),
                       ", df = ", chiSqResults[[k]]$df,
                       ", p = ", sprintf("%.3f", chiSqResults[[k]]$pvalue),
                       " (", sigText, ")</li>")
      }
      
      margSigText <- if (marginalChiSq$p.value < 0.05) "significant" else "not significant"
      html <- paste0(html, "<li>Marginal Table: ",
                     "\u03C7\u00B2 = ", sprintf("%.2f", as.numeric(marginalChiSq$statistic)),
                     ", df = ", as.integer(marginalChiSq$parameter),
                     ", p = ", sprintf("%.3f", marginalChiSq$p.value),
                     " (", margSigText, ")</li>")
      
      html <- paste0(html, "</ul>")
      
      # ─────────────────────────────────────────────────────────────────────────
      # (B) CMH test
      # ─────────────────────────────────────────────────────────────────────────
      
      cmhSig <- cmhTest$p.value < 0.05
      cmhSigText <- if (cmhSig) "" else "not "
      cmhInterpretation <- if (cmhSig) {
        "conditional dependence (the odds ratio in at least one of the partial tables is not equal to 1)"
      } else {
        "conditional independence (the odds ratios in all of the partial tables are equal to 1)"
      }
      
      html <- paste0(html, 
        "<p><strong>(B)</strong> The Cochran-Mantel-Haenszel test is <strong>",
        cmhSigText, "significant</strong> (\u03C7\u00B2 = ", 
        sprintf("%.2f", as.numeric(cmhTest$statistic)),
        ", df = ", as.integer(cmhTest$parameter),
        ", p = ", sprintf("%.3f", cmhTest$p.value),
        "), suggesting ", cmhInterpretation, ".</p>"
      )
      
      # ─────────────────────────────────────────────────────────────────────────
      # (C) MH homogeneity test
      # ─────────────────────────────────────────────────────────────────────────
      
      mhSig <- mhResult$pvalue < 0.05
      mhSigText <- if (mhSig) "" else "not "
      mhInterpretation <- if (mhSig) "heterogeneity" else "homogeneity"
      
      html <- paste0(html,
        "<p><strong>(C)</strong> The Mantel-Haenszel test for homogeneity of odds ratios is <strong>",
        mhSigText, "significant</strong> (\u03C7\u00B2 = ",
        sprintf("%.2f", mhResult$statistic),
        ", df = ", mhResult$df,
        ", p = ", sprintf("%.3f", mhResult$pvalue),
        "), indicating ", mhInterpretation, " of the odds ratios across strata.</p>"
      )
      
      # ─────────────────────────────────────────────────────────────────────────
      # (D) BDT homogeneity test
      # ─────────────────────────────────────────────────────────────────────────
      
      bdtSig <- bdtResult$pvalue < 0.05
      bdtSigText <- if (bdtSig) "" else "not "
      bdtInterpretation <- if (bdtSig) "heterogeneity" else "homogeneity"
      
      html <- paste0(html,
        "<p><strong>(D)</strong> The Breslow-Day-Tarone test for homogeneity of odds ratios is <strong>",
        bdtSigText, "significant</strong> (\u03C7\u00B2 = ",
        sprintf("%.2f", bdtResult$statistic),
        ", df = ", bdtResult$df,
        ", p = ", sprintf("%.3f", bdtResult$pvalue),
        "), indicating ", bdtInterpretation, " of the odds ratios across strata.</p>"
      )
      
      # ─────────────────────────────────────────────────────────────────────────
      # (E) Overall interpretation
      # ─────────────────────────────────────────────────────────────────────────
      
      # Detect direction change in ORs
      orValues <- sapply(orsAndCIs, function(x) x$or)
      directionChange <- any(orValues < 1) && any(orValues > 1)
      
      html <- paste0(html, "<p><strong>(E) Overall interpretation:</strong> ")
      
      if (bdtSig) {
        # Heterogeneity exists
        if (directionChange) {
          # Interaction effect with direction change
          html <- paste0(html,
                         "Significant heterogeneity of odds ratios across strata, with a change in the ",
                         "<strong>direction</strong> of the association between '", rowVar, "' and '", colVar,
                         "', indicates an <strong>interaction effect</strong>: '", strataVar,
                         "' modifies the direction of the relationship between '", rowVar, "' and '", colVar, "'. ",
                         "Since the stratum-specific odds ratios are in opposite directions, ",
                         "the CMH test result may be unreliable and the common odds ratio is not a meaningful summary. ",
                         "Stratum-specific chi-squared tests should be evaluated to assess within-stratum associations."
          )
        } else {
          # Specification effect (heterogeneity in strength only)
          cmhClause <- if (cmhSig) {
            paste0("The CMH test indicates a significant conditional association between '",
                   rowVar, "' and '", colVar, "' after controlling for '", strataVar, "'. ")
          } else {
            paste0("The CMH test does not indicate a significant conditional association between '",
                   rowVar, "' and '", colVar, "' after controlling for '", strataVar, "'. ")
          }
          html <- paste0(html, cmhClause,
                         "Significant heterogeneity of odds ratios across strata has been detected, ",
                         "indicating an <strong>interaction effect</strong>: '", strataVar,
                         "' modifies the <strong>strength</strong> of the association between '", rowVar, "' and '", colVar,
                         "', although the direction remains consistent. ",
                         "Because the association varies in magnitude across strata, ",
                         "stratum-specific odds ratios should be reported rather than a single common odds ratio."
          )
        }
      } else {
        # Homogeneity exists
        if (cmhSig) {
          # Significant CMH with homogeneity: consistent conditional association
          html <- paste0(html,
                         "The CMH test indicates a significant conditional association between '",
                         rowVar, "' and '", colVar, "' after controlling for '", strataVar, "'. ",
                         "Given the homogeneity of odds ratios across strata, '", strataVar,
                         "' does not significantly modify this association. ",
                         "The conditional association between '", rowVar, "' and '", colVar,
                         "' is consistent (in direction and magnitude) across levels of '", strataVar,
                         "', and can be summarised using the Mantel-Haenszel estimate of a common odds ratio (",
                         sprintf("%.3f", commonOR), "; 95%% CI: ", sprintf("%.3f", commonCI[1]), "\u2013", sprintf("%.3f", commonCI[2]), ")."
          )
        } else {
          # Non-significant CMH with homogeneity: conditional independence
          html <- paste0(html,
                         "The CMH test does not indicate a significant conditional association between '",
                         rowVar, "' and '", colVar, "' after controlling for '", strataVar, "'. ",
                         "This suggests <strong>conditional independence</strong>: any marginal association, if present, ",
                         "vanishes when the data are stratified by '", strataVar, "'."
          )
        }
      }
      
      html <- paste0(html, "</p>")
      
      # ─────────────────────────────────────────────────────────────────────────
      # Cautionary note
      # ─────────────────────────────────────────────────────────────────────────
      
      html <- paste0(html,
        "<p style='font-size: 0.9em; color: #666; margin-top: 1em;'><em>",
        "Note: The interpretation guidelines provided are suggested based on the statistical tests' outcomes ",
        "and should be further evaluated within the context of your study. In case a table features a zero ",
        "along any of the diagonals, the Haldane-Anscombe correction (adding 0.5 to every cell) is applied ",
        "for the calculation of the odds ratios and the weights used in the Mantel-Haenszel test of homogeneity.",
        "</em></p>"
      )
      
      html <- paste0(html, "</div>")
      
      self$results$interpretationNote$setContent(html)
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Populate: Method information
    # ═══════════════════════════════════════════════════════════════════════════
    
    .populateMethodInfo = function() {
      
      if (!self$options$showMethodInfo) {
        return()
      }
      
      html <- "<div style='font-family: sans-serif; line-height: 1.6; font-size: 0.9em;'>"
      
      # ─────────────────────────────────────────────────────────────────────────
      # Overview
      # ─────────────────────────────────────────────────────────────────────────
      
      html <- paste0(html, "
        <h3 style='color: #2874A6; margin-top: 0.5em;'>Stratified Analysis Overview</h3>
        <p>Stratified analysis examines the relationship between two dichotomous variables 
        (row variable and column variable) whilst controlling for a third variable (the stratifying variable). 
        By creating separate 2\u00D72 tables for each level of the stratifying variable, this approach 
        allows researchers to assess whether the association between the row and column variables is 
        <strong>confounded</strong> or <strong>modified</strong> by the stratifying variable.</p>
        
        <p>The analysis addresses two fundamental questions:</p>
        <ol>
          <li><strong>Conditional independence:</strong> Is there an association between the two dichotomous variables 
          after controlling for the stratifying variable?</li>
          <li><strong>Homogeneity:</strong> Is the strength (and direction) of association consistent 
          across strata, or does it vary (effect modification/interaction)?</li>
        </ol>
      ")
      
      # ─────────────────────────────────────────────────────────────────────────
      # Odds Ratio
      # ─────────────────────────────────────────────────────────────────────────
      
      html <- paste0(html, "
        <h3 style='color: #2874A6; margin-top: 1.5em;'>Odds Ratio and Confidence Intervals</h3>
        <p>The odds ratio (OR) quantifies the strength of association between exposure and outcome 
        within each stratum. For a 2\u00D72 table with cells <em>a</em>, <em>b</em>, <em>c</em>, <em>d</em> 
        (reading left-to-right, top-to-bottom), the odds ratio is calculated as:</p>
        
        <p style='text-align: center;'><em>OR</em> = (<em>a</em> \u00D7 <em>d</em>) / (<em>b</em> \u00D7 <em>c</em>)</p>
        
        <p>An OR of 1 indicates no association; OR &gt; 1 indicates positive association; OR &lt; 1 indicates negative association.</p>
        
        <p>The 95% confidence interval is calculated using the standard error of the log odds ratio:</p>
        <p style='text-align: center;'><em>SE</em>(ln OR) = \u221A(1/<em>a</em> + 1/<em>b</em> + 1/<em>c</em> + 1/<em>d</em>)</p>
        
        <p><strong>Haldane-Anscombe correction:</strong> When any cell contains zero, the odds ratio 
        is undefined or zero. The Haldane-Anscombe correction adds 0.5 to all four cells of the table 
        before calculating the OR and its confidence interval. This correction provides a finite, 
        interpretable estimate whilst introducing minimal bias (Fleiss et al. 2003; Pagano & Gauvreau 2018).</p>
      ")
      
      # ─────────────────────────────────────────────────────────────────────────
      # CMH Test
      # ─────────────────────────────────────────────────────────────────────────
      
      html <- paste0(html, "
        <h3 style='color: #2874A6; margin-top: 1.5em;'>Cochran-Mantel-Haenszel (CMH) Test for Conditional Independence</h3>
        <p>The CMH test assesses conditional independence: whether there is an association 
        between the row and column variables after controlling for the stratifying variable. The null hypothesis 
        states that the odds ratio equals 1 in every stratum (no association within any stratum). 
        Rejection indicates evidence of association in at least one stratum.</p>
        
        <p>The test statistic pools information across all strata, weighting each stratum's contribution 
        by its sample size. A significant CMH test indicates that, adjusting for the stratifying variable, 
        there is evidence of association between the row and column variables.</p>
        
        <p>The CMH test also provides the <strong>Mantel-Haenszel estimate of the common odds ratio</strong>, 
        which is a weighted average of the stratum-specific odds ratios. This common OR is an appropriate 
        summary measure only when the odds ratios are homogeneous across strata.</p>
        
        <p><strong>Important caveat:</strong> If the stratum-specific odds ratios are in opposite directions 
        (some &gt; 1, some &lt; 1), the CMH test may fail to detect an association even when one exists, and the 
        common OR may be misleading. In such cases, stratum-specific analyses should be prioritised.</p>
      ")
      
      # ─────────────────────────────────────────────────────────────────────────
      # Homogeneity Tests
      # ─────────────────────────────────────────────────────────────────────────
      
      html <- paste0(html, "
        <h3 style='color: #2874A6; margin-top: 1.5em;'>Tests for Homogeneity of Odds Ratios</h3>
        
        <h4 style='color: #2874A6; margin-top: 1em;'>Mantel-Haenszel Test for Homogeneity</h4>
        <p>This test assesses whether the odds ratios are constant across strata. The null hypothesis 
        is that all stratum-specific odds ratios are equal. The test statistic is based on the weighted 
        sum of squared deviations of the log odds ratios from their weighted average (Lachin 2000).</p>
        
        <h4 style='color: #2874A6; margin-top: 1em;'>Breslow-Day-Tarone Test</h4>
        <p>An alternative test for homogeneity that is generally more powerful than the MH test, 
        particularly when the common OR deviates substantially from 1 (Breslow & Day 1980). The Tarone 
        correction adjusts for the fact that the expected cell counts under homogeneity depend on the 
        unknown common OR, which must be estimated from the data.</p>
        
        <p><strong>Interpretation:</strong> A significant result from either homogeneity test indicates 
        <strong>effect modification</strong> (also called <strong>interaction</strong>): the strength and/or direction of 
        association between the row and column variables varies across levels of the stratifying variable. 
        When homogeneity is rejected, reporting a single common OR is inappropriate, and stratum-specific 
        effects should be examined and reported.</p>
      ")
      
      # ─────────────────────────────────────────────────────────────────────────
      # Interpretational Scenarios
      # ─────────────────────────────────────────────────────────────────────────
      
      html <- paste0(html, "
        <h3 style='color: #2874A6; margin-top: 1.5em;'>Interpretational Scenarios</h3>
        <p>The joint pattern of CMH and homogeneity test results leads to four interpretational scenarios 
        (Azen & Walker 2021; Sheskin 2011):</p>
        
        <ol>
          <li><strong>Significant CMH with homogeneity (non-significant MH and BDT test):</strong> Indicates conditional dependence with 
          consistent association across strata. The common odds ratio is a reliable summary.</li>
          
          <li><strong>Significant CMH with heterogeneity (significant MH and BDT test):</strong> Suggests conditional dependence but 
          varying strength and/or direction of association across strata (interaction). The common OR should not be 
          used as a summary measure; report stratum-specific odds ratios instead.</li>
          
         <li><strong>Non-significant CMH with homogeneity (non-significant MH and BDT test):</strong> Implies conditional independence: 
          no association is detected within strata. Any marginal association (if originally present) vanishes when the data are stratified.</li>
          
          <li><strong>Non-significant CMH with heterogeneity (significant MH and BDT test):</strong> The most complex scenario. 
          If odds ratios are in opposite directions across strata, the CMH test may be unreliable. 
          Stratum-specific chi-squared tests should be evaluated.</li>
        </ol>
      ")
      
      html <- paste0(html, "</div>")
      
      self$results$methodInfo$setContent(html)
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Populate: References
    # ═══════════════════════════════════════════════════════════════════════════
    
    .populateReferences = function() {
      
      references_html <- paste0(
        "<div style='font-size: 0.85em; color: #444; margin: 15px 0; line-height: 1.5;'>",
        "<h3 style='color: #2874A6; margin-top: 0.5em; margin-bottom: 0.5em;'>References</h3>",
        "<p style='margin-left: 20px; text-indent: -20px;'>",
        "Alberti, G. (2024). <em>From Data to Insights. A Beginner's Guide to Cross-Tabulation Analysis</em>. Chapman & Hall.</p>",
        "<p style='margin-left: 20px; text-indent: -20px;'>",
        "Azen, R., & Walker, C. M. (2021). <em>Categorical Data Analysis for the Behavioral and Social Sciences</em> (2nd ed.). Routledge.</p>",
        "<p style='margin-left: 20px; text-indent: -20px;'>",
        "Breslow, N. E., & Day, N. E. (1980). <em>Statistical Methods in Cancer Research. Volume I: The Analysis of Case-Control Studies</em>. IARC Scientific Publications.</p>",
        "<p style='margin-left: 20px; text-indent: -20px;'>",
        "Fleiss, J. L., Levin, B., & Paik, M. C. (2003). <em>Statistical Methods for Rates and Proportions</em> (3rd ed.). Wiley.</p>",
        "<p style='margin-left: 20px; text-indent: -20px;'>",
        "Hoehle, M. (2000). Breslow-Day-Tarone Test [R code]. Retrieved from https://online.stat.psu.edu/onlinecourses/sites/stat504/files/lesson04/breslowday.test_.R</p>",
        "<p style='margin-left: 20px; text-indent: -20px;'>",
        "Lachin, J. M. (2000). <em>Biostatistical Methods: The Assessment of Relative Risks</em>. Wiley.</p>",
        "<p style='margin-left: 20px; text-indent: -20px;'>",
        "Pagano, M., & Gauvreau, K. (2018). <em>Principles of Biostatistics</em> (2nd ed.). Chapman and Hall/CRC.</p>",
        "<p style='margin-left: 20px; text-indent: -20px;'>",
        "Sheskin, D. J. (2011). <em>Handbook of Parametric and Nonparametric Statistical Procedures</em> (5th ed.). Chapman & Hall/CRC.</p>",
        "</div>"
      )
      
      self$results$legendNote$setContent(references_html)
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Plot Data Preparation Functions
    # ═══════════════════════════════════════════════════════════════════════════
    
    .prepareForestData = function(orsAndCIs, marginalORandCI, commonOR, 
                                  commonCI, strataNames, K) {
      
      # Extract OR estimates and CIs for each stratum
      # NOTE: Your .calculateORandCI returns ci_lower and ci_upper, not ci vector
      or_vec <- sapply(orsAndCIs, function(x) x$or)
      ci_lower_vec <- sapply(orsAndCIs, function(x) x$ci_lower)
      ci_upper_vec <- sapply(orsAndCIs, function(x) x$ci_upper)
      
      # Build data frame
      plotData <- data.frame(
        stratum = strataNames,
        or = or_vec,
        ciLower = ci_lower_vec,
        ciUpper = ci_upper_vec,
        type = "Stratum-specific",
        stringsAsFactors = FALSE
      )
      
      # Add marginal OR
      marginal_row <- data.frame(
        stratum = "Marginal (collapsed)",
        or = marginalORandCI$or,
        ciLower = marginalORandCI$ci_lower,
        ciUpper = marginalORandCI$ci_upper,
        type = "Marginal",
        stringsAsFactors = FALSE
      )
      
      # Add common MH OR
      common_row <- data.frame(
        stratum = "Common (MH pooled)",
        or = commonOR,
        ciLower = commonCI[1],
        ciUpper = commonCI[2],
        type = "Common",
        stringsAsFactors = FALSE
      )
      
      plotData <- rbind(plotData, marginal_row, common_row)
      
      # Reverse order so first stratum appears at top
      plotData$stratum <- factor(plotData$stratum, levels = rev(plotData$stratum))
      
      return(list(
        data = plotData,
        n_strata = K
      ))
    },
    
    .prepareDiagnosticTreeData = function(cmh_p, mh_p, bdt_p) {
      
      # Guard against NULL, empty, or NA values
      if (is.null(cmh_p) || is.null(mh_p) || is.null(bdt_p) ||
          length(cmh_p) == 0 || length(mh_p) == 0 || length(bdt_p) == 0 ||
          is.na(cmh_p) || is.na(mh_p) || is.na(bdt_p)) {
        return(NULL)
      }
      
      # Determine significance at α = 0.05
      cmh_sig <- cmh_p < 0.05
      mh_sig <- mh_p < 0.05
      bdt_sig <- bdt_p < 0.05
      
      # Determine scenario (matching your interpretation logic)
      if (cmh_sig && !mh_sig && !bdt_sig) {
        scenario <- "Conditional dependence with homogeneity"
        scenario_num <- 1
        scenario_description <- "Homogeneous association:\nConsistent relationship across strata"
      } else if (cmh_sig && (mh_sig || bdt_sig)) {
        scenario <- "Conditional dependence with heterogeneity"
        scenario_num <- 2
        scenario_description <- "Heterogeneous association:\nEffect modification (interaction) present"
      } else if (!cmh_sig && !mh_sig && !bdt_sig) {
        scenario <- "Conditional independence with homogeneity"
        scenario_num <- 3
        scenario_description <- "Conditional independence:\nAssociation (if any) vanishes when stratified"
      } else {
        scenario <- "Conditional independence with heterogeneity"
        scenario_num <- 4
        scenario_description <- "Opposing associations:\nEffects cancel across strata"
      }
      
      return(list(
        cmh_p = cmh_p,
        mh_p = mh_p,
        bdt_p = bdt_p,
        cmh_sig = cmh_sig,
        mh_sig = mh_sig,
        bdt_sig = bdt_sig,
        scenario = scenario,
        scenario_num = scenario_num,
        scenario_description = scenario_description
      ))
    },
    
    .prepareTrajectoryData = function(orsAndCIs, strataNames) {
      
      # Extract OR estimates and CIs
      or_vec <- sapply(orsAndCIs, function(x) x$or)
      ci_lower_vec <- sapply(orsAndCIs, function(x) x$ci_lower)
      ci_upper_vec <- sapply(orsAndCIs, function(x) x$ci_upper)
      
      # Build data frame with strata as ordered positions
      plotData <- data.frame(
        stratum = strataNames,
        position = 1:length(strataNames),
        or = or_vec,
        log_or = log(or_vec),
        log_ciLower = log(ci_lower_vec),
        log_ciUpper = log(ci_upper_vec),
        stringsAsFactors = FALSE
      )
      
      return(plotData)
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Plot Rendering Functions
    # ═══════════════════════════════════════════════════════════════════════════
    
    .forestPlot = function(image, ggtheme, theme, ...) {
      
      # Retrieve state
      plotState <- image$state
      
      if (is.null(plotState) || is.null(plotState$data)) {
        return(FALSE)
      }
      
      plotData <- plotState$data
      n_points <- nrow(plotData)
      
      # Define colour scheme: significant (CI excludes 1) vs non-significant (CI crosses 1)
      col_sig <- "#D4AC0D"
      col_nonsig <- "#2874A6"
      
      point_cols <- ifelse(plotData$ciLower > 1 | plotData$ciUpper < 1,
                           col_sig,
                           col_nonsig)
      
      # Define point shapes: circle=21 for strata, diamond=23 for common, square=22 for marginal
      point_pch <- ifelse(plotData$type == "Common", 23,
                          ifelse(plotData$type == "Marginal", 22, 21))
      
      # Define point sizes
      point_cex <- ifelse(plotData$type == "Common", 1.6,
                          ifelse(plotData$type == "Marginal", 1.4, 1.2))
      
      # Calculate x-axis range on log scale
      data_min <- min(plotData$ciLower, na.rm = TRUE)
      data_max <- max(plotData$ciUpper, na.rm = TRUE)
      
      # Determine range that includes both data and reference line at 1
      x_min <- min(data_min, 1) * 0.5
      x_max <- max(data_max, 1) * 2
      
      # Calculate margins for labels
      max_label_chars <- max(nchar(as.character(plotData$stratum)))
      left_margin <- max(10, max_label_chars * 0.5)
      
      # Set up plot
      par(
        mar = c(4, left_margin, 3, 2) + 0.1,
        family = "sans"
      )
      
      # Y positions (reversed so first stratum appears at top)
      y_pos <- rev(seq_len(n_points))
      
      # Create empty plot with log scale
      plot(
        x = NULL, y = NULL,
        xlim = c(x_min, x_max),
        ylim = c(0.5, n_points + 0.5),
        xlab = "",
        ylab = "",
        axes = FALSE,
        frame.plot = FALSE,
        log = "x"
      )
      
      # Add vertical reference line at OR = 1
      abline(v = 1, lty = 2, col = "#34495E", lwd = 1.5)
      
      # Add light horizontal grid lines for readability
      abline(h = seq_len(n_points), col = "#E8E8E8", lwd = 0.5)
      
      # Draw confidence interval whiskers
      segments(
        x0 = plotData$ciLower,
        x1 = plotData$ciUpper,
        y0 = y_pos,
        y1 = y_pos,
        col = point_cols,
        lwd = 1.5
      )
      
      # Draw whisker caps
      cap_height <- 0.12
      segments(
        x0 = plotData$ciLower,
        x1 = plotData$ciLower,
        y0 = y_pos - cap_height,
        y1 = y_pos + cap_height,
        col = point_cols,
        lwd = 1.5
      )
      segments(
        x0 = plotData$ciUpper,
        x1 = plotData$ciUpper,
        y0 = y_pos - cap_height,
        y1 = y_pos + cap_height,
        col = point_cols,
        lwd = 1.5
      )
      
      # Draw point estimates
      points(
        x = plotData$or,
        y = y_pos,
        pch = point_pch,
        col = point_cols,
        bg = point_cols,
        cex = point_cex
      )
      
      # Add x-axis with sensible breaks
      axis_breaks <- c(0.01, 0.02, 0.05, 0.1, 0.25, 0.5, 1, 2, 4, 10, 25, 50, 100)
      axis_breaks <- axis_breaks[axis_breaks >= x_min & axis_breaks <= x_max]
      axis(1, at = axis_breaks, col = "#808080", col.axis = "#505050")
      
      # Add y-axis with stratum labels
      axis(2, at = y_pos, labels = plotData$stratum, las = 1, tick = FALSE,
           col.axis = "#505050", cex.axis = 0.9)
      
      # Add axis label
      mtext("Odds Ratio (log scale)", side = 1, line = 2.5, col = "#505050")
      
      # Add title
      mtext("Odds Ratios Across Strata", side = 3, line = 1,
            font = 2, cex = 1.1, col = "#303030")
      
      # Add legend
      legend(
        "bottomright",
        legend = c("Significant (CI excludes 1)", "Non-significant (CI includes 1)"),
        pch = 19,
        col = c(col_sig, col_nonsig),
        bty = "n",
        cex = 0.75,
        text.col = "#505050"
      )
      
      TRUE
    },
    
    .diagnosticTree = function(image, ggtheme, theme, ...) {
      
      # Retrieve state
      treeData <- image$state
      
      if (is.null(treeData)) {
        return(FALSE)
      }
      
      # Build flow-chart style decision tree
      # Define node positions
      nodes <- data.frame(
        x = c(2, 1, 3, 0.5, 1.5, 2.5, 3.5),  # Horizontal positions
        y = c(4, 2, 2, 0, 0, 0, 0),  # Vertical positions (4=top, 0=bottom)
        label = c(
          "CMH Test",
          "Homogeneity Tests\n(MH & BDT)",
          "Homogeneity Tests\n(MH & BDT)",
          "Conditional independence:\nAssociation (if any) vanishes\nwhen stratified",
          "Opposing associations:\nEffects cancel across strata",
          "Homogeneous association:\nConsistent relationship\n(replication) across strata",
          "Heterogeneous association:\nEffect modification \n(interaction) present"
        ),
        node_type = c("decision", "decision", "decision", 
                      "terminal", "terminal", "terminal", "terminal"),
        is_active = c(
          TRUE,  # CMH always evaluated
          !treeData$cmh_sig,  # Left homogeneity (if CMH non-sig)
          treeData$cmh_sig,   # Right homogeneity (if CMH sig)
          !treeData$cmh_sig && !treeData$mh_sig && !treeData$bdt_sig,  # Scenario 3
          !treeData$cmh_sig && (treeData$mh_sig || treeData$bdt_sig),  # Scenario 4
          treeData$cmh_sig && !treeData$mh_sig && !treeData$bdt_sig,   # Scenario 1
          treeData$cmh_sig && (treeData$mh_sig || treeData$bdt_sig)    # Scenario 2
        ),
        stringsAsFactors = FALSE
      )
      
      # Define edges (connections between nodes)
      edges <- data.frame(
        x = c(2, 2, 1, 1, 3, 3),
        y = c(4, 4, 2, 2, 2, 2),
        xend = c(1, 3, 0.5, 1.5, 2.5, 3.5),
        yend = c(2, 2, 0, 0, 0, 0),
        label = c(
          "p > .05\n(non-sig)",
          "p < .05\n(sig)",
          "p > .05",
          "p < .05",
          "p > .05",
          "p < .05"
        ),
        is_active = c(
          !treeData$cmh_sig,
          treeData$cmh_sig,
          !treeData$cmh_sig && !treeData$mh_sig,
          !treeData$cmh_sig && (treeData$mh_sig || treeData$bdt_sig),
          treeData$cmh_sig && !treeData$mh_sig,
          treeData$cmh_sig && (treeData$mh_sig || treeData$bdt_sig)
        ),
        stringsAsFactors = FALSE
      )
      
      # Colour coding: active path in blue, inactive greyed out
      nodes$fill_color <- ifelse(nodes$is_active & nodes$node_type == "terminal",
                                 "#2874A6",  # Active terminal: blue
                                 ifelse(nodes$is_active, "#ECF0F1",  # Active decision: light grey
                                        "#BDC3C7"))  # Inactive: medium grey
      
      nodes$border_color <- ifelse(nodes$is_active, "#2C3E50", "#95A5A6")
      nodes$text_color <- ifelse(nodes$is_active & nodes$node_type == "terminal",
                                 "white", "#2C3E50")
      
      edges$line_color <- ifelse(edges$is_active, "#2C3E50", "#D5DBDB")
      edges$line_width <- ifelse(edges$is_active, 1.2, 0.6)
      
      # Create plot
      plot <- ggplot2::ggplot() +
        
        # Draw edges (arrows)
        ggplot2::geom_segment(data = edges,
                              ggplot2::aes(x = x, y = y, xend = xend, yend = yend,
                                           color = line_color, linewidth = line_width),
                              arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm"), 
                                                     type = "closed")) +
        
        # Edge labels
        ggplot2::geom_text(data = edges,
                           ggplot2::aes(x = (x + xend) / 2, y = (y + yend) / 2, label = label),
                           size = 2.9, hjust = 0.5, vjust = -0.5, color = "#2C3E50") +
        
        # Draw nodes (rectangles)
        # Terminal nodes get extra width for longer text
        ggplot2::geom_rect(data = nodes,
                           ggplot2::aes(xmin = ifelse(node_type == "terminal", x - 0.47, x - 0.37),
                                        xmax = ifelse(node_type == "terminal", x + 0.47, x + 0.37),
                                        ymin = y - 0.4, ymax = y + 0.4,
                                        fill = fill_color, color = border_color),
                           linewidth = 1) +
        
        # Node labels
        ggplot2::geom_text(data = nodes,
                           ggplot2::aes(x = x, y = y, label = label, color = text_color),
                           size = 3.5, fontface = "bold", lineheight = 0.9) +
        
        # Manual scales
        ggplot2::scale_color_identity() +
        ggplot2::scale_fill_identity() +
        ggplot2::scale_linewidth_identity() +
        
        # Coordinate system and limits
        ggplot2::coord_cartesian(xlim = c(-0.5, 4.5), ylim = c(-0.8, 4.8)) +
        
        # Labels
        ggplot2::labs(
          title = "Diagnostic Decision Path",
          subtitle = paste0("Your data: ", treeData$scenario)
        ) +
        
        # Theme
        ggplot2::theme_void() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, size = 13, face = "bold",
                                             margin = ggplot2::margin(b = 5)),
          plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 11, 
                                                color = "#2874A6", face = "bold",
                                                margin = ggplot2::margin(b = 10))
        )
      
      print(plot)
      return(TRUE)
    },
    
    .trajectoryPlot = function(image, ggtheme, theme, ...) {
      
      # Retrieve state
      plotData <- image$state
      
      if (is.null(plotData) || nrow(plotData) < 2) {
        # Need at least 2 strata for trajectory
        return(FALSE)
      }
      
      # Create trajectory plot
      plot <- ggplot2::ggplot(plotData, ggplot2::aes(x = position, y = log_or)) +
        
        # Reference line at log(OR) = 0 (OR = 1)
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", 
                            color = "#34495E", linewidth = 0.5) +
        
        # Confidence ribbon
        ggplot2::geom_ribbon(ggplot2::aes(ymin = log_ciLower, ymax = log_ciUpper),
                             fill = "#85C1E9", alpha = 0.3) +
        
        # Trajectory line
        ggplot2::geom_line(color = "#2874A6", linewidth = 1) +
        
        # Points
        ggplot2::geom_point(color = "#2874A6", size = 3, shape = 21, fill = "white") +
        
        # X-axis with stratum labels
        ggplot2::scale_x_continuous(breaks = plotData$position,
                                    labels = plotData$stratum) +
        
        # Y-axis
        ggplot2::scale_y_continuous(
          breaks = log(c(0.25, 0.5, 1, 2, 4)),
          labels = c("0.25", "0.5", "1", "2", "4")
        ) +
        
        # Labels
        ggplot2::labs(
          x = "Stratum (ordered)",
          y = "Odds Ratio (log scale)",
          title = "Odds Ratio Trajectory Across Ordered Strata"
        ) +
        
        # Apply jamovi theme
        ggtheme +
        
        # Additional customisation
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, size = 12, face = "bold"),
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 9)
        )
      
      print(plot)
      return(TRUE)
    }
  )
)
