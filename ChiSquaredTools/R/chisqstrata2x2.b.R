# This file is a generated template, your changes will not be overwritten

#' @importFrom stats chisq.test mantelhaen.test pchisq xtabs as.formula
#' @importFrom ggplot2 ggplot aes geom_point geom_errorbarh geom_hline geom_vline geom_segment geom_rect geom_text labs theme_minimal theme element_text element_blank scale_x_continuous scale_y_discrete coord_cartesian annotate
#' @export
chisqstrata2x2Class <- R6::R6Class(
  "chisqstrata2x2Class",
  inherit = chisqstrata2x2Base,
  private = list(
    
    .init = function() {
      
      # ═══════════════════════════════════════════════════════════════════════════
      # Early return if required variables not specified
      # ═══════════════════════════════════════════════════════════════════════════
      
      if (is.null(self$options$rows) || 
          is.null(self$options$cols) || 
          is.null(self$options$strata)) {
        return()
      }
      
      rowVar <- self$options$rows
      colVar <- self$options$cols
      strataVar <- self$options$strata
      data <- self$data
      
      # Ensure variables are factors for level extraction
      if (!is.factor(data[[rowVar]])) {
        data[[rowVar]] <- as.factor(data[[rowVar]])
      }
      if (!is.factor(data[[colVar]])) {
        data[[colVar]] <- as.factor(data[[colVar]])
      }
      if (!is.factor(data[[strataVar]])) {
        data[[strataVar]] <- as.factor(data[[strataVar]])
      }
      
      # Get factor levels
      rowLevels <- levels(data[[rowVar]])
      colLevels <- levels(data[[colVar]])
      strataLevels <- levels(data[[strataVar]])
      K <- length(strataLevels)
      
      # ═══════════════════════════════════════════════════════════════════════════
      # Guard: Skip structure rebuild if clearWith options unchanged
      # This prevents table flickering when toggling display-only options
      # ═══════════════════════════════════════════════════════════════════════════
      
      structureKey <- paste(
        rowVar, colVar, strataVar,
        self$options$counts,
        self$options$rowRef,
        self$options$colRef,
        K,
        paste(rowLevels, collapse = ","),
        paste(colLevels, collapse = ","),
        paste(strataLevels, collapse = ","),
        sep = "|"
      )
      
      if (identical(private$.structureKey, structureKey)) {
        # Structure already built for these options, skip rebuild
        return()
      }
      private$.structureKey <- structureKey
      
      # ═══════════════════════════════════════════════════════════════════════════
      # Initialise partial tables (one per stratum)
      # ═══════════════════════════════════════════════════════════════════════════
      
      partialTablesGroup <- self$results$partialTablesGroup
      
      for (k in 1:K) {
        # Always add item - jamovi handles duplicates gracefully when clearWith triggers
        partialTablesGroup$addItem(key = k)
        table <- partialTablesGroup$get(key = k)
        
        # Set title (safe to call repeatedly)
        table$setTitle(paste0(strataVar, " = ", strataLevels[k]))
        
        # Add row name column
        table$addColumn(
          name = 'rowname',
          title = rowVar,
          type = 'text'
        )
        
        # Add data columns (2 columns for 2x2 table)
        for (j in 1:2) {
          table$addColumn(
            name = paste0("col", j),
            title = colLevels[j],
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
        
        # Add empty rows (2 data rows + 1 total row)
        for (i in 1:2) {
          table$addRow(rowKey = i, values = list(rowname = rowLevels[i]))
        }
        table$addRow(rowKey = 'total', values = list(rowname = 'Total'))
      }
      
      # ═══════════════════════════════════════════════════════════════════════════
      # Initialise marginal table
      # ═══════════════════════════════════════════════════════════════════════════
      
      marginalTable <- self$results$marginalTable
      marginalTable$setTitle("Marginal Table (collapsed across strata)")
      
      # Add row name column
      marginalTable$addColumn(
        name = 'rowname',
        title = rowVar,
        type = 'text'
      )
      
      # Add data columns
      for (j in 1:2) {
        marginalTable$addColumn(
          name = paste0("col", j),
          title = colLevels[j],
          type = 'integer',
          superTitle = colVar
        )
      }
      
      # Add row total column
      marginalTable$addColumn(
        name = 'rowtotal',
        title = 'Total',
        type = 'integer'
      )
      
      # Add empty rows
      for (i in 1:2) {
        marginalTable$addRow(rowKey = i, values = list(rowname = rowLevels[i]))
      }
      marginalTable$addRow(rowKey = 'total', values = list(rowname = 'Total'))
      
      # ═══════════════════════════════════════════════════════════════════════════
      # Initialise partial chi-squared table (always shown)
      # ═══════════════════════════════════════════════════════════════════════════
      
      chiSqTable <- self$results$partialChiSqTable
      
      # Add rows for each stratum
      for (k in 1:K) {
        chiSqTable$addRow(rowKey = k, values = list(
          stratum = paste0("Partial Table ", k, " (", strataLevels[k], ")")
        ))
      }
      # Add marginal row
      chiSqTable$addRow(rowKey = 'marginal', values = list(stratum = "Marginal Table"))
      # Add pooled row
      chiSqTable$addRow(rowKey = 'pooled', values = list(stratum = "Pooled (sum of partial)"))
      
      # ═══════════════════════════════════════════════════════════════════════════
      # Initialise odds ratio table
      # ═══════════════════════════════════════════════════════════════════════════
      
      orTable <- self$results$oddsRatioTable
      
      # Add rows for each stratum
      for (k in 1:K) {
        orTable$addRow(rowKey = k, values = list(
          stratum = paste0("Partial Table ", k, " (", strataLevels[k], ")")
        ))
      }
      # Add marginal row
      orTable$addRow(rowKey = 'marginal', values = list(stratum = "Marginal Table"))
      # Add common OR row
      orTable$addRow(rowKey = 'common', values = list(stratum = "MH Common OR"))
      
      # ═══════════════════════════════════════════════════════════════════════════
      # Initialise CMH test table
      # ═══════════════════════════════════════════════════════════════════════════
      
      cmhTable <- self$results$cmhTestTable
      cmhTable$addRow(rowKey = 1, values = list(test = "Cochran-Mantel-Haenszel"))
      
      # ═══════════════════════════════════════════════════════════════════════════
      # Initialise homogeneity tests table
      # ═══════════════════════════════════════════════════════════════════════════
      
      homogTable <- self$results$homogeneityTable
      homogTable$addRow(rowKey = 1, values = list(test = "Mantel-Haenszel"))
      homogTable$addRow(rowKey = 2, values = list(test = "Breslow-Day-Tarone"))
      
      # ═══════════════════════════════════════════════════════════════════════════
      # Initialise interpretation table
      # ═══════════════════════════════════════════════════════════════════════════
      
      interpTable <- self$results$interpretationTable
      interpTable$addRow(rowKey = 'condIndep', values = list(topic = 'Conditional Independence/Dependence'))
      interpTable$addRow(rowKey = 'homogeneity', values = list(topic = 'Homogeneity/Heterogeneity'))
      interpTable$addRow(rowKey = 'scenario', values = list(topic = 'Interpretation'))
      
      # ═══════════════════════════════════════════════════════════════════════════
      # Initialise method info HTML element with empty content
      # This prevents layout shift when showMethodInfo is toggled
      # ═══════════════════════════════════════════════════════════════════════════
      
      self$results$methodInfo$setContent('')
    },
    
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
        
        # Pooled chi-squared test (sum of stratum-specific chi-squared values)
        # This test is robust when stratum-specific associations operate in opposite directions
        pooledChiSq <- list(
          statistic = sum(sapply(chiSqResults, function(x) x$statistic)),
          df = sum(sapply(chiSqResults, function(x) x$df)),
          pvalue = stats::pchisq(
            sum(sapply(chiSqResults, function(x) x$statistic)),
            df = sum(sapply(chiSqResults, function(x) x$df)),
            lower.tail = FALSE
          )
        )
        
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
      # 8. Prepare plot states
      # ═══════════════════════════════════════════════════════════════════════════
      
      # Forest plot state
      # Always prepare forest plot state (visibility controlled by r.yaml)
      forestData <- private$.prepareForestData(orsAndCIs, marginalORandCI, commonOR, 
                                               commonCI, strataNames, K)
      self$results$forestPlot$setState(forestData)
      
      # Trajectory plot state (only if strata marked as ordered)
      if (self$options$showTrajectoryPlot && self$options$strataOrdered) {
        trajectoryData <- private$.prepareTrajectoryData(orsAndCIs, strataNames)
        image <- self$results$trajectoryPlot
        image$setState(trajectoryData)
      }
      
      private$.populatePartialChiSqTable(chiSqResults, marginalChiSq, pooledChiSq, strataNames)
      
      # Populate CMH table with conditional footnote
      private$.populateCMHTable(cmhTest, commonOR, commonCI)
      
      # Populate Homogeneity table with conditional footnote
      private$.populateHomogeneityTable(mhResult, bdtResult, K)
      
      # ═══════════════════════════════════════════════════════════════════════════
      # 9. Generate interpretation Table
      # ═══════════════════════════════════════════════════════════════════════════
      
      private$.populateInterpretationTable(
        marginalChiSq, pooledChiSq, cmhTest, mhResult, bdtResult,
        orsAndCIs, commonOR, commonCI, strataNames,
        rowVar, colVar, strataVar
      )
      
      # ═══════════════════════════════════════════════════════════════════════════
      # 10. Populate method information
      # ═══════════════════════════════════════════════════════════════════════════
      
      private$.populateMethodInfo()
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
    # Helper: Generate OR annotation text (plain text for table footnote)
    # ═══════════════════════════════════════════════════════════════════════════
    
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
        
        # Get pre-existing table from array
        table <- partialTablesGroup$get(key = k)
        
        # Populate rows using setRow (structure already created in .init)
        for (i in 1:2) {
          rowValues <- list(rowname = rowNames[i])
          for (j in 1:2) {
            rowValues[[paste0("col", j)]] <- partialTab[i, j]
          }
          rowValues[['rowtotal']] <- sum(partialTab[i,])
          table$setRow(rowKey = i, values = rowValues)
        }
        
        # Populate column totals row
        totalRowValues <- list(rowname = 'Total')
        for (j in 1:2) {
          totalRowValues[[paste0("col", j)]] <- sum(partialTab[,j])
        }
        totalRowValues[['rowtotal']] <- sum(partialTab)
        table$setRow(rowKey = 'total', values = totalRowValues)
        
        # Add OR annotation as table footnote (always shown)
        annotationText <- private$.generateORAnnotationText(
          or = orsAndCIs[[k]]$or,
          rowLevels = rowNames,
          colLevels = colNames,
          rowRefPos = rowRefPos,
          colRefPos = colRefPos
        )
        table$setNote('orNote', annotationText)
      }
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Populate: Marginal table
    # ═══════════════════════════════════════════════════════════════════════════
    
    .populateMarginalTable = function(marginalTable, rowVar, colVar,
                                      marginalORandCI, rowRefPos, colRefPos) {
      
      table <- self$results$marginalTable
      rowNames <- rownames(marginalTable)
      colNames <- colnames(marginalTable)
      
      # Populate rows using setRow (structure already created in .init)
      for (i in 1:2) {
        rowValues <- list(rowname = rowNames[i])
        for (j in 1:2) {
          rowValues[[paste0("col", j)]] <- marginalTable[i, j]
        }
        rowValues[['rowtotal']] <- sum(marginalTable[i,])
        table$setRow(rowKey = i, values = rowValues)
      }
      
      # Populate column totals row
      totalRowValues <- list(rowname = 'Total')
      for (j in 1:2) {
        totalRowValues[[paste0("col", j)]] <- sum(marginalTable[,j])
      }
      totalRowValues[['rowtotal']] <- sum(marginalTable)
      table$setRow(rowKey = 'total', values = totalRowValues)
      
      # Add OR annotation (always shown)
      annotationText <- private$.generateORAnnotationText(
        or = marginalORandCI$or,
        rowLevels = rowNames,
        colLevels = colNames,
        rowRefPos = rowRefPos,
        colRefPos = colRefPos
      )
      table$setNote('orNote', annotationText)
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Populate: Odds ratio table
    # ═══════════════════════════════════════════════════════════════════════════
    
    .populateOddsRatioTable = function(orsAndCIs, marginalORandCI, commonOR, 
                                       commonCI, strataNames) {
      
      table <- self$results$oddsRatioTable
      K <- length(orsAndCIs)
      
      # Update rows for each partial table using setRow
      for (k in 1:K) {
        or_val <- orsAndCIs[[k]]$or
        ci_lower <- orsAndCIs[[k]]$ci_lower
        ci_upper <- orsAndCIs[[k]]$ci_upper
        
        table$setRow(rowKey = k, values = list(
          stratum = paste0("Partial Table ", k, " (", strataNames[k], ")"),
          or = or_val,
          ciLower = ci_lower,
          ciUpper = ci_upper
        ))
        
        # Highlight OR in red if CI excludes 1 (statistically significant)
        if (!is.na(ci_lower) && !is.na(ci_upper) && (ci_lower > 1 || ci_upper < 1)) {
          table$addFormat(rowKey = k, col = 'or', format = Cell.NEGATIVE)
        }
      }
      
      # Update marginal table row
      table$setRow(rowKey = 'marginal', values = list(
        stratum = "Marginal Table",
        or = marginalORandCI$or,
        ciLower = marginalORandCI$ci_lower,
        ciUpper = marginalORandCI$ci_upper
      ))
      
      # Highlight marginal OR if significant
      if (!is.na(marginalORandCI$ci_lower) && !is.na(marginalORandCI$ci_upper) && 
          (marginalORandCI$ci_lower > 1 || marginalORandCI$ci_upper < 1)) {
        table$addFormat(rowKey = 'marginal', col = 'or', format = Cell.NEGATIVE)
      }
      
      # Update MH common OR row
      table$setRow(rowKey = 'common', values = list(
        stratum = "MH Common OR",
        or = commonOR,
        ciLower = commonCI[1],
        ciUpper = commonCI[2]
      ))
      
      # Highlight common OR if significant
      if (!is.na(commonCI[1]) && !is.na(commonCI[2]) && (commonCI[1] > 1 || commonCI[2] < 1)) {
        table$addFormat(rowKey = 'common', col = 'or', format = Cell.NEGATIVE)
      }
      # Add footnote about CI overlap interpretation
      table$setNote(
        'ciOverlapNote',
        paste0(
          "Overlapping stratum-specific confidence intervals do not necessarily indicate homogeneity of odds ratios. ",
          "Two 95% CIs can overlap substantially yet still differ significantly (Cumming & Finch 2005). ",
          "Use the formal homogeneity tests below to assess whether odds ratios are equal across strata."
        )
      )
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Populate: Partial chi-squared table
    # ═══════════════════════════════════════════════════════════════════════════
    
    .populatePartialChiSqTable = function(chiSqResults, marginalChiSq, pooledChiSq, strataNames) {
      
      table <- self$results$partialChiSqTable
      K <- length(chiSqResults)
      
      # Update rows for each partial table using setRow
      for (k in 1:K) {
        table$setRow(rowKey = k, values = list(
          stratum = paste0("Partial Table ", k, " (", strataNames[k], ")"),
          chisq = chiSqResults[[k]]$statistic,
          df = chiSqResults[[k]]$df,
          pvalue = chiSqResults[[k]]$pvalue
        ))
      }
      
      # Update marginal table row
      table$setRow(rowKey = 'marginal', values = list(
        stratum = "Marginal Table",
        chisq = as.numeric(marginalChiSq$statistic),
        df = as.integer(marginalChiSq$parameter),
        pvalue = marginalChiSq$p.value
      ))
      
      # Update pooled chi-squared row
      table$setRow(rowKey = 'pooled', values = list(
        stratum = "Pooled (sum of partial)",
        chisq = pooledChiSq$statistic,
        df = pooledChiSq$df,
        pvalue = pooledChiSq$pvalue
      ))
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Populate: CMH test table (with conditional footnote)
    # ═══════════════════════════════════════════════════════════════════════════
    
    .populateCMHTable = function(cmhTest, commonOR, commonCI) {
      
      table <- self$results$cmhTestTable
      
      table$setRow(rowKey = 1, values = list(
        test = "Cochran-Mantel-Haenszel",
        statistic = as.numeric(cmhTest$statistic),
        df = as.integer(cmhTest$parameter),
        pvalue = cmhTest$p.value,
        commonOR = commonOR,
        commonCILower = commonCI[1],
        commonCIUpper = commonCI[2]
      ))
      
      # Add conditional footnote based on significance
      cmhSig <- cmhTest$p.value < 0.05
      if (cmhSig) {
        cmhFootnote <- paste0(
          "The CMH test is significant, suggesting conditional dependence: ",
          "the odds ratio in at least one partial table is significantly different from 1."
        )
      } else {
        cmhFootnote <- paste0(
          "The CMH test is not significant, suggesting conditional independence: ",
          "the odds ratios across all partial tables are not significantly different from 1."
        )
      }
      table$setNote('cmhInterpretation', cmhFootnote)
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Populate: Homogeneity tests table (with conditional footnote)
    # ═══════════════════════════════════════════════════════════════════════════
    
    .populateHomogeneityTable = function(mhResult, bdtResult, K) {
      
      table <- self$results$homogeneityTable
      
      # Update Mantel-Haenszel homogeneity test row
      table$setRow(rowKey = 1, values = list(
        test = "Mantel-Haenszel",
        statistic = mhResult$statistic,
        df = mhResult$df,
        pvalue = mhResult$pvalue
      ))
      
      # Update Breslow-Day-Tarone test row
      table$setRow(rowKey = 2, values = list(
        test = "Breslow-Day-Tarone",
        statistic = bdtResult$statistic,
        df = bdtResult$df,
        pvalue = bdtResult$pvalue
      ))
      
      # Add conditional footnote based on significance
      heterogeneitySig <- mhResult$pvalue < 0.05 || bdtResult$pvalue < 0.05
      if (heterogeneitySig) {
        homogFootnote <- paste0(
          "At least one test is significant: the assumption that the odds ratios ",
          "are equal across strata must be rejected (heterogeneity). ",
          "The common (Mantel-Haenszel) odds ratio does not adequately summarise the association; ",
          "examine stratum-specific odds ratios."
        )
      } else {
        homogFootnote <- paste0(
          "Neither test is significant: there is no evidence against the assumption ",
          "that the odds ratios are equal across strata (homogeneity). ",
          "The common (Mantel-Haenszel) odds ratio provides a valid summary."
        )
      }
      table$setNote('homogeneityInterpretation', homogFootnote)
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Add Interpretation Table
    # ═══════════════════════════════════════════════════════════════════════════
    
    .populateInterpretationTable = function(marginalChiSq, pooledChiSq, cmhTest, 
                                            mhResult, bdtResult, orsAndCIs,
                                            commonOR, commonCI, strataNames,
                                            rowVar, colVar, strataVar) {
      
      # ─────────────────────────────────────────────────────────────────────────
      # Guard against invalid test results
      # ─────────────────────────────────────────────────────────────────────────
      
      if (is.null(cmhTest$p.value) || is.na(cmhTest$p.value) ||
          is.null(mhResult$pvalue) || is.na(mhResult$pvalue) ||
          is.null(bdtResult$pvalue) || is.na(bdtResult$pvalue)) {
        
        table <- self$results$interpretationTable
        table$setRow(rowKey = 'condIndep', values = list(
          result = "Could not be determined (invalid test results)"
        ))
        table$setRow(rowKey = 'homogeneity', values = list(
          result = "Could not be determined (invalid test results)"
        ))
        table$setRow(rowKey = 'scenario', values = list(
          result = "Interpretation unavailable"
        ))
        return()
      }
      
      # ─────────────────────────────────────────────────────────────────────────
      # Determine significance flags
      # ─────────────────────────────────────────────────────────────────────────
      
      marginalSig <- marginalChiSq$p.value < 0.05
      pooledSig <- pooledChiSq$pvalue < 0.05
      cmhSig <- cmhTest$p.value < 0.05
      mhSig <- mhResult$pvalue < 0.05
      bdtSig <- bdtResult$pvalue < 0.05
      heterogeneitySig <- mhSig || bdtSig
      
      # ─────────────────────────────────────────────────────────────────────────
      # Assess directional reversal
      # ─────────────────────────────────────────────────────────────────────────
      
      anyStratumSigAbove1 <- any(sapply(orsAndCIs, function(x) x$ci_lower > 1))
      anyStratumSigBelow1 <- any(sapply(orsAndCIs, function(x) x$ci_upper < 1))
      trueDirectionalReversal <- anyStratumSigAbove1 && anyStratumSigBelow1
      anyStratumSig <- anyStratumSigAbove1 || anyStratumSigBelow1
      nStrataSigAbove1 <- sum(sapply(orsAndCIs, function(x) x$ci_lower > 1))
      nStrataSigBelow1 <- sum(sapply(orsAndCIs, function(x) x$ci_upper < 1))
      nStrataSig <- nStrataSigAbove1 + nStrataSigBelow1
      
      # ─────────────────────────────────────────────────────────────────────────
      # STAGE 1: Conditional dependence text
      # ─────────────────────────────────────────────────────────────────────────
      
      cmhFailedDueToReversal <- !cmhSig && pooledSig && trueDirectionalReversal
      
      if (cmhSig) {
        stage1Text <- paste0(
          "CMH test significant (p < 0.05): the association between '", rowVar, 
          "' and '", colVar, "' persists after controlling for '", strataVar, "'."
        )
        conditionalDependence <- TRUE
      } else if (cmhFailedDueToReversal) {
        stage1Text <- paste0(
          "CMH test not significant, but pooled chi-squared is significant with genuine ",
          "directional reversal. Conditional dependence detected despite CMH failure."
        )
        conditionalDependence <- TRUE
      } else {
        stage1Text <- paste0(
          "CMH test not significant (p \u2265 0.05): no overall association between '", 
          rowVar, "' and '", colVar, "' after controlling for '", strataVar, "'."
        )
        conditionalDependence <- FALSE
      }
      
      # ─────────────────────────────────────────────────────────────────────────
      # STAGE 2: Homogeneity text
      # ─────────────────────────────────────────────────────────────────────────
      
      if (heterogeneitySig) {
        stage2Text <- "Significant heterogeneity: odds ratios differ across strata."
        if (trueDirectionalReversal) {
          stage2Text <- paste0(stage2Text, " Effects reverse direction across strata.")
        } else if (nStrataSig == 1) {
          stage2Text <- paste0(stage2Text, " One stratum appears to drive this heterogeneity.")
        }
      } else {
        stage2Text <- "No significant heterogeneity: odds ratios are consistent across strata."
      }
      
      # ─────────────────────────────────────────────────────────────────────────
      # STAGE 3: Scenario and recommendation
      # ─────────────────────────────────────────────────────────────────────────
      
      if (marginalSig && !conditionalDependence && !heterogeneitySig) {
        scenario <- "Spuriousness (Simpson's Paradox)"
        recommendation <- paste0(
          "'", strataVar, "' acts as a confounder. The marginal association is spurious."
        )
      } else if (marginalSig && !cmhSig && heterogeneitySig && !cmhFailedDueToReversal) {
        scenario <- "Simpson's Paradox / Outlier-Driven Heterogeneity"
        recommendation <- paste0(
          "'", strataVar, "' acts as an effect modifier. Examine stratum-specific odds ratios."
        )
      } else if (cmhFailedDueToReversal) {
        scenario <- "Interaction (Directional Reversal)"
        recommendation <- paste0(
          "'", strataVar, "' is an effect modifier with directional reversal. ",
          "Report stratum-specific odds ratios."
        )
      } else if (marginalSig && cmhSig && !heterogeneitySig) {
        if (anyStratumSig) {
          scenario <- "Replication"
          recommendation <- paste0(
            "'", strataVar, "' is neither confounder nor effect modifier. ",
            "Common OR (", sprintf("%.2f", commonOR), ") provides a valid summary."
          )
        } else {
          scenario <- "Spuriousness (CMH Discordant)"
          recommendation <- paste0(
            "CMH significant but no individual stratum is. '", strataVar, 
            "' may act as a confounder."
          )
        }
      } else if (marginalSig && cmhSig && heterogeneitySig) {
        scenario <- "Interaction"
        recommendation <- paste0(
          "'", strataVar, "' acts as an effect modifier. Report stratum-specific odds ratios."
        )
      } else if (!marginalSig && cmhSig) {
        scenario <- "Suppression (Simpson's Paradox)"
        if (!heterogeneitySig) {
          recommendation <- paste0(
            "'", strataVar, "' was masking the relationship. Common OR (", 
            sprintf("%.2f", commonOR), ") provides a valid summary."
          )
        } else {
          recommendation <- paste0(
            "'", strataVar, "' was masking the relationship but odds ratios are heterogeneous. ",
            "Report stratum-specific odds ratios."
          )
        }
      } else if (!marginalSig && !conditionalDependence && !heterogeneitySig) {
        scenario <- "No Association"
        recommendation <- paste0(
          "No association between '", rowVar, "' and '", colVar, 
          "' regardless of '", strataVar, "'."
        )
      } else {
        scenario <- "Ambiguous Pattern"
        recommendation <- "Examine stratum-specific results carefully."
      }
      
      stage3Text <- paste0(scenario, ": ", recommendation, " (Based on \u03B1 = 0.05)")
      
      # ─────────────────────────────────────────────────────────────────────────
      # Populate table
      # ─────────────────────────────────────────────────────────────────────────
      
      table <- self$results$interpretationTable
      table$setRow(rowKey = 'condIndep', values = list(result = stage1Text))
      table$setRow(rowKey = 'homogeneity', values = list(result = stage2Text))
      table$setRow(rowKey = 'scenario', values = list(result = stage3Text))
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Populate: Method information
    # ═══════════════════════════════════════════════════════════════════════════
    
    .populateMethodInfo = function() {
      
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
      # CMH Test
      # ─────────────────────────────────────────────────────────────────────────
      
      html <- paste0(html, "
        <h3 style='color: #2874A6;'>Cochran-Mantel-Haenszel (CMH) Test</h3>
        <p>The CMH test evaluates whether there is a consistent association between the row and column 
        variables across all strata. Under the null hypothesis of conditional independence, the common 
        odds ratio equals 1. The test statistic follows a chi-squared distribution with 1 degree of freedom.</p>
        
        <p>The Mantel-Haenszel common odds ratio provides a weighted average of the stratum-specific 
        odds ratios, where the weights are based on the variance of each stratum's contribution. 
        This estimator is most appropriate when the odds ratios are homogeneous across strata.</p>
        ")
      
      # ─────────────────────────────────────────────────────────────────────────
      # Homogeneity Tests
      # ─────────────────────────────────────────────────────────────────────────
      
      html <- paste0(html, "
        <h3 style='color: #2874A6;'>Tests for Homogeneity of Odds Ratios</h3>
        <p>Two tests are provided to assess whether the stratum-specific odds ratios are homogeneous:</p>
        
        <p><strong>Mantel-Haenszel Test:</strong> Tests whether the weighted log odds ratios are 
        consistent across strata. Uses inverse-variance weights to assess departures from homogeneity.</p>
        
        <p><strong>Breslow-Day-Tarone Test:</strong> A more commonly used test that compares observed 
        cell frequencies to those expected under a common odds ratio. The Tarone correction improves 
        the chi-squared approximation, particularly with sparse data.</p>
        
        <p>If either test is significant, this suggests <strong>heterogeneity</strong>: the association 
        between the row and column variables varies across levels of the stratifying variable 
        (effect modification or interaction). In such cases, stratum-specific odds ratios should be 
        reported rather than the common odds ratio.</p>
        ")
      
      # ─────────────────────────────────────────────────────────────────────────
      # Pooled Chi-Squared
      # ─────────────────────────────────────────────────────────────────────────
      
      html <- paste0(html, "
        <h3 style='color: #2874A6;'>Pooled Chi-Squared Test</h3>
        <p>The pooled chi-squared test sums the stratum-specific chi-squared values, providing an 
        alternative test for conditional dependence. Unlike the CMH test, this approach is robust 
        when stratum-specific associations operate in opposite directions (some odds ratios &gt; 1 
        and others &lt; 1), which can cause the CMH test to fail due to cancellation effects.</p>
        
        <p>When the pooled chi-squared test is significant but the CMH test is not, and the 
        odds ratios show directional reversal, this indicates that the CMH test has failed to 
        detect a genuine conditional association.</p>
        ")
      
      # ─────────────────────────────────────────────────────────────────────────
      # Diagnostic Scenarios
      # ─────────────────────────────────────────────────────────────────────────
      
      html <- paste0(html, "
        <h3 style='color: #2874A6;'>Diagnostic Scenarios</h3>
        <p>The joint pattern of test results indicates different substantive conclusions. 
        Let X denote the row variable, Y denote the column variable, and Z denote the stratifying variable:</p>
        
        <ol style='line-height: 1.8;'>
          <li><strong>Spuriousness:</strong> Marginal association is significant, but CMH test indicates 
          conditional independence with homogeneous odds ratios near 1. The apparent X\u2013Y relationship 
          was due to their separate associations with Z. Z is a <em>confounder</em>.</li>
          
          <li><strong>Spuriousness (CMH Discordant):</strong> Although both marginal and CMH tests are 
          significant with homogeneous odds ratios, no individual stratum shows a significant association 
          (all confidence intervals include 1). The CMH result reflects pooling of individually weak 
          effects rather than a genuine conditional association. Z may act as a <em>confounder</em>.</li>
          
          <li><strong>Simpson's Paradox:</strong> Marginal association is significant, but CMH test is 
          non-significant with significant heterogeneity (without true directional reversal). 
          Stratum-specific associations vary in magnitude such that the aggregate effect is nullified. 
          Z is an <em>effect modifier</em>.</li>
          
          <li><strong>Replication:</strong> Both marginal and conditional associations are significant, 
          with homogeneous odds ratios and at least one stratum individually confirming the association 
          (confidence interval excluding 1). The X\u2013Y relationship is consistent across levels of Z. 
          Z is neither a confounder nor an effect modifier.</li>
          
          <li><strong>Interaction:</strong> Both marginal and conditional associations are significant, 
          but odds ratios are heterogeneous. The X\u2013Y relationship varies in strength across levels 
          of Z. Z is an <em>effect modifier</em>.</li>
          
          <li><strong>Interaction (Directional Reversal):</strong> The CMH test is non-significant, but 
          the pooled chi-squared test is significant and stratum-specific odds ratios show genuine 
          directional reversal (at least one confidence interval entirely above 1 and at least one 
          entirely below 1). The CMH test fails in this scenario because opposing effects cancel out. 
          The pooled test correctly identifies conditional dependence. Z is an <em>effect modifier</em> 
          with qualitative interaction.</li>
          
          <li><strong>Suppression:</strong> No marginal association, but a conditional association 
          emerges when stratified (significant CMH). Z was masking a real X\u2013Y relationship. 
          If odds ratios are homogeneous, the common odds ratio provides a valid summary; if 
          heterogeneous, report stratum-specific odds ratios.</li>
          
          <li><strong>No Association:</strong> Neither marginal nor conditional association is significant, 
          with homogeneous odds ratios near 1. No relationship exists between X and Y, regardless of Z.</li>
          
          <li><strong>Ambiguous Pattern:</strong> No overall association is detected, but odds ratios 
          are heterogeneous across strata. This may reflect opposing effects that are individually too 
          weak to detect, insufficient statistical power, or sampling variability. Examine stratum-specific 
          results carefully and consider the study context.</li>
        </ol>
        
        <p style='margin-top: 1em;'><strong>Note on Simpson's Paradox:</strong> In the broader literature 
        (e.g., Azen &amp; Walker, 2021), Simpson's Paradox refers to any situation where marginal and 
        conditional conclusions conflict. This encompasses both <em>Spuriousness</em> (marginal dependence 
        but conditional independence) and <em>Suppression</em> (marginal independence but conditional 
        dependence). The labels used here provide more specific mechanistic descriptions of how the 
        paradox manifests.</p>
        ")
      
      # ─────────────────────────────────────────────────────────────────────────
      # Haldane-Anscombe Correction
      # ─────────────────────────────────────────────────────────────────────────
      
      html <- paste0(html, "
        <h3 style='color: #2874A6;'>Haldane-Anscombe Correction</h3>
        <p>When a 2\u00D72 table contains a zero cell, the odds ratio is either 0 or undefined. 
        The Haldane-Anscombe correction adds 0.5 to all four cells of the table, allowing 
        estimation of the odds ratio and its confidence interval. This correction is automatically 
        applied when needed for odds ratio calculations and Mantel-Haenszel weights.</p>
        ")
      
      html <- paste0(html, "</div>")
      
      self$results$methodInfo$setContent(html)
    },
    
    # ═══════════════════════════════════════════════════════════════════════════
    # Plot Data Preparation Functions
    # ═══════════════════════════════════════════════════════════════════════════
    
    .prepareForestData = function(orsAndCIs, marginalORandCI, commonOR, 
                                  commonCI, strataNames, K) {
      
      # Extract OR estimates and CIs for each stratum
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
    },
    # Structure cache key for preventing unnecessary rebuilds
    .structureKey = NULL
  )
)