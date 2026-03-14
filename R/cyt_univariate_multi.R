#' Univariate Tests for Multi-Level Categorical Predictors
#'
#' @description
#' `cyt_univariate_multi` provides univariate statistical testing for
#' categorical predictors with more than two levels. For one-way
#' designs, each categorical predictor and numeric outcome pair receives
#' a global test followed by pairwise comparisons when appropriate.
#' Users may choose between classical ANOVA with Tukey's Honest
#' Significant Difference (HSD) or a non-parametric Kruskal-Wallis test
#' followed by pairwise Wilcoxon rank-sum tests. The function also
#' supports explicit two-way ANOVA and ANCOVA designs with optional
#' `primary:secondary` and `primary:covariate` interaction terms.
#'
#' @param data A data frame or matrix containing both categorical
#'   and continuous variables. Character columns will be converted
#'   to factors.
#' @param method Character specifying the type of global test to
#'   perform. Use `"anova"` (default) for one-way ANOVA with Tukey
#'   HSD or `"kruskal"` for Kruskal-Wallis with pairwise Wilcoxon
#'   tests. `"kruskal"` is only supported when `design = "one_way"`.
#' @param design Character specifying the model design. Use `"one_way"`
#'   (default) to preserve the existing multi-level ANOVA/Kruskal-Wallis
#'   workflow, `"two_way"` for two-way ANOVA, or `"ancova"` for ANCOVA.
#' @param cat_vars Optional character vector of predictor column
#'   names used by the one-way workflow. When `NULL`, all factor or
#'   character columns in `data` are used.
#' @param cont_vars Optional character vector of numeric outcome
#'   variable names. When `NULL`, all numeric columns in `data` are
#'   used, excluding `covariate_col` for ANCOVA.
#' @param p_adjust_method Character string specifying the method for
#'   p-value adjustment across pairwise Kruskal-Wallis follow-up
#'   comparisons. Passed to `p.adjust`. Default is `"BH"`.
#' @param primary_cat_var Optional primary categorical predictor used by
#'   the two-way ANOVA and ANCOVA workflows.
#' @param secondary_cat_var Optional secondary categorical predictor used
#'   by the two-way ANOVA and ANCOVA workflows.
#' @param covariate_col Optional numeric covariate used by the ANCOVA
#'   workflow.
#' @param include_primary_secondary_interaction Logical. Whether to
#'   include the `primary:secondary` interaction term when
#'   `secondary_cat_var` is supplied.
#' @param include_primary_covariate_interaction Logical. Whether to
#'   include the `primary:covariate` interaction term when
#'   `covariate_col` is supplied.
#' @param format_output Logical. If `TRUE`, returns a list with
#'   global results, pairwise results, and assumption summaries;
#'   otherwise (default) returns a list of numeric vectors keyed by
#'   `"Outcome_Categorical"` for the one-way workflow. Two-way ANOVA and
#'   ANCOVA return raw model bundles keyed by outcome when
#'   `format_output = FALSE`.
#' @param progress Optional. A Shiny `Progress` object for reporting
#'   progress updates during the analysis.
#' @return Either a list of adjusted pairwise p-values (if
#'   `format_output = FALSE` and `design = "one_way"`) or a list with
#'   `results`, `pairwise`, and `assumptions` data frames
#'   (if `format_output = TRUE`).
#' @examples
#' data("ExampleData1")
#' cyt_univariate_multi(ExampleData1[, c(1:2, 5:6)], method = "kruskal",
#'   format_output = TRUE
#' )
#' @author Shubh Saraswat
#' @importFrom stats aov as.formula kruskal.test TukeyHSD
#' @export
cyt_univariate_multi <- function(
  data,
  method = c("anova", "kruskal"),
  design = c("one_way", "two_way", "ancova"),
  cat_vars = NULL,
  cont_vars = NULL,
  p_adjust_method = "BH",
  primary_cat_var = NULL,
  secondary_cat_var = NULL,
  covariate_col = NULL,
  include_primary_secondary_interaction = FALSE,
  include_primary_covariate_interaction = FALSE,
  format_output = FALSE,
  progress = NULL
) {
  safe_numeric <- function(x) {
    if (is.null(x)) {
      return(NA_real_)
    }

    value <- suppressWarnings(as.numeric(unname(x)[1]))
    if (!length(value) || is.na(value)) {
      return(NA_real_)
    }

    value
  }

  normalize_names <- function(x) {
    if (is.null(x)) {
      return(NULL)
    }

    make.names(x, unique = FALSE)
  }

  factor_or_character <- function(x) {
    is.factor(x) || is.character(x)
  }

  format_p_value <- function(p) {
    if (!is.finite(p) || is.na(p)) {
      return("NA")
    }

    formatC(p, digits = 3, format = "f")
  }

  bool_label <- function(x, modeled = FALSE) {
    if (isTRUE(modeled)) {
      return("Modeled")
    }
    if (isTRUE(x)) {
      return("Yes")
    }
    if (identical(x, FALSE)) {
      return("No")
    }

    NA_character_
  }

  empty_global_df <- function() {
    data.frame(
      Outcome = character(),
      Categorical = character(),
      Comparison = character(),
      Test = character(),
      Statistic = numeric(),
      P_value = numeric(),
      stringsAsFactors = FALSE
    )
  }

  empty_pairwise_df <- function() {
    data.frame(
      Outcome = character(),
      Categorical = character(),
      Comparison = character(),
      Test = character(),
      Estimate = numeric(),
      P_adj = numeric(),
      stringsAsFactors = FALSE
    )
  }

  empty_complex_results_df <- function() {
    data.frame(
      Outcome = character(),
      Design = character(),
      Model_Formula = character(),
      Primary = character(),
      Secondary = character(),
      Covariate = character(),
      Term = character(),
      SS_Type = character(),
      Df = numeric(),
      Sum_Sq = numeric(),
      Mean_Sq = numeric(),
      Statistic = numeric(),
      P_value = numeric(),
      Interpret_Caution = character(),
      stringsAsFactors = FALSE
    )
  }

  empty_complex_pairwise_df <- function() {
    data.frame(
      Outcome = character(),
      Design = character(),
      Model_Formula = character(),
      Effect = character(),
      Contrast = character(),
      Method = character(),
      Estimate = numeric(),
      P_adj = numeric(),
      Interpret_Caution = character(),
      stringsAsFactors = FALSE
    )
  }

  empty_complex_assumption_df <- function() {
    data.frame(
      Outcome = character(),
      Design = character(),
      Model_Formula = character(),
      Normality_P = numeric(),
      Normality_Met = character(),
      Variance_P = numeric(),
      Variance_Met = character(),
      Cell_Count_Min = numeric(),
      Low_Cell_Count = character(),
      Covariate_Variation_Issue = character(),
      Slope_Homogeneity_P = numeric(),
      Slope_Homogeneity_Met = character(),
      Interaction_Significant = character(),
      Interaction_Term = character(),
      Interaction_P = numeric(),
      Warnings = character(),
      stringsAsFactors = FALSE
    )
  }

  build_complex_formula <- function(
    outcome,
    design,
    primary,
    secondary,
    covariate,
    include_ps,
    include_pc
  ) {
    rhs_terms <- primary

    if (identical(design, "two_way")) {
      rhs_terms <- c(rhs_terms, secondary)
      if (isTRUE(include_ps)) {
        rhs_terms <- c(rhs_terms, paste(primary, secondary, sep = ":"))
      }
    } else {
      if (!is.null(secondary)) {
        rhs_terms <- c(rhs_terms, secondary)
      }
      rhs_terms <- c(rhs_terms, covariate)
      if (isTRUE(include_ps) && !is.null(secondary)) {
        rhs_terms <- c(rhs_terms, paste(primary, secondary, sep = ":"))
      }
      if (isTRUE(include_pc)) {
        rhs_terms <- c(rhs_terms, paste(primary, covariate, sep = ":"))
      }
    }

    paste(outcome, "~", paste(rhs_terms, collapse = " + "))
  }

  extract_ss_rows <- function(table_obj, ss_type, outcome, meta, caution_text) {
    if (is.null(table_obj)) {
      return(empty_complex_results_df())
    }

    tbl_df <- as.data.frame(table_obj, stringsAsFactors = FALSE)
    tbl_df$Term <- rownames(tbl_df)
    rownames(tbl_df) <- NULL
    tbl_df <- tbl_df[
      !(tbl_df$Term %in% c("Residuals", "(Intercept)")),
      ,
      drop = FALSE
    ]
    if (nrow(tbl_df) == 0L) {
      return(empty_complex_results_df())
    }

    sum_sq_col <- intersect(c("Sum Sq", "Sum_Sq", "Sum.Sq"), names(tbl_df))
    mean_sq_col <- intersect(c("Mean Sq", "Mean_Sq", "Mean.Sq"), names(tbl_df))
    df_col <- intersect("Df", names(tbl_df))
    stat_col <- intersect(c("F value", "F", "F.value"), names(tbl_df))
    p_col <- intersect(c("Pr(>F)", "Pr..F.", "Pr(>Chisq)"), names(tbl_df))

    df_vals <- if (length(df_col) > 0L) {
      suppressWarnings(as.numeric(tbl_df[[df_col[1]]]))
    } else {
      rep(NA_real_, nrow(tbl_df))
    }
    sum_sq_vals <- if (length(sum_sq_col) > 0L) {
      suppressWarnings(as.numeric(tbl_df[[sum_sq_col[1]]]))
    } else {
      rep(NA_real_, nrow(tbl_df))
    }
    mean_sq_vals <- if (length(mean_sq_col) > 0L) {
      suppressWarnings(as.numeric(tbl_df[[mean_sq_col[1]]]))
    } else {
      suppressWarnings(sum_sq_vals / df_vals)
    }
    stat_vals <- if (length(stat_col) > 0L) {
      suppressWarnings(as.numeric(tbl_df[[stat_col[1]]]))
    } else {
      rep(NA_real_, nrow(tbl_df))
    }
    p_vals <- if (length(p_col) > 0L) {
      suppressWarnings(as.numeric(tbl_df[[p_col[1]]]))
    } else {
      rep(NA_real_, nrow(tbl_df))
    }

    data.frame(
      Outcome = outcome,
      Design = meta$design,
      Model_Formula = meta$model_formula,
      Primary = meta$primary,
      Secondary = meta$secondary,
      Covariate = meta$covariate,
      Term = tbl_df$Term,
      SS_Type = ss_type,
      Df = df_vals,
      Sum_Sq = sum_sq_vals,
      Mean_Sq = mean_sq_vals,
      Statistic = stat_vals,
      P_value = p_vals,
      Interpret_Caution = caution_text,
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  }

  get_interaction_p <- function(table_obj, term) {
    if (is.null(table_obj)) {
      return(NA_real_)
    }

    tbl_df <- as.data.frame(table_obj, stringsAsFactors = FALSE)
    if (!(term %in% rownames(tbl_df))) {
      return(NA_real_)
    }

    p_col <- intersect(c("Pr(>F)", "Pr..F.", "Pr(>Chisq)"), names(tbl_df))
    if (length(p_col) == 0L) {
      return(NA_real_)
    }

    suppressWarnings(as.numeric(tbl_df[term, p_col[1]]))
  }

  collect_interaction_caution <- function(type_tables, interaction_terms) {
    if (length(interaction_terms) == 0L) {
      return(list(
        caution_text = "",
        significant = FALSE,
        interaction_term = NA_character_,
        interaction_p = NA_real_
      ))
    }

    significant_messages <- character()
    significant_terms <- character()
    significant_ps <- numeric()

    for (term in interaction_terms) {
      p_val <- get_interaction_p(type_tables$type_III, term)
      if (is.na(p_val)) {
        p_val <- get_interaction_p(type_tables$type_II, term)
      }
      if (is.na(p_val)) {
        p_val <- get_interaction_p(type_tables$type_I, term)
      }
      if (is.finite(p_val) && !is.na(p_val) && p_val < 0.05) {
        significant_terms <- c(significant_terms, term)
        significant_ps <- c(significant_ps, p_val)
        significant_messages <- c(
          significant_messages,
          paste0(
            "Interaction term significant (p = ",
            format_p_value(p_val),
            "). Main effect comparisons reflect marginal means averaged ",
            "across the other factor or covariate; interpret with caution."
          )
        )
      }
    }

    if (length(significant_messages) == 0L) {
      return(list(
        caution_text = "",
        significant = FALSE,
        interaction_term = NA_character_,
        interaction_p = NA_real_
      ))
    }

    order_idx <- order(significant_ps)
    list(
      caution_text = paste(unique(significant_messages), collapse = " "),
      significant = TRUE,
      interaction_term = paste(significant_terms[order_idx], collapse = "; "),
      interaction_p = significant_ps[order_idx][1]
    )
  }

  names(data) <- make.names(names(data), unique = TRUE)
  method <- match.arg(method)
  design <- match.arg(design)
  analysis_message <- switch(
    design,
    one_way = "Running Multi-level Univariate Tests...",
    two_way = "Running Two-way ANOVA...",
    ancova = "Running ANCOVA..."
  )
  if (!is.null(progress)) {
    progress$set(
      message = analysis_message,
      value = 0
    )
  }
  if (is.null(p_adjust_method) || !nzchar(p_adjust_method)) {
    p_adjust_method <- "none"
  }

  x1_df <- as.data.frame(data)

  if (!is.null(progress)) {
    progress$inc(0.05, detail = "Preparing data")
  }
  char_cols <- names(x1_df)[sapply(x1_df, is.character)]
  if (length(char_cols) > 0) {
    x1_df[char_cols] <- lapply(x1_df[char_cols], as.factor)
  }

  primary_cat_var <- normalize_names(primary_cat_var)
  secondary_cat_var <- normalize_names(secondary_cat_var)
  covariate_col <- normalize_names(covariate_col)
  cat_vars <- normalize_names(cat_vars)
  cont_vars <- normalize_names(cont_vars)

  if (design != "one_way") {
    if (!requireNamespace("car", quietly = TRUE)) {
      stop(
        "Package 'car' is required for two-way ANOVA and ANCOVA support.",
        call. = FALSE
      )
    }
    if (!requireNamespace("emmeans", quietly = TRUE)) {
      stop(
        "Package 'emmeans' is required for two-way ANOVA and ANCOVA pairwise comparisons.",
        call. = FALSE
      )
    }
    if (method != "anova") {
      stop(
        "'kruskal' is only supported when design = 'one_way'.",
        call. = FALSE
      )
    }

    all_names <- names(x1_df)
    check_exists <- function(x, label) {
      if (!is.null(x) && !(x %in% all_names)) {
        stop(label, " '", x, "' was not found in data.", call. = FALSE)
      }
    }

    check_exists(primary_cat_var, "Primary factor")
    check_exists(secondary_cat_var, "Secondary factor")
    check_exists(covariate_col, "Covariate")

    role_vars <- Filter(Negate(is.null), c(
      primary_cat_var,
      secondary_cat_var,
      covariate_col
    ))
    if (length(unique(role_vars)) != length(role_vars)) {
      stop(
        "Primary, secondary, and covariate columns must be distinct.",
        call. = FALSE
      )
    }

    if (design == "two_way") {
      if (is.null(primary_cat_var) || is.null(secondary_cat_var)) {
        stop(
          "Two-way ANOVA requires both primary_cat_var and secondary_cat_var.",
          call. = FALSE
        )
      }
    }

    if (design == "ancova") {
      if (is.null(primary_cat_var) || is.null(covariate_col)) {
        stop(
          "ANCOVA requires both primary_cat_var and covariate_col.",
          call. = FALSE
        )
      }
    }

    if (
      isTRUE(include_primary_secondary_interaction) &&
        is.null(secondary_cat_var)
    ) {
      stop(
        "include_primary_secondary_interaction requires secondary_cat_var.",
        call. = FALSE
      )
    }
    if (
      isTRUE(include_primary_covariate_interaction) &&
        is.null(covariate_col)
    ) {
      stop(
        "include_primary_covariate_interaction requires covariate_col.",
        call. = FALSE
      )
    }

    if (!is.null(primary_cat_var) &&
      !factor_or_character(x1_df[[primary_cat_var]])) {
      stop("primary_cat_var must be categorical.", call. = FALSE)
    }
    if (!is.null(secondary_cat_var) &&
      !factor_or_character(x1_df[[secondary_cat_var]])) {
      stop("secondary_cat_var must be categorical.", call. = FALSE)
    }
    if (!is.null(covariate_col) && !is.numeric(x1_df[[covariate_col]])) {
      stop("covariate_col must be numeric.", call. = FALSE)
    }

    if (!is.null(primary_cat_var) && is.character(x1_df[[primary_cat_var]])) {
      x1_df[[primary_cat_var]] <- as.factor(x1_df[[primary_cat_var]])
    }
    if (!is.null(secondary_cat_var) &&
      is.character(x1_df[[secondary_cat_var]])) {
      x1_df[[secondary_cat_var]] <- as.factor(x1_df[[secondary_cat_var]])
    }

    if (is.null(cont_vars)) {
      cont_vars <- names(x1_df)[vapply(x1_df, is.numeric, logical(1))]
      if (!is.null(covariate_col)) {
        cont_vars <- setdiff(cont_vars, covariate_col)
      }
    }
    if (length(cont_vars) == 0L) {
      stop(
        "No numeric outcomes are available for the selected design.",
        call. = FALSE
      )
    }

    total_iter <- length(cont_vars)
    iter_inc <- if (total_iter > 0) 0.70 / total_iter else 0
    current_iter <- 0L
    complex_rows <- list()
    complex_pairwise <- list()
    complex_assumptions <- list()
    raw_results <- list()
    warning_messages <- character()

    if (!is.null(progress)) {
      progress$inc(
        0.05,
        detail = "Checking model inputs and outcomes"
      )
    }

    for (outcome in cont_vars) {
      current_iter <- current_iter + 1L
      if (!is.null(progress)) {
        progress$inc(
          iter_inc,
          detail = paste(
            "Processing outcome",
            current_iter,
            "of",
            total_iter,
            ":",
            outcome
          )
        )
      }

      model_formula <- build_complex_formula(
        outcome = outcome,
        design = design,
        primary = primary_cat_var,
        secondary = secondary_cat_var,
        covariate = covariate_col,
        include_ps = include_primary_secondary_interaction,
        include_pc = include_primary_covariate_interaction
      )
      meta <- list(
        design = design,
        model_formula = model_formula,
        primary = primary_cat_var %||% "",
        secondary = secondary_cat_var %||% "",
        covariate = covariate_col %||% ""
      )

      model_cols <- unique(Filter(Negate(is.null), c(
        outcome,
        primary_cat_var,
        secondary_cat_var,
        covariate_col
      )))
      outcome_df <- x1_df[, model_cols, drop = FALSE]
      outcome_df <- outcome_df[stats::complete.cases(outcome_df), , drop = FALSE]

      if (nrow(outcome_df) == 0L) {
        skip_message <- paste0(
          "Outcome '", outcome, "' skipped: no complete cases remained ",
          "after filtering the modeled variables."
        )
        warning_messages <- c(warning_messages, skip_message)
        complex_assumptions[[length(complex_assumptions) + 1L]] <- data.frame(
          Outcome = outcome,
          Design = design,
          Model_Formula = model_formula,
          Normality_P = NA_real_,
          Normality_Met = NA_character_,
          Variance_P = NA_real_,
          Variance_Met = NA_character_,
          Cell_Count_Min = NA_real_,
          Low_Cell_Count = NA_character_,
          Covariate_Variation_Issue = NA_character_,
          Slope_Homogeneity_P = NA_real_,
          Slope_Homogeneity_Met = NA_character_,
          Interaction_Significant = NA_character_,
          Interaction_Term = NA_character_,
          Interaction_P = NA_real_,
          Warnings = skip_message,
          stringsAsFactors = FALSE
        )
        next
      }

      outcome_df[[primary_cat_var]] <- droplevels(
        as.factor(outcome_df[[primary_cat_var]])
      )
      if (!is.null(secondary_cat_var)) {
        outcome_df[[secondary_cat_var]] <- droplevels(
          as.factor(outcome_df[[secondary_cat_var]])
        )
      }

      skip_messages <- character()
      if (nlevels(outcome_df[[primary_cat_var]]) < 2L) {
        skip_messages <- c(
          skip_messages,
          paste0(
            "Outcome '", outcome, "' skipped: the primary factor has fewer ",
            "than 2 levels after filtering."
          )
        )
      }
      if (!is.null(secondary_cat_var) &&
        nlevels(outcome_df[[secondary_cat_var]]) < 2L) {
        skip_messages <- c(
          skip_messages,
          paste0(
            "Outcome '", outcome, "' skipped: the secondary factor has fewer ",
            "than 2 levels after filtering."
          )
        )
      }

      cell_factor <- if (design == "ancova" && is.null(secondary_cat_var)) {
        droplevels(outcome_df[[primary_cat_var]])
      } else {
        interaction(
          outcome_df[[primary_cat_var]],
          outcome_df[[secondary_cat_var]],
          drop = TRUE,
          sep = ":"
        )
      }
      cell_counts <- table(cell_factor)
      min_cell_count <- if (length(cell_counts) > 0L) {
        min(as.numeric(cell_counts))
      } else {
        NA_real_
      }
      low_cell_count <- is.finite(min_cell_count) && !is.na(min_cell_count) &&
        min_cell_count < 3

      covariate_variation_issue <- FALSE
      primary_covariate_issue <- FALSE
      if (design == "ancova") {
        covariate_split <- split(outcome_df[[covariate_col]], cell_factor)
        covariate_variation_issue <- any(vapply(
          covariate_split,
          function(x) {
            vals <- x[is.finite(x)]
            length(unique(vals)) < 2L || stats::sd(vals) == 0
          },
          logical(1)
        ))

        if (isTRUE(include_primary_covariate_interaction)) {
          primary_split <- split(
            outcome_df[[covariate_col]],
            outcome_df[[primary_cat_var]]
          )
          primary_counts <- table(outcome_df[[primary_cat_var]])
          primary_covariate_issue <- any(vapply(
            names(primary_split),
            function(level_name) {
              vals <- primary_split[[level_name]]
              vals <- vals[is.finite(vals)]
              cnt <- as.numeric(primary_counts[level_name])
              cnt < 3 || length(unique(vals)) < 2L || stats::sd(vals) == 0
            },
            logical(1)
          ))
        }
      }

      preflight_messages <- character()
      if (isTRUE(low_cell_count)) {
        preflight_messages <- c(
          preflight_messages,
          paste0(
            "Outcome '", outcome, "': at least one factor cell has fewer than 3 ",
            "observations (minimum = ", min_cell_count, ")."
          )
        )
      }
      if (isTRUE(covariate_variation_issue)) {
        preflight_messages <- c(
          preflight_messages,
          paste0(
            "Outcome '", outcome, "': at least one factor cell has fewer than 2 ",
            "unique covariate values or zero covariate variance."
          )
        )
      }
      if (isTRUE(primary_covariate_issue)) {
        preflight_messages <- c(
          preflight_messages,
          paste0(
            "Outcome '", outcome, "': at least one primary-factor level has fewer ",
            "than 3 complete cases or insufficient covariate variation for the ",
            "primary:covariate interaction."
          )
        )
      }

      if (length(skip_messages) > 0L) {
        warning_messages <- c(warning_messages, skip_messages, preflight_messages)
        complex_assumptions[[length(complex_assumptions) + 1L]] <- data.frame(
          Outcome = outcome,
          Design = design,
          Model_Formula = model_formula,
          Normality_P = NA_real_,
          Normality_Met = NA_character_,
          Variance_P = NA_real_,
          Variance_Met = NA_character_,
          Cell_Count_Min = min_cell_count,
          Low_Cell_Count = bool_label(low_cell_count),
          Covariate_Variation_Issue = bool_label(
            covariate_variation_issue || primary_covariate_issue
          ),
          Slope_Homogeneity_P = NA_real_,
          Slope_Homogeneity_Met = NA_character_,
          Interaction_Significant = NA_character_,
          Interaction_Term = NA_character_,
          Interaction_P = NA_real_,
          Warnings = paste(c(skip_messages, preflight_messages), collapse = " "),
          stringsAsFactors = FALSE
        )
        next
      }

      formula_obj <- stats::as.formula(model_formula)
      model_matrix <- stats::model.matrix(formula_obj, data = outcome_df)
      if (qr(model_matrix)$rank < ncol(model_matrix)) {
        rank_message <- paste0(
          "Outcome '", outcome, "' skipped: the fitted model is rank-deficient ",
          "or singular."
        )
        warning_messages <- c(
          warning_messages,
          rank_message,
          preflight_messages
        )
        complex_assumptions[[length(complex_assumptions) + 1L]] <- data.frame(
          Outcome = outcome,
          Design = design,
          Model_Formula = model_formula,
          Normality_P = NA_real_,
          Normality_Met = NA_character_,
          Variance_P = NA_real_,
          Variance_Met = NA_character_,
          Cell_Count_Min = min_cell_count,
          Low_Cell_Count = bool_label(low_cell_count),
          Covariate_Variation_Issue = bool_label(
            covariate_variation_issue || primary_covariate_issue
          ),
          Slope_Homogeneity_P = NA_real_,
          Slope_Homogeneity_Met = NA_character_,
          Interaction_Significant = NA_character_,
          Interaction_Term = NA_character_,
          Interaction_P = NA_real_,
          Warnings = paste(c(rank_message, preflight_messages), collapse = " "),
          stringsAsFactors = FALSE
        )
        next
      }

      fit_base <- tryCatch(
        stats::lm(formula_obj, data = outcome_df),
        error = function(e) e
      )
      if (inherits(fit_base, "error")) {
        fit_message <- paste0(
          "Outcome '", outcome, "' skipped because model fitting failed: ",
          conditionMessage(fit_base)
        )
        warning_messages <- c(
          warning_messages,
          fit_message,
          preflight_messages
        )
        complex_assumptions[[length(complex_assumptions) + 1L]] <- data.frame(
          Outcome = outcome,
          Design = design,
          Model_Formula = model_formula,
          Normality_P = NA_real_,
          Normality_Met = NA_character_,
          Variance_P = NA_real_,
          Variance_Met = NA_character_,
          Cell_Count_Min = min_cell_count,
          Low_Cell_Count = bool_label(low_cell_count),
          Covariate_Variation_Issue = bool_label(
            covariate_variation_issue || primary_covariate_issue
          ),
          Slope_Homogeneity_P = NA_real_,
          Slope_Homogeneity_Met = NA_character_,
          Interaction_Significant = NA_character_,
          Interaction_Term = NA_character_,
          Interaction_P = NA_real_,
          Warnings = paste(c(fit_message, preflight_messages), collapse = " "),
          stringsAsFactors = FALSE
        )
        next
      }

      outcome_df_type3 <- outcome_df
      factor_cols <- Filter(Negate(is.null), c(
        primary_cat_var,
        secondary_cat_var
      ))
      for (factor_col in factor_cols) {
        outcome_df_type3[[factor_col]] <- droplevels(
          as.factor(outcome_df_type3[[factor_col]])
        )
        if (nlevels(outcome_df_type3[[factor_col]]) > 1L) {
          stats::contrasts(outcome_df_type3[[factor_col]]) <- stats::contr.sum(
            nlevels(outcome_df_type3[[factor_col]])
          )
        }
      }

      fit_type3 <- tryCatch(
        stats::lm(formula_obj, data = outcome_df_type3),
        error = function(e) e
      )
      if (inherits(fit_type3, "error")) {
        fit3_message <- paste0(
          "Outcome '", outcome, "' skipped because Type III model fitting failed: ",
          conditionMessage(fit_type3)
        )
        warning_messages <- c(
          warning_messages,
          fit3_message,
          preflight_messages
        )
        complex_assumptions[[length(complex_assumptions) + 1L]] <- data.frame(
          Outcome = outcome,
          Design = design,
          Model_Formula = model_formula,
          Normality_P = NA_real_,
          Normality_Met = NA_character_,
          Variance_P = NA_real_,
          Variance_Met = NA_character_,
          Cell_Count_Min = min_cell_count,
          Low_Cell_Count = bool_label(low_cell_count),
          Covariate_Variation_Issue = bool_label(
            covariate_variation_issue || primary_covariate_issue
          ),
          Slope_Homogeneity_P = NA_real_,
          Slope_Homogeneity_Met = NA_character_,
          Interaction_Significant = NA_character_,
          Interaction_Term = NA_character_,
          Interaction_P = NA_real_,
          Warnings = paste(c(fit3_message, preflight_messages), collapse = " "),
          stringsAsFactors = FALSE
        )
        next
      }

      type_tables <- list(
        type_I = tryCatch(
          stats::anova(fit_base),
          error = function(e) NULL
        ),
        type_II = tryCatch(
          car::Anova(fit_base, type = 2),
          error = function(e) NULL
        ),
        type_III = tryCatch(
          car::Anova(fit_type3, type = 3),
          error = function(e) NULL
        )
      )

      interaction_terms <- character()
      if (
        isTRUE(include_primary_secondary_interaction) &&
          !is.null(secondary_cat_var)
      ) {
        interaction_terms <- c(
          interaction_terms,
          paste(primary_cat_var, secondary_cat_var, sep = ":")
        )
      }
      if (
        isTRUE(include_primary_covariate_interaction) &&
          !is.null(covariate_col)
      ) {
        interaction_terms <- c(
          interaction_terms,
          paste(primary_cat_var, covariate_col, sep = ":")
        )
      }

      caution_info <- collect_interaction_caution(
        type_tables,
        interaction_terms
      )

      ss_rows <- list(
        extract_ss_rows(
          type_tables$type_II,
          "Type II",
          outcome,
          meta,
          caution_info$caution_text
        ),
        extract_ss_rows(
          type_tables$type_III,
          "Type III",
          outcome,
          meta,
          caution_info$caution_text
        ),
        extract_ss_rows(
          type_tables$type_I,
          "Type I",
          outcome,
          meta,
          caution_info$caution_text
        )
      )
      ss_rows <- Filter(function(x) nrow(x) > 0L, ss_rows)
      if (length(ss_rows) > 0L) {
        complex_rows[[length(complex_rows) + 1L]] <- do.call(rbind, ss_rows)
      }

      effect_pairwise <- list()
      factor_effects <- Filter(Negate(is.null), c(
        primary_cat_var,
        secondary_cat_var
      ))
      for (effect_name in factor_effects) {
        emm_obj <- tryCatch(
          suppressMessages(
            emmeans::emmeans(fit_base, specs = effect_name)
          ),
          error = function(e) NULL
        )
        if (is.null(emm_obj)) {
          next
        }

        pair_obj <- tryCatch(
          suppressMessages(
            emmeans::contrast(
              emm_obj,
              method = "pairwise",
              adjust = "tukey"
            )
          ),
          error = function(e) NULL
        )
        if (is.null(pair_obj)) {
          next
        }

        pair_df <- tryCatch(
          suppressMessages(as.data.frame(summary(pair_obj))),
          error = function(e) NULL
        )
        if (is.null(pair_df) || nrow(pair_df) == 0L) {
          next
        }

        effect_pairwise[[length(effect_pairwise) + 1L]] <- data.frame(
          Outcome = outcome,
          Design = design,
          Model_Formula = model_formula,
          Effect = effect_name,
          Contrast = pair_df$contrast,
          Method = "emmeans (Tukey)",
          Estimate = suppressWarnings(as.numeric(pair_df$estimate)),
          P_adj = suppressWarnings(as.numeric(pair_df$p.value)),
          Interpret_Caution = caution_info$caution_text,
          stringsAsFactors = FALSE,
          row.names = NULL
        )
      }
      if (length(effect_pairwise) > 0L) {
        complex_pairwise[[length(complex_pairwise) + 1L]] <- do.call(
          rbind,
          effect_pairwise
        )
      }

      resids <- stats::residuals(fit_base)
      normality_p <- tryCatch(
        stats::shapiro.test(resids)$p.value,
        error = function(e) NA_real_
      )
      variance_p <- tryCatch(
        safe_numeric(car::leveneTest(y = resids, group = cell_factor)[1, "Pr(>F)"]),
        error = function(e) NA_real_
      )

      slope_p <- NA_real_
      slope_label <- NA_character_
      if (design == "ancova") {
        if (isTRUE(include_primary_covariate_interaction)) {
          slope_label <- "Modeled"
        } else {
          slope_formula <- stats::as.formula(
            paste(
              outcome,
              "~",
              paste(
                c(
                  primary_cat_var,
                  covariate_col,
                  paste(primary_cat_var, covariate_col, sep = ":")
                ),
                collapse = " + "
              )
            )
          )
          slope_fit <- tryCatch(
            stats::lm(slope_formula, data = outcome_df_type3),
            error = function(e) NULL
          )
          if (!is.null(slope_fit)) {
            slope_table <- tryCatch(
              car::Anova(slope_fit, type = 3),
              error = function(e) NULL
            )
            slope_p <- get_interaction_p(
              slope_table,
              paste(primary_cat_var, covariate_col, sep = ":")
            )
          }
          slope_label <- bool_label(!is.na(slope_p) && slope_p > 0.05)
        }
      }

      warning_texts <- preflight_messages
      if (nzchar(caution_info$caution_text)) {
        warning_texts <- c(warning_texts, caution_info$caution_text)
      }
      if (length(warning_texts) > 0L) {
        warning_messages <- c(warning_messages, warning_texts)
      }

      complex_assumptions[[length(complex_assumptions) + 1L]] <- data.frame(
        Outcome = outcome,
        Design = design,
        Model_Formula = model_formula,
        Normality_P = normality_p,
        Normality_Met = bool_label(!is.na(normality_p) && normality_p > 0.05),
        Variance_P = variance_p,
        Variance_Met = bool_label(!is.na(variance_p) && variance_p > 0.05),
        Cell_Count_Min = min_cell_count,
        Low_Cell_Count = bool_label(low_cell_count),
        Covariate_Variation_Issue = bool_label(
          covariate_variation_issue || primary_covariate_issue
        ),
        Slope_Homogeneity_P = slope_p,
        Slope_Homogeneity_Met = slope_label,
        Interaction_Significant = bool_label(caution_info$significant),
        Interaction_Term = caution_info$interaction_term,
        Interaction_P = caution_info$interaction_p,
        Warnings = paste(unique(warning_texts), collapse = " "),
        stringsAsFactors = FALSE,
        row.names = NULL
      )

      raw_results[[outcome]] <- list(
        model = fit_base,
        model_type_III = fit_type3,
        ss_tables = type_tables,
        pairwise = if (length(effect_pairwise) > 0L) {
          do.call(rbind, effect_pairwise)
        } else {
          NULL
        },
        assumptions = complex_assumptions[[length(complex_assumptions)]]
      )
    }

    if (!format_output) {
      if (length(unique(warning_messages)) > 0L) {
        warning(paste(unique(warning_messages), collapse = "\n"), call. = FALSE)
      }
      if (!is.null(progress)) {
        progress$set(message = analysis_message, value = 1, detail = "Finished")
      }
      return(raw_results)
    }

    if (!is.null(progress)) {
      progress$inc(0.05, detail = "Formatting results")
    }

    global_df <- if (length(complex_rows) > 0L) {
      do.call(rbind, complex_rows)
    } else {
      empty_complex_results_df()
    }
    pairwise_df <- if (length(complex_pairwise) > 0L) {
      do.call(rbind, complex_pairwise)
    } else {
      empty_complex_pairwise_df()
    }
    assumption_df <- if (length(complex_assumptions) > 0L) {
      do.call(rbind, complex_assumptions)
    } else {
      empty_complex_assumption_df()
    }

    if (nrow(global_df) > 0L) {
      global_df$Df <- round(global_df$Df, 3)
      global_df$Sum_Sq <- round(global_df$Sum_Sq, 3)
      global_df$Mean_Sq <- round(global_df$Mean_Sq, 3)
      global_df$Statistic <- round(global_df$Statistic, 3)
      global_df$P_value <- round(global_df$P_value, 3)
      global_df$SS_Type <- factor(
        global_df$SS_Type,
        levels = c("Type II", "Type III", "Type I")
      )
      global_df <- global_df[
        order(global_df$Outcome, global_df$Term, global_df$SS_Type),
        ,
        drop = FALSE
      ]
      global_df$SS_Type <- as.character(global_df$SS_Type)
    }

    if (nrow(pairwise_df) > 0L) {
      pairwise_df$Estimate <- round(pairwise_df$Estimate, 3)
      pairwise_df$P_adj <- round(pairwise_df$P_adj, 3)
    }

    if (nrow(assumption_df) > 0L) {
      assumption_df$Normality_P <- round(assumption_df$Normality_P, 3)
      assumption_df$Variance_P <- round(assumption_df$Variance_P, 3)
      assumption_df$Cell_Count_Min <- round(assumption_df$Cell_Count_Min, 3)
      assumption_df$Slope_Homogeneity_P <- round(
        assumption_df$Slope_Homogeneity_P,
        3
      )
      assumption_df$Interaction_P <- round(assumption_df$Interaction_P, 3)
    }

    if (!is.null(progress)) {
      progress$set(message = analysis_message, value = 1, detail = "Finished")
    }

    if (length(unique(warning_messages)) > 0L) {
      warning(paste(unique(warning_messages), collapse = "\n"), call. = FALSE)
    }

    return(list(
      results = global_df,
      pairwise = pairwise_df,
      assumptions = assumption_df
    ))
  }

  if (!is.null(progress)) {
    progress$inc(
      0.05,
      detail = "Preparing outcomes and factor comparisons"
    )
  }
  if (is.null(cat_vars)) {
    cat_vars <- names(x1_df)[sapply(x1_df, is.factor)]
  }
  if (is.null(cont_vars)) {
    cont_vars <- names(x1_df)[sapply(x1_df, is.numeric)]
  }

  valid_cats <- cat_vars[vapply(
    cat_vars,
    function(v) {
      n <- length(levels(x1_df[[v]]))
      n > 1L && !(method == "anova" && n > 10L)
    },
    logical(1)
  )]
  total_iter <- length(valid_cats) * length(cont_vars)
  iter_inc <- if (total_iter > 0) 0.70 / total_iter else 0
  current_iter <- 0L

  test_results <- list()
  global_rows <- list()
  pairwise_rows <- list()
  assumption_results <- list()

  for (cat_var in cat_vars) {
    levs <- levels(x1_df[[cat_var]])
    n_lvls <- length(levs)
    if (n_lvls <= 1) {
      next
    }
    if (method == "anova" && n_lvls > 10) {
      next
    }

    for (outcome in cont_vars) {
      current_iter <- current_iter + 1L
      if (!is.null(progress)) {
        progress$inc(
          iter_inc,
          detail = paste(
            "Processing comparison",
            current_iter,
            "of",
            total_iter,
            ":",
            outcome,
            "by",
            cat_var
          )
        )
      }

      key <- paste(outcome, cat_var, sep = "_")

      if (method == "anova") {
        model <- stats::aov(
          stats::as.formula(paste(outcome, "~", cat_var)),
          data = x1_df
        )
        model_summary <- summary(model)[[1]]
        global_rows[[length(global_rows) + 1L]] <- data.frame(
          Outcome = outcome,
          Categorical = cat_var,
          Comparison = "Overall",
          Test = "ANOVA",
          Statistic = safe_numeric(model_summary[1, "F value"]),
          P_value = safe_numeric(model_summary[1, "Pr(>F)"]),
          stringsAsFactors = FALSE,
          row.names = NULL
        )

        tuk <- TukeyHSD(model)[[cat_var]]
        p_vals <- tuk[, "p adj"]
        names(p_vals) <- rownames(tuk)
        test_results[[key]] <- round(p_vals, 4)

        pairwise_rows[[key]] <- data.frame(
          Outcome = outcome,
          Categorical = cat_var,
          Comparison = rownames(tuk),
          Test = "TukeyHSD",
          Estimate = as.numeric(tuk[, "diff"]),
          P_adj = as.numeric(tuk[, "p adj"]),
          stringsAsFactors = FALSE,
          row.names = NULL
        )

        resids <- stats::residuals(model)
        norm_p <- tryCatch(
          stats::shapiro.test(resids)$p.value,
          error = function(e) NA_real_
        )
        bartlett_p <- tryCatch(
          stats::bartlett.test(
            stats::as.formula(paste(outcome, "~", cat_var)),
            data = x1_df
          )$p.value,
          error = function(e) NA_real_
        )
        assumption_results[[length(assumption_results) + 1L]] <- list(
          Outcome = outcome,
          Categorical = cat_var,
          normality_p = round(norm_p, 4),
          homogeneity_p = round(bartlett_p, 4),
          normality_ok = !is.na(norm_p) && norm_p > 0.05,
          homogeneity_ok = !is.na(bartlett_p) && bartlett_p > 0.05
        )
      } else {
        kw <- stats::kruskal.test(
          stats::as.formula(paste(outcome, "~", cat_var)),
          data = x1_df
        )
        global_rows[[length(global_rows) + 1L]] <- data.frame(
          Outcome = outcome,
          Categorical = cat_var,
          Comparison = "Overall",
          Test = "Kruskal-Wallis",
          Statistic = safe_numeric(kw$statistic),
          P_value = safe_numeric(kw$p.value),
          stringsAsFactors = FALSE,
          row.names = NULL
        )

        if (is.finite(kw$p.value) && kw$p.value < 0.05) {
          observed_levels <- levels(base::droplevels(x1_df[[cat_var]]))
          pair_rows <- lapply(
            utils::combn(observed_levels, 2, simplify = FALSE),
            function(level_pair) {
              pair_df <- x1_df[
                x1_df[[cat_var]] %in% level_pair,
                c(outcome, cat_var),
                drop = FALSE
              ]
              pair_df <- stats::na.omit(pair_df)

              grp1 <- pair_df[[outcome]][pair_df[[cat_var]] == level_pair[1]]
              grp2 <- pair_df[[outcome]][pair_df[[cat_var]] == level_pair[2]]
              if (!length(grp1) || !length(grp2)) {
                return(NULL)
              }

              wt <- tryCatch(
                stats::wilcox.test(
                  grp1,
                  grp2,
                  conf.int = TRUE,
                  exact = FALSE
                ),
                error = function(e) NULL
              )
              if (is.null(wt)) {
                return(NULL)
              }

              data.frame(
                Outcome = outcome,
                Categorical = cat_var,
                Comparison = paste(level_pair, collapse = " vs "),
                Test = "Pairwise Wilcoxon",
                Estimate = safe_numeric(wt$estimate),
                P_adj_raw = safe_numeric(wt$p.value),
                stringsAsFactors = FALSE,
                row.names = NULL
              )
            }
          )

          pair_rows <- Filter(Negate(is.null), pair_rows)
          pair_df <- if (length(pair_rows) > 0L) {
            do.call(rbind, pair_rows)
          } else {
            data.frame(
              Outcome = character(),
              Categorical = character(),
              Comparison = character(),
              Test = character(),
              Estimate = numeric(),
              P_adj_raw = numeric(),
              stringsAsFactors = FALSE,
              row.names = NULL
            )
          }

          if (nrow(pair_df) > 0L) {
            pair_df$P_adj <- stats::p.adjust(
              pair_df$P_adj_raw,
              method = p_adjust_method
            )
            pair_df$P_adj_raw <- NULL
            test_results[[key]] <- stats::setNames(
              round(pair_df$P_adj, 4),
              pair_df$Comparison
            )
          } else {
            pair_df$P_adj_raw <- NULL
            pair_df$P_adj <- numeric(0)
            test_results[[key]] <- numeric(0)
          }

          pairwise_rows[[key]] <- pair_df
        } else {
          test_results[[key]] <- numeric(0)
          pairwise_rows[[key]] <- empty_pairwise_df()
        }
      }
    }
  }

  if (length(test_results) == 0 && length(global_rows) == 0) {
    warning(
      "No valid comparisons were performed. ",
      "Check that your data has numeric columns and factors with sufficient levels."
    )
    if (!format_output) {
      return(list())
    }

    return(list(
      results = empty_global_df(),
      pairwise = empty_pairwise_df(),
      assumptions = NULL
    ))
  }

  if (!format_output) {
    if (!is.null(progress)) {
      progress$set(message = analysis_message, value = 1, detail = "Finished")
    }
    return(test_results)
  }

  if (!is.null(progress)) {
    progress$inc(0.05, detail = "Formatting results")
  }

  global_df <- if (length(global_rows) > 0L) {
    do.call(rbind, global_rows)
  } else {
    empty_global_df()
  }

  pairwise_tables <- Filter(
    function(x) is.data.frame(x) && nrow(x) > 0L,
    pairwise_rows
  )
  pairwise_df <- if (length(pairwise_tables) > 0L) {
    do.call(rbind, pairwise_tables)
  } else {
    empty_pairwise_df()
  }

  global_df$Statistic <- round(global_df$Statistic, 3)
  global_df$P_value <- round(global_df$P_value, 3)

  if (nrow(pairwise_df) > 0L) {
    pairwise_df$Estimate <- round(pairwise_df$Estimate, 3)
    pairwise_df$P_adj <- round(pairwise_df$P_adj, 3)
  }

  if (!is.null(progress)) {
    progress$set(message = analysis_message, value = 1, detail = "Finished")
  }

  assumption_df <- if (length(assumption_results) > 0L) {
    do.call(
      rbind,
      lapply(assumption_results, function(a) {
        data.frame(
          Outcome = a$Outcome,
          Categorical = a$Categorical,
          Normality_P = a$normality_p,
          Normality_Met = ifelse(
            isTRUE(a$normality_ok),
            "\u2714 Yes",
            "\u2718 No"
          ),
          Homogeneity_P = a$homogeneity_p,
          Homogeneity_Met = ifelse(
            isTRUE(a$homogeneity_ok),
            "\u2714 Yes",
            "\u2718 No"
          ),
          stringsAsFactors = FALSE,
          row.names = NULL
        )
      })
    )
  } else {
    NULL
  }

  list(
    results = global_df,
    pairwise = pairwise_df,
    assumptions = assumption_df
  )
}
