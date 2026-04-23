utils::data(
  "ExampleData1",
  package = "CytokineProfileShinyApp",
  envir = environment()
)
utils::data(
  "ExampleData5",
  package = "CytokineProfileShinyApp",
  envir = environment()
)

appdriver_ci_enabled <- function() {
  flag <- Sys.getenv("CYTOKINEPROFILE_RUN_APPDRIVER", unset = "false")
  tolower(flag) %in% c("1", "true", "yes", "on")
}

skip_if_appdriver_disabled <- function() {
  if (!appdriver_ci_enabled()) {
    testthat::skip("AppDriver tests are disabled for this CI job.")
  }
}

ex1_full <- as.data.frame(ExampleData1)
ex1_numeric <- ex1_full[, -c(1:3), drop = FALSE]
ex1_group <- ex1_full[, -c(2:3), drop = FALSE]
ex1_binary_group <- subset(ex1_full, Group != "ND")
ex1_binary_group_treatment <- subset(
  ex1_full,
  Group != "ND" & Treatment != "Unstimulated"
)
ex1_pca <- subset(
  ex1_full[, -c(3, 23), drop = FALSE],
  Group != "ND" & Treatment != "Unstimulated"
)
ex5_mint <- subset(ExampleData5[, -c(2, 4), drop = FALSE], Group != "ND")
ex5_mint_split <- subset(ExampleData5[, -4, drop = FALSE], Group != "ND")

with_temp_pdf_device <- function(code) {
  pdf_file <- tempfile(fileext = ".pdf")
  grDevices::pdf(pdf_file)
  dev_num <- grDevices::dev.cur()
  on.exit(
    {
      if (
        !is.null(grDevices::dev.list()) && dev_num %in% grDevices::dev.list()
      ) {
        grDevices::dev.off(dev_num)
      }
      if (file.exists(pdf_file)) {
        unlink(pdf_file)
      }
    },
    add = TRUE
  )

  force(code)
}

draw_mock_base_plot <- function(label = NULL) {
  graphics::plot.new()
  if (!is.null(label) && length(label)) {
    label <- as.character(label[[1]])
    if (nzchar(label)) {
      graphics::title(main = label)
    }
  }
  invisible(NULL)
}

record_test_plot <- function(code) {
  if (grDevices::dev.cur() == 1) {
    stop("record_test_plot requires an active graphics device.", call. = FALSE)
  }

  grDevices::dev.control(displaylist = "enable")
  force(code)
  grDevices::recordPlot()
}

suppress_known_plot_warnings <- function(code) {
  withCallingHandlers(
    code,
    warning = function(w) {
      warning_message <- conditionMessage(w)
      if (
        grepl("`aes_string\\(\\)` was deprecated", warning_message) ||
          grepl("the standard deviation is zero", warning_message, fixed = TRUE) ||
          grepl(
            "sPLS-DA dropped unusable predictors before fitting",
            warning_message,
            fixed = TRUE
          ) ||
          (
            grepl("sPLS-DA dropped ", warning_message, fixed = TRUE) &&
              grepl(
                "no retained predictor values",
                warning_message,
                fixed = TRUE
              )
          )
      ) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

first_non_null <- function(x) {
  values <- Filter(Negate(is.null), x)
  if (!length(values)) {
    return(NULL)
  }

  values[[1]]
}

make_progress_recorder <- function() {
  log <- list(set = list(), inc = list())

  list(
    set = function(message = NULL, value = NULL, detail = NULL) {
      log$set[[length(log$set) + 1L]] <<- list(
        message = message,
        value = value,
        detail = detail
      )
      invisible(NULL)
    },
    inc = function(amount, detail = NULL) {
      log$inc[[length(log$inc) + 1L]] <<- list(
        amount = amount,
        detail = detail
      )
      invisible(NULL)
    },
    get_log = function() log
  )
}

helper_invalid_numeric_df <- data.frame(
  good = c(1, 2, 3),
  has_na = c(1, NA_real_, 3),
  has_nan = c(1, NaN, 3),
  has_inf = c(1, Inf, 3),
  has_ninf = c(1, -Inf, 3)
)

helper_font_settings <- list(
  base_size = 12,
  plot_title = 14,
  x_title = 13,
  y_title = 13,
  x_text = 11,
  y_text = 10,
  legend_title = 12,
  legend_text = 11,
  strip_text = 12,
  annotation_text = 10,
  row_names = 9,
  col_names = 8,
  cell_text = 7,
  variable_names = 15,
  point_labels = 16
)

anova_test_df <- data.frame(
  Group = factor(rep(c("A", "B", "C"), each = 4)),
  Batch = factor(rep(c("X", "Y"), times = 6)),
  Outcome1 = c(1, 2, 2, 3, 4, 5, 5, 6, 7, 8, 8, 9),
  Outcome2 = c(2, 3, 3, 4, 5, 6, 6, 7, 8, 9, 9, 10),
  stringsAsFactors = FALSE
)

anova_invalid_df <- data.frame(
  Group = factor(rep("A", 4)),
  Outcome = c(1, 2, 3, 4)
)

bp2_test_df <- data.frame(
  Group = factor(rep(c("A", "B"), each = 4)),
  Marker1 = c(1, 2, 0, 4, 5, 6, -1, 8),
  Marker2 = c(2, 4, 8, 16, 32, 64, 128, 256)
)

heatmap_constant_df <- data.frame(
  Group = factor(c("A", "B", "A", "B")),
  Marker1 = c(1, 1, 1, 1),
  Marker2 = c(1, 2, 3, 4)
)

heatmap_bad_df <- data.frame(
  Marker1 = c(1, NA_real_, 3),
  Marker2 = c(2, Inf, 4)
)

make_plsr_sparse_missing_fixture <- function() {
  data.frame(
    Outcome = c(10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32),
    MarkerA = c(5, 6, 8, 9, 11, 12, 14, 15, 17, 18, 20, 21),
    MarkerB = c(30, 29, 28, 26, 25, 23, 22, 20, 19, 18, 16, 15),
    MarkerC = c(100, NA, 102, 103, NA, 105, 106, 107, 108, NA, 110, 111),
    MarkerSparse = c(1.5, NA, NA, NA, NA, 2.5, NA, NA, NA, NA, NA, NA)
  )
}

make_plsr_vip_single_predictor_fixture <- function() {
  data.frame(
    Outcome = c(5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27),
    Driver = c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24),
    Weak = c(9, -8, 7, -6, 5, -4, 3, -2, 1, -3, 2, -1)
  )
}

make_plsr_one_component_fixture <- function() {
  data.frame(
    Outcome = c(8, 10, 12, 14, 16, 18),
    MarkerA = c(1, 2, 3, 4, 5, 6),
    MarkerB = c(10, NA, 12, 13, NA, 15),
    MarkerC = c(30, 31, 33, 34, 36, 37)
  )
}

make_plsr_unusable_predictor_fixture <- function() {
  data.frame(
    Outcome = c(2, 4, 6, 8, 10, 12),
    MissingAll = rep(NA_real_, 6),
    SparseFew = c(1, NA, NA, NA, 2, NA),
    Constant = rep(7, 6)
  )
}

make_splsda_sparse_missing_fixture <- function() {
  data.frame(
    Group = factor(c(rep("A", 6), rep("B", 6))),
    MarkerA = c(NA, 1.2, 1.7, 2.1, 2.6, 3.0, 6.0, 6.5, 7.0, 7.4, 7.9, 8.3),
    MarkerB = c(NA, 4.3, 4.0, 4.6, 4.2, 4.8, 5.1, 5.6, 5.3, 5.9, 5.5, 6.0),
    MarkerC = c(
      NA,
      10.2,
      10.5,
      10.1,
      10.7,
      10.9,
      12.8,
      13.2,
      12.9,
      13.5,
      13.1,
      13.8
    ),
    MarkerD = c(NA, 2.1, 2.7, 2.4, 3.0, 2.8, 4.0, 4.6, 4.3, 4.9, 4.5, 5.2),
    MarkerSparse = c(90, NA, NA, NA, NA, 88, NA, NA, NA, NA, NA, NA)
  )
}

make_splsda_unusable_predictor_fixture <- function() {
  data.frame(
    Group = factor(c("A", "A", "A", "B", "B", "B")),
    MissingAll = rep(NA_real_, 6),
    SparseFew = c(1, NA, NA, NA, 2, NA),
    Constant = rep(7, 6)
  )
}

make_splsda_vip_single_predictor_fixture <- function() {
  data.frame(
    Group = factor(c(rep("A", 8), rep("B", 8))),
    Driver = c(
      1.0,
      1.4,
      1.7,
      2.1,
      2.4,
      2.8,
      3.1,
      3.5,
      6.2,
      6.6,
      7.0,
      7.4,
      7.7,
      8.1,
      8.5,
      8.9
    ),
    WeakNoise = c(
      3.1,
      2.7,
      3.4,
      2.8,
      3.3,
      2.9,
      3.2,
      2.6,
      3.0,
      2.8,
      3.5,
      2.7,
      3.4,
      2.9,
      3.1,
      2.5
    ),
    WeakDrift = c(
      1.5,
      1.8,
      1.4,
      1.9,
      1.6,
      2.0,
      1.7,
      2.1,
      1.6,
      1.9,
      1.5,
      2.0,
      1.7,
      2.1,
      1.8,
      2.2
    )
  )
}

make_ancova_interaction_fixture <- function() {
  design_grid <- expand.grid(
    Group = c("A", "B"),
    Treatment = c("T1", "T2"),
    Replicate = seq_len(6),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  design_grid$Time <- rep(seq(1, 6), times = 4)
  design_grid$IL.10 <- with(
    design_grid,
    10 +
      ifelse(Group == "B", 1.5, 0) +
      ifelse(Treatment == "T2", 1.0, 0) +
      0.6 * Time +
      ifelse(Group == "B", 0.25 * Time, 0) +
      ifelse(Treatment == "T2", 0.35 * Time, 0) +
      Replicate * 0.02
  )
  design_grid$IL.17F <- with(
    design_grid,
    6 +
      ifelse(Group == "B", 0.8, 0) +
      ifelse(Treatment == "T2", 0.5, 0) +
      0.4 * Time +
      ifelse(Treatment == "T2", 0.2 * Time, 0) +
      Replicate * 0.01
  )

  design_grid$Group <- factor(design_grid$Group)
  design_grid$Treatment <- factor(design_grid$Treatment)
  design_grid
}
