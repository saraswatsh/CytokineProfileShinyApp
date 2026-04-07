app_builtin_dataset_names <- getFromNamespace(
  "app_builtin_dataset_names",
  "CytokineProfileShinyApp"
)
app_asset_href <- getFromNamespace(
  "app_asset_href",
  "CytokineProfileShinyApp"
)
app_builtin_dataset <- getFromNamespace(
  "app_builtin_dataset",
  "CytokineProfileShinyApp"
)
font_settings_known_fields <- getFromNamespace(
  "font_settings_known_fields",
  "CytokineProfileShinyApp"
)
font_settings_defaults <- getFromNamespace(
  "font_settings_defaults",
  "CytokineProfileShinyApp"
)
font_settings_has_values <- getFromNamespace(
  "font_settings_has_values",
  "CytokineProfileShinyApp"
)
validate_font_setting_scalar <- getFromNamespace(
  "validate_font_setting_scalar",
  "CytokineProfileShinyApp"
)
normalize_font_settings <- getFromNamespace(
  "normalize_font_settings",
  "CytokineProfileShinyApp"
)
apply_font_settings_ggplot <- getFromNamespace(
  "apply_font_settings_ggplot",
  "CytokineProfileShinyApp"
)
font_settings_ggplot_text_size <- getFromNamespace(
  "font_settings_ggplot_text_size",
  "CytokineProfileShinyApp"
)
font_settings_mixomics_text_scale <- getFromNamespace(
  "font_settings_mixomics_text_scale",
  "CytokineProfileShinyApp"
)
font_settings_mixomics_indiv_cex <- getFromNamespace(
  "font_settings_mixomics_indiv_cex",
  "CytokineProfileShinyApp"
)
font_settings_mixomics_scale <- getFromNamespace(
  "font_settings_mixomics_scale",
  "CytokineProfileShinyApp"
)
font_settings_mixomics_indiv_args <- getFromNamespace(
  "font_settings_mixomics_indiv_args",
  "CytokineProfileShinyApp"
)
font_settings_mixomics_loadings_args <- getFromNamespace(
  "font_settings_mixomics_loadings_args",
  "CytokineProfileShinyApp"
)
font_settings_plotvar_cex <- getFromNamespace(
  "font_settings_plotvar_cex",
  "CytokineProfileShinyApp"
)
font_settings_plotvar_args <- getFromNamespace(
  "font_settings_plotvar_args",
  "CytokineProfileShinyApp"
)
font_settings_base_graphics <- getFromNamespace(
  "font_settings_base_graphics",
  "CytokineProfileShinyApp"
)
font_settings_heatmap_args <- getFromNamespace(
  "font_settings_heatmap_args",
  "CytokineProfileShinyApp"
)
summarize_invalid_numeric_columns <- getFromNamespace(
  "summarize_invalid_numeric_columns",
  "CytokineProfileShinyApp"
)
format_invalid_numeric_summary <- getFromNamespace(
  "format_invalid_numeric_summary",
  "CytokineProfileShinyApp"
)
step2_normalize_missing_tokens <- getFromNamespace(
  "step2_normalize_missing_tokens",
  "CytokineProfileShinyApp"
)
step2_is_numeric_like <- getFromNamespace(
  "step2_is_numeric_like",
  "CytokineProfileShinyApp"
)
step2_parse_numeric_values <- getFromNamespace(
  "step2_parse_numeric_values",
  "CytokineProfileShinyApp"
)
step2_conflicting_type_cols <- getFromNamespace(
  "step2_conflicting_type_cols",
  "CytokineProfileShinyApp"
)
step2_restore_bucket_selection <- getFromNamespace(
  "step2_restore_bucket_selection",
  "CytokineProfileShinyApp"
)
step2_classify_columns <- getFromNamespace(
  "step2_classify_columns",
  "CytokineProfileShinyApp"
)
step2_apply_type_overrides <- getFromNamespace(
  "step2_apply_type_overrides",
  "CytokineProfileShinyApp"
)
safe_zscore_column <- getFromNamespace(
  "safe_zscore_column",
  "CytokineProfileShinyApp"
)
read_uploaded_flat_file <- getFromNamespace(
  "read_uploaded_flat_file",
  "CytokineProfileShinyApp"
)

test_that("run_app forwards the resolved app directory to shiny::runApp", {
  called <- NULL

  testthat::local_mocked_bindings(
    run_app_path = function() "C:/pkg/inst/app",
    .package = "CytokineProfileShinyApp"
  )
  testthat::local_mocked_bindings(
    runApp = function(appDir, display.mode) {
      called <<- list(appDir = appDir, display.mode = display.mode)
      invisible("ok")
    },
    .package = "shiny"
  )

  expect_invisible(run_app())
  expect_equal(called$appDir, "C:/pkg/inst/app")
  expect_equal(called$display.mode, "normal")
})

test_that("run_app errors when the installed app file cannot be found", {
  testthat::local_mocked_bindings(
    run_app_path = function() "",
    .package = "CytokineProfileShinyApp"
  )

  expect_error(run_app(), "Could not find app directory.")
})

test_that("app runtime helpers resolve package assets and built-in datasets", {
  expect_equal(
    app_builtin_dataset_names(),
    c(
      "ExampleData1",
      "ExampleData2",
      "ExampleData3",
      "ExampleData4",
      "ExampleData5"
    )
  )
  expect_match(app_asset_href("logo.png"), "^app-www/logo\\.png$")

  data_df <- app_builtin_dataset("ExampleData1")
  expect_s3_class(data_df, "data.frame")
  expect_true("Group" %in% names(data_df))
  expect_error(
    app_builtin_dataset("MissingDataset"),
    "Unknown built-in dataset"
  )
})

test_that("font setting helpers validate and normalize supported values", {
  expect_setequal(
    font_settings_known_fields(),
    c(
      "base_size",
      "plot_title",
      "x_title",
      "y_title",
      "x_text",
      "y_text",
      "legend_title",
      "legend_text",
      "strip_text",
      "annotation_text",
      "row_names",
      "col_names",
      "cell_text",
      "variable_names",
      "point_labels"
    )
  )

  defaults <- font_settings_defaults(12)
  expect_equal(defaults$base_size, 12)
  expect_equal(defaults$plot_title, 13)
  expect_equal(defaults$point_labels, 11)

  expect_false(font_settings_has_values(NULL))
  expect_false(font_settings_has_values(list(base_size = NULL)))
  expect_true(font_settings_has_values(list(base_size = NULL, plot_title = 13)))

  expect_null(validate_font_setting_scalar(NULL, "base_size"))
  expect_equal(validate_font_setting_scalar(12, "base_size"), 12)
  expect_error(
    validate_font_setting_scalar(c(1, 2), "base_size"),
    "font_settings\\$base_size must be a single positive numeric value."
  )
  expect_error(
    validate_font_setting_scalar(0, "base_size"),
    "font_settings\\$base_size must be a single positive numeric value."
  )

  expect_null(normalize_font_settings(activate = FALSE))
  expect_error(
    normalize_font_settings(font_settings = 1),
    "font_settings must be a named list."
  )
  expect_error(
    normalize_font_settings(font_settings = list(unknown = 1)),
    "Unknown font_settings field"
  )
  expect_error(
    normalize_font_settings(legacy = 1, activate = TRUE),
    "legacy font settings must be supplied as a named list."
  )

  normalized <- normalize_font_settings(
    font_settings = list(base_size = 13, plot_title = 20, legend_text = 8),
    supported_fields = c("plot_title", "legend_text", "x_title"),
    legacy = list(base_size = 12, x_title = 15, plot_title = 18, ignored = 99)
  )
  expect_equal(
    names(normalized),
    c("base_size", "plot_title", "legend_text", "x_title")
  )
  expect_equal(normalized$base_size, 13)
  expect_equal(normalized$plot_title, 20)
  expect_equal(normalized$legend_text, 8)
  expect_equal(normalized$x_title, 15)
})

test_that("font setting plotting helpers compute expected sizes", {
  base_plot <- ggplot2::ggplot(ex1_full, ggplot2::aes(Group, IL.10)) +
    ggplot2::geom_boxplot() +
    ggplot2::labs(title = "Example", x = "Group", y = "IL.10")

  themed_plot <- apply_font_settings_ggplot(base_plot, helper_font_settings)
  expect_equal(themed_plot$theme$text$size, helper_font_settings$base_size)
  expect_equal(
    themed_plot$theme$plot.title$size,
    helper_font_settings$plot_title
  )
  expect_equal(
    themed_plot$theme$axis.title.x$size,
    helper_font_settings$x_title
  )
  expect_equal(themed_plot$theme$axis.text.y$size, helper_font_settings$y_text)
  expect_identical(apply_font_settings_ggplot(base_plot, NULL), base_plot)

  expect_equal(font_settings_ggplot_text_size(NULL), 4)
  expect_equal(
    font_settings_ggplot_text_size(20, reference_points = 10, default_size = 4),
    8
  )

  expect_equal(font_settings_mixomics_text_scale(NULL), 1)
  expect_equal(
    font_settings_mixomics_text_scale(
      22,
      reference_points = 11,
      default_scale = 1
    ),
    2
  )

  expect_equal(font_settings_mixomics_indiv_cex(NULL), 3)
  expect_equal(
    font_settings_mixomics_indiv_cex(
      22,
      reference_points = 11,
      default_cex = 3
    ),
    6
  )

  expect_equal(font_settings_mixomics_scale(NULL), 1)
  expect_equal(font_settings_mixomics_scale(list(base_size = 22)), 2)

  indiv_args <- font_settings_mixomics_indiv_args(helper_font_settings)
  expect_equal(indiv_args$size.title, helper_font_settings$plot_title)
  expect_equal(indiv_args$size.legend.title, helper_font_settings$legend_title)
  expect_equal(
    indiv_args$cex,
    font_settings_mixomics_indiv_cex(helper_font_settings$point_labels)
  )
  expect_equal(font_settings_mixomics_indiv_args(NULL), list())

  loading_args <- font_settings_mixomics_loadings_args(helper_font_settings)
  expect_equal(loading_args$size.name, helper_font_settings$variable_names / 10)
  expect_true(loading_args$size.axis > 0)
  expect_equal(font_settings_mixomics_loadings_args(NULL), list())

  expect_equal(font_settings_plotvar_cex(NULL), 4)
  expect_equal(
    font_settings_plotvar_cex(20, reference_points = 10, default_cex = 4),
    8
  )
  expect_equal(font_settings_plotvar_args(NULL), list())
  expect_equal(
    font_settings_plotvar_args(helper_font_settings, show_var_names = FALSE),
    list()
  )
  expect_equal(
    font_settings_plotvar_args(helper_font_settings, show_var_names = TRUE)$cex,
    font_settings_plotvar_cex(helper_font_settings$variable_names)
  )

  base_graphics <- font_settings_base_graphics(helper_font_settings)
  expect_equal(base_graphics$cex, helper_font_settings$base_size / 11)
  expect_equal(
    base_graphics$variable_cex,
    helper_font_settings$variable_names / 10
  )
  expect_equal(font_settings_base_graphics(NULL), list())

  heatmap_args <- font_settings_heatmap_args(helper_font_settings)
  expect_equal(heatmap_args$fontsize, helper_font_settings$base_size)
  expect_equal(heatmap_args$fontsize_row, helper_font_settings$row_names)
  expect_equal(heatmap_args$fontsize_col, helper_font_settings$col_names)
  expect_equal(heatmap_args$fontsize_number, helper_font_settings$cell_text)

  fallback_heatmap_args <- font_settings_heatmap_args(list(base_size = 12))
  expect_equal(fallback_heatmap_args$fontsize_row, 11)
  expect_equal(fallback_heatmap_args$fontsize_col, 11)
  expect_equal(fallback_heatmap_args$fontsize_number, 11)
  expect_equal(font_settings_heatmap_args(NULL), list())
})

test_that("numeric validation helpers summarize invalid values", {
  issue_df <- summarize_invalid_numeric_columns(helper_invalid_numeric_df)
  expect_equal(issue_df$column, c("has_na", "has_nan", "has_inf", "has_ninf"))
  expect_equal(issue_df$issues, c("NA", "NaN", "Inf", "-Inf"))

  filtered_issue_df <- summarize_invalid_numeric_columns(
    helper_invalid_numeric_df,
    columns = c("has_inf", "missing", "good")
  )
  expect_equal(filtered_issue_df$column, "has_inf")
  expect_equal(filtered_issue_df$issues, "Inf")

  empty_issue_df <- summarize_invalid_numeric_columns(data.frame(
    group = letters[1:3]
  ))
  expect_s3_class(empty_issue_df, "data.frame")
  expect_equal(nrow(empty_issue_df), 0)

  expect_equal(format_invalid_numeric_summary(NULL), "")
  expect_equal(format_invalid_numeric_summary(empty_issue_df), "")
  expect_equal(
    format_invalid_numeric_summary(issue_df[1:2, , drop = FALSE]),
    "has_na [NA]; has_nan [NaN]"
  )
})

test_that("Step 2 helpers normalize missing tokens and restore selections safely", {
  expect_equal(
    step2_normalize_missing_tokens(c("1", " NA ", "", "n/a", "NULL", "NaN")),
    c("1", NA, NA, NA, NA, NA)
  )

  expect_true(step2_is_numeric_like(c("1", "2.5", "NA", "", "NULL", "NaN")))
  expect_false(step2_is_numeric_like(c("1", "two", "3", "groupA")))

  expect_equal(
    step2_parse_numeric_values(c("1", "2.5", "NA", "bad", "1,200", "*3")),
    c(1, 2.5, NA_real_, NA_real_, 1200, 3)
  )

  expect_equal(
    step2_conflicting_type_cols(c("Group", "Batch"), c("IL6", "Batch")),
    "Batch"
  )

  expect_equal(
    step2_restore_bucket_selection(NULL, c("Group", "Batch")),
    c("Group", "Batch")
  )
  expect_equal(
    step2_restore_bucket_selection(character(0), c("Group", "Batch")),
    character(0)
  )
  expect_equal(
    step2_restore_bucket_selection(c("Group", "IL6"), c("Group", "Batch")),
    "Group"
  )
})

test_that("Step 2 classification and override helpers cover the remaining branches", {
  numeric_values <- c(1, 2, NA_real_)
  expect_equal(step2_normalize_missing_tokens(numeric_values), numeric_values)
  expect_equal(step2_parse_numeric_values(numeric_values), numeric_values)
  expect_true(step2_is_numeric_like(numeric_values))
  expect_false(step2_is_numeric_like(c(" NA ", "", "null")))
  expect_true(step2_is_numeric_like(c("1", "OOR <5", "7", "NA")))

  classified <- step2_classify_columns(data.frame(
    ..cyto_id.. = 1:3,
    Group = c("A", "B", "C"),
    Batch = factor(c("X", "X", "Y")),
    IL6 = c(1, 2, 3),
    IL10 = c(4, 5, 6),
    stringsAsFactors = FALSE
  ))
  expect_equal(classified$all, c("Group", "Batch", "IL6", "IL10"))
  expect_equal(classified$categorical, c("Group", "Batch"))
  expect_equal(classified$numerical, c("IL6", "IL10"))

  expect_null(step2_apply_type_overrides(NULL))

  empty_df <- data.frame()
  expect_identical(step2_apply_type_overrides(empty_df), empty_df)

  original_df <- data.frame(
    Group = c("B", "A", "NA"),
    Dose = c("1,200", "*3", "OOR <5"),
    Batch = c("late", "early", ""),
    stringsAsFactors = FALSE
  )

  expect_identical(
    step2_apply_type_overrides(
      original_df,
      factor_cols = "Group",
      numeric_cols = "Group"
    ),
    original_df
  )

  overridden <- step2_apply_type_overrides(
    original_df,
    factor_cols = c("Group", "Batch"),
    numeric_cols = "Dose",
    factor_order_enable = TRUE,
    factor_order_col = c("Group", "Batch"),
    factor_levels_csv = "A, B"
  )
  expect_equal(overridden$Dose, c(1200, 3, 5))
  expect_true(is.factor(overridden$Group))
  expect_true(is.factor(overridden$Batch))
  expect_equal(levels(overridden$Group), c("A", "B"))
  expect_equal(as.character(overridden$Group[1:2]), c("B", "A"))
  expect_true(is.na(overridden$Group[3]))
  expect_equal(as.character(overridden$Batch[1:2]), c("late", "early"))
  expect_true(is.na(overridden$Batch[3]))
})

test_that("safe_zscore_column handles all-missing, non-finite, zero-variance, and standard inputs", {
  expect_error(
    safe_zscore_column(c(NA_real_, NA_real_)),
    "columns that contain only missing values"
  )
  expect_error(
    safe_zscore_column(c(1, Inf, 3)),
    "requires all non-missing selected values to be finite"
  )
  expect_equal(safe_zscore_column(c(2, 2, 2)), c(0, 0, 0))
  expect_equal(
    safe_zscore_column(c(1, 2, 3)),
    as.numeric(scale(c(1, 2, 3))),
    tolerance = 1e-10
  )
})

test_that("apply_scale supports automatic selection and strict validation branches", {
  input_df <- data.frame(
    group = c("A", "B", "C"),
    x = c(1, 2, 4),
    y = c(2, 3, 6),
    stringsAsFactors = FALSE
  )

  expect_equal(apply_scale(input_df, scale = "none"), input_df)
  expect_equal(
    apply_scale(input_df, columns = character(), scale = "log2"),
    input_df
  )
  expect_equal(
    apply_scale(input_df, scale = "log10")$x,
    log10(input_df$x)
  )
  expect_equal(
    apply_scale(as.matrix(input_df[c("x", "y")]), scale = "zscore")$x,
    as.numeric(scale(input_df$x)),
    tolerance = 1e-10
  )
  expect_error(
    apply_scale(input_df, columns = "missing", scale = "none"),
    "requested for transformation but are missing"
  )
  expect_error(
    apply_scale(data.frame(x = c(1, 0, 3)), scale = "log2"),
    "requires all non-missing selected values to be finite and greater than 0"
  )
  expect_error(
    apply_scale(data.frame(x = c(1, Inf, 3)), scale = "zscore"),
    "non-finite non-missing values: x"
  )
  expect_error(
    apply_scale(data.frame(x = c(NA_real_, NA_real_)), scale = "zscore"),
    "only missing values: x"
  )
  expect_error(
    apply_scale(input_df, scale = "custom"),
    "When scale = 'custom', a valid function must be provided"
  )
})

test_that("adjust_p remains a thin wrapper over stats::p.adjust", {
  p_values <- c(0.01, 0.02, 0.1)
  expect_equal(
    adjust_p(p_values, method = "holm"),
    stats::p.adjust(p_values, method = "holm")
  )
})

test_that("read_uploaded_flat_file reads CSV uploads directly and preserves headers", {
  path <- tempfile(pattern = "csv upload ", fileext = ".csv")
  writeLines(
    c(
      "\"Group Name\",\"IL-6 (pg/mL)\"",
      "\"A\",1.5",
      "\"B\",2.5"
    ),
    path
  )

  result <- read_uploaded_flat_file(path, "csv")

  expect_identical(class(result), "data.frame")
  expect_false(inherits(result, "data.table"))
  expect_identical(names(result), c("Group Name", "IL-6 (pg/mL)"))
  expect_equal(result[["Group Name"]], c("A", "B"))
  expect_equal(result[["IL-6 (pg/mL)"]], c(1.5, 2.5))
})

test_that("read_uploaded_flat_file reads tab-delimited uploads from spaced paths", {
  dir_path <- tempfile(pattern = "tab upload dir ")
  dir.create(dir_path)
  path <- file.path(dir_path, "upload file.txt")
  writeLines(
    c(
      "Sample ID\tTNF-alpha",
      "S1\t10",
      "S2\t20"
    ),
    path
  )

  result <- read_uploaded_flat_file(path, "txt")

  expect_identical(class(result), "data.frame")
  expect_identical(names(result), c("Sample ID", "TNF-alpha"))
  expect_equal(result[["Sample ID"]], c("S1", "S2"))
  expect_equal(result[["TNF-alpha"]], c(10, 20))
})

test_that("read_uploaded_flat_file reports unsupported extensions and read failures clearly", {
  expect_error(
    read_uploaded_flat_file(tempfile(fileext = ".xlsx"), "xlsx"),
    "Unsupported uploaded flat-file extension: xlsx"
  )

  missing_path <- tempfile(pattern = "missing csv upload ")
  dir.create(missing_path)
  expect_error(
    read_uploaded_flat_file(missing_path, "csv"),
    "Failed to read uploaded CSV file:"
  )
})
