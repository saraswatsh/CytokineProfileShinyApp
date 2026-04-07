library(shinytest2)

test_that("{shinytest2} recording: sPLSDA-Group-Treatment", {
    skip_on_cran()
    skip_if_appdriver_disabled()
    app_dir <- testthat::test_path("../../inst/app")
    app <- AppDriver$new(
        app_dir,
        variant = platform_variant(),
        name = "splsda-group-trt",
        load_timeout = 30000,
        timeout = 30000
    )
    app$click("nav_start_home")
    app$wait_for_idle(4000)

    app$upload_file(datafile = test_path("ExampleData1.csv"))
    app$wait_for_idle(4000)

    app$click("open_editor")
    app$wait_for_idle(4000)

    app$click("bioplex_confirm_modal")
    app$wait_for_idle(4000)

    app$click("next1", timeout_ = 30000)
    app$wait_for_idle(7000)

    app$set_inputs(
        selected_numerical_cols = c(
            "IL-17F",
            "GM-CSF",
            "IFN-G",
            "IL-10",
            "CCL-20/MIP-3A",
            "IL-12/P70",
            "IL-13",
            "IL-15",
            "IL-17A",
            "IL-22",
            "IL-9",
            "IL-1B",
            "IL-33",
            "IL-2",
            "IL-21",
            "IL-4",
            "IL-23",
            "IL-5",
            "IL-6",
            "IL-17E/IL-25",
            "IL-27",
            "IL-31",
            "TNF-A",
            "TNF-B",
            "IL-28A"
        )
    )
    app$set_inputs(step2_scale = "log10")
    app$wait_for_idle(4000)

    app$click("next2", timeout_ = 30000)
    app$wait_for_idle(4000)
    app$click("menu_splsda", timeout_ = 30000)
    app$wait_for_idle(4000)
    app$set_inputs(splsda_group_col2 = "Treatment")
    app$set_inputs(splsda_cv_opt = "LOOCV")
    app$set_inputs(splsda_pch = c("4", "16", "5"))
    app$set_inputs(splsda_ellipse = TRUE)
    app$set_inputs(splsda_conf_mat = TRUE)
    app$set_inputs(splsda_style = "3D")
    app$set_inputs(splsda_roc = TRUE)
    app$wait_for_idle(4000)
    app$click("next4", timeout_ = 60000)
    app$wait_for_idle(15000)
    app$expect_screenshot(name = "results_group_treatment")
    app$stop()
})
