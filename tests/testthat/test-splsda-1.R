library(shinytest2)

test_that("{shinytest2} recording: sPLSDA-group", {
    skip_on_cran()
    skip_if_appdriver_disabled()
    app_dir <- app_test_dir()
    app <- AppDriver$new(
        app_dir,
        variant = platform_variant(),
        name = "splsda-group",
        load_timeout = 30000,
        timeout = 30000
    )
    app$click("nav_start_home")
    app$wait_for_idle(4000)

    app$upload_file(datafile = test_path("ExampleData1.csv"))
    app$wait_for_idle()
    app_wait_for_dom(app, "#open_editor")

    app$click("open_editor")
    app$wait_for_idle(4000)

    app$click("bioplex_confirm_modal")
    app$wait_for_idle(4000)

    app$click("next1", timeout_ = 30000)
    app$wait_for_idle(7000)
    app_wait_for_input_binding(app, "selected_numerical_cols")
    app_wait_for_input_binding(app, "selected_categorical_cols")

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
    app$set_inputs(selected_categorical_cols = "Group")
    app$set_inputs(step2_scale = "log2")
    app$wait_for_idle(4000)
    app$click("next2", timeout_ = 30000)
    app_wait_for_dom(app, "#menu_splsda")
    app$click("menu_splsda", timeout_ = 30000)
    app_wait_for_input_binding(app, "splsda_cv_opt")
    app$set_inputs(splsda_cv_opt = "LOOCV")
    app$set_inputs(splsda_pch = c("4", "16", "1"))
    app$set_inputs(splsda_style = "3D")
    app$set_inputs(splsda_ellipse = TRUE)
    app$set_inputs(splsda_conf_mat = TRUE)
    app$set_inputs(splsda_roc = TRUE)
    app$click("next4", timeout_ = 60000)
    app_wait_for_result_ui(app, "#splsda_tabs", timeout = 60000)
    app_wait_for_dom(app, "#splsda_overallIndivPlot", timeout = 60000)

    app$set_inputs(splsda_tabs = "sPLS-DA Plot", wait_ = FALSE)
    app_wait_for_dom(app, "#splsda_overallIndivPlot")
    app_expect_stable_screenshot(app, name = "splsda_plot")

    app$set_inputs(splsda_tabs = "Loadings", wait_ = FALSE)
    app_wait_for_dom(app, "#splsda_loadingsUI")
    app_expect_stable_screenshot(app, name = "loadings")

    app$set_inputs(splsda_tabs = "VIP Scores", wait_ = FALSE)
    app_wait_for_dom(app, "#splsda_vipScoresUI")
    app_expect_stable_screenshot(app, name = "vip_scores")

    app$set_inputs(splsda_tabs = "VIP Model Plot", wait_ = FALSE)
    app_wait_for_dom(app, "#splsda_vipIndivPlot")
    app_expect_stable_screenshot(app, name = "vip_model_plot")

    app$set_inputs(splsda_tabs = "VIP Loadings", wait_ = FALSE)
    app_wait_for_dom(app, "#splsda_vipLoadingsUI")
    app_expect_stable_screenshot(app, name = "vip_loadings")

    app$set_inputs(splsda_tabs = "ROC", wait_ = FALSE)
    app_wait_for_dom(app, "#splsda_overallRocPlot")
    app_expect_stable_screenshot(app, name = "roc")

    app$set_inputs(splsda_tabs = "Cross-Validation", wait_ = FALSE)
    app_wait_for_dom(app, "#splsda_overallCvPlot")
    app_expect_stable_screenshot(app, name = "cross_validation")

    app$set_inputs(splsda_tabs = "Confusion Matrix", wait_ = FALSE)
    app_wait_for_dom(app, "#splsda_confMatrix")
    app_expect_stable_screenshot(app, name = "confusion_matrix")
    app$stop()
})