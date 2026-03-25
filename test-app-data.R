library(shinytest2)

test_that("{shinytest2} recording: app-data", {
    skip_on_cran()
    skip_if_appdriver_disabled()
    app_dir <- app_test_dir()
    app <- AppDriver$new(
        app_dir,
        variant = platform_variant(),
        name = "app-data",
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

    app$set_inputs(view_data = TRUE)
    app$wait_for_idle(4000)
    app_expect_stable_screenshot(app, name = "preview_data")

    app$set_inputs(show_summary = TRUE)
    app$wait_for_idle(4000)
    app$set_inputs(step1_data_tabs = "Summary Statistics")
    app_expect_stable_screenshot(app, name = "summary_statistics")

    app$click("next1", timeout_ = 30000)
    app$wait_for_idle(7000)
    app_wait_for_input_binding(app, "selected_categorical_cols")
    app_wait_for_input_binding(app, "selected_numerical_cols")

    app$set_inputs(selected_categorical_cols = "Group")
    app$set_inputs(
        selected_numerical_cols = c(
            "Time",
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
            "IL-27",
            "TNF-A",
            "TNF-B"
        )
    )
    app$set_inputs(factor_cols = "Time")
    app$click("apply_types", timeout_ = 30000)
    app$wait_for_idle(3000)

    app$set_inputs(step2_scale = "log2")
    app$set_inputs(
        filter_accordion = "Filter by Categorical Values",
        wait_ = FALSE
    )
    app_wait_for_input_binding(app, "filter_Time")
    app_wait_for_input_binding(app, "filter_Group")
    app$set_inputs(filter_Time = c("20", "72"), wait_ = FALSE)
    app$set_inputs(filter_Group = c("PreT2D", "T2D"))
    app_expect_stable_screenshot(app, name = "filtered_data")

    app$click("preview_transform", timeout_ = 30000)
    app_wait_for_dom(app, ".modal.show", timeout = 30000)
    app_expect_stable_screenshot(app, name = "preview_transform")
    app$click(selector = ".modal.show .modal-footer button")
    app$stop()
})