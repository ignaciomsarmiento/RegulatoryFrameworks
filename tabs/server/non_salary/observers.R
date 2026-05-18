# =============================================================================
# observers.R - observeEvent handlers (button observers, country/wage selection observers, tenure observer).
# Sourced into non_salary_server_core's environment.
# =============================================================================

  observeEvent(input$show_by_tenure, {
    if (!isTRUE(input$show_by_tenure)) {
      mode <- input$compare_mode
      if (is.null(mode) || length(mode) == 0) {
        mode <- "country"
      }
      if (identical(mode, "wage")) {
        updateSelectizeInput(session, ns("mw_selection"), selected = wage_levels)
        selected_wage_level(wage_levels)
        last_wage_selection(wage_levels)
      }
      return()
    }
    selection <- selected_wage_level()
    if (is.null(selection) || length(selection) == 0) {
      selection <- "1sm"
    }
    selection <- selection[selection %in% wage_levels]
    if (length(selection) == 0) {
      selection <- "1sm"
    }
    if (length(selection) > 1) {
      selection <- selection[1]
    }
    updateSelectizeInput(session, ns("mw_selection"), selected = selection)
  })
  
  
  # ---- First Selection ----
  observeEvent(input$btn_total,  { selected_breakdown_type("total") })
  observeEvent(input$btn_payer,  { selected_breakdown_type("payer") })
  observeEvent(input$btn_component,  { 
    selected_breakdown_type("component") 
  })
  observeEvent(input$all,  {
    selected_cost_category("all")
    selected_component_filter("all_component")
    option1_selected(TRUE)
  })
  observeEvent(input$country_selection_user, {
    selection <- input$country_selection_user
    if (is.null(selection) || length(selection) == 0) {
      selection <- "All"
    }

    previous <- last_country_selection()
    if ("All" %in% selection && length(selection) > 1) {
      if (!("All" %in% previous)) {
        selection <- "All"
      } else {
        selection <- setdiff(selection, "All")
      }
    }

    mode <- input$compare_mode
    if (is.null(mode) || length(mode) == 0) {
      mode <- "country"
    }
    if (identical(mode, "wage")) {
      if (is.null(selection) || length(selection) == 0 || "All" %in% selection) {
        preferred <- last_single_country()
        if (is.null(preferred) || preferred == "") {
          preferred <- ns_variables$countries[ns_variables$countries != "All"][1]
        }
        selection <- preferred
      } else if (length(selection) > 1) {
        selection <- selection[1]
      }
    }

    if (!identical(selection, input$country_selection_user)) {
      updateSelectizeInput(session, ns("country_selection_user"), selected = selection)
    }

    ns_variables$country_sel <- selection
    last_country_selection(selection)
    if (length(selection) == 1 && selection != "All") {
      last_single_country(selection)
    }
  })

  observeEvent(input$country_button, {
    code <- input$country_button
    if (is.null(code) || length(code) == 0 || code == "") {
      return()
    }
    ns_variables$country_sel <- code
    last_country_selection(code)
    last_single_country(code)
    updateSelectizeInput(session, ns("country_selection_user"), selected = code)
  })

  observeEvent(input$table_country_button, {
    code <- input$table_country_button
    if (is.null(code) || length(code) == 0 || code == "") {
      return()
    }
    selected_table_country(code)
  })

  # ---- Continued observers (originally lines 1240-1369) ----
  observeEvent(selected_cost_category(), {
    valid_choices <- option2_choices_for_group(selected_cost_category())
    if (!selected_breakdown_type() %in% valid_choices) {
      selected_breakdown_type(valid_choices[1])
    }
  })

  observeEvent(input$compare_mode, {
    mode <- input$compare_mode
    if (is.null(mode) || length(mode) == 0) {
      return()
    }
    previous_mode <- last_compare_mode()
    if (identical(previous_mode, "wage") && identical(mode, "country")) {
      reset_across_defaults()
      return()
    }
    if (identical(mode, "wage")) {
      preferred <- "ARG"
      if (!preferred %in% ns_variables$countries) {
        preferred <- last_single_country()
      }
      if (is.null(preferred) || preferred == "" || !preferred %in% ns_variables$countries) {
        preferred <- ns_variables$countries[ns_variables$countries != "All"][1]
      }
      updateSelectizeInput(session, ns("country_selection_user"), selected = preferred)
      updateSelectizeInput(session, ns("mw_selection"), selected = wage_levels)
      selected_wage_level(wage_levels)
      last_wage_selection(wage_levels)
      if (!is.null(preferred) && preferred != "") {
        last_single_country(preferred)
      }
    } else {
      wages <- selected_wage_level()
      if (length(wages) > 1) {
        wages <- wages[1]
        updateSelectizeInput(session, ns("mw_selection"), selected = wages)
        selected_wage_level(wages)
        last_wage_selection(wages)
      }
      if (!is.null(ns_variables$order_country) &&
          length(ns_variables$order_country) > 0 &&
          any(grepl("\\bMW\\b", ns_variables$order_country))) {
        ns_variables$order_country <- NULL
      }
    }
    last_compare_mode(mode)
  })
  
  
  # ---- MW Selection ----
  observeEvent(input$mw_selection, {
    selection <- input$mw_selection
    if (is.null(selection) || length(selection) == 0) {
      selection <- last_wage_selection()
    }
    selection <- unique(selection)
    selection <- selection[selection %in% wage_levels]
    if (length(selection) == 0) {
      selection <- last_wage_selection()
    }

    mode <- input$compare_mode
    if (is.null(mode)) {
      mode <- "country"
    }

    if (identical(mode, "country") && length(selection) > 1) {
      selection <- selection[1]
      showNotification("Across-country comparisons use one wage level.", type = "message")
    }

    if (identical(mode, "wage")) {
      countries <- ns_variables$country_sel
      if (is.null(countries) || length(countries) != 1 || "All" %in% countries) {
        selection <- last_wage_selection()
      }
    }

    if (!identical(selection, input$mw_selection)) {
      updateSelectizeInput(session, ns("mw_selection"), selected = selection)
    }

    selected_wage_level(selection)
    last_wage_selection(selection)
  })
  
  # ---- Components ----
  observeEvent(input$all_component,  { selected_component_filter("all_component") })
  observeEvent(input$bonus,  { 
    selected_component_filter("bonuses_and_benefits")
    selected_cost_category("bonuses_and_benefits")
    option1_selected(TRUE)
  })
  observeEvent(input$social,  { 
    selected_cost_category("social")
    selected_component_filter("social")
    selected_social_subcomponent("pensions")
    option1_selected(TRUE)
  })
  observeEvent(input$occupational_risk_main, {
    selected_cost_category("social")
    selected_component_filter("social")
    selected_social_subcomponent("occupational_risk")
    selected_breakdown_type("total")
    option1_selected(TRUE)
  })
  observeEvent(input$payroll, {
    selected_cost_category("payroll_taxes")
    selected_component_filter("all_component")
    option1_selected(TRUE)
  })
  observeEvent(input$pensions,  { selected_social_subcomponent("pensions") })
  observeEvent(input$health, { selected_social_subcomponent("health") })
  observeEvent(input$occupational_risk, { selected_social_subcomponent("occupational_risk") })
  observeEvent(selected_social_subcomponent(), {
    social_subcomponent <- safe_value(selected_social_subcomponent(), "pensions")
    if (selected_cost_category() == "social" &&
        social_subcomponent == "occupational_risk" &&
        selected_breakdown_type() == "payer") {
      selected_breakdown_type("total")
    }
  })
  
  # ---- Bonuses and Benefits ----
  observeEvent(input$all_bonuses,  { selected_bonus_component("all_bonuses") })
  observeEvent(input$ab,  { selected_bonus_component("ab") })
  observeEvent(input$pl,  { selected_bonus_component("pl") })
  observeEvent(input$ob,  { selected_bonus_component("ob") })
  observeEvent(input$up,  { selected_bonus_component("up") })
