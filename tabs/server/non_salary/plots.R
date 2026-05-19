# =============================================================================
# plots.R - Selector renderUIs (country_selection, mw_selection_ui) and the main output$plot renderPlotly block.
# Sourced into non_salary_server_core's environment.
# =============================================================================

  output$country_selection <- renderUI({
    mode <- input$compare_mode
    if (is.null(mode) || length(mode) == 0) {
      mode <- "country"
    }
    choices <- ns_variables$countries
    selected_country <- "All"
    options <- NULL
    if (identical(mode, "wage")) {
      choices <- ns_variables$countries[ns_variables$countries != "All"]
      preferred <- "ARG"
      if (!preferred %in% choices) {
        preferred <- last_single_country()
      }
      if (is.null(preferred) || preferred == "" || !preferred %in% choices) {
        preferred <- choices[1]
      }
      selected_country <- preferred
      options <- list(maxItems = 1)
    }
    div(
      class = "pretty-select",
      selectizeInput(
        inputId = ns("country_selection_user"),
        label = NULL,
        choices = choices,
        selected = selected_country,
        multiple = TRUE,
        options = options
      )
    )
  })

  output$mw_selection_ui <- renderUI({
    mode <- input$compare_mode
    if (is.null(mode) || length(mode) == 0) {
      mode <- "country"
    }
    tenure_view <- isTRUE(input$show_by_tenure)
    allow_multiple <- identical(mode, "wage") && !tenure_view
    selection <- selected_wage_level()
    if (is.null(selection) || length(selection) == 0) {
      selection <- last_wage_selection()
    }
    selection <- selection[selection %in% wage_levels]
    if (length(selection) == 0) {
      selection <- "1sm"
    }
    if (!allow_multiple && length(selection) > 1) {
      selection <- selection[1]
    }

    selectize_options <- NULL
    if (allow_multiple) {
      selectize_options <- list(plugins = list("remove_button"))
    }
    div(
      class = "pretty-select mw-select",
      selectizeInput(
        inputId = ns("mw_selection"),
        label = NULL,
        choices = wage_choices,
        selected = selection,
        multiple = allow_multiple,
        options = selectize_options
      )
    )
  })

  # ---- output$plot (originally lines 1371-4604) ----
  # ---- Graph ----
  output$plot <- renderPlotly({
    
    # Requirements
    req(selected_breakdown_type())
    req(selected_wage_level())
    
    # Results from user click
    cost_category <- safe_value(selected_cost_category(), "all")
    breakdown_type <- safe_value(selected_breakdown_type(), "total")
    wage_level <- safe_value(selected_wage_level(), "1sm")
    component_filter <- safe_value(selected_component_filter(), "all_component")
    bonus_component <- safe_value(selected_bonus_component(), "all_bonuses")
    social_subcomponent <- safe_value(selected_social_subcomponent(), "pensions")
    is_cross_country <- identical(input$compare_mode, "country")
    ns_variables$missing_payroll_countries <- NULL
    ns_variables$missing_occ_risk_countries <- NULL

    sources <- resolve_sources()
    df_non_salary <- sources$non_salary
    df_non_salary_payer <- sources$non_salary_payer
    df_non_salary_component <- sources$non_salary_component
    bonus_hover_lookup <- build_bonus_hover_lookup(sources$bonus_hover_source)

    if (tenure_enabled()) {
      df_non_salary <- apply_tenure_filter(df_non_salary)
      df_non_salary_payer <- apply_tenure_filter(df_non_salary_payer)
      df_non_salary_component <- apply_tenure_filter(df_non_salary_component)
    }
    
    country_sel <- ns_variables$country_sel
    if (is.null(country_sel) || length(country_sel) == 0) {
      country_sel <- "All"
      ns_variables$country_sel <- country_sel
      last_country_selection(country_sel)
    }
    
    wage_codes <- wage_level
    if (is.null(wage_codes) || length(wage_codes) == 0) {
      wage_codes <- "1sm"
    }
    wage_codes <- as.character(wage_codes)
    wage_codes <- wage_codes[!is.na(wage_codes)]
    if (length(wage_codes) == 0) {
      wage_codes <- "1sm"
    }
    wage_codes <- wage_levels[wage_levels %in% wage_codes]
    if (length(wage_codes) == 0) {
      wage_codes <- "1sm"
    }
    compare_wages <- identical(input$compare_mode, "wage") && length(wage_codes) > 1
    if (tenure_enabled() && length(wage_codes) > 1) {
      wage_codes <- wage_codes[1]
      compare_wages <- FALSE
    }
    if (!compare_wages && length(wage_codes) > 1) {
      wage_codes <- wage_codes[1]
    }
    if (!compare_wages &&
        !is.null(ns_variables$order_country) &&
        length(ns_variables$order_country) > 0 &&
        any(grepl("\\bMW\\b", ns_variables$order_country))) {
      ns_variables$order_country <- NULL
    }
    # Transform values from "1sm" → "1 MW"
    wage_filter <- format_wage_label(wage_codes)
    set_order_country <- function(new_order) {
      new_order <- new_order[!is.na(new_order) & new_order != ""]
      if (length(new_order) == 0) return()
      if (!identical(ns_variables$order_country, new_order)) {
        ns_variables$order_country <- new_order
      }
    }
    panel_order <- function() {
      if (compare_wages) {
        return(wage_filter)
      }
      current <- ns_variables$order_country
      if (is.null(current) || length(current) == 0) {
        return(NULL)
      }
      if (any(grepl("\\bMW\\b", current))) {
        return(NULL)
      }
      current
    }
    if (compare_wages) {
      set_order_country(wage_filter)
      if (length(ns_variables$country_sel) != 1 || "All" %in% ns_variables$country_sel) {
        return(NULL)
      }
    }
    y_axis_title <- y_axis_title_text()

    apply_wage_panels <- function(df) {
      if (!compare_wages || !"wage" %in% names(df)) {
        return(df)
      }
      df$country <- df$wage
      df
    }

    filter_missing_payroll <- function(df, candidates = NULL) {
      if (!is_cross_country || cost_category != "payroll_taxes") {
        return(df)
      }
      missing <- find_missing_countries(df, candidates)
      ns_variables$missing_payroll_countries <- missing
      if (length(missing) > 0) {
        df <- df |> filter(!country %in% missing)
      }
      df
    }

    filter_missing_occ_risk <- function(df, candidates = NULL) {
      if (!is_cross_country || cost_category != "social" || social_subcomponent != "occupational_risk") {
        return(df)
      }
      missing <- find_missing_countries(df, candidates)
      ns_variables$missing_occ_risk_countries <- missing
      if (length(missing) > 0) {
        df <- df |> filter(!country %in% missing)
      }
      df
    }

    filter_bonus_component <- function(df, component_code) {
      if (is.null(df) || is.null(component_code) || component_code == "") {
        return(df)
      }
      if ("component" %in% names(df)) {
        return(df |> filter(component == component_code))
      }
      if (!"min_max_component" %in% names(df)) {
        return(df)
      }
      component_key <- switch(
        component_code,
        ab = "annual_bonuses",
        pl = "paid_leave",
        up = "unemployment_protection",
        ob = "other_bonuses",
        component_code
      )
      df |> filter(grepl(component_key, min_max_component))
    }

    prepare_tenure_axis <- function(df) {
      df <- df |>
        mutate(tenure_value = suppressWarnings(as.numeric(as.character(tenure))))
      use_numeric_tenure <- all(!is.na(df$tenure_value))
      if (use_numeric_tenure) {
        df <- df |>
          mutate(tenure_plot = tenure_value) |>
          arrange(tenure_value)
      } else {
        df <- df |>
          mutate(tenure_plot = as.character(tenure)) |>
          arrange(tenure_plot)
      }
      df
    }

    build_tenure_bar <- function(df, scenario_label, color, y_axis_title, y_range, show_y_title) {
      sub <- df |> filter(Scenario == scenario_label)
      if (nrow(sub) == 0) {
        return(NULL)
      }
      plot_ly(
        data = sub,
        x = ~tenure_plot,
        y = ~value,
        type = "bar",
        marker = list(color = color),
        showlegend = FALSE,
        hovertemplate = ns_hovertemplate
      ) |>
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)",
          xaxis = list(
            title = "Tenure (in years)",
            showgrid = FALSE,
            zeroline = FALSE,
            showline = FALSE
          ),
          yaxis = list(
            title = ifelse(show_y_title, y_axis_title, ""),
            range = y_range,
            showgrid = FALSE,
            zeroline = FALSE,
            showline = FALSE
          )
        )
    }

    build_tenure_stack <- function(df, scenario_label, colors, stack_order, y_axis_title, y_range,
                                   show_y_title, show_legend) {
      sub <- df |> filter(Scenario == scenario_label)
      if (nrow(sub) == 0) {
        return(NULL)
      }
      sub <- sub |>
        mutate(Type = factor(Type, levels = stack_order))
      fig <- plot_ly(type = "bar")
      for (type in levels(sub$Type)) {
        sub_type <- sub |> filter(Type == type)
        if (nrow(sub_type) == 0) {
          next
        }
        color <- colors[[type]]
        if (is.null(color)) {
          color <- "#00C1FF"
        }
        fig <- fig |> add_trace(
          x = sub_type$tenure_plot,
          y = sub_type$value,
          name = type,
          marker = list(color = color),
          showlegend = show_legend,
          legendgroup = type,
          hovertemplate = ns_hovertemplate
        )
      }
      fig |>
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)",
          barmode = "stack",
          xaxis = list(
            title = "Tenure (in years)",
            showgrid = FALSE,
            zeroline = FALSE,
            showline = FALSE
          ),
          yaxis = list(
            title = ifelse(show_y_title, y_axis_title, ""),
            range = y_range,
            showgrid = FALSE,
            zeroline = FALSE,
            showline = FALSE
          )
        )
    }

    build_tenure_figure <- function(p_min, p_max, country_label, show_legend = FALSE) {
      fig <- subplot(
        p_min,
        p_max,
        nrows = 1,
        shareY = TRUE,
        titleX = TRUE,
        margin = 0.01
      ) |>
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)",
          margin = list(l = 70, r = 30, b = 90, t = 40)
        )
      if (show_legend) {
        fig <- fig |> layout(
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.25
          )
        )
      }
      fig <- apply_labor_plot_theme(fig)

      title_annotations <- list(
        list(
          text = paste0(country_label, " - Min"),
          x = 0.23,
          y = 1.02,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          font = list(family = plotly_font_family, size = 14)
        ),
        list(
          text = paste0(country_label, " - Max"),
          x = 0.77,
          y = 1.02,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          font = list(family = plotly_font_family, size = 14)
        )
      )
      existing_annotations <- fig$x$layout$annotations
      if (is.null(existing_annotations)) {
        existing_annotations <- list()
      }
      fig |> layout(annotations = c(existing_annotations, title_annotations))
    }

    build_exclusion_notice <- function(message) {
      annotations <- list(
        list(
          text = message,
          xref = "paper",
          yref = "paper",
          x = 0.5,
          y = 0.5,
          xanchor = "center",
          yanchor = "middle",
          align = "center",
          showarrow = FALSE,
          font = list(family = plotly_font_family, size = 18, color = "#0f3b66")
        )
      )
      plot_ly(
        type = "scatter",
        mode = "markers",
        x = numeric(0),
        y = numeric(0),
        showlegend = FALSE
      ) |>
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)",
          xaxis = list(visible = FALSE, showgrid = FALSE, zeroline = FALSE, showline = FALSE),
          yaxis = list(visible = FALSE, showgrid = FALSE, zeroline = FALSE, showline = FALSE),
          annotations = annotations,
          margin = list(l = 40, r = 20, t = 40, b = 40)
        )
    }

    if (tenure_enabled()) {
      if (length(ns_variables$country_sel) != 1 || "All" %in% ns_variables$country_sel) {
        showNotification("Please select one country.", type = "error")
        return(NULL)
      }

      country_sel <- ns_variables$country_sel
      country_label <- country_display_name(country_sel)
      excluded_countries <- c("BRA", "CHL", "COL", "PER")
      if (country_sel %in% excluded_countries) {
        message <- paste0(
          country_label,
          " is excluded since its statutory non-salary labor costs do not vary with years of employee tenure."
        )
        return(build_exclusion_notice(message))
      }
      if (cost_category == "payroll_taxes" && country_sel == "ECU") {
        message <- paste0(
          country_label,
          " is excluded since its statutory payroll taxes do not vary with years of employee tenure."
        )
        return(build_exclusion_notice(message))
      }

      if (breakdown_type == "total") {
        df_src <- NULL
        if (cost_category == "all") {
          df_src <- df_non_salary
        } else if (cost_category %in% c("bonuses_and_benefits", "payroll_taxes", "social")) {
          data_key <- if (cost_category == "social") social_subcomponent else cost_category
          df_src <- get_group_data(data_key)
        }
        if (is.null(df_src)) {
          showNotification("Data not available for this selection.", type = "error")
          return(NULL)
        }
        scenario_source <- if ("type" %in% names(df_src)) {
          "type"
        } else if ("min_max_total" %in% names(df_src)) {
          "min_max_total"
        } else {
          NULL
        }
        if (is.null(scenario_source) || !"tenure" %in% names(df_src)) {
          showNotification("Tenure data not available for this selection.", type = "error")
          return(NULL)
        }
        df_tenure <- df_src |>
          filter(
            country == country_sel,
            wage %in% wage_filter
          )
        if (nrow(df_tenure) == 0) {
          showNotification("No Data for this combination.", type = "error")
          return(NULL)
        }
        df_tenure <- df_tenure |>
          mutate(
            Scenario = ifelse(grepl("_min$", .data[[scenario_source]]), "Min", "Max")
          ) |>
          prepare_tenure_axis()

        ns_variables$df_final <- df_tenure
        y_max <- max(df_tenure$value, na.rm = TRUE)
        y_range <- if (is.finite(y_max)) c(0, y_max * 1.1) else NULL

        p_min <- build_tenure_bar(df_tenure, "Min", "#00C1FF", y_axis_title, y_range, TRUE)
        p_max <- build_tenure_bar(df_tenure, "Max", "#002244", y_axis_title, y_range, FALSE)

        if (is.null(p_min) || is.null(p_max)) {
          showNotification("No Data for this combination.", type = "error")
          return(NULL)
        }
        return(build_tenure_figure(p_min, p_max, country_label))
      }

      if (breakdown_type == "payer") {
        if (cost_category == "social" && identical(social_subcomponent, "occupational_risk")) {
          showNotification("Occupational risk contributions are only available in total.", type = "message")
          return(NULL)
        }
        df_src <- NULL
        if (cost_category == "all") {
          df_src <- df_non_salary_payer
        } else if (cost_category == "social") {
          df_src <- get_payer_data(social_subcomponent)
        } else if (cost_category == "payroll_taxes") {
          df_src <- get_payer_data("payroll_taxes")
        }
        if (is.null(df_src) || !"min_max_payer" %in% names(df_src)) {
          showNotification("Data not available for this selection.", type = "error")
          return(NULL)
        }
        if (!"tenure" %in% names(df_src)) {
          showNotification("Tenure data not available for this selection.", type = "error")
          return(NULL)
        }
        df_tenure <- df_src |>
          filter(
            country == country_sel,
            wage %in% wage_filter
          ) |>
          mutate(
            Scenario = ifelse(grepl("_min$", min_max_payer), "Min", "Max"),
            Type = ifelse(grepl("employer", min_max_payer), "Employer", "Employee")
          ) |>
          prepare_tenure_axis()

        if (nrow(df_tenure) == 0) {
          showNotification("No Data for this combination.", type = "error")
          return(NULL)
        }

        ns_variables$df_final <- df_tenure
        y_max <- df_tenure |>
          group_by(Scenario, tenure_plot) |>
          summarize(total = sum(value, na.rm = TRUE), .groups = "drop") |>
          summarize(max_total = max(total, na.rm = TRUE), .groups = "drop") |>
          pull(max_total)
        y_range <- if (is.finite(y_max)) c(0, y_max * 1.1) else NULL

        payer_colors <- c("Employer" = "#002244", "Employee" = "#00C1FF")
        p_min <- build_tenure_stack(
          df_tenure,
          "Min",
          payer_colors,
          c("Employee", "Employer"),
          y_axis_title,
          y_range,
          TRUE,
          TRUE
        )
        p_max <- build_tenure_stack(
          df_tenure,
          "Max",
          payer_colors,
          c("Employee", "Employer"),
          y_axis_title,
          y_range,
          FALSE,
          FALSE
        )
        if (is.null(p_min) || is.null(p_max)) {
          showNotification("No Data for this combination.", type = "error")
          return(NULL)
        }
        return(build_tenure_figure(p_min, p_max, country_label, show_legend = TRUE))
      }

      if (breakdown_type == "component") {
        df_src <- NULL
        stack_order <- NULL
        colors <- NULL
        component_mode <- "stack"
        if (cost_category == "all") {
          df_src <- df_non_salary_component
          stack_order <- component_stack_order
          colors <- component_palette
        } else if (cost_category == "bonuses_and_benefits") {
          df_src <- get_component_data("bonuses_and_benefits")
          stack_order <- bonus_stack_order
          colors <- bonus_palette
        } else if (cost_category == "payroll_taxes") {
          df_src <- get_component_data("payroll_taxes")
          if (is.null(df_src)) {
            showNotification("Data not available for payroll taxes by component.", type = "error")
            return(NULL)
          }
          if (!"tenure" %in% names(df_src)) {
            showNotification("Tenure data not available for payroll taxes by component.", type = "error")
            return(NULL)
          }
          df_tenure <- df_src |>
            filter(country == country_sel, wage %in% wage_filter) |>
            mutate(
              Scenario = ifelse(min_max_component == "min", "Min", "Max"),
              Type = component
            ) |>
            prepare_tenure_axis()
          if (nrow(df_tenure) == 0) {
            showNotification("No Data for this combination.", type = "error")
            return(NULL)
          }
          components <- unique(df_tenure$Type)
          base_colors <- c("#002244", "#00C1FF", "#726AA8", "#335B8E", "#B9BAB5",
                           "#8EA2BF", "#6F6779", "#4A90D9", "#A8C5DA")
          colors <- setNames(base_colors[seq_along(components)], components)
          ns_variables$df_final <- df_tenure
          y_max <- df_tenure |>
            group_by(Scenario, tenure_plot) |>
            summarize(total = sum(value, na.rm = TRUE), .groups = "drop") |>
            summarize(max_total = max(total, na.rm = TRUE)) |>
            pull(max_total)
          y_range <- if (is.finite(y_max)) c(0, y_max * 1.1) else NULL
          p_min <- build_tenure_stack(df_tenure, "Min", colors, components,
                                      y_axis_title, y_range, TRUE, TRUE)
          p_max <- build_tenure_stack(df_tenure, "Max", colors, components,
                                      y_axis_title, y_range, FALSE, FALSE)
          if (is.null(p_min) || is.null(p_max)) {
            showNotification("No Data for this combination.", type = "error")
            return(NULL)
          }
          return(build_tenure_figure(p_min, p_max, country_label, show_legend = TRUE))
        }
        if (is.null(df_src) || !"min_max_component" %in% names(df_src)) {
          showNotification("Data not available for this selection.", type = "error")
          return(NULL)
        }
        if (!"tenure" %in% names(df_src)) {
          showNotification("Tenure data not available for this selection.", type = "error")
          return(NULL)
        }
        df_tenure <- df_src |>
          filter(
            country == country_sel,
            wage %in% wage_filter
          )
        if (cost_category == "bonuses_and_benefits" && bonus_component != "all_bonuses") {
          component_key <- switch(
            bonus_component,
            ab = "annual_bonuses",
            pl = "paid_leave",
            up = "unemployment_protection",
            ob = "other_bonuses",
            NULL
          )
          if (!is.null(component_key)) {
            df_tenure <- df_tenure |> filter(grepl(component_key, min_max_component))
          }
          component_mode <- "single"
        }

        if (nrow(df_tenure) == 0) {
          showNotification("No Data for this combination.", type = "error")
          return(NULL)
        }

        if (cost_category == "all") {
          df_tenure <- df_tenure |>
            mutate(
              Scenario = ifelse(grepl("_min$", min_max_component), "Min", "Max"),
              Type = dplyr::case_when(
                grepl("_pension", min_max_component) ~ "Pensions",
                grepl("_health", min_max_component) ~ "Health",
                grepl("_bonuses_and_benefits", min_max_component) ~ "Bonuses and Benefits",
                grepl("_occupational_risk", min_max_component) ~ "Occupational Risk",
                grepl("_payroll_taxes", min_max_component) ~ "Payroll Taxes",
                TRUE ~ NA_character_
              )
            )
        } else {
          df_tenure <- df_tenure |>
            mutate(
              Scenario = ifelse(grepl("_min$", min_max_component), "Min", "Max"),
              Type = dplyr::case_when(
                grepl("annual_bonuses", min_max_component) ~ "Annual and other periodic bonuses",
                grepl("paid_leave", min_max_component) ~ "Paid Leave",
                grepl("unemployment_protection", min_max_component) ~ "Unemployment Protection",
                grepl("other_bonuses", min_max_component) ~ "Other bonuses",
                TRUE ~ NA_character_
              )
            )
        }

        df_tenure <- df_tenure |> filter(!is.na(Scenario)) |> prepare_tenure_axis()
        if (nrow(df_tenure) == 0) {
          showNotification("No Data for this combination.", type = "error")
          return(NULL)
        }

        ns_variables$df_final <- df_tenure
        if (component_mode == "single") {
          y_max <- max(df_tenure$value, na.rm = TRUE)
        } else {
          y_max <- df_tenure |>
            group_by(Scenario, tenure_plot) |>
            summarize(total = sum(value, na.rm = TRUE), .groups = "drop") |>
            summarize(max_total = max(total, na.rm = TRUE), .groups = "drop") |>
            pull(max_total)
        }
        y_range <- if (is.finite(y_max)) c(0, y_max * 1.1) else NULL

        if (component_mode == "single") {
          component_label <- unique(na.omit(df_tenure$Type))
          component_label <- if (length(component_label) > 0) component_label[1] else "Component"
          color <- colors[[component_label]]
          if (is.null(color)) {
            color <- "#00C1FF"
          }
          p_min <- build_tenure_bar(df_tenure, "Min", color, y_axis_title, y_range, TRUE)
          p_max <- build_tenure_bar(df_tenure, "Max", color, y_axis_title, y_range, FALSE)
          if (is.null(p_min) || is.null(p_max)) {
            showNotification("No Data for this combination.", type = "error")
            return(NULL)
          }
          return(build_tenure_figure(p_min, p_max, country_label))
        }

        p_min <- build_tenure_stack(
          df_tenure,
          "Min",
          colors,
          stack_order,
          y_axis_title,
          y_range,
          TRUE,
          TRUE
        )
        p_max <- build_tenure_stack(
          df_tenure,
          "Max",
          colors,
          stack_order,
          y_axis_title,
          y_range,
          FALSE,
          FALSE
        )
        if (is.null(p_min) || is.null(p_max)) {
          showNotification("No Data for this combination.", type = "error")
          return(NULL)
        }
        return(build_tenure_figure(p_min, p_max, country_label, show_legend = TRUE))
      }

      showNotification("Tenure view not available for this selection.", type = "message")
      return(NULL)
    }
    within_compare_bargap <- 0.4

    build_multicategory_stack <- function(df, colors, y_axis_title,
                                          stack_order = names(colors),
                                          legend_order = names(colors),
                                          legend_traceorder = "normal") {
      df <- df |>
        filter(!is.na(Type), !is.na(value)) |>
        mutate(
          Scenario = factor(Scenario, levels = c("Min", "Max")),
          wage = factor(wage, levels = wage_filter),
          Type = factor(Type, levels = stack_order)
        ) |>
        arrange(Scenario, wage)

      fig <- plot_ly(type = "bar")
      for (type in levels(df$Type)) {
        sub <- df |> filter(Type == type)
        if (nrow(sub) == 0) {
          next
        }
        hover_text <- NULL
        if ("hover_text" %in% names(sub)) {
          hover_text <- sub$hover_text
        }
        fig <- fig |> add_trace(
          x = list(sub$Scenario, sub$wage),
          y = sub$value,
          name = type,
          marker = list(color = colors[[type]]),
          hovertemplate = ns_hovertemplate,
          legendrank = match(type, legend_order)
        )
      }

      fig <- fig |>
        layout(
          barmode = "stack",
          bargap = within_compare_bargap,
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)",
          xaxis = list(
            title = "",
            type = "multicategory",
            showgrid = FALSE,
            zeroline = FALSE,
            showline = FALSE
          ),
          yaxis = list(
            title = y_axis_title,
            showgrid = FALSE,
            zeroline = FALSE,
            showline = FALSE
          ),
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.25,
            traceorder = legend_traceorder
          )
        )

      fig <- apply_labor_plot_theme(fig)
      fig <- fig |> layout(annotations = plot_footer_annotations())
      fig
    }
    build_component_subplot <- function(df, show_legend, y_axis_title,
                                        legend_order = component_legend_order,
                                        xaxis_title = "",
                                        yaxis_range = NULL) {
      # Use a single plot_ly with color = ~Type (instead of looping add_trace)
      # and force xaxis type = "category" with explicit categoryarray so plotly
      # does not mix discrete/non-discrete data when subplots are combined.
      if (is.null(df) || nrow(df) == 0) {
        return(plot_ly(type = "bar"))
      }
      df$Scenario <- as.character(df$Scenario)
      df$Type <- factor(df$Type, levels = component_stack_order)
      df <- df |> arrange(Type)

      plot_ly(
        data = df,
        x = ~Scenario,
        y = ~value,
        type = "bar",
        color = ~Type,
        colors = component_palette,
        legendgroup = ~Type,
        showlegend = show_legend,
        hovertemplate = ns_hovertemplate
      ) |>
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)",
          xaxis = list(
            title = xaxis_title,
            type = "category",
            categoryorder = "array",
            categoryarray = c("Min", "Max"),
            tickvals = c("Min", "Max"),
            ticktext = c("Min", "Max"),
            tickangle = 90,
            showgrid = FALSE,
            zeroline = FALSE,
            showline = FALSE
          ),
          yaxis = list(
            title = y_axis_title,
            range = yaxis_range,
            showgrid = FALSE,
            zeroline = FALSE,
            showline = FALSE
          ),
          barmode = "stack"
        )
    }
    build_bonus_subplot <- function(df, show_legend, y_axis_title,
                                    legend_order = bonus_stack_order,
                                    xaxis_title = "") {
      if (is.null(df) || nrow(df) == 0) {
        return(plot_ly(type = "bar"))
      }

      if (!"hover_text" %in% names(df)) {
        df$hover_text <- ""
      }

      df <- df |>
        mutate(
          Scenario = as.character(Scenario),
          Type = as.character(Type),
          value = ifelse(is.na(value), 0, value),
          hover_text = ifelse(is.na(hover_text), "", hover_text)
        ) |>
        filter(
          Scenario %in% c("Min", "Max"),
          Type %in% legend_order
        )

      missing_pairs <- expand.grid(
        Scenario = c("Min", "Max"),
        Type = legend_order,
        stringsAsFactors = FALSE
      ) |>
        anti_join(
          df |> distinct(Scenario, Type),
          by = c("Scenario", "Type")
        ) |>
        mutate(
          value = 0,
          hover_text = ""
        )

      df <- bind_rows(df, missing_pairs) |>
        mutate(
          Scenario = factor(Scenario, levels = c("Min", "Max")),
          Type = factor(Type, levels = legend_order)
        ) |>
        arrange(Type, Scenario) |>
        mutate(Scenario = as.character(Scenario))

      plot_ly(
        data = df,
        x = ~Scenario,
        y = ~value,
        type = "bar",
        color = ~Type,
        colors = bonus_palette,
        legendgroup = ~Type,
        showlegend = show_legend,
        hovertemplate = ns_hovertemplate
      ) |>
        layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)",
          xaxis = list(
            title = xaxis_title,
            type = "category",
            categoryorder = "array",
            categoryarray = c("Min", "Max"),
            tickvals = c("Min", "Max"),
            ticktext = c("Min", "Max"),
            showgrid = FALSE,
            zeroline = FALSE,
            showline = FALSE,
            tickangle = 90
          ),
          yaxis = list(
            title = y_axis_title,
            showgrid = FALSE,
            zeroline = FALSE,
            showline = FALSE
          ),
          barmode = "stack"
        )
    }

    prepare_payer_data <- function(df, aggregate = FALSE) {
      if (is.null(df)) {
        return(NULL)
      }
      df <- df |>
        filter(wage %in% wage_filter)
      if (aggregate) {
        df <- df |>
          group_by(country, wage, min_max_payer) |>
          summarize(
            value = sum(value, na.rm = TRUE),
            has_value = any(!is.na(value)),
            .groups = "drop"
          ) |>
          mutate(value = ifelse(has_value, value, NA_real_)) |>
          select(-has_value)
      }
      df |>
        apply_wage_panels() |>
        mutate(
          group = ifelse(grepl("_min$", min_max_payer), "Min", "Max"),
          payer = ifelse(grepl("employer", min_max_payer), "Employer", "Employee"),
          group = factor(group, levels = c("Min", "Max"))
        ) |>
        filter(!is.na(value))
    }

    prepare_all_payer_data <- function(df) {
      if (is.null(df)) {
        return(NULL)
      }
      if ("type_by_payer" %in% names(df)) {
        return(df |>
          select(country, type_by_payer, value) |>
          mutate(
            group = ifelse(grepl("_min$", type_by_payer), "Min", "Max"),
            payer = ifelse(grepl("^total_cost_employer", type_by_payer), "Employer", "Employee"),
            group = factor(group, levels = c("Min", "Max"))
          ))
      }
      if ("min_max_payer" %in% names(df)) {
        return(df |>
          select(country, min_max_payer, value) |>
          mutate(
            group = ifelse(grepl("_min$", min_max_payer), "Min", "Max"),
            payer = ifelse(grepl("employer", min_max_payer), "Employer", "Employee"),
            group = factor(group, levels = c("Min", "Max"))
          ))
      }
      NULL
    }

    prepare_all_component_data <- function(df) {
      if (is.null(df)) {
        return(NULL)
      }
      if ("type_by_component" %in% names(df)) {
        return(df |>
          select(any_of(c("country", "wage", "type_by_component", "value"))) |>
          mutate(
            group = ifelse(grepl("_min$", type_by_component), "Min", "Max"),
            payer = ifelse(grepl("_pension", type_by_component), "Pensions",
                           ifelse(grepl("_health", type_by_component), "Health",
                                  ifelse(grepl("_bonuses", type_by_component), "Bonuses and Benefits",
                                         ifelse(grepl("_occupational", type_by_component),
                                                "Occupational Risk", "Payroll Taxes")))),
            group = factor(group, levels = c("Min", "Max"))
          ))
      }
      if ("min_max_component" %in% names(df)) {
        return(df |>
          select(any_of(c("country", "wage", "min_max_component", "value"))) |>
          mutate(
            group = ifelse(grepl("_min$", min_max_component), "Min", "Max"),
            payer = dplyr::case_when(
              grepl("_pension", min_max_component) ~ "Pensions",
              grepl("_health", min_max_component) ~ "Health",
              grepl("_bonuses_and_benefits", min_max_component) ~ "Bonuses and Benefits",
              grepl("_occupational_risk", min_max_component) ~ "Occupational Risk",
              grepl("_payroll_taxes", min_max_component) ~ "Payroll Taxes",
              TRUE ~ "Payroll Taxes"
            ),
            group = factor(group, levels = c("Min", "Max"))
          ))
      }
      NULL
    }

    plot_payer_subplots <- function(df, y_axis_title) {
      if (is.null(df) || nrow(df) == 0) {
        showNotification("No Data for this combination.", type = "error")
        return(NULL)
      }
      country_levels <- panel_order()
      if (is.null(country_levels) || length(country_levels) == 0) {
        country_levels <- unique(df$country)
      }
      df <- df |>
        mutate(
          country = factor(country, levels = country_levels),
          Type = factor(payer, levels = c("Employee", "Employer")),
          Scenario = factor(group, levels = c("Min", "Max")),
          Scenario = as.character(Scenario)
        ) |>
        arrange(country)

      colors <- c("Employer" = "#002244", "Employee" = "#00C1FF")
      paises <- unique(df$country)
      plot_list <- list()

      for (i in seq_along(paises)) {
        pais <- paises[i]
        data_pais <- df |> filter(country == pais)

        if (nrow(data_pais) == 0) next

        show_legend <- i == 1

        p <- plot_ly(
          data = data_pais,
          x = ~Scenario,
          y = ~value,
          type = "bar",
          color = ~Type,
          colors = colors,
          legendgroup = ~Type,
          showlegend = show_legend,
          hovertemplate = ns_hovertemplate
        ) |>
          layout(
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor  = "rgba(0,0,0,0)",
            xaxis = list(
              title = pais,
              type = "category",
              categoryorder = "array",
              categoryarray = c("Min", "Max"),
              tickvals = c("Min", "Max"),
              ticktext = c("Min", "Max"),
              showgrid = FALSE,
              zeroline = FALSE,
              showline = FALSE,
              tickangle = 90
            ),
            yaxis = list(
              title = ifelse(i == 1, y_axis_title, ""),
              showgrid = FALSE,
              zeroline = FALSE,
              showline = FALSE
            ),
            barmode = "stack"
          )

        plot_list[[i]] <- p
      }

      n_plots <- length(plot_list)
      fig <- subplot(
        plot_list,
        nrows = 1,
        shareY = TRUE,
        titleX = TRUE,
        widths = rep(1 / n_plots, n_plots),
        margin = 0.01
      ) |>
        layout(
          title = "",
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.25
          ),
          margin = list(
            l = 70,
            r = 30,
            b = 110,
            t = 20
          )
        )

      apply_labor_plot_theme(fig)
    }

    # ========================================================================
    # COMPARE WAGES CASES
    # ========================================================================

    if (compare_wages && breakdown_type == "total") {
      if (length(ns_variables$country_sel) != 1 || "All" %in% ns_variables$country_sel) {
        return(NULL)
      }

      if (cost_category == "all") {
        df <- df_non_salary |>
          dplyr::filter(
            wage %in% wage_filter,
            country == ns_variables$country_sel
          ) |>
          mutate(
            Scenario = ifelse(type == "total_cost_min", "Min", "Max")
          ) |>
          select(wage, Scenario, value)
      } else if (cost_category == "social") {
        df <- get_group_data(social_subcomponent)
        if (is.null(df)) {
          showNotification("Data not available for this selection.", type = "error")
          return(NULL)
        }
        df <- df |>
          dplyr::filter(
            wage %in% wage_filter,
            country == ns_variables$country_sel
          ) |>
          mutate(
            Scenario = ifelse(grepl("_min$", min_max_total), "Min", "Max")
          ) |>
          select(wage, Scenario, value)
      } else {
        # OPTIMIZED: get_group_data() instead of readRDS()
        df <- get_group_data(cost_category)
        if (is.null(df)) {
          showNotification("Data not available for this selection.", type = "error")
          return(NULL)
        }
        df <- df |>
          dplyr::filter(
            wage %in% wage_filter,
            country == ns_variables$country_sel
          ) |>
          mutate(
            Scenario = ifelse(grepl("_min$", min_max_total), "Min", "Max")
          ) |>
          select(wage, Scenario, value)
      }

      if (nrow(df) == 0) {
        showNotification("No Data for this combination.", type = "error")
        return(NULL)
      }

      df <- df |>
        mutate(
          wage = factor(wage, levels = wage_filter),
          Scenario = factor(Scenario, levels = c("Min", "Max"))
        ) |>
        arrange(Scenario, wage)

      if (cost_category == "social") {
        social_colors <- resolve_social_colors(social_subcomponent)
        fig <- plot_ly(
          data = df,
          x = list(df$Scenario, df$wage),
          y = ~value,
          type = "bar",
          color = ~Scenario,
          colors = social_colors,
          showlegend = FALSE,
          hovertemplate = ns_hovertemplate
        ) |>
          layout(
            bargap = within_compare_bargap,
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor  = "rgba(0,0,0,0)",
            xaxis = list(
              title = "",
              type = "multicategory",
              showgrid = FALSE,
              zeroline = FALSE,
              showline = FALSE
            ),
            yaxis = list(
              title = y_axis_title,
              showgrid = FALSE,
              zeroline = FALSE,
              showline = FALSE
            )
          )
      } else {
        fig <- plot_ly(
          data = df,
          x = list(df$Scenario, df$wage),
          y = ~value,
          type = "bar",
          marker = list(color = "#002244"),
          showlegend = FALSE,
          hovertemplate = ns_hovertemplate
        ) |>
          layout(
            bargap = within_compare_bargap,
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor  = "rgba(0,0,0,0)",
            xaxis = list(
              title = "",
              type = "multicategory",
              showgrid = FALSE,
              zeroline = FALSE,
              showline = FALSE
            ),
            yaxis = list(
              title = y_axis_title,
              showgrid = FALSE,
              zeroline = FALSE,
              showline = FALSE
            )
          )
      }

      fig <- apply_labor_plot_theme(fig)
      fig <- fig |> layout(annotations = plot_footer_annotations())
      return(fig)
    }

    if (compare_wages && breakdown_type == "payer" && cost_category == "all") {
      if (length(ns_variables$country_sel) != 1 || "All" %in% ns_variables$country_sel) {
        return(NULL)
      }

      df <- df_non_salary_payer |>
        dplyr::filter(
          wage %in% wage_filter,
          country == ns_variables$country_sel
        ) |>
        mutate(
          Scenario = ifelse(grepl("_min$", min_max_payer), "Min", "Max"),
          Type = ifelse(grepl("employer", min_max_payer), "Employer", "Employee")
        ) |>
        select(wage, Scenario, Type, value)

      if (nrow(df) == 0) {
        showNotification("No Data for this combination.", type = "error")
        return(NULL)
      }

      colors <- c("Employer" = "#002244", "Employee" = "#00C1FF")
      return(build_multicategory_stack(
        df,
        colors,
        y_axis_title,
        stack_order = c("Employee", "Employer"),
        legend_order = c("Employee", "Employer")
      ))
    }

    if (compare_wages && breakdown_type == "payer" && cost_category %in% c("social", "payroll_taxes")) {
      if (cost_category == "social" && social_subcomponent == "occupational_risk") {
        showNotification("Occupational risk contributions are only available in total.", type = "message")
        return(NULL)
      }
      if (length(ns_variables$country_sel) != 1 || "All" %in% ns_variables$country_sel) {
        return(NULL)
      }

      payer_key <- if (cost_category == "social") {
        social_subcomponent
      } else {
        "payroll_taxes"
      }
      df_raw <- get_payer_data(payer_key)
      if (is.null(df_raw)) {
        showNotification("Data not available for this selection.", type = "error")
        return(NULL)
      }
      df_raw <- df_raw |> filter(country == ns_variables$country_sel)

      df_long <- prepare_payer_data(
        df_raw,
        aggregate = identical(payer_key, "payroll_taxes")
      )
      if (is.null(df_long) || nrow(df_long) == 0) {
        showNotification("No Data for this combination.", type = "error")
        return(NULL)
      }

      df <- df_long |>
        transmute(
          wage,
          Scenario = group,
          Type = payer,
          value
        )

      colors <- c("Employer" = "#002244", "Employee" = "#00C1FF")
      return(build_multicategory_stack(
        df,
        colors,
        y_axis_title,
        stack_order = c("Employee", "Employer"),
        legend_order = c("Employee", "Employer")
      ))
    }

    if (compare_wages && breakdown_type == "component" && cost_category == "all") {
      if (length(ns_variables$country_sel) != 1 || "All" %in% ns_variables$country_sel) {
        return(NULL)
      }

      df <- df_non_salary_component |>
        dplyr::filter(
          wage %in% wage_filter,
          country == ns_variables$country_sel
        ) |>
        apply_wage_panels()
      df <- prepare_all_component_data(df)
      if (!is.null(df)) {
        df <- df |>
          transmute(
            wage,
            Scenario = group,
            Type = payer,
            value
          )
      }

      if (is.null(df) || nrow(df) == 0) {
        showNotification("No Data for this combination.", type = "error")
        return(NULL)
      }

      return(build_multicategory_stack(
        df,
        component_palette,
        y_axis_title,
        stack_order = component_stack_order,
        legend_order = component_legend_order
      ))
    }

    if (compare_wages && breakdown_type == "component" && cost_category == "bonuses_and_benefits") {
      if (length(ns_variables$country_sel) != 1 || "All" %in% ns_variables$country_sel) {
        return(NULL)
      }

      # OPTIMIZED: get_component_data() instead of readRDS()
      df <- get_component_data("bonuses_and_benefits")
      if (is.null(df)) {
        showNotification("Data not available.", type = "error")
        return(NULL)
      }
      df <- df |>
        dplyr::filter(
          wage %in% wage_filter,
          country == ns_variables$country_sel
        ) |>
        mutate(
          Scenario = ifelse(grepl("_min$", min_max_component), "Min", "Max"),
          Type = dplyr::case_when(
            grepl("annual_bonuses", min_max_component) ~ "Annual and other periodic bonuses",
            grepl("paid_leave", min_max_component) ~ "Paid Leave",
            grepl("unemployment_protection", min_max_component) ~ "Unemployment Protection",
            grepl("other_bonuses", min_max_component) ~ "Other bonuses",
            TRUE ~ NA_character_
          )
        ) |>
        select(country, wage, Scenario, Type, value)

      if (bonus_component != "all_bonuses") {
        component_label <- switch(
          bonus_component,
          ab = "Annual and other periodic bonuses",
          pl = "Paid Leave",
          up = "Unemployment Protection",
          ob = "Other bonuses",
          bonus_component
        )
        df <- df |> filter(Type == component_label)
      }

      df <- df |>
        left_join(
          bonus_hover_lookup,
          by = c("country", "wage", "Scenario" = "group", "Type" = "Type")
        )

      if (nrow(df) == 0) {
        showNotification("No Data for this combination.", type = "error")
        return(NULL)
      }

      colors <- c(
        "Annual and other periodic bonuses" = "#002244",
        "Paid Leave" = "#8EA2BF",
        "Unemployment Protection" = "#B9BAB5",
        "Other bonuses" = "#6F6779"
      )
      return(build_multicategory_stack(df, colors, y_axis_title))
    }

    if (compare_wages && breakdown_type == "component" && cost_category == "payroll_taxes") {
      if (length(ns_variables$country_sel) != 1 || "All" %in% ns_variables$country_sel) {
        return(NULL)
      }

      df <- get_component_data("payroll_taxes")
      if (is.null(df)) {
        showNotification("Data not available for payroll taxes by component.", type = "error")
        return(NULL)
      }
      df <- df |>
        dplyr::filter(
          wage %in% wage_filter,
          country == ns_variables$country_sel
        ) |>
        mutate(
          Scenario = ifelse(min_max_component == "min", "Min", "Max"),
          Type = component
        ) |>
        select(country, wage, Scenario, Type, value)

      if (nrow(df) == 0) {
        showNotification("No Data for this combination.", type = "error")
        return(NULL)
      }

      components <- unique(df$Type)
      base_colors <- c("#002244", "#00C1FF", "#726AA8", "#335B8E", "#B9BAB5",
                       "#8EA2BF", "#6F6779", "#4A90D9", "#A8C5DA")
      colors <- setNames(base_colors[seq_along(components)], components)

      return(build_multicategory_stack(
        df,
        colors,
        y_axis_title,
        stack_order = components,
        legend_order = components
      ))
    }

    if (compare_wages && breakdown_type == "component" && cost_category == "social") {
      if (length(ns_variables$country_sel) != 1 || "All" %in% ns_variables$country_sel) {
        return(NULL)
      }

      # OPTIMIZED: get_group_data() instead of readRDS()
      df <- get_group_data(social_subcomponent)
      if (is.null(df)) {
        showNotification("Data not available.", type = "error")
        return(NULL)
      }
      df <- df |>
        dplyr::filter(
          wage %in% wage_filter,
          country == ns_variables$country_sel
        ) |>
        mutate(
          Scenario = ifelse(grepl("_min$", min_max_total), "Min", "Max")
        ) |>
        select(wage, Scenario, value)

      if (nrow(df) == 0) {
        showNotification("No Data for this combination.", type = "error")
        return(NULL)
      }

      df <- df |>
        mutate(
          wage = factor(wage, levels = wage_filter),
          Scenario = factor(Scenario, levels = c("Min", "Max"))
        ) |>
        arrange(Scenario, wage)

      fig <- plot_ly(
        data = df,
        x = list(df$Scenario, df$wage),
        y = ~value,
        type = "bar",
        marker = list(color = "#002244"),
        showlegend = FALSE,
        hovertemplate = ns_hovertemplate
      ) |>
        layout(
          bargap = within_compare_bargap,
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)",
          xaxis = list(
            title = "",
            type = "multicategory",
            showgrid = FALSE,
            zeroline = FALSE,
            showline = FALSE
          ),
          yaxis = list(
            title = y_axis_title,
            showgrid = FALSE,
            zeroline = FALSE,
            showline = FALSE
          )
        )

      fig <- apply_labor_plot_theme(fig)
      fig <- fig |> layout(annotations = plot_footer_annotations())
      return(fig)
    }

    if (breakdown_type == "payer" && cost_category %in% c("social", "payroll_taxes")) {
      if (cost_category == "social" && social_subcomponent == "occupational_risk") {
        showNotification("Occupational risk contributions are only available in total.", type = "message")
        return(NULL)
      }
      payer_key <- if (cost_category == "social") {
        social_subcomponent
      } else {
        "payroll_taxes"
      }
      df_raw <- get_payer_data(payer_key)
      if (is.null(df_raw)) {
        showNotification("Data not available for this selection.", type = "error")
        return(NULL)
      }
      if (length(ns_variables$country_sel) > 1) {
        if ("All" %in%  ns_variables$country_sel) {
          showNotification("Please select only countries.", type = "error")
          return(NULL)
        }
        df_raw <- df_raw |> filter(country %in% ns_variables$country_sel)
      } else if (length(ns_variables$country_sel) == 1 && ns_variables$country_sel != "All") {
        df_raw <- df_raw |> filter(country == ns_variables$country_sel)
      } else {
        ns_variables$countries <- c("All", unique(df_raw$country))
      }
      if (is_cross_country && cost_category == "payroll_taxes") {
        candidate_countries <- if (length(ns_variables$country_sel) == 0 ||
          "All" %in% ns_variables$country_sel) {
          sources$countries
        } else {
          ns_variables$country_sel
        }
        df_missing_src <- df_raw |> filter(wage %in% wage_filter)
        df_missing_src <- filter_missing_payroll(df_missing_src, candidate_countries)
        missing <- ns_variables$missing_payroll_countries
        if (length(missing) > 0) {
          df_raw <- df_raw |> filter(!country %in% missing)
        }
      }

      df_long <- prepare_payer_data(
        df_raw,
        aggregate = identical(payer_key, "payroll_taxes")
      )
      if (is.null(df_long) || nrow(df_long) == 0) {
        showNotification("No Data for this combination.", type = "error")
        return(NULL)
      }

      if (is_cross_country && cost_category == "payroll_taxes") {
        order_src <- get_group_data("payroll_taxes")
        if (!is.null(order_src)) {
          order_df <- order_src |>
            filter(wage %in% wage_filter) |>
            apply_wage_panels() |>
            select(country, min_max_total, value) |>
            mutate(type = ifelse(grepl("_min$", min_max_total), "Min", "Max"))
          if (length(ns_variables$country_sel) > 0 && !"All" %in% ns_variables$country_sel) {
            order_df <- order_df |> filter(country %in% ns_variables$country_sel)
          }
          candidate_countries <- if (length(ns_variables$country_sel) == 0 ||
            "All" %in% ns_variables$country_sel) {
            sources$countries
          } else {
            ns_variables$country_sel
          }
          order_df <- filter_missing_payroll(order_df, candidate_countries)
          if (nrow(order_df) > 0) {
            order_wide <- order_df |>
              group_by(country) |>
              summarize(
                total_cost_min = min(value, na.rm = TRUE),
                total_cost_max = max(value, na.rm = TRUE),
                .groups = "drop"
              ) |>
              arrange(total_cost_min)
            set_order_country(unique(order_wide$country))
          }
        }
      }

      return(plot_payer_subplots(df_long, y_axis_title))
    }
    
    
    # ---- ALL and Total ----
    
    if (cost_category=="all" & breakdown_type == "total" & length(ns_variables$country_sel)==1) {
      
      if(ns_variables$country_sel=="All"){
        
        # Filtering total non salary
        df <- df_non_salary |>
          dplyr::filter(
            wage %in% wage_filter
          ) |>
          apply_wage_panels()
        
        if (nrow(df) == 0) {
          showNotification("No Data for this combination.", type = "error")
          return(NULL)
        }
        
        df_wide <- df |>
          tidyr::pivot_wider(
            names_from = type,
            values_from = value
          ) |>
          arrange(total_cost_min) |>
          mutate(country = factor(country, levels = country))
        
        set_order_country(unique(as.character(df_wide$country)))
        
        df_mm <- df_wide |>
          tidyr::pivot_longer(
            cols = c(total_cost_min, total_cost_max),
            names_to = "Scenario",
            values_to = "value"
          ) |>
          mutate(
            Scenario = ifelse(Scenario == "total_cost_min", "Min", "Max"),
            Scenario = factor(Scenario, levels = c("Min", "Max")),
            country  = factor(country, levels = panel_order())
          )
        
        ns_variables$df_final=df_mm
        scenario_colors <- if (cost_category == "social") {
          resolve_social_colors(social_subcomponent)
        } else {
          c("Min" = "#00C1FF", "Max" = "#002244")
        }

        paises <- unique(df_mm$country)
        plot_list <- list()
        
        for (i in seq_along(paises)) {
          
          pais <- paises[i]
          data_pais <- df_mm |> filter(country == pais)
          
          p <- plot_ly(
            data = data_pais,
            x = ~Scenario,
            y = ~value,
            type = "bar",
            color = ~Scenario,
            colors = scenario_colors,
            showlegend = FALSE,
            hovertemplate = ns_hovertemplate
          ) |>
            layout(
              barmode = "stack",   
              
              paper_bgcolor = "rgba(0,0,0,0)",
              plot_bgcolor  = "rgba(0,0,0,0)",
              
              xaxis = list(
                title = pais,
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE,
                tickangle = 90
              ),
              
              yaxis = list(
                title = ifelse(i == 1, y_axis_title, ""),
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE
              )
            )
          
          plot_list[[i]] <- p
        }
        
        fig <- subplot(
          plot_list,
          nrows = 1,
          shareY = TRUE,
          titleX = TRUE,
          widths = rep(1 / length(plot_list), length(plot_list)),
          margin = 0.01
        ) |>
          layout(
            margin = list(l = 70, r = 30, b = 110, t = 20),
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor  = "rgba(0,0,0,0)"
          )
        
        return(apply_labor_plot_theme(fig))
      }
      
      else{
        df <- df_non_salary |>
          filter(
            wage %in% wage_filter,
            country == ns_variables$country_sel
          ) |>
          apply_wage_panels()
        
        if (nrow(df) == 0) {
          showNotification("No Data for this combination.", type = "error")
          return(NULL)
        }
        
        df_wide <- df |>
          tidyr::pivot_wider(
            names_from = type,
            values_from = value
          ) |>
          arrange(total_cost_min) |>
          mutate(country = factor(country, levels = country))
        
        df_mm <- df_wide |>
          tidyr::pivot_longer(
            cols = c(total_cost_min, total_cost_max),
            names_to = "Scenario",
            values_to = "value"
          ) |>
          mutate(
            Scenario = ifelse(Scenario == "total_cost_min", "Min", "Max"),
            Scenario = factor(Scenario, levels = c("Min", "Max")),
            country  = factor(country, levels = panel_order())
          )
        
        ns_variables$df_final=df_mm
        scenario_colors <- if (cost_category == "social") {
          resolve_social_colors(social_subcomponent)
        } else {
          c("Min" = "#00C1FF", "Max" = "#002244")
        }

        paises <- unique(df_mm$country)
        plot_list <- list()
        
        for (i in seq_along(paises)) {
          
          pais <- paises[i]
          data_pais <- df_mm |> filter(country == pais)
          
          p <- plot_ly(
            data = data_pais,
            x = ~Scenario,
            y = ~value,
            type = "bar",
            color = ~Scenario,
            colors = scenario_colors,
            showlegend = FALSE,
            hovertemplate = ns_hovertemplate
          ) |>
            layout(
              barmode = "stack",
              
              paper_bgcolor = "rgba(0,0,0,0)",
              plot_bgcolor  = "rgba(0,0,0,0)",
              
              xaxis = list(
                title = pais,
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE,
                tickangle = 90
              ),
              
              yaxis = list(
                title = ifelse(i == 1, y_axis_title, ""),
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE
              )
            )
          
          plot_list[[i]] <- p
        }
        
        fig <- subplot(
          plot_list,
          nrows = 1,
          shareY = TRUE,
          titleX = TRUE,
          widths = rep(1 / length(plot_list), length(plot_list)),
          margin = 0.01
        ) |>
          layout(
            margin = list(l = 70, r = 30, b = 110, t = 20),
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor  = "rgba(0,0,0,0)"
          )
        
        return(apply_labor_plot_theme(fig))
      }
      
    }
    
    if (cost_category=="all" & breakdown_type == "total" & length(ns_variables$country_sel)>1) {
      if ("All" %in%  ns_variables$country_sel) {
        showNotification("Please select only countries.", type = "error")
        return(NULL)
      }
      ns_variables$countries=c("All",unique(df_non_salary$country))
      # Filtering total non salary
      df <- df_non_salary |>
        filter(
          wage %in% wage_filter,
          country %in% ns_variables$country_sel
        ) |>
        apply_wage_panels()
      
      if (nrow(df) == 0) {
        showNotification("No Data for this combination.", type = "error")
        return(NULL)
      }
      
      df_wide <- df |>
        tidyr::pivot_wider(
          names_from = type,
          values_from = value
        ) |>
        arrange(total_cost_min) |>
        mutate(country = factor(country, levels = country))
      
      set_order_country(unique(as.character(df_wide$country)))
      
      df_mm <- df_wide |>
        tidyr::pivot_longer(
          cols = c(total_cost_min, total_cost_max),
          names_to = "Scenario",
          values_to = "value"
        ) |>
        mutate(
          Scenario = ifelse(Scenario == "total_cost_min", "Min", "Max"),
          Scenario = factor(Scenario, levels = c("Min", "Max")),
          country  = factor(country, levels = panel_order())
        )
      
      ns_variables$df_final=df_mm
      
      paises <- unique(df_mm$country)
      plot_list <- list()
      
      for (i in seq_along(paises)) {
        
        pais <- paises[i]
        data_pais <- df_mm |> filter(country == pais)
        
        p <- plot_ly(
          data = data_pais,
          x = ~Scenario,
          y = ~value,
          type = "bar",
          color = ~Scenario,
          colors = c("Min" = "#00C1FF", "Max" = "#002244"),
          showlegend = FALSE,
          hovertemplate = ns_hovertemplate
        ) |>
          layout(
            barmode = "stack",
            
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor  = "rgba(0,0,0,0)",
            
            xaxis = list(
              title = pais,
              showgrid = FALSE,
              zeroline = FALSE,
              showline = FALSE,
              tickangle = 90
            ),
            
            yaxis = list(
              title = ifelse(i == 1, y_axis_title, ""),
              showgrid = FALSE,
              zeroline = FALSE,
              showline = FALSE
            )
          )
        
        plot_list[[i]] <- p
      }
      
      fig <- subplot(
        plot_list,
        nrows = 1,
        shareY = TRUE,
        titleX = TRUE,
        widths = rep(1 / length(plot_list), length(plot_list)),
        margin = 0.01
      ) |>
        layout(
          margin = list(l = 70, r = 30, b = 110, t = 20),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)"
        )
      
      return(apply_labor_plot_theme(fig))
    }
    
    # ---- ALL By Payer ----
    
    if (cost_category=="all" & breakdown_type == "payer" & length(ns_variables$country_sel)==1) {
      
      ns_variables$countries=c("All",unique(df_non_salary$country))
      
      if(ns_variables$country_sel=="All"){
        df_long <- df_non_salary_payer |>
          filter(
            wage %in% wage_filter
          ) |>
          apply_wage_panels()
        df_long <- prepare_all_payer_data(df_long)
        
        if (is.null(df_long) || nrow(df_long) == 0) {
          showNotification("No Data for this combination.", type = "error")
          return(NULL)
        }

        df_long <- df_long |>
          mutate(country = factor(country, levels = panel_order())) |> 
          arrange(country)
        
        df <- df_long
        df$Type <- factor(df$payer, levels = c("Employee", "Employer"))
        df$Scenario <- factor(df$group, levels = c("Min", "Max"))
        
        colors <- c("Employer" = "#002244", "Employee" = "#00C1FF")
        
        paises <- unique(df$country)
        plot_list <- list()
        
        for (i in seq_along(paises)) {
          
          pais <- paises[i]
          data_pais <- df |> filter(country == pais)
          
          if (nrow(data_pais) == 0) next
          
          show_legend <- i == 1
          
          p <- plot_ly(
            data = data_pais,
            x = ~Scenario,
            y = ~value,
            type = "bar",
            color = ~Type,
            colors = colors,
            legendgroup = ~Type,
            showlegend = show_legend,
            hovertemplate = ns_hovertemplate
          ) |>
            layout(
              paper_bgcolor = "rgba(0,0,0,0)",
              plot_bgcolor  = "rgba(0,0,0,0)",
              xaxis = list(
                title = pais,
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE,
                tickangle = 90
              ),
              
              yaxis = list(
                title = ifelse(i == 1, y_axis_title, ""),
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE
              ),
              
              barmode = "stack"
            )
          
          plot_list[[i]] <- p
        }
        
        n_plots <- length(plot_list)
        fig <- subplot(
          plot_list,
          nrows = 1,
          shareY = TRUE,
          titleX = TRUE,
          widths = rep(1 / n_plots, n_plots), 
          margin = 0.01
        ) |>
          layout(
            title = "",
            
            legend = list(
              orientation = "h",
              x = 0.5,
              xanchor = "center",
              y = -0.25
            ),
            
            margin = list(
              l = 70,
              r = 30,
              b = 110,
              t = 20
            )
          )
        return(apply_labor_plot_theme(fig))
      }
      
      else{
        
        df_long <- df_non_salary_payer |>
          filter(
            wage %in% wage_filter,
            country== ns_variables$country_sel
          ) |>
          apply_wage_panels()
        df_long <- prepare_all_payer_data(df_long)

        if (is.null(df_long) || nrow(df_long) == 0) {
          showNotification("No Data for this combination.", type = "error")
          return(NULL)
        }
        
        
        df <- df_long
        df$Type <- factor(df$payer, levels = c("Employee", "Employer"))
        df$Scenario <- factor(df$group, levels = c("Min", "Max"))
        
        colors <- c("Employer" = "#002244", "Employee" = "#00C1FF")
        
        paises <- unique(df$country)
        plot_list <- list()
        
        for (i in seq_along(paises)) {
          
          pais <- paises[i]
          data_pais <- df |> filter(country == pais)
          
          if (nrow(data_pais) == 0) next
          
          show_legend <- i == 1
          
          p <- plot_ly(
            data = data_pais,
            x = ~Scenario,
            y = ~value,
            type = "bar",
            color = ~Type,
            colors = colors,
            legendgroup = ~Type,
            showlegend = show_legend,
            hovertemplate = ns_hovertemplate
          ) |>
            layout(
              paper_bgcolor = "rgba(0,0,0,0)",
              plot_bgcolor  = "rgba(0,0,0,0)",
              
              xaxis = list(
                title = pais,
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE,
                tickangle = 90
              ),
              
              yaxis = list(
                title = ifelse(i == 1, y_axis_title, ""),
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE
              ),
              
              barmode = "stack"
            )
          
          plot_list[[i]] <- p
        }
        
        n_plots <- length(plot_list)
        fig <- subplot(
          plot_list,
          nrows = 1,
          shareY = TRUE,
          titleX = TRUE,
          widths = rep(1 / n_plots, n_plots), 
          margin = 0.01
        ) |>
          layout(
            title = "",
            
            legend = list(
              orientation = "h",
              x = 0.5,
              xanchor = "center",
              y = -0.25
            ),
            
            margin = list(
              l = 70,
              r = 30,
              b = 110,
              t = 20
            )
          )
        return(apply_labor_plot_theme(fig))
      }
      
    }
    
    if (cost_category=="all" & breakdown_type == "payer" & length(ns_variables$country_sel)>1) {
      
      ns_variables$countries=c("All",unique(df_non_salary$country))
      
      if ("All" %in%  ns_variables$country_sel) {
        showNotification("Please select only countries.", type = "error")
        return(NULL)
      }
      
      df_long <- df_non_salary_payer |>
        filter(
          wage %in% wage_filter,
          country %in% ns_variables$country_sel
        ) |>
        apply_wage_panels()
      df_long <- prepare_all_payer_data(df_long)
      
      if (is.null(df_long) || nrow(df_long) == 0) {
        showNotification("No Data for this combination.", type = "error")
        return(NULL)
      }
      
      df_long <- df_long |>
        mutate(country = factor(country, levels = panel_order())) |> 
        arrange(country)
      
      
      df <- df_long
      df$Type <- factor(df$payer, levels = c("Employee", "Employer"))
      df$Scenario <- factor(df$group, levels = c("Min", "Max"))
      
      colors <- c("Employer" = "#002244", "Employee" = "#00C1FF")
      
      paises <- unique(df$country)
      plot_list <- list()
      
      ns_variables$df_final=df
      for (i in seq_along(paises)) {
        pais <- paises[i]
        data_pais <- df |> filter(country == pais)
        
        show_legend <- ifelse(i == 1, TRUE, FALSE)
        
        p <- plot_ly(data_pais, x = ~Scenario, y = ~value, type = 'bar',
                     color = ~Type, colors = colors, legendgroup = ~Type,
                     showlegend = show_legend,
                     hovertemplate = ns_hovertemplate) |>
          layout(
            paper_bgcolor = "rgba(0,0,0,0)",   
            plot_bgcolor  = "rgba(0,0,0,0)", 
            xaxis = list(
              title = pais,
              showgrid = FALSE,
              zeroline = FALSE,
              showline = FALSE
            ),
            yaxis = list(
              title = ifelse(i == 1, y_axis_title, ""),
              range = c(0, 140),
              showgrid = FALSE,
              zeroline = FALSE,
              showline = FALSE
            ),
            barmode = 'stack'
          )
        
        plot_list[[i]] <- p
      }
      
      n_plots <- length(plot_list)
      fig <- subplot(
        plot_list,
        nrows = 1,
        shareY = TRUE,
        titleX = TRUE,
        widths = rep(1 / n_plots, n_plots), 
        margin = 0.01
      ) |>
        layout(
          title = "",
          
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.25
          ),
          
          margin = list(
            l = 70,
            r = 30,
            b = 110,
            t = 20
          )
        )
      
      return(apply_labor_plot_theme(fig))
    }
    
    # ---- ALL by Component ----

    if (cost_category=="all" & breakdown_type == "component" & length(ns_variables$country_sel)==1) {
      ns_variables$countries=c("All",unique(df_non_salary$country))
      if(ns_variables$country_sel=="All"){
        df_long <- df_non_salary_component |>
          filter(
            wage %in% wage_filter
          ) |>
          apply_wage_panels()
        df_long <- prepare_all_component_data(df_long)
        
        if (is.null(df_long) || nrow(df_long) == 0) {
          showNotification("No Data for this combination.", type = "error")
          return(NULL)
        }

        df_long <- df_long |>
          mutate(country = factor(country, levels = panel_order())) |> 
          arrange(country)
        
        df <- df_long
        df$Type <- factor(df$payer, levels = component_stack_order)
        df$Scenario <- factor(df$group, levels = c("Min", "Max"))
        
        paises <- unique(df$country)
        plot_list <- list()
        
        for (i in seq_along(paises)) {
          
          pais <- paises[i]
          data_pais <- df |> filter(country == pais)
          
          if (nrow(data_pais) == 0) next
          
          show_legend <- i == 1
          
          p <- build_component_subplot(
            data_pais,
            show_legend = show_legend,
            y_axis_title = ifelse(i == 1, y_axis_title, ""),
            legend_order = component_legend_order,
            xaxis_title = pais
          )
          
          plot_list[[i]] <- p
        }
        
        n_plots <- length(plot_list)
        fig <- subplot(
          plot_list,
          nrows = 1,
          shareY = TRUE,
          titleX = TRUE,
          widths = rep(1 / n_plots, n_plots), 
          margin = 0.01
        ) |>
          layout(
            title = "",
            
            legend = list(
              orientation = "h",
              x = 0.5,
              xanchor = "center",
              y = -0.25,
              traceorder = "normal"
            ),
            
            margin = list(
              l = 70,
              r = 30,
              b = 110,
              t = 20
            )
          )
        return(apply_labor_plot_theme(fig))
      }
      else{
        df_long <- df_non_salary_component |>
          filter(
            wage %in% wage_filter,
            country==ns_variables$country_sel
          ) |>
          apply_wage_panels()
        df_long <- prepare_all_component_data(df_long)

        if (is.null(df_long) || nrow(df_long) == 0) {
          showNotification("No Data for this combination.", type = "error")
          return(NULL)
        }
        
        
        df <- df_long
        df$Type <- factor(df$payer, levels = component_stack_order)
        df$Scenario <- factor(df$group, levels = c("Min", "Max"))
        
        paises <- unique(df$country)
        plot_list <- list()
        
        for (i in seq_along(paises)) {
          
          pais <- paises[i]
          data_pais <- df |> filter(country == pais)
          
          if (nrow(data_pais) == 0) next
          
          show_legend <- i == 1
          
          p <- build_component_subplot(
            data_pais,
            show_legend = show_legend,
            y_axis_title = ifelse(i == 1, y_axis_title, ""),
            legend_order = component_legend_order,
            xaxis_title = pais
          )
          
          plot_list[[i]] <- p
        }
        
        n_plots <- length(plot_list)
        fig <- subplot(
          plot_list,
          nrows = 1,
          shareY = TRUE,
          titleX = TRUE,
          widths = rep(1 / n_plots, n_plots), 
          margin = 0.01
        ) |>
          layout(
            title = "",
            
            legend = list(
              orientation = "h",
              x = 0.5,
              xanchor = "center",
              y = -0.25,
              traceorder = "normal"
            ),
            
            margin = list(
              l = 70,
              r = 30,
              b = 110,
              t = 20
            )
          )
        return(apply_labor_plot_theme(fig))
      }
    }
    
    if (cost_category=="all" & breakdown_type == "component" & length(ns_variables$country_sel)>1) {
      ns_variables$countries=c("All",unique(df_non_salary$country))
      
      if ("All" %in%  ns_variables$country_sel) {
        showNotification("Please select only countries.", type = "error")
        return(NULL)
      }
      df_long <- df_non_salary_component |>
        filter(
          wage %in% wage_filter,
          country %in% ns_variables$country_sel 
        ) |>
        apply_wage_panels()
      df_long <- prepare_all_component_data(df_long)
      
      if (is.null(df_long) || nrow(df_long) == 0) {
        showNotification("No Data for this combination.", type = "error")
        return(NULL)
      }
      
      
      df <- df_long
      df$Type <- factor(df$payer, levels = component_stack_order)
      df$Scenario <- factor(df$group, levels = c("Min", "Max"))
      
      paises <- unique(df$country)
      plot_list <- list()
      
      ns_variables$df_final=df
      for (i in seq_along(paises)) {
        pais <- paises[i]
        data_pais <- df |> filter(country == pais)
        
        show_legend <- ifelse(i == 1, TRUE, FALSE)
        
        p <- build_component_subplot(
          data_pais,
          show_legend = show_legend,
          y_axis_title = ifelse(i == 1, y_axis_title, ""),
          legend_order = component_legend_order,
          xaxis_title = pais,
          yaxis_range = c(0, 140)
        )
        
        plot_list[[i]] <- p
      }
      
      n_plots <- length(plot_list)
      fig <- subplot(
        plot_list,
        nrows = 1,
        shareY = TRUE,
        titleX = TRUE,
        widths = rep(1 / n_plots, n_plots), 
        margin = 0.01
      ) |>
        layout(
          title = "",
          
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.25
          ),
          
          margin = list(
            l = 70,
            r = 30,
            b = 110,
            t = 20
          )
        )
      
      return(apply_labor_plot_theme(fig))
    }
    
    # ---- bonuses and benefits/Payroll and Total ----
    
    if (breakdown_type == "total" &&
        cost_category %in% c("bonuses_and_benefits", "payroll_taxes", "social") &&
        length(ns_variables$country_sel) == 1) {
    
    if(ns_variables$country_sel=="All"){
        # OPTIMIZED: get_group_data() instead of readRDS()
        data_key <- if (cost_category == "social") social_subcomponent else cost_category
        df <- get_group_data(data_key)
        if (is.null(df)) {
          showNotification("Data not available.", type = "error")
          return(NULL)
        }
        df <- df |>
          filter(
            wage %in% wage_filter
          ) |>
          apply_wage_panels() |>
          select(country, min_max_total, value) |>
          mutate(
            type = ifelse(grepl("_min$", min_max_total), "Min", "Max")
          )
        all_countries <- unique(df$country)
        df <- filter_missing_payroll(df, sources$countries)
        df <- filter_missing_occ_risk(df, sources$countries)
        ns_variables$countries=c("All", all_countries)
      
    
        
        if (nrow(df) == 0) {
          showNotification("No Data for this combination.", type = "error")
          return(NULL)
        }
        
        df_wide=df |>
          group_by(country) |>
          summarize(
            total_cost_min = min(value, na.rm = TRUE),
            total_cost_max = max(value, na.rm = TRUE)
          )|>
          arrange(total_cost_min) |>
          mutate(country = factor(country, levels = country))

        set_order_country(unique(as.character(df_wide$country)))
        
        df_mm <- df_wide |>
          tidyr::pivot_longer(
            cols = c(total_cost_min, total_cost_max),
            names_to = "Scenario",
            values_to = "value"
          ) |>
          mutate(
            Scenario = ifelse(Scenario == "total_cost_min", "Min", "Max"),
            Scenario = factor(Scenario, levels = c("Min", "Max")),
            country  = factor(country, levels = panel_order())
          )
        
        ns_variables$df_final=df_mm
        scenario_colors <- if (cost_category == "social") {
          resolve_social_colors(social_subcomponent)
        } else {
          c("Min" = "#00C1FF", "Max" = "#002244")
        }

        paises <- unique(df_mm$country)
        plot_list <- list()
        
        for (i in seq_along(paises)) {
          
          pais <- paises[i]
          data_pais <- df_mm |> filter(country == pais)
          
          p <- plot_ly(
            data = data_pais,
            x = ~Scenario,
            y = ~value,
            type = "bar",
            color = ~Scenario,
            colors = scenario_colors,
            showlegend = FALSE,
            hovertemplate = ns_hovertemplate
          ) |>
            layout(
              barmode = "stack",   
              
              paper_bgcolor = "rgba(0,0,0,0)",
              plot_bgcolor  = "rgba(0,0,0,0)",
              
              xaxis = list(
                title = pais,
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE,
                tickangle = 90
              ),
              
              yaxis = list(
                title = ifelse(i == 1, y_axis_title, ""),
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE
              )
            )
          
          plot_list[[i]] <- p
        }
        
        fig <- subplot(
          plot_list,
          nrows = 1,
          shareY = TRUE,
          titleX = TRUE,
          widths = rep(1 / length(plot_list), length(plot_list)),
          margin = 0.01
        ) |>
          layout(
            margin = list(l = 70, r = 30, b = 110, t = 20),
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor  = "rgba(0,0,0,0)"
          )
        
        return(apply_labor_plot_theme(fig))
    }
    else{
        # OPTIMIZED: get_group_data() instead of readRDS()
        data_key <- if (cost_category == "social") social_subcomponent else cost_category
        df <- get_group_data(data_key)
        if (is.null(df)) {
          showNotification("Data not available.", type = "error")
          return(NULL)
        }
        df <- df |>
          filter(
            wage %in% wage_filter,
            country==ns_variables$country_sel
          ) |>
          apply_wage_panels() |>
          select(country, min_max_total, value) |>
          mutate(
            type = ifelse(grepl("_min$", min_max_total), "Min", "Max")
          )
        df <- filter_missing_payroll(df, ns_variables$country_sel)
        df <- filter_missing_occ_risk(df, ns_variables$country_sel)
        df <- filter_missing_occ_risk(df, ns_variables$country_sel)
      
        
        if (nrow(df) == 0) {
          showNotification("No Data for this combination.", type = "error")
          return(NULL)
        }
        
        
        df_wide=df |>
          group_by(country) |>
          summarize(
            total_cost_min = min(value, na.rm = TRUE),
            total_cost_max = max(value, na.rm = TRUE)
          )|>
          arrange(total_cost_min) |>
          mutate(country = factor(country, levels = country))

        set_order_country(unique(as.character(df_wide$country)))
        
        df_mm <- df_wide |>
          tidyr::pivot_longer(
            cols = c(total_cost_min, total_cost_max),
            names_to = "Scenario",
            values_to = "value"
          ) |>
          mutate(
            Scenario = ifelse(Scenario == "total_cost_min", "Min", "Max"),
            Scenario = factor(Scenario, levels = c("Min", "Max")),
            country  = factor(country, levels = panel_order())
          )
        
        ns_variables$df_final=df_mm
        
        paises <- unique(df_mm$country)
        plot_list <- list()
        
        for (i in seq_along(paises)) {
          
          pais <- paises[i]
          data_pais <- df_mm |> filter(country == pais)
          
          p <- plot_ly(
            data = data_pais,
            x = ~Scenario,
            y = ~value,
            type = "bar",
            color = ~Scenario,
            colors = c("Min" = "#00C1FF", "Max" = "#002244"),
            showlegend = FALSE,
            hovertemplate = ns_hovertemplate
          ) |>
            layout(
              barmode = "stack",   
              
              paper_bgcolor = "rgba(0,0,0,0)",
              plot_bgcolor  = "rgba(0,0,0,0)",
              
              xaxis = list(
                title = pais,
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE,
                tickangle = 90
              ),
              
              yaxis = list(
                title = ifelse(i == 1, y_axis_title, ""),
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE
              )
            )
          
          plot_list[[i]] <- p
        }
        
        fig <- subplot(
          plot_list,
          nrows = 1,
          shareY = TRUE,
          titleX = TRUE,
          widths = rep(1 / length(plot_list), length(plot_list)),
          margin = 0.01
        ) |>
          layout(
            margin = list(l = 70, r = 30, b = 110, t = 20),
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor  = "rgba(0,0,0,0)"
          )
        
        return(apply_labor_plot_theme(fig))
    }
    }
    
    if (breakdown_type == "total" &&
        cost_category %in% c("bonuses_and_benefits", "payroll_taxes", "social") &&
        length(ns_variables$country_sel) > 1) {
      
      if ("All" %in%  ns_variables$country_sel) {
        showNotification("Please select only countries.", type = "error")
        return(NULL)
      }
        # OPTIMIZED: get_group_data() instead of readRDS()
        data_key <- if (cost_category == "social") social_subcomponent else cost_category
        df <- get_group_data(data_key)
        if (is.null(df)) {
          showNotification("Data not available.", type = "error")
          return(NULL)
        }
        df <- df |>
          filter(
            wage %in% wage_filter,
            country %in% ns_variables$country_sel
          ) |>
          apply_wage_panels() |>
          select(country, min_max_total, value) |>
          mutate(
            type = ifelse(grepl("_min$", min_max_total), "Min", "Max")
          )
        df <- filter_missing_payroll(df, ns_variables$country_sel)
          
        
        if (nrow(df) == 0) {
          showNotification("No Data for this combination.", type = "error")
          return(NULL)
        }
        
        
        df_wide=df |>
          group_by(country) |>
          summarize(
            total_cost_min = min(value, na.rm = TRUE),
            total_cost_max = max(value, na.rm = TRUE)
          )|>
          arrange(total_cost_min) |>
          mutate(country = factor(country, levels = country))

        set_order_country(unique(as.character(df_wide$country)))
        
        df_mm <- df_wide |>
          tidyr::pivot_longer(
            cols = c(total_cost_min, total_cost_max),
            names_to = "Scenario",
            values_to = "value"
          ) |>
          mutate(
            Scenario = ifelse(Scenario == "total_cost_min", "Min", "Max"),
            Scenario = factor(Scenario, levels = c("Min", "Max")),
            country  = factor(country, levels = panel_order())
          )
        
        ns_variables$df_final=df_mm
        
        paises <- unique(df_mm$country)
        plot_list <- list()
        
        for (i in seq_along(paises)) {
          
          pais <- paises[i]
          data_pais <- df_mm |> filter(country == pais)
          
          p <- plot_ly(
            data = data_pais,
            x = ~Scenario,
            y = ~value,
            type = "bar",
            color = ~Scenario,
            colors = c("Min" = "#00C1FF", "Max" = "#002244"),
            showlegend = FALSE,
            hovertemplate = ns_hovertemplate
          ) |>
            layout(
              barmode = "stack",   
              
              paper_bgcolor = "rgba(0,0,0,0)",
              plot_bgcolor  = "rgba(0,0,0,0)",
              
              xaxis = list(
                title = pais,
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE,
                tickangle = 90
              ),
              
              yaxis = list(
                title = ifelse(i == 1, y_axis_title, ""),
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE
              )
            )
          
          plot_list[[i]] <- p
        }
        
        fig <- subplot(
          plot_list,
          nrows = 1,
          shareY = TRUE,
          titleX = TRUE,
          widths = rep(1 / length(plot_list), length(plot_list)),
          margin = 0.01
        ) |>
          layout(
            margin = list(l = 70, r = 30, b = 110, t = 20),
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor  = "rgba(0,0,0,0)"
          )
        
        return(apply_labor_plot_theme(fig))
    }
    
    # ---- bonuses and benefits and Components ----
    
    if ((cost_category=="bonuses_and_benefits") & breakdown_type == "component" & bonus_component == "all_bonuses") {
      # OPTIMIZED: get_component_data() instead of readRDS()
      df <- get_component_data("bonuses_and_benefits")
      if (is.null(df)) {
        showNotification("Data not available.", type = "error")
        return(NULL)
      }
      df_long <- df  |>
        filter(
          wage %in% wage_filter
        ) |>
        apply_wage_panels()
      
      if ("component" %in% names(df_long)) {
        df_long <- df_long |>
          mutate(
            payer = dplyr::case_when(
              component == "ab" ~ "Annual and other periodic bonuses",
              component == "pl" ~ "Paid Leave",
              component == "up" ~ "Unemployment Protection",
              component == "ob" ~ "Other bonuses",
              TRUE ~ NA_character_
            )
          )
      } else {
        df_long <- df_long |>
          mutate(
            payer = dplyr::case_when(
              grepl("annual_bonuses", min_max_component) ~ "Annual and other periodic bonuses",
              grepl("paid_leave", min_max_component) ~ "Paid Leave",
              grepl("unemployment_protection", min_max_component) ~ "Unemployment Protection",
              grepl("other_bonuses", min_max_component) ~ "Other bonuses",
              TRUE ~ NA_character_
            )
          )
      }

      df_long <- df_long |>
        select(country, wage, min_max_component, value, payer) |>
        mutate(
          group = ifelse(grepl("_min$", min_max_component), "Min", "Max"),
          group = factor(group, levels = c("Min", "Max"))
        )
      
      df_long <- df_long |>
        left_join(
          bonus_hover_lookup,
          by = c("country", "wage", "group" = "group", "payer" = "Type")
        )
      
      if (length(ns_variables$country_sel) > 1) {
        if ("All" %in%  ns_variables$country_sel) {
          showNotification("Please select only countries.", type = "error")
          return(NULL)
        }
        df_long <- df_long |> filter(country %in% ns_variables$country_sel)
      } else if (length(ns_variables$country_sel) == 1 && ns_variables$country_sel != "All") {
        df_long <- df_long |> filter(country == ns_variables$country_sel)
      } else {
        ns_variables$countries=c("All",unique(df$country))
      }
      
      country_levels <- panel_order()
      if (is.null(country_levels) || length(country_levels) == 0) {
        country_levels <- unique(df_long$country)
      }
      df_long <- df_long |>
        mutate(country = factor(country, levels = country_levels)) |> 
        arrange(country)
      
      if (nrow(df_long) == 0) {
        showNotification("No Data for this combination.", type = "error")
        return(NULL)
      }
      
      df <- df_long
      df$Type <- factor(df$payer, levels = bonus_stack_order)
      df$Scenario <- factor(df$group, levels = c("Min", "Max"))
      
      paises <- unique(df$country)
      plot_list <- list()
      
      for (i in seq_along(paises)) {
        pais <- paises[i]
        data_pais <- df |> filter(country == pais)
        
        if (nrow(data_pais) == 0) next
        
        show_legend <- i == 1
        
        p <- build_bonus_subplot(
          data_pais,
          show_legend = show_legend,
          y_axis_title = ifelse(i == 1, y_axis_title, ""),
          xaxis_title = pais
        )
        
        plot_list[[i]] <- p
      }
      
      n_plots <- length(plot_list)
      fig <- subplot(
        plot_list,
        nrows = 1,
        shareY = TRUE,
        titleX = TRUE,
        widths = rep(1 / n_plots, n_plots), 
        margin = 0.01
      ) |>
        layout(
          title = "",
          
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.25
          ),
          
          margin = list(
            l = 70,
            r = 30,
            b = 110,
            t = 20
          )
        )
      return(apply_labor_plot_theme(fig))
    }
    if (breakdown_type == "component" & component_filter!="all_component" & length(ns_variables$country_sel)==1) {
      
      scenario_colors <- c("Min" = "#00C1FF", "Max" = "#002244")
      if (component_filter == "bonuses_and_benefits" && bonus_component != "all_bonuses") {
        bonus_color <- switch(
          bonus_component,
          ab = bonus_palette[["Annual and other periodic bonuses"]],
          pl = bonus_palette[["Paid Leave"]],
          up = bonus_palette[["Unemployment Protection"]],
          ob = bonus_palette[["Other bonuses"]],
          "#002244"
        )
        scenario_colors <- c("Min" = bonus_color, "Max" = bonus_color)
      }

      if(ns_variables$country_sel=="All"){
        if(component_filter=="bonuses_and_benefits" & bonus_component!="all_bonuses"){
          # OPTIMIZED: get_component_data() instead of readRDS()
          df <- get_component_data("bonuses_and_benefits")
          if (is.null(df)) {
            showNotification("Data not available.", type = "error")
            return(NULL)
          }
          df <- df |>
            filter(
              wage %in% wage_filter
            )
          df <- filter_bonus_component(df, bonus_component) |>
            apply_wage_panels() |>
            select(country, min_max_component, value) |>
            mutate(
              type = ifelse(grepl("_min$", min_max_component), "Min", "Max")
            )
          ns_variables$countries=c("All",unique(df$country))
        } else if(component_filter=="social"){
          # OPTIMIZED: get_group_data() instead of readRDS()
          df <- get_group_data(social_subcomponent)
          if (is.null(df)) {
            showNotification("Data not available.", type = "error")
            return(NULL)
          }
          df <- df |>
            filter(
              wage %in% wage_filter
            ) |>
            apply_wage_panels() |>
            select(country, min_max_total, value) |>
            mutate(
              type = ifelse(grepl("_min$", min_max_total), "Min", "Max")
            )
          ns_variables$countries=c("All",unique(df$country))
        } else if (!is.null(input$component_type) &&
                   length(input$component_type) > 0 &&
                   identical(input$component_type, "Total")){
            # OPTIMIZED: get_group_data() instead of readRDS()
            df <- get_group_data(component_filter)
            if (is.null(df)) {
              showNotification("Data not available.", type = "error")
              return(NULL)
            }
            df <- df |>
              filter(
                wage %in% wage_filter
              ) |>
              apply_wage_panels() |>
              select(country, min_max_total, value) |>
              mutate(
                type = ifelse(grepl("_min$", min_max_total), "Min", "Max")
              )
            ns_variables$countries=c("All",unique(df$country))
        }
        
        if (is.null(df) || nrow(df) == 0) {
          showNotification("No Data for this combination.", type = "error")
          return(NULL)
        }
        
        df_wide=df |>
          group_by(country) |>
          summarize(
            total_cost_min = min(value, na.rm = TRUE),
            total_cost_max = max(value, na.rm = TRUE)
          )|>
          arrange(total_cost_min) |>
          mutate(country = factor(country, levels = country))
        
        df_mm <- df_wide |>
          tidyr::pivot_longer(
            cols = c(total_cost_min, total_cost_max),
            names_to = "Scenario",
            values_to = "value"
          ) |>
          mutate(
            Scenario = ifelse(Scenario == "total_cost_min", "Min", "Max"),
            Scenario = factor(Scenario, levels = c("Min", "Max")),
            country  = factor(country, levels = panel_order())
          )
        
        ns_variables$df_final=df_mm
        
        paises <- unique(df_mm$country)
        plot_list <- list()
        
        for (i in seq_along(paises)) {
          
          pais <- paises[i]
          data_pais <- df_mm |> filter(country == pais)
          
          p <- plot_ly(
            data = data_pais,
            x = ~Scenario,
            y = ~value,
            type = "bar",
            color = ~Scenario,
            colors = scenario_colors,
            showlegend = FALSE,
            hovertemplate = ns_hovertemplate
          ) |>
            layout(
              barmode = "stack",   
              
              paper_bgcolor = "rgba(0,0,0,0)",
              plot_bgcolor  = "rgba(0,0,0,0)",
              
              xaxis = list(
                title = pais,
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE,
                tickangle = 90
              ),
              
              yaxis = list(
                title = ifelse(i == 1, y_axis_title, ""),
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE
              )
            )
          
          plot_list[[i]] <- p
        }
        
        fig <- subplot(
          plot_list,
          nrows = 1,
          shareY = TRUE,
          titleX = TRUE,
          widths = rep(1 / length(plot_list), length(plot_list)),
          margin = 0.01
        ) |>
          layout(
            margin = list(l = 70, r = 30, b = 110, t = 20),
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor  = "rgba(0,0,0,0)"
          )
        
        return(apply_labor_plot_theme(fig))
      }
      else{
        if(component_filter=="bonuses_and_benefits" & bonus_component!="all_bonuses"){
          # OPTIMIZED
          df <- get_component_data("bonuses_and_benefits")
          if (is.null(df)) {
            showNotification("Data not available.", type = "error")
            return(NULL)
          }
          df <- df |>
            filter(
              wage %in% wage_filter,
              country==ns_variables$country_sel
            )
          df <- filter_bonus_component(df, bonus_component) |>
            apply_wage_panels() |>
            select(country, min_max_component, value) |>
            mutate(
              type = ifelse(grepl("_min$", min_max_component), "Min", "Max")
            )
        } else if(component_filter=="social"){
          # OPTIMIZED
          df <- get_group_data(social_subcomponent)
          if (is.null(df)) {
            showNotification("Data not available.", type = "error")
            return(NULL)
          }
          df <- df |>
            filter(
              wage %in% wage_filter,
              country==ns_variables$country_sel
            ) |>
            apply_wage_panels() |>
            select(country, min_max_total, value) |>
            mutate(
              type = ifelse(grepl("_min$", min_max_total), "Min", "Max")
            )
        } else if (!is.null(input$component_type) &&
                   length(input$component_type) > 0 &&
                   identical(input$component_type, "Total")){
            # OPTIMIZED
            df <- get_group_data(component_filter)
            if (is.null(df)) {
              showNotification("Data not available.", type = "error")
              return(NULL)
            }
            df <- df |>
              filter(
                wage %in% wage_filter,
                country==ns_variables$country_sel
              ) |>
              apply_wage_panels() |>
              select(country, min_max_total, value) |>
              mutate(
                type = ifelse(grepl("_min$", min_max_total), "Min", "Max")
              )
        }
        if (is.null(df) || nrow(df) == 0) {
          showNotification("No Data for this combination.", type = "error")
          return(NULL)
        }
        
        
        df_wide=df |>
          group_by(country) |>
          summarize(
            total_cost_min = min(value, na.rm = TRUE),
            total_cost_max = max(value, na.rm = TRUE)
          )|>
          arrange(total_cost_min) |>
          mutate(country = factor(country, levels = country))
        
        df_mm <- df_wide |>
          tidyr::pivot_longer(
            cols = c(total_cost_min, total_cost_max),
            names_to = "Scenario",
            values_to = "value"
          ) |>
          mutate(
            Scenario = ifelse(Scenario == "total_cost_min", "Min", "Max"),
            Scenario = factor(Scenario, levels = c("Min", "Max")),
            country  = factor(country, levels = panel_order())
          )
        
        ns_variables$df_final=df_mm
        
        paises <- unique(df_mm$country)
        plot_list <- list()
        
        for (i in seq_along(paises)) {
          
          pais <- paises[i]
          data_pais <- df_mm |> filter(country == pais)
          
          p <- plot_ly(
            data = data_pais,
            x = ~Scenario,
            y = ~value,
            type = "bar",
            color = ~Scenario,
            colors = scenario_colors,
            showlegend = FALSE,
            hovertemplate = ns_hovertemplate
          ) |>
            layout(
              barmode = "stack",   
              
              paper_bgcolor = "rgba(0,0,0,0)",
              plot_bgcolor  = "rgba(0,0,0,0)",
              
              xaxis = list(
                title = pais,
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE,
                tickangle = 90
              ),
              
              yaxis = list(
                title = ifelse(i == 1, y_axis_title, ""),
                showgrid = FALSE,
                zeroline = FALSE,
                showline = FALSE
              )
            )
          
          plot_list[[i]] <- p
        }
        
        fig <- subplot(
          plot_list,
          nrows = 1,
          shareY = TRUE,
          titleX = TRUE,
          widths = rep(1 / length(plot_list), length(plot_list)),
          margin = 0.01
        ) |>
          layout(
            margin = list(l = 70, r = 30, b = 110, t = 20),
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor  = "rgba(0,0,0,0)"
          )
        
        return(apply_labor_plot_theme(fig))
      }
      
    }
    if (breakdown_type == "component" & component_filter!="all_component" & length(ns_variables$country_sel)>1) {
      
      if ("All" %in%  ns_variables$country_sel) {
        showNotification("Please select only countries.", type = "error")
        return(NULL)
      }
      scenario_colors <- c("Min" = "#00C1FF", "Max" = "#002244")
      if (component_filter == "bonuses_and_benefits" && bonus_component != "all_bonuses") {
        bonus_color <- switch(
          bonus_component,
          ab = bonus_palette[["Annual and other periodic bonuses"]],
          pl = bonus_palette[["Paid Leave"]],
          up = bonus_palette[["Unemployment Protection"]],
          ob = bonus_palette[["Other bonuses"]],
          "#002244"
        )
        scenario_colors <- c("Min" = bonus_color, "Max" = bonus_color)
      }
      if(component_filter=="bonuses_and_benefits" & bonus_component!="all_bonuses"){
        # OPTIMIZED
        df <- get_component_data("bonuses_and_benefits")
        if (is.null(df)) {
          showNotification("Data not available.", type = "error")
          return(NULL)
        }
        df <- df |>
          filter(
            wage %in% wage_filter,
            country %in% ns_variables$country_sel 
          )
        df <- filter_bonus_component(df, bonus_component) |>
          apply_wage_panels() |>
          select(country, min_max_component, value) |>
          mutate(
            type = ifelse(grepl("_min$", min_max_component), "Min", "Max")
          )
      } else if(component_filter=="social"){
        # OPTIMIZED
        df <- get_component_data(social_subcomponent)
        if (is.null(df)) {
          showNotification("Data not available.", type = "error")
          return(NULL)
        }
        df <- df |>
          filter(
            wage %in% wage_filter,
            country %in% ns_variables$country_sel 
          )
        if ("component" %in% names(df)) {
          df <- df |> filter(component == bonus_component)
        }
        df <- df |>
          apply_wage_panels() |>
          select(country, min_max_component, value) |>
          mutate(
            type = ifelse(grepl("_min$", min_max_component), "Min", "Max")
          )
      } else if (!is.null(input$component_type) &&
                 length(input$component_type) > 0 &&
                 identical(input$component_type, "Total")){
          # OPTIMIZED
          df <- get_group_data(component_filter)
          if (is.null(df)) {
            showNotification("Data not available.", type = "error")
            return(NULL)
          }
          df <- df |>
            filter(
              wage %in% wage_filter,
              country %in% ns_variables$country_sel
            ) |>
            apply_wage_panels() |>
            select(country, min_max_total, value) |>
            mutate(
              type = ifelse(grepl("_min$", min_max_total), "Min", "Max")
            )
      }
      if (is.null(df) || nrow(df) == 0) {
        showNotification("No Data for this combination.", type = "error")
        return(NULL)
      }
      
      
      df_wide=df |>
        group_by(country) |>
        summarize(
          total_cost_min = min(value, na.rm = TRUE),
          total_cost_max = max(value, na.rm = TRUE)
        )|>
        arrange(total_cost_min) |>
        mutate(country = factor(country, levels = country))
      
      df_mm <- df_wide |>
        tidyr::pivot_longer(
          cols = c(total_cost_min, total_cost_max),
          names_to = "Scenario",
          values_to = "value"
        ) |>
        mutate(
          Scenario = ifelse(Scenario == "total_cost_min", "Min", "Max"),
          Scenario = factor(Scenario, levels = c("Min", "Max")),
          country  = factor(country, levels = panel_order())
        )
      
      ns_variables$df_final=df_mm
      
      paises <- unique(df_mm$country)
      plot_list <- list()
      
      for (i in seq_along(paises)) {
        
        pais <- paises[i]
        data_pais <- df_mm |> filter(country == pais)
        
        p <- plot_ly(
          data = data_pais,
          x = ~Scenario,
          y = ~value,
          type = "bar",
          color = ~Scenario,
          colors = scenario_colors,
          showlegend = FALSE,
          hovertemplate = ns_hovertemplate
        ) |>
          layout(
            barmode = "stack",   
            
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor  = "rgba(0,0,0,0)",
            
            xaxis = list(
              title = pais,
              showgrid = FALSE,
              zeroline = FALSE,
              showline = FALSE,
              tickangle = 90
            ),
            
            yaxis = list(
              title = ifelse(i == 1, y_axis_title, ""),
              showgrid = FALSE,
              zeroline = FALSE,
              showline = FALSE
            )
          )
        
        plot_list[[i]] <- p
      }
      
      fig <- subplot(
        plot_list,
        nrows = 1,
        shareY = TRUE,
        titleX = TRUE,
        widths = rep(1 / length(plot_list), length(plot_list)),
        margin = 0.01
      ) |>
        layout(
          margin = list(l = 70, r = 30, b = 110, t = 20),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)"
        )
      
      return(apply_labor_plot_theme(fig))
    }
    
  })
