# =============================================================================
# init.R - Constants, resolve_sources, tenure_enabled, ns_variables, data_sources observer, and reactiveVal declarations.
# Sourced into non_salary_server_core's environment so all bindings live in that scope.
# =============================================================================



  if (is.null(data_sources)) {
    data_sources <- LABOR_DATA_SOURCES_ACROSS
  }
  resolve_sources <- function() {
    if (is.function(data_sources)) {
      data_sources()
    }
    else {
      data_sources
    }
  }
  tenure_enabled <- reactive({
    isTRUE(enable_tenure) && isTRUE(input$show_by_tenure)
  })
  
  # ============================================================================
  # DATOS: Ahora vienen de global.R (pre-cargados)
  # ============================================================================
  
  # Constantes de global.R
  wage_levels <- WAGE_LEVELS
  wage_labels <- WAGE_LABELS
  wage_choices <- WAGE_CHOICES
  plotly_font_family <- PLOTLY_FONT_FAMILY
  component_palette <- COMPONENT_PALETTE
  component_stack_order <- COMPONENT_STACK_ORDER
  component_legend_order <- COMPONENT_LEGEND_ORDER
  bonus_palette <- BONUS_PALETTE
  bonus_stack_order <- BONUS_STACK_ORDER
  country_name_map <- COUNTRY_NAME_MAP
  
  build_bonus_hover_lookup <- function(bonus_hover_source) {
    required_bonus_columns <- c(
      "min_max_total",
      "annual_or_periodic_bonuses",
      "paid_leave",
      "unemployment_protection",
      "other_bonuses",
      "legislation"
    )
    if (is.null(bonus_hover_source) ||
        !all(required_bonus_columns %in% names(bonus_hover_source))) {
      return(data.frame(
        country = character(0),
        wage = character(0),
        group = character(0),
        Type = character(0),
        hover_text = character(0),
        stringsAsFactors = FALSE
      ))
    }
    bonus_hover_source |>
      mutate(
        group = ifelse(grepl("_min$", min_max_total), "Min", "Max")
      ) |>
      select(
        country,
        wage,
        group,
        legislation,
        annual_or_periodic_bonuses,
        paid_leave,
        unemployment_protection,
        other_bonuses
      ) |>
      tidyr::pivot_longer(
        cols = c(
          annual_or_periodic_bonuses,
          paid_leave,
          unemployment_protection,
          other_bonuses
        ),
        names_to = "Type",
        values_to = "detail_text"
      ) |>
      mutate(
        Type = dplyr::recode(
          Type,
          annual_or_periodic_bonuses = "Annual and other periodic bonuses",
          paid_leave = "Paid Leave",
          unemployment_protection = "Unemployment Protection",
          other_bonuses = "Other bonuses"
        ),
        detail_text = dplyr::coalesce(detail_text, ""),
        legislation = dplyr::coalesce(legislation, ""),
        hover_text = dplyr::case_when(
          detail_text != "" & legislation != "" ~ paste0(detail_text, "<br><br>", legislation),
          detail_text != "" ~ detail_text,
          legislation != "" ~ legislation,
          TRUE ~ ""
        )
      ) |>
      select(country, wage, group, Type, hover_text)
  }
  
  # non_salary variables
  initial_countries <- if (is.function(data_sources)) character(0) else data_sources$countries
  ns_variables <- reactiveValues(
    order_country = NULL,
    country_sel = "All",
    countries = initial_countries,
    df_final = NULL,
    df_final_tabla = NULL,
    missing_payroll_countries = NULL,
    missing_occ_risk_countries = NULL
  )

  if (is.function(data_sources)) {
    observeEvent(data_sources(), {
      sources <- resolve_sources()
      if (!is.null(sources$countries)) {
        ns_variables$countries <- sources$countries
      }
      if (isTRUE(enable_tenure) && identical(input$compare_mode, "wage")) {
        choices <- ns_variables$countries[ns_variables$countries != "All"]
        preferred <- if ("ARG" %in% choices) "ARG" else choices[1]
        if (!is.null(preferred) &&
            preferred != "" &&
            (is.null(ns_variables$country_sel) || ns_variables$country_sel == "All")) {
          ns_variables$country_sel <- preferred
          last_country_selection(preferred)
          last_single_country(preferred)
          updateSelectizeInput(session, ns("country_selection_user"), selected = preferred)
        }
      }
    }, ignoreInit = FALSE)
  }
  
  # ---- Selection Groups: Button results ----
  selected_cost_category <- reactiveVal("all") # First Filter
  selected_breakdown_type <- reactiveVal("total") # Total, by Payer, By Component   
  selected_wage_level <- reactiveVal("1sm") # 1 MW / 2 MW / 5 MW / 10 MW / 15 MW
  selected_component_filter <- reactiveVal("all_component")
  selected_bonus_component <- reactiveVal("all_bonuses")
  selected_social_subcomponent <- reactiveVal("pensions")
  option1_selected <- reactiveVal(FALSE)
  table_visible <- reactiveVal(FALSE)
  last_country_selection <- reactiveVal("All")
  last_wage_selection <- reactiveVal("1sm")
  last_single_country <- reactiveVal(NULL)
  last_compare_mode <- reactiveVal("country")
  selected_table_country <- reactiveVal(NULL)
  option1_selected(TRUE)
