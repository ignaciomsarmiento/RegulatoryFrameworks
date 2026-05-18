# =============================================================================
# helpers.R - Utility functions and session-scoped caches. 
# Sourced into non_salary_server_core's environment so <- NULL initializers and <<- mutations share that scope.# =============================================================================

  reset_across_defaults <- function() {
    ns_variables$country_sel <- "All"
    ns_variables$order_country <- NULL
    last_country_selection("All")
    last_wage_selection("1sm")
    last_single_country(NULL)
    selected_cost_category("all")
    selected_breakdown_type("total")
    selected_wage_level("1sm")
    selected_component_filter("all_component")
    selected_bonus_component("all_bonuses")
    selected_social_subcomponent("pensions")
    option1_selected(TRUE)
    updateSelectizeInput(session, ns("mw_selection"), selected = "1sm")
    updateSelectizeInput(session, ns("country_selection_user"), selected = "All")
    shinyjs::runjs(sprintf("$('#%s').click();", ns("all")))
  }
  
  # Helper functions - use those from global.R
  # country_display_name, format_wage_label, format_wage_phrase, format_country_phrase
  # are already defined in global.R
  
  safe_value <- function(value, fallback) {
    if (is.null(value) || length(value) == 0) {
      return(fallback)
    }
    value
  }

  regex_escape <- function(text) {
    gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", text, perl = TRUE)
  }

  country_flag_src <- function(country_code) {
    if (is.null(country_code) || length(country_code) == 0 ||
        is.na(country_code[1]) || !nzchar(as.character(country_code[1]))) {
      return(NULL)
    }
    code <- as.character(country_code[1])
    flag_code <- COUNTRY_FLAG_MAP[[code]]
    if (is.null(flag_code) || is.na(flag_code) || !nzchar(flag_code)) {
      flag_code <- tolower(substr(code, 1, 2))
    }
    sprintf("https://flagcdn.com/24x18/%s.png", flag_code)
  }

  country_title_tag <- function(country_label, country_code) {
    label_text <- if (is.null(country_label) || length(country_label) == 0 ||
                      is.na(country_label[1]) || !nzchar(as.character(country_label[1]))) {
      if (is.null(country_code) || length(country_code) == 0 ||
          is.na(country_code[1]) || !nzchar(as.character(country_code[1]))) {
        ""
      } else {
        country_display_name(country_code)
      }
    } else {
      as.character(country_label[1])
    }
    label <- toupper(label_text)
    flag_src <- country_flag_src(country_code)
    title_children <- list()
    if (!is.null(flag_src)) {
      title_children <- append(title_children, list(tags$img(
        src = flag_src,
        class = "labor-reg-country-title-flag",
        alt = label_text
      )))
    }
    title_children <- append(title_children, list(tags$span(label)))
    do.call(tags$div, c(list(class = "labor-reg-country-title"), title_children))
  }

  decorate_table_partial_country_title <- function(fragment, country_name, country_code) {
    if (is.null(fragment) || length(fragment) == 0 || !nzchar(fragment[1]) ||
        is.null(country_name) || length(country_name) == 0 ||
        is.na(country_name[1]) || !nzchar(as.character(country_name[1]))) {
      return(fragment)
    }
    title_html <- as.character(country_title_tag(country_name[1], country_code))
    title_match <- regexpr(
      '(?is)<div class="labor-reg-country-title">[\\s\\S]*?</div>',
      fragment[1],
      perl = TRUE
    )
    if (title_match[1] < 0) {
      return(fragment[1])
    }
    match_length <- attr(title_match, "match.length")
    title_start <- title_match[1]
    title_end <- title_start + match_length - 1
    prefix <- if (title_start > 1) substr(fragment[1], 1, title_start - 1) else ""
    suffix <- if (title_end < nchar(fragment[1])) {
      substr(fragment[1], title_end + 1, nchar(fragment[1]))
    } else {
      ""
    }
    paste0(prefix, title_html, suffix)
  }

  # Session-scoped cache; intentional super-assignment to function-level state.
  table_partial_manifest_cache <- NULL

  get_table_partial_manifest <- function() {
    if (!is.null(table_partial_manifest_cache)) {
      return(table_partial_manifest_cache)
    }
    manifest_path <- "data/non_salary/tables/table_partials_manifest.csv"
    if (!file.exists(manifest_path)) {
      table_partial_manifest_cache <<- data.frame(
        sheet_name = character(0),
        table_id = character(0),
        output_file = character(0),
        stringsAsFactors = FALSE
      )
      return(table_partial_manifest_cache)
    }
    manifest <- read.csv(manifest_path, stringsAsFactors = FALSE, check.names = FALSE)
    required <- c("sheet_name", "table_id", "output_file")
    if (!all(required %in% names(manifest))) {
      warning("Table partial manifest is missing required columns.")
      manifest <- data.frame(
        sheet_name = character(0),
        table_id = character(0),
        output_file = character(0),
        stringsAsFactors = FALSE
      )
    }
    manifest$sheet_name <- trimws(manifest$sheet_name)
    manifest$table_id <- trimws(manifest$table_id)
    manifest$output_file <- trimws(manifest$output_file)
    table_partial_manifest_cache <<- manifest
    table_partial_manifest_cache
  }

  table_partial_id_for_sheet <- function(sheet_name) {
    manifest <- get_table_partial_manifest()
    if (is.null(sheet_name) || length(sheet_name) == 0 || nrow(manifest) == 0) {
      return(NULL)
    }
    match_idx <- match(sheet_name, manifest$sheet_name)
    if (is.na(match_idx)) {
      return(NULL)
    }
    table_id <- manifest$table_id[[match_idx]]
    if (!nzchar(table_id)) {
      return(NULL)
    }
    table_id
  }

  table_partial_path <- function(table_id) {
    if (is.null(table_id) || length(table_id) == 0 || !nzchar(table_id)) {
      return(NULL)
    }
    file.path("data/non_salary/tables/html", paste0(table_id, ".html"))
  }

  table_partial_cache <- new.env(parent = emptyenv())

  get_table_partial_html <- function(table_id) {
    if (exists(table_id, envir = table_partial_cache, inherits = FALSE)) {
      return(get(table_id, envir = table_partial_cache, inherits = FALSE))
    }
    path <- table_partial_path(table_id)
    if (is.null(path) || !file.exists(path)) {
      assign(table_id, NULL, envir = table_partial_cache)
      return(NULL)
    }
    html <- paste(readLines(path, warn = FALSE), collapse = "\n")
    html <- gsub("United States", "US4", html, fixed = TRUE)
    assign(table_id, html, envir = table_partial_cache)
    html
  }

  table_partial_country_names <- function(table_id) {
    partial <- get_table_partial_html(table_id)
    if (is.null(partial) || !nzchar(partial)) {
      return(character(0))
    }
    matches <- regmatches(
      partial,
      gregexpr('data-country="[^"]+"', partial, perl = TRUE)
    )[[1]]
    if (length(matches) == 0 || identical(matches, character(0))) {
      return(character(0))
    }
    countries <- gsub('^data-country="|"$', "", matches)
    unique(countries)
  }

  table_partial_fragment <- function(table_id, country_name, country_code = NULL) {
    partial <- get_table_partial_html(table_id)
    if (is.null(partial) || !nzchar(partial) ||
        is.null(country_name) || length(country_name) == 0) {
      return(NULL)
    }
    country_attr <- as.character(htmltools::htmlEscape(country_name[1], attribute = TRUE))
    pattern <- sprintf(
      '(?is)<section class="labor-reg-country-fragment" data-country="%s">[\\s\\S]*?</section>',
      regex_escape(country_attr)
    )
    fragment <- regmatches(partial, regexpr(pattern, partial, perl = TRUE))
    if (length(fragment) == 0 || identical(fragment, character(0)) || !nzchar(fragment[1])) {
      return(NULL)
    }
    HTML(decorate_table_partial_country_title(fragment[1], country_name[1], country_code))
  }

  table_country_codes <- function(sheet_name = "TL All P") {
    partial_id <- table_partial_id_for_sheet(sheet_name)
    table_countries <- if (!is.null(partial_id)) {
      table_partial_country_names(partial_id)
    } else {
      character(0)
    }
    if (length(table_countries) == 0) {
      data <- get_excel_table(sheet_name)
      if (is.null(data) || !is.data.frame(data) || !"Country" %in% names(data)) {
        return(character(0))
      }
      table_countries <- unique(data$Country)
      table_countries <- table_countries[!is.na(table_countries) & table_countries != ""]
    }
    available_codes <- ns_variables$countries
    available_codes <- available_codes[!is.na(available_codes) & available_codes != "All"]
    codes <- available_codes[vapply(
      available_codes,
      function(code) country_display_name(code) %in% table_countries,
      logical(1)
    )]
    codes
  }

  current_table_country <- function(sheet_name = "TL All P") {
    codes <- table_country_codes(sheet_name)
    if (length(codes) == 0) {
      return(NULL)
    }
    selected <- selected_table_country()
    if (!is.null(selected) && length(selected) == 1 && selected %in% codes) {
      return(selected)
    }
    if ("ARG" %in% codes) {
      return("ARG")
    }
    codes[1]
  }

  build_country_flag_buttons <- function(countries, active_country, input_name) {
    countries <- countries[!is.na(countries) & countries != "All"]
    if (length(countries) == 0 || is.null(active_country) || length(active_country) == 0) {
      return(NULL)
    }

    buttons <- lapply(countries, function(code) {
      flag_code <- COUNTRY_FLAG_MAP[[code]]
      if (is.null(flag_code) || is.na(flag_code)) {
        flag_code <- tolower(substr(code, 1, 2))
      }
      btn_class <- "labor-country-button"
      if (identical(code, active_country)) {
        btn_class <- paste(btn_class, "active")
      }
      tags$button(
        type = "button",
        class = btn_class,
        title = country_display_name(code),
        onclick = sprintf(
          "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
          ns(input_name),
          code
        ),
        tags$img(
          src = sprintf("https://flagcdn.com/24x18/%s.png", flag_code),
          class = "labor-flag",
          alt = country_display_name(code)
        ),
        tags$span(class = "labor-country-label", toupper(code)),
        tags$span(class = "labor-country-hover", country_display_name(code))
      )
    })

    tags$div(
      class = "labor-country-panel",
      tags$div(class = "labor-country-buttons", buttons)
    )
  }

  apply_tenure_filter <- function(df) {
    if (!tenure_enabled() || is.null(df) || !"tenure" %in% names(df)) {
      return(df)
    }
    tenure_value <- input$tenure_selection
    if (is.null(tenure_value) || length(tenure_value) == 0) {
      return(df)
    }
    df |> dplyr::filter(tenure == tenure_value)
  }

  exclude_health_countries <- function(df) {
    if (is.null(df) || nrow(df) == 0 || !"country" %in% names(df)) {
      return(df)
    }
    mode <- safe_value(input$compare_mode, "country")
    cost_category <- safe_value(selected_cost_category(), "all")
    social_subcomponent <- safe_value(selected_social_subcomponent(), "pensions")
    if (identical(mode, "country") && cost_category == "social" && social_subcomponent == "health") {
      df <- df |> dplyr::filter(!country %in% c("US4", "BRA", "ESP"))
    }
    df
  }

  normalize_hetero_key <- function(text) {
    if (is.null(text) || length(text) == 0) return("")
    tolower(gsub("\\s+", " ", trimws(text)))
  }

  # Session-scoped caches; intentional super-assignment to function-level state.
  hetero_manifest_cache <- NULL
  get_heterogeneity_manifest <- function() {
    if (!is.null(hetero_manifest_cache)) {
      return(hetero_manifest_cache)
    }

    het_manifest_path <- "data/non_salary/tables/table_partials_heterogeneity_manifest.csv"
    if (!file.exists(het_manifest_path)) {
      hetero_manifest_cache <<- data.frame(
        sheet_prefix = character(0),
        table_id = character(0),
        output_file = character(0),
        manifest_sheet_name = character(0),
        description = character(0),
        category_label = character(0),
        stringsAsFactors = FALSE
      )
      return(hetero_manifest_cache)
    }

    manifest <- read.csv(het_manifest_path, stringsAsFactors = FALSE, check.names = FALSE)
    required <- c("sheet_prefix", "table_id", "category_label")
    if (!all(required %in% names(manifest))) {
      warning("Heterogeneity table manifest is missing required columns.")
      manifest <- data.frame(
        sheet_prefix = character(0),
        table_id = character(0),
        output_file = character(0),
        manifest_sheet_name = character(0),
        description = character(0),
        category_label = character(0),
        stringsAsFactors = FALSE
      )
    }
    manifest$sheet_prefix <- as.character(manifest$sheet_prefix)
    manifest$table_id <- trimws(manifest$table_id)
    manifest$category_label <- trimws(manifest$category_label)
    hetero_manifest_cache <<- manifest
    hetero_manifest_cache
  }

  hetero_cache <- NULL
  get_heterogeneity_sections <- function() {
    if (!is.null(hetero_cache)) {
      return(hetero_cache)
    }

    het_manifest <- get_heterogeneity_manifest()
    if (nrow(het_manifest) == 0) {
      hetero_cache <<- list(sections = list(), countries = character(0))
      return(hetero_cache)
    }

    sections <- list()
    all_codes <- character(0)
    name_to_code <- setNames(names(COUNTRY_NAME_MAP), unname(COUNTRY_NAME_MAP))

    for (row_idx in seq_len(nrow(het_manifest))) {
      table_id <- het_manifest$table_id[[row_idx]]
      category <- het_manifest$category_label[[row_idx]]
      partial_html <- get_table_partial_html(table_id)
      if (is.null(partial_html) || !nzchar(partial_html)) next

      display_names <- table_partial_country_names(table_id)
      for (display_name in display_names) {
        code <- name_to_code[[display_name]]
        if (is.null(code) || is.na(code)) next
        pattern <- sprintf(
          '(?is)<section class="labor-reg-country-fragment" data-country="%s">[\\s\\S]*?</section>',
          regex_escape(htmltools::htmlEscape(display_name, attribute = TRUE))
        )
        fragment <- regmatches(partial_html, regexpr(pattern, partial_html, perl = TRUE))
        if (length(fragment) == 0 || !nzchar(fragment[1])) next
        decorated <- decorate_table_partial_country_title(fragment[1], display_name, code)
        sections[[length(sections) + 1]] <- list(
          sheet_name = paste(category, code, sep = " - "),
          country = code,
          category = category,
          category_key = normalize_hetero_key(category),
          table_html = as.character(decorated)
        )
        all_codes <- c(all_codes, code)
      }
    }

    hetero_cache <<- list(sections = sections, countries = sort(unique(all_codes)))
    hetero_cache
  }

  heterogeneity_sheet_suffix <- function(country_code) {
    if (identical(country_code, "ESP")) return("Spain")
    if (identical(country_code, "US4")) return("US")
    country_code
  }

  heterogeneity_workbook_path <- function(country_code) {
    if (country_code %in% c("ESP", "US4")) {
      return("data/non_salary/tables/excel/heterogeneity_spain_us.xlsx")
    }
    "data/non_salary/tables/excel/tables_heterogeneity.xlsx"
  }

  heterogeneity_sheet_name <- function(category, country_code) {
    manifest <- get_heterogeneity_manifest()
    if (nrow(manifest) == 0) return(NULL)
    category_key <- normalize_hetero_key(category)
    match_idx <- which(normalize_hetero_key(manifest$category_label) == category_key)
    if (length(match_idx) == 0) return(NULL)
    paste0(manifest$sheet_prefix[[match_idx[1]]], heterogeneity_sheet_suffix(country_code))
  }

  hetero_excel_cache <- new.env(parent = emptyenv())
  get_heterogeneity_excel_table <- function(category, country_code) {
    workbook_path <- heterogeneity_workbook_path(country_code)
    sheet_name <- heterogeneity_sheet_name(category, country_code)
    if (is.null(sheet_name) || is.null(workbook_path) || !file.exists(workbook_path)) {
      return(NULL)
    }

    cache_key <- paste(workbook_path, sheet_name, sep = "::")
    if (exists(cache_key, envir = hetero_excel_cache, inherits = FALSE)) {
      return(get(cache_key, envir = hetero_excel_cache, inherits = FALSE))
    }

    available_sheets <- readxl::excel_sheets(workbook_path)
    if (!sheet_name %in% available_sheets) {
      assign(cache_key, NULL, envir = hetero_excel_cache)
      return(NULL)
    }

    data <- as.data.frame(readxl::read_excel(workbook_path, sheet = sheet_name))
    assign(cache_key, data, envir = hetero_excel_cache)
    data
  }

  table_text_to_html <- function(value) {
    if (is.null(value) || length(value) == 0 || is.na(value[1])) {
      return(tags$span(class = "labor-reg-neutral", "-"))
    }
    text <- as.character(value[1])
    text <- gsub("\r\n|\r|\n", "\n", text)
    text <- gsub("\\n{3,}", "\n\n", text)
    text <- trimws(text)
    if (!nzchar(text)) {
      return(tags$span(class = "labor-reg-neutral", "-"))
    }
    text <- htmltools::htmlEscape(text)
    HTML(gsub("\n", "<br>", text, fixed = TRUE))
  }

  build_pension_country_table <- function(data, country_code) {
    if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
      return(NULL)
    }
    row <- data[1, , drop = FALSE]
    detail_columns <- setdiff(names(row), "Country")
    country_label <- if ("Country" %in% names(row)) {
      as.character(row$Country[1])
    } else {
      country_display_name(country_code)
    }

    accordion_items <- lapply(detail_columns, function(column_name) {
      header_label <- gsub("\r\n|\r|\n", " ", column_name)
      header_label <- gsub("\\s+", " ", trimws(header_label))
      tags$details(
        class = "labor-reg-accordion-item",
        open = "open",
        tags$summary(
          class = "labor-reg-accordion-header",
          tags$span(class = "labor-reg-accordion-title", header_label)
        ),
        tags$div(
          class = "labor-reg-accordion-body",
          table_text_to_html(row[[column_name]][1])
        )
      )
    })

    tags$div(
      class = "labor-reg-table",
      country_title_tag(country_label, country_code),
      tags$div(class = "labor-reg-accordion", accordion_items)
    )
  }

  # Session-scoped cache; intentional super-assignment to function-level state.
  regulation_cache <- NULL
  get_regulation_sections <- function() {
    if (!is.null(regulation_cache)) {
      return(regulation_cache)
    }
    manifest <- get_table_partial_manifest()
    sections <- list()
    for (row_idx in seq_len(nrow(manifest))) {
      table_id <- manifest$table_id[[row_idx]]
      sheet_name <- manifest$sheet_name[[row_idx]]
      html <- get_table_partial_html(table_id)
      if (is.null(html) || !nzchar(html)) next
      sections[[length(sections) + 1]] <- list(
        sheet_name = sheet_name,
        table_html = html
      )
    }
    regulation_cache <<- list(sections = sections)
    regulation_cache
  }

  cross_table_sheet_for_selection <- function(cost_category = NULL,
                                              breakdown_type = NULL,
                                              component_filter = NULL,
                                              bonus_component = NULL,
                                              social_subcomponent = NULL) {
    cost_category <- safe_value(if (is.null(cost_category)) selected_cost_category() else cost_category, "all")
    breakdown_type <- safe_value(if (is.null(breakdown_type)) selected_breakdown_type() else breakdown_type, "total")
    component_filter <- safe_value(if (is.null(component_filter)) selected_component_filter() else component_filter, "all_component")
    bonus_component <- safe_value(if (is.null(bonus_component)) selected_bonus_component() else bonus_component, "all_bonuses")
    social_subcomponent <- safe_value(if (is.null(social_subcomponent)) selected_social_subcomponent() else social_subcomponent, "pensions")

    show_table <- (breakdown_type == "component") ||
      (cost_category == "social") ||
      (cost_category == "payroll_taxes")
    if (!show_table) return(NULL)
    if (component_filter == "all_component" && cost_category == "all") return(NULL)

    if (component_filter == "bonuses_and_benefits") {
      return(switch(
        bonus_component,
        all_bonuses = "TL All B",
        ab = "TL ab",
        pl = "TL pl",
        up = "TL up",
        ob = "TL ob",
        NULL
      ))
    }
    if (cost_category == "payroll_taxes") return("TL Pt")
    if (cost_category == "social" && social_subcomponent == "health") return("TL H")
    if (cost_category == "social" && social_subcomponent == "pensions") return("TL All P")
    if (cost_category == "social" && social_subcomponent == "occupational_risk") return("TL Or")
    NULL
  }

  hetero_category_for_selection <- function(cost_category, social_subcomponent) {
    if (cost_category == "all") return("All")
    if (cost_category == "bonuses_and_benefits") return("Bonuses")
    if (cost_category == "payroll_taxes") return("Payroll Taxes")
    if (cost_category == "social") {
      return(switch(
        social_subcomponent,
        pensions = "Pension",
        health = "Health",
        occupational_risk = "Oc Risk",
        NULL
      ))
    }
    NULL
  }

  find_missing_countries <- function(df, candidates = NULL) {
    if (!is.null(candidates)) {
      candidates <- unique(candidates)
      candidates <- candidates[!is.na(candidates) & candidates != "All"]
    }
    if (is.null(df) || nrow(df) == 0 || !"country" %in% names(df) || !"value" %in% names(df)) {
      return(if (is.null(candidates)) character(0) else candidates)
    }
    summary <- df |>
      group_by(country) |>
      summarize(
        max_val = suppressWarnings(max(value, na.rm = TRUE)),
        .groups = "drop"
      )
    if (is.null(candidates) || length(candidates) == 0) {
      candidates <- summary$country
    }
    missing <- summary |>
      filter(!is.finite(max_val) | max_val <= 0) |>
      pull(country)
    missing <- union(missing, setdiff(candidates, summary$country))
    missing
  }

  get_group_data <- function(group_name) {
    sources <- resolve_sources()
    df <- apply_tenure_filter(sources$group_data[[group_name]])
    exclude_health_countries(df)
  }

  get_component_data <- function(component_name) {
    sources <- resolve_sources()
    apply_tenure_filter(sources$component_data[[component_name]])
  }

  get_payer_data <- function(payer_name) {
    sources <- resolve_sources()
    df <- apply_tenure_filter(sources$payer_data[[payer_name]])
    exclude_health_countries(df)
  }
  
  category_subject_text <- function() {
    cost_category <- safe_value(selected_cost_category(), "all")
    breakdown_type <- safe_value(selected_breakdown_type(), "total")
    bonus_component <- safe_value(selected_bonus_component(), "all_bonuses")
    social_subcomponent <- safe_value(selected_social_subcomponent(), "pensions")

    subject <- switch(
      cost_category,
      all = "Non-salary labor costs",
      bonuses_and_benefits = "Bonuses and benefits",
      social = switch(
        social_subcomponent,
        pensions = "Pensions contributions",
        health = "Health contributions",
        occupational_risk = "Occupational risk contributions",
        "Social security contributions"
      ),
      payroll_taxes = "Payroll taxes",
      "Non-salary labor costs"
    )

    if (cost_category == "bonuses_and_benefits" && breakdown_type == "component") {
      subject <- switch(
        bonus_component,
        all_bonuses = "Bonuses and benefits",
        ab = "Annual and other periodic bonuses",
        pl = "Paid leave",
        up = "Unemployment protection",
        ob = "Other bonuses and benefits",
        subject
      )
    }

    subject
  }

  parent_ns_id <- function(name) {
    current <- session$ns("")
    parent  <- sub("[^-]+-$", "", current)
    paste0(parent, name)
  }

  plot_title_text <- function() {
    cost_category <- safe_value(selected_cost_category(), "all")
    breakdown_type <- safe_value(selected_breakdown_type(), "total")
    bonus_component <- safe_value(selected_bonus_component(), "all_bonuses")

    subject <- category_subject_text()

    view_phrase <- ""
    if (breakdown_type == "payer") {
      view_phrase <- " by payer"
    } else if (breakdown_type == "component") {
      if (cost_category == "all") {
        view_phrase <- " by component"
      } else if (cost_category == "bonuses_and_benefits" && bonus_component == "all_bonuses") {
        view_phrase <- " by component"
      }
    }

    #country_phrase <- format_country_phrase(ns_variables$country_sel)
    wage_phrase <- format_wage_phrase(selected_wage_level())
    tenure_phrase <- ""
    if (tenure_enabled()) {
      tenure_phrase <- format_tenure_phrase(input$tenure_selection)
      if (!is.null(tenure_phrase) && tenure_phrase != "") {
        tenure_phrase <- paste0(" ", tenure_phrase)
      }
    }

    paste0(
      subject,
      view_phrase,
      " ",
      #country_phrase, #deprecated, but can be added again
      "as a percentage of wages"
      #, #deprecated, but can be added again
      #wage_phrase,
      #" (%)",
      #tenure_phrase
    )
  }
  
  y_axis_title_text <- function() {
    cost_category <- safe_value(selected_cost_category(), "all")

    if (cost_category == "bonuses_and_benefits") {
      return("Bonuses and benefits as share of wages (%)")
    }
    if (cost_category == "social") {
      social_subcomponent <- safe_value(selected_social_subcomponent(), "pensions")
      if (social_subcomponent == "pensions") {
        return("Pensions contribution as share of wages (%)")
      }
      if (social_subcomponent == "health") {
        return("Health contribution as share of wages (%)")
      }
      if (social_subcomponent == "occupational_risk") {
        return("Occupational risk as share of wages (%)")
      }
      return("Social security contributions as share of wages (%)")
    }
    if (cost_category == "payroll_taxes") {
      return("Payroll taxes as share of wages (%)")
    }
    "Non-salary costs as share of wages (%)"
  }
  
  plot_footer_annotations <- function() {
    access_date <- format(Sys.Date(), "%Y-%m-%d")
    mode <- safe_value(input$compare_mode, "country")
    is_cross_country <- identical(mode, "country")
    cost_category <- safe_value(selected_cost_category(), "all")
    breakdown_type <- safe_value(selected_breakdown_type(), "total")
    bonus_component <- safe_value(selected_bonus_component(), "all_bonuses")
    social_subcomponent <- safe_value(selected_social_subcomponent(), "pensions")

    us4_clause <- "US4 denotes the simple average across the states of New York, California, Texas, and Florida."

    payroll_and_contribution_clause <- paste(
      "For payroll taxes and contributions, percentages result from a combination of the contribution (or tax)",
      "rates and the contribution (or tax) bases to which they apply, so they are not necessarily equivalent",
      "to statutory rates."
    )
    contribution_clause <- paste(
      "For all contributions, percentages result from a combination of the contribution rates and the contribution",
      "bases to which they apply, so they are not necessarily equivalent to statutory rates."
    )
    occupational_risk_tenure_clause <- paste(
      "For all contributions, percentages result from a combination of the tax rates and the tax bases to which",
      "they apply, so they are not necessarily equivalent to statutory rates."
    )
    payroll_clause <- paste(
      "For payroll taxes, percentages result from a combination of the tax rates and the tax bases",
      "to which they apply, so they are not necessarily equivalent to statutory rates."
    )
    non_quantifiable_clause <- paste(
      "Non-quantifiable non-salary benefits include profit sharing bonuses (Chile, Dominican Republic, Ecuador,",
      "Mexico, Peru, Bolivia), family allowances/subsidies (Bolivia, Colombia), transport subsidies (Brazil),",
      "and relocation expenses (Ecuador)."
    )

    bonus_component_label <- function(value) {
      switch(
        value,
        ab = "annual and other periodic bonuses",
        pl = "paid leave",
        up = "unemployment protection",
        ob = "other bonuses and benefits",
        "bonuses and benefits"
      )
    }

    note_text <- ""
    note_sentences <- character(0)
    plot_output_id <- ns("plot")
    plot_width_px <- session$clientData[[paste0("output_", plot_output_id, "_width")]]
    if (is.null(plot_width_px) || !is.finite(plot_width_px)) {
      plot_width_px <- 900
    }
    note_width_px <- max(360, plot_width_px - 20)
    avg_char_px <- 5.0
    note_wrap_width <- floor(note_width_px / avg_char_px)
    note_wrap_width <- max(90, min(note_wrap_width, 260))

    if (identical(cost_category, "all")) {
      note_sentences <- c(
        "Bars show the minimum and maximum non-salary labor costs as a percentage of wages."  # “All”-“Total”/”By payer”/”By component”: 
      )
      if (is_cross_country) {
        note_sentences <- c(note_sentences, us4_clause)
      }
      note_sentences <- c(note_sentences, payroll_and_contribution_clause)
    } else if (identical(cost_category, "bonuses_and_benefits")) {
      if (identical(breakdown_type, "component") && bonus_component %in% c("ab", "pl", "up", "ob")) {
        note_sentences <- c(
          paste0(
            "Bars show the minimum and maximum cost of legally mandated ",
            bonus_component_label(bonus_component),
            " as a percentage of wages."
          )
        )
        if (is_cross_country) {
          note_sentences <- c(note_sentences, us4_clause)
        }
      } else {
        note_sentences <- c(
          "Bars show the minimum and maximum cost of legally mandated bonuses and benefits as a percentage of wages."
        )
        if (is_cross_country) {
          note_sentences <- c(note_sentences, us4_clause, non_quantifiable_clause)
        }
      }
    } else if (identical(cost_category, "social")) {
      component_label <- switch(
        social_subcomponent,
        pensions = "pension contributions",
        health = "health contributions",
        occupational_risk = "occupational risk contributions",
        "social security contributions"
      )
      note_sentences <- c(
        paste0(
          "Bars show the minimum and maximum ",
          component_label,
          " as a percentage of wages."
        )
      )
      if (is_cross_country) {
        note_sentences <- c(note_sentences, us4_clause)
      }
      note_sentences <- c(
        note_sentences,
        if (!is_cross_country && tenure_enabled() && identical(social_subcomponent, "occupational_risk")) {
          occupational_risk_tenure_clause
        } else {
          contribution_clause
        }
      )
    } else if (identical(cost_category, "payroll_taxes")) {
      note_sentences <- c(
        "Bars show the minimum and maximum payroll taxes as a percentage of wages."
      )
      if (is_cross_country) {
        note_sentences <- c(note_sentences, us4_clause)
      }
      note_sentences <- c(note_sentences, payroll_clause)
    } else {
      note_sentences <- c(
        "Bars show the minimum and maximum non-salary labor costs as a percentage of wages."
      )
      if (is_cross_country) {
        note_sentences <- c(note_sentences, us4_clause)
      }
    }

    note_text <- paste("Note:", paste(note_sentences, collapse = " "))
    note_lines <- strwrap(note_text, width = note_wrap_width)
    if (length(note_lines) == 0) {
      note_lines <- ""
    }
    note_text <- paste(note_lines, collapse = "<br>")
    note_line_count <- length(note_lines)
    line_height_px <- 14
    note_yshift <- -100
    source_padding_px <- 10
    source_yshift <- note_yshift - (line_height_px * note_line_count) - source_padding_px
    margin_b <- max(240, abs(source_yshift) + line_height_px + 40)

    annotations <- list(
      list(
        text = note_text,
        xref = "paper",
        yref = "paper",
        x = 0,
        y = 0,
        xanchor = "left",
        yanchor = "top",
        align = "left",
        yshift = note_yshift,
        width = note_width_px,
        showarrow = FALSE,
        font = list(family = plotly_font_family, size = 10)
      ),
      list(
        text = paste0(
          "Source: World Bank Latin America and the Caribbean Chief Economist office (LCRCE), Regulatory Frameworks Database, 2026. Access date: ",
          access_date
        ),
        xref = "paper",
        yref = "paper",
        x = 0,
        y = 0,
        xanchor = "left",
        yanchor = "top",
        align = "left",
        yshift = source_yshift,
        showarrow = FALSE,
        font = list(family = plotly_font_family, size = 10)
      )
    )
    attr(annotations, "margin_b") <- margin_b
    annotations
  }
  
  ns_hovertemplate <- "%{y:.1f}%<extra></extra>"

  enforce_numeric_hover <- function(fig) {
    if (!is.null(fig$x$data) && length(fig$x$data) > 0) {
      for (i in seq_along(fig$x$data)) {
        fig$x$data[[i]]$hovertemplate <- ns_hovertemplate
        fig$x$data[[i]]$hoverinfo <- "y"
        fig$x$data[[i]]$customdata <- NULL
      }
    }
    if (!is.null(fig$x$attrs) && length(fig$x$attrs) > 0) {
      for (i in seq_along(fig$x$attrs)) {
        fig$x$attrs[[i]]$hovertemplate <- ns_hovertemplate
        fig$x$attrs[[i]]$hoverinfo <- "y"
        fig$x$attrs[[i]]$customdata <- NULL
      }
    }
    fig
  }

  apply_labor_plot_theme <- function(fig) {
    fig <- enforce_numeric_hover(fig)
    annotations <- plot_footer_annotations()
    margin_b <- attr(annotations, "margin_b")
    if (is.null(margin_b) || is.na(margin_b)) {
      margin_b <- 230
    }
    fig |>
      layout(
        font = list(family = plotly_font_family),
        title = list(
          text = plot_title_text(),
          x = 0.5,
          xanchor = "center"
        ),
        annotations = annotations,
        margin = list(t = 60, b = margin_b)
      )
  }

  resolve_social_colors <- function(social_subcomponent) {
    component_label <- switch(
      social_subcomponent,
      pensions = "Pensions",
      health = "Health",
      occupational_risk = "Occupational Risk",
      "Social"
    )
    base_color <- component_palette[[component_label]]
    if (is.null(base_color) || is.na(base_color)) {
      base_color <- "#002244"
    }
    c(
      "Min" = base_color,
      "Max" = base_color
    )
  }
  

  # ---- Interleaved helpers (originally lines 1212-1238) ----
  option2_choices_for_group <- function(cost_category) {
    mode <- safe_value(input$compare_mode, "country")
    is_within <- identical(mode, "wage")
    switch(
      cost_category,
      all = c("total", "payer", "component"),
      bonuses_and_benefits = c("total", "component"),
      social = c("total", "payer"),
      payroll_taxes = if (is_within) c("total", "payer", "component") else c("total", "payer"),
      c("total")
    )
  }
