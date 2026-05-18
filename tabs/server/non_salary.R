# =============================================================================
# tabs/server/non_salary.R - Thin shim. The core functionality happens in 
# tabs/server/non_salary/{init,helpers,observers,plots,tables}.R and sourced
# below into the function's environment so all closures resolve correctly.
# =============================================================================
# REQUIRES: global.R must be loaded before this file
# CONTAINS: non_salary_server_core() definition + view-toggle wrappers
#           (labor_server, non_salary_across_server, non_salary_within_server).
# =============================================================================

non_salary_server_core <- function(input,
                                   output,
                                   session,
                                   data_sources = NULL,
                                   enable_tenure = FALSE) {
  ns <- session$ns

  source("tabs/server/non_salary/init.R",      local = environment())
  source("tabs/server/non_salary/helpers.R",   local = environment())
  source("tabs/server/non_salary/observers.R", local = environment())
  source("tabs/server/non_salary/plots.R",     local = environment())
  source("tabs/server/non_salary/tables.R",    local = environment())
}

non_salary_across_server <- function(input, output, session) {
  non_salary_server_core(
    input,
    output,
    session,
    data_sources = LABOR_DATA_SOURCES_ACROSS,
    enable_tenure = FALSE
  )
}

non_salary_within_server <- function(input, output, session) {
  current_data_source <- reactive({
    if (isTRUE(input$show_by_tenure)) {
      LABOR_DATA_SOURCES_WITHIN
    } else {
      LABOR_DATA_SOURCES_WITHIN_NO_TENURE
    }
  })
  non_salary_server_core(
    input,
    output,
    session,
    data_sources = current_data_source,
    enable_tenure = TRUE
  )
}

labor_server <- function(input, output, session) {
  ns <- session$ns
  selected_view      <- reactiveVal("across")  # default: cross-country view
  across_initialized <- reactiveVal(FALSE)
  within_initialized <- reactiveVal(FALSE)

  observeEvent(input$choose_across, {
    selected_view("across")
    updateQueryString("?view=across", mode = "push", session = session)
  })
  observeEvent(input$choose_within, {
    selected_view("within")
    updateQueryString("?view=within", mode = "push", session = session)
  })

  # Deep-link support: only override default when URL explicitly requests "within"
  observeEvent(session$clientData$url_search, {
    query <- parseQueryString(session$clientData$url_search)
    view  <- query$view
    if (!is.null(view) && view == "within") {
      selected_view("within")
    }
  })

  output$labor_content <- renderUI({
    view <- selected_view()
    tagList(
      labor_view_selector_ui(ns, view),
      if (identical(view, "across")) {
        non_salary_across_ui(ns("across"), show_header = FALSE)
      } else if (identical(view, "within")) {
        non_salary_within_ui(ns("within"), show_header = FALSE)
      }
    )
  })

  observeEvent(selected_view(), {
    view <- selected_view()
    if (identical(view, "across") && !isTRUE(across_initialized())) {
      moduleServer("across", non_salary_across_server)
      across_initialized(TRUE)
    }
    if (identical(view, "within") && !within_initialized()) {
      moduleServer("within", non_salary_within_server)
      within_initialized(TRUE)
    }
  })
}
