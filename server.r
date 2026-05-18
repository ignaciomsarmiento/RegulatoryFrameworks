# =============================================================================
# server.R - servier Landing page
# =============================================================================

shinyServer(function(input, output, session) {

  # =========================================================================
  # PRE-LOAD MODULE FILES (once per session, outside any reactive context)
  # Sourcing inside renderUI / observeEvent re-parses large files on every
  # navigation — moving them here makes first-click instant.
  # =========================================================================

  source("tabs/ui/topic_template.R", local = TRUE)
  source("tabs/ui/non_salary.R",     local = TRUE)
  source("tabs/server/non_salary.R", local = TRUE)

  # =========================================================================
  # REACTIVE VALUES
  # =========================================================================

  navigation <- reactiveValues(
    current_view = "landing",
    previous_view = NULL,
    selected_topic = NULL,
    selected_country = NULL
  )

  labor_initialized <- reactiveVal(FALSE)
  
  # =========================================================================
  # CONFIGURATION
  # =========================================================================
  
  topics <- list(
    labor = list(
      title = "Non-salary labor costs",
      description = "Yearly bonuses, social security contributions, and employment benefits",
      ui_function = "render_labor_ui",
      server_function = "labor_server"
    ),
    minwage = list(
      title = "Minimum Wages",
      description = "Minimum wage policies and trends across the region"
    ),
    firing = list(
      title = "Firing Costs",
      description = "Dismissal rules, severance payments, and termination procedures"
    ),
    btax = list(
      title = "Business Taxes",
      description = "Corporate tax rates, incentives, and fiscal policies"
    )
  )
  
  countries <- list(
    argentina = "Argentina",
    bolivia = "Bolivia",
    brazil = "Brazil",
    chile = "Chile",
    colombia = "Colombia",
    costa_rica = "Costa Rica",
    dominican = "Dominican Republic",
    ecuador = "Ecuador",
    el_salvador = "El Salvador",
    guatemala = "Guatemala",
    honduras = "Honduras",
    mexico = "Mexico",
    nicaragua = "Nicaragua",
    panama = "Panama",
    paraguay = "Paraguay",
    peru = "Peru",
    uruguay = "Uruguay",
    venezuela = "Venezuela"
  )
  
  # =========================================================================
  # NAVIGATION HANDLERS
  # =========================================================================
  
  observeEvent(input$main_tabs, {
    if (input$main_tabs == "landing") {
      session$sendCustomMessage("header-active", "landing")
    }
    if (input$main_tabs == "content" &&
        is.null(navigation$selected_topic) &&
        is.null(navigation$selected_country)) {
      navigation$selected_topic <- "labor"
      navigation$current_view <- "content"
      session$sendCustomMessage("header-active", "labor")
    }
  }, ignoreInit = FALSE)
  
  observeEvent(input$topic_selected, {
    navigation$selected_topic <- input$topic_selected
    navigation$selected_country <- NULL
    navigation$previous_view <- "topics"
    navigation$current_view <- "content"
    
    session$sendCustomMessage("header-active", input$topic_selected)
    updateTabsetPanel(session, "main_tabs", selected = "content")
  })
  
  # =========================================================================
  # DYNAMIC CONTENT RENDERING  (REACTIVATED FOR TOPICS)
  # =========================================================================
  
  output$dynamic_content <- renderUI({
    
    # ---- TOPIC SELECTED ----
    if (!is.null(navigation$selected_topic)) {
      topic_id <- navigation$selected_topic
      
      # LABOR — Non-Salary Costs
      if (topic_id == "labor") {
        return(labor_ui("labor"))
      }

      if (topic_id %in% c("minwage", "firing", "btax")) {
        topic <- topics[[topic_id]]
        return(topic_under_construction_ui(
          active = topic_id,
          title = topic$title,
          subtitle = topic$description
        ))
      }

      if (identical(topic_id, "forthcoming")) {
        return(topic_under_construction_ui(
          active = NULL,
          title = "Coming Soon",
          subtitle = "This dashboard page is under construction."
        ))
      }
      
      # Other topic UIs
      ui_func_name <- if (topic_id %in% names(topics)) topics[[topic_id]]$ui_function else NULL
      if (!is.null(ui_func_name) && exists(ui_func_name)) {
        return(do.call(ui_func_name, list()))
      }
      
      return(
        div(class = "text-center", style = "padding: 50px;",
            h3("Topic not implemented"))
      )
    }
    
    # ---- COUNTRY SELECTED ----
    if (!is.null(navigation$selected_country)) {
      country_id <- navigation$selected_country
      country_name <- countries[[country_id]]
      return(render_country_dashboard(country_name, country_id))
    }
    
    return(
      div(class = "text-center", style = "padding: 50px;",
          h3("No selection made"))
    )
  })
  
  # =========================================================================
  # UI RENDERING FUNCTIONS
  # =========================================================================
  
  render_labor_ui <- function() {
    labor_ui("labor")
  }
  
  render_minwage_ui <- function() {
    source("modules/minwage/ui.R", local = TRUE)$value
    minwage_ui("minwage")
  }
  
  render_btax_ui <- function() {
    source("modules/btax/ui.R", local = TRUE)$value
    btax_ui("btax")
  }
  
  render_social_ui <- function() {
    source("modules/social/ui.R", local = TRUE)$value
    social_ui("social")
  }
  
  # =========================================================================
  # MODULE SERVER CALLS  (SIMPLIFIED)
  # =========================================================================
  
  observeEvent(navigation$selected_topic, {
    topic_id <- navigation$selected_topic

    if (topic_id == "labor" && !labor_initialized()) {
      callModule(labor_server, "labor")
      labor_initialized(TRUE)
    }
  })
  
})
