# =============================================================================
# ui.R - User Interface for the landing page
# =============================================================================

# ============================
# UI sources for each page
# ============================

source("tabs/ui/topic_template.R", local = TRUE)

# Set to TRUE if want to add dashboard navigation buttons on the landing page.
show_header_nav_on_landing <- FALSE

# ============================
# MAIN UI
# ============================

shinyUI(
  fluidPage(
    shinyjs::useShinyjs(),
    
    # ---- HEAD ----
    tags$head(
      tags$title("Regulatory Frameworks Explorer"),
      tags$link(rel = "icon", type = "image/png", href = "favicon.png"),
      tags$link(
        href = "https://fonts.googleapis.com/css2?family=National+Park&display=swap",
        rel = "stylesheet"
      ),
      includeCSS("www/styles.css"),
      includeCSS("www/labor.css"),
      
      # Loading-message styles live in www/labor.css; tab-history sync in www/tab-history.js;
      # custom message handlers + header-topic state in www/header.js;
      # output-loading toggles in www/output-loading.js.

      tags$script(HTML(sprintf(
        "window.showHeaderNavOnLanding = %s;",
        tolower(as.character(show_header_nav_on_landing))
      ))),
      tags$script(src = "header.js"),
      tags$script(src = "output-loading.js"),
      tags$script(src = "tab-history.js")
    ),
    # ---- HEADER ----
    tags$div(
      class = "header",
      tags$div(
        class = "header-content",
        
        # Logo (izquierda)
        tags$img(
          src = "WB.png",
          class = "wb-logo",
          style = "cursor: pointer;",
          onclick = "if (window.setHeaderTopic) window.setHeaderTopic('landing'); document.querySelector('a[data-value=\"landing\"]').click();"
        ),
        
        tags$nav(
          class = "header-topic-nav",
          `aria-label` = "Dashboard sections",
          lapply(topic_nav_items(), function(item) {
            item_class <- "header-topic-link"
            if (identical(item$id, "landing")) {
              item_class <- paste(item_class, "active")
            }
            tags$a(
              href = "#",
              class = item_class,
              `data-topic` = item$id,
              onclick = topic_nav_onclick(item$id),
              item$label
            )
          })
        )
      )
    ),
    
    # ---- MAIN BODY ----
    div(
      class = "main-content",
      tabsetPanel(
        id = "main_tabs",
        type="hidden",
        selected = "landing",
        
        # ============================
        # 1. LANDING PAGE
        # ============================
        tabPanel(
          "landing",
          # Top section: image (left) + text (right)
          tags$div(
            class = "landing-container",

            tags$div(
              class = "landing-left-col",
              tags$div(class = "landing-image-box")
            ),

            tags$div(
              class = "landing-right-col",

              tags$div(
                class = "landing-header-row",
                tags$div(class = "landing-title", h1("Shaping the Playing Field for Economic Growth"))
              ),
              tags$div(class = "landing-eyebrow", "An inventory of critical regulatory frameworks for business in Latin America "),

              tags$div(
                class = "landing-desc-row",
                p("Policy outcomes depend on design. Small differences in how regulations are written and implemented can produce large differences in incentives, compliance costs, and unintended effects."),
                p("This inventory compares labor market, social protection, and tax regulations across 11 Latin American countries—Argentina, Bolivia, Brazil, Chile, Colombia, Ecuador, the Dominican Republic, Honduras, Mexico, Paraguay, and Peru. These frameworks shape workers' incomes and conditions, and firms' labor costs and hiring decisions, influencing how productive activity is organized."),
                p("These frameworks operate alongside many other rules that affect business behavior. Mapping the full regulatory ecosystem is beyond the scope of this inventory. Instead, this resource compiles structured, detailed information on three foundational areas that directly affect workers and firms—creating a basis for comparison, diagnostics, and policy learning, and supporting efforts to simplify regulatory burdens, reduce contradictions, and improve policy design.")
              )
            )
          ),

          # Bottom section: Explore heading + 4 cards (full width)
          tags$div(
            class = "landing-bottom-section",

            tags$p(
              style = "font-size: 32px; font-weight: 500; color: var(--solid-blue)",
              "Explore"
            ),
            tags$div(
              class = "landing-cards-grid",

              tags$div(
                class = "landing-widget-grid",
                tags$div(class = "widget-icon", style = "background-color: var(--solid-blue);", tags$div(class = "icon-arrow-mask")),
                tags$a(
                  class = "widget-card", href = "#",
                  onclick = "if (window.setHeaderTopic) window.setHeaderTopic('labor'); Shiny.setInputValue('topic_selected', 'labor', {priority: 'event'});",
                  h3("Non-salary labor costs")
                )
              ),

              tags$div(
                class = "landing-widget-grid",
                tags$div(class = "widget-icon", style = "background-color: #A71261;", tags$div(class = "icon-arrow-mask")),
                tags$a(
                  class = "widget-card", href = "#",
                  onclick = "if (window.setHeaderTopic) window.setHeaderTopic('minwage'); Shiny.setInputValue('topic_selected', 'minwage', {priority: 'event'});",
                  h3("Minimum wages")
                )
              ),

              tags$div(
                class = "landing-widget-grid",
                tags$div(class = "widget-icon", style = "background-color: #9D906C;", tags$div(class = "icon-arrow-mask")),
                tags$a(
                  class = "widget-card", href = "#",
                  onclick = "if (window.setHeaderTopic) window.setHeaderTopic('firing'); Shiny.setInputValue('topic_selected', 'firing', {priority: 'event'});",
                  h3("Firing costs")
                )
              ),

              tags$div(
                class = "landing-widget-grid",
                tags$div(class = "widget-icon", style = "background-color: #1F6B56;", tags$div(class = "icon-arrow-mask")),
                tags$a(
                  class = "widget-card", href = "#",
                  onclick = "if (window.setHeaderTopic) window.setHeaderTopic('btax'); Shiny.setInputValue('topic_selected', 'btax', {priority: 'event'});",
                  h3("Business taxes")
                )
              )
            )
          ),
          
          tags$div(class = "footer", tags$p("© 2026 World Bank Group"))
        ),


        # ============================
        # 4. CONTENT MODULE PAGE
        # ============================
        tabPanel(
          "content",
          div(
            class = "content-area",
            uiOutput("dynamic_content")
          ),
          tags$div(
            class = "footer",
            tags$p(class = "footer-text", "© 2026 World Bank Group")
          )
        )
      )
    )
  )
)
