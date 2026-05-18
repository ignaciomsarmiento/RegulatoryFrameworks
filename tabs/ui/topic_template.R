# =============================================================================
# Place holders for other user interfaces
# =============================================================================

topic_nav_items <- function() {
  list(
    list(id = "landing", label = "Home"),
    list(id = "labor", label = "Non-salary labor costs"),
    list(id = "minwage", label = "Minimum Wages"),
    list(id = "firing", label = "Firing Costs"),
    list(id = "btax", label = "Business Taxes")
  )
}

topic_nav_onclick <- function(topic_id) {
  if (identical(topic_id, "landing")) {
    return(paste(
      "if (window.setHeaderTopic) window.setHeaderTopic('landing');",
      "var tabBtn = document.querySelector('a[data-value=\"landing\"]');",
      "if (tabBtn) {",
      "if (window.jQuery && window.jQuery(tabBtn).tab) { window.jQuery(tabBtn).tab('show'); }",
      "tabBtn.click();",
      "}",
      "return false;"
    ))
  }

  sprintf(
    paste(
      "if (window.setHeaderTopic) window.setHeaderTopic('%s');",
      "Shiny.setInputValue('topic_selected', '%s', {priority: 'event'});",
      "return false;"
    ),
    topic_id,
    topic_id
  )
}

topic_nav_ui <- function(active = NULL) {
  tags$nav(
    class = "topic-nav",
    `aria-label` = "Dashboard sections",
    lapply(topic_nav_items(), function(item) {
      item_class <- "topic-nav-link"
      if (!is.null(active) && identical(active, item$id)) {
        item_class <- paste(item_class, "active")
      }
      if (identical(item$id, "landing")) {
        item_class <- paste(item_class, "topic-nav-home")
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
}

topic_under_construction_ui <- function(active, title, subtitle) {
  tagList(
    tags$section(
      class = "topic-template-page under-construction-page",
      tags$div(
        class = "topic-template-header",
        tags$span(class = "topic-status-pill", "Under construction"),
        h1(class = "topic-title", title),
        p(class = "topic-subtitle", subtitle)
      )
    )
  )
}
