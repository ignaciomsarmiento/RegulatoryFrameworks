# =============================================================================
# tables.R - output$tabla_detalle renderUI, button renderUIs (option2/component/table_country/country/bonus), and download handlers.
# Sourced into non_salary_server_core's environment.
# =============================================================================
  
  
  output$tabla_detalle <- renderUI({
    
    table_visible(FALSE)
    ns_variables$df_final_tabla <- NULL
    
    cost_category <- safe_value(selected_cost_category(), "all")
    breakdown_type <- safe_value(selected_breakdown_type(), "total")
    component_filter <- safe_value(selected_component_filter(), "all_component")
    bonus_component <- safe_value(selected_bonus_component(), "all_bonuses")
    social_subcomponent <- safe_value(selected_social_subcomponent(), "pensions")

    if (tenure_enabled()) {
      country_sel <- ns_variables$country_sel
      excluded_countries <- c("BRA", "CHL", "COL", "PER")
      if (!is.null(country_sel) &&
          length(country_sel) == 1 &&
          country_sel %in% excluded_countries) {
        return(NULL)
      }
    }

    if (identical(safe_value(input$compare_mode, "country"), "wage")) {
      hetero_meta <- get_heterogeneity_sections()
      country_sel <- ns_variables$country_sel
      if (is.null(country_sel) || length(country_sel) == 0 || "All" %in% country_sel) {
        return(NULL)
      }
      country_code <- country_sel[1]
      if (!(country_code %in% hetero_meta$countries)) {
        return(NULL)
      }
      category <- hetero_category_for_selection(cost_category, social_subcomponent)
      if (is.null(category)) {
        return(NULL)
      }
      category_key <- normalize_hetero_key(category)
      matching <- Filter(function(s) {
        identical(s$country, country_code) && identical(s$category_key, category_key)
      }, hetero_meta$sections)

      if (length(matching) == 0) {
        return(NULL)
      }

      hetero_section <- matching[[1]]
      hetero_title <- "Non-salary costs that vary and sources of variation"

      table_visible(TRUE)
      ns_variables$df_final_tabla <- get_heterogeneity_excel_table(category, country_code)

      category_lower <- tolower(category_subject_text())
      across_btn_id  <- parent_ns_id("choose_across")
      note_ui <- tags$p(
        class = "labor-within-note",
        HTML(sprintf(
          'Note: Full detail on %s available in <a href="#" onclick="$(\'#%s\').click(); return false;">cross-country view</a>.',
          htmltools::htmlEscape(category_lower),
          across_btn_id
        ))
      )

      # .section-title CSS lives in www/labor.css
      return(tagList(
        div(class = "section-title", hetero_title),
        div(class = "labor-reg-partial-container", HTML(hetero_section$table_html)),
        note_ui
      ))
    }
    
    con_sel <- ns_variables$country_sel
    if (is.null(con_sel) || length(con_sel) == 0) {
      con_sel <- "All"
    }
    con_sel_names <- con_sel
    if (!"All" %in% con_sel) {
      con_sel_names <- vapply(con_sel, country_display_name, character(1))
    }
    
    title_case_simple <- function(text) {
      if (is.null(text) || length(text) == 0 || !nzchar(text)) return(text)
      minor_words <- c("a", "an", "and", "as", "at", "but", "by", "for", "from",
                       "if", "in", "nor", "of", "on", "or", "per", "the", "to",
                       "vs", "via", "with")
      original_words <- strsplit(text, "\\s+")[[1]]
      words <- strsplit(tolower(text), "\\s+")[[1]]
      total <- length(words)
      for (i in seq_along(words)) {
        orig <- original_words[i]
        w <- words[i]
        if (grepl("^[A-Z]{2,}$", orig)) {
          words[i] <- orig
        } else if (i != 1 && i != total && w %in% minor_words) {
          words[i] <- w
        } else {
          words[i] <- paste0(toupper(substr(w, 1, 1)), substr(w, 2, nchar(w)))
        }
      }
      paste(words, collapse = " ")
    }
    
    # ---- Title ----
    component_label <- NULL
    if (cost_category == "payroll_taxes") {
      component_label <- "Payroll Taxes"
    } else if (component_filter == "bonuses_and_benefits") {
      component_label <- switch(
        bonus_component,
        all_bonuses = "All Bonuses",
        ab = "Annual and other periodic bonuses",
        pl = "Paid Leave",
        up = "Unemployment Protection",
        ob = "Other bonuses and benefits",
        "Bonuses and benefits"
      )
    } else if (social_subcomponent == "health") {
      component_label <- "Health"
    } else if (social_subcomponent == "payroll_taxes") {
      component_label <- "Payroll Taxes"
    } else if (social_subcomponent == "pensions") {
      component_label <- "Pensions"
    } else if (social_subcomponent == "occupational_risk") {
      component_label <- "Occupational Risk Insurance"
    }
    
    title_text <- NULL
    if (!is.null(component_label)) {
      title_text <- "Regulation detail by country"
    }
    
    title_ui <- NULL
    if (!is.null(title_text)) {
      title_ui <- tags$div(
        style = paste(
          "font-weight: 600;",
          "margin: 8px 0 12px 0;",
          "color: #0f3b66;",
          "text-align: left;",
          "font-size: 20px;",
          "font-family:", plotly_font_family, ";"
        ),
        title_text
      )
    }
    
    # ---- Helper: load sheet, filter by country, store in ns_variables ----
    load_and_filter <- function(sheet_name) {
      data <- get_excel_table(sheet_name)
      if (is.null(data)) return(NULL)
      data <- as.data.frame(data)
      if ("Country" %in% names(data)) {
        data$Country[data$Country == "United States"] <- "US4"
      }
      if (!"All" %in% con_sel) {
        data <- data |> dplyr::filter(Country %in% con_sel_names)
      }
      ns_variables$df_final_tabla <- data
      data
    }

    # =========================================================================
    # CROSS-COUNTRY: HTML TABLES
    # =========================================================================
    if (identical(safe_value(input$compare_mode, "country"), "country")) {
      show_table <- (breakdown_type == "component") ||
        (cost_category == "social") ||
        (cost_category == "payroll_taxes")
      if (!show_table) return()
      if (component_filter == "all_component" && cost_category == "all") return()

      sheet_name <- cross_table_sheet_for_selection(cost_category, breakdown_type, component_filter, bonus_component, social_subcomponent)
      if (is.null(sheet_name)) return(NULL)

      data <- load_and_filter(sheet_name)
      if (is.null(data)) return(NULL)
      table_visible(TRUE)

      partial_id <- table_partial_id_for_sheet(sheet_name)
      if (!is.null(partial_id)) {
        selected_code <- current_table_country(sheet_name)
        if (is.null(selected_code)) {
          return(NULL)
        }
        selected_country_name <- country_display_name(selected_code)
        selected_data <- data |> dplyr::filter(Country == selected_country_name)
        if (nrow(selected_data) == 0) {
          return(NULL)
        }
        partial_fragment <- table_partial_fragment(partial_id, selected_country_name, selected_code)
        ns_variables$df_final_tabla <- selected_data
        return(tagList(
          title_ui,
          if (!is.null(partial_fragment)) {
            partial_fragment
          } else {
            build_pension_country_table(selected_data, selected_code)
          }
        ))
      }

      reg_sections <- get_regulation_sections()
      matching <- Filter(function(s) identical(s$sheet_name, sheet_name), reg_sections$sections)
      if (length(matching) == 0) return(NULL)
      section <- matching[[1]]

      visible_countries <- character(0)
      if (!is.null(con_sel) && length(con_sel) > 0 && !"All" %in% con_sel) {
        visible_countries <- con_sel_names
      }
      visible_json <- jsonlite::toJSON(visible_countries, auto_unbox = TRUE)
      show_all <- if (is.null(con_sel) || length(con_sel) == 0 || "All" %in% con_sel) "true" else "false"

      # .excel-table / .country-hidden CSS lives in www/labor.css
      return(tagList(
        title_ui,
        tags$script(HTML(sprintf("
          (function() {
            var visible = %s || [];
            var showAll = %s;
            document.querySelectorAll('.excel-table table').forEach(function(table) {
              var rows = table.querySelectorAll('tr');
              var lastCountry = null;
              rows.forEach(function(row, idx) {
                if (idx === 0) return;
                var firstCell = row.querySelector('td');
                if (!firstCell) return;
                var text = (firstCell.textContent || '').replace(/\\s+/g, ' ').trim();
                if (text) {
                  lastCountry = text;
                }
                var country = text || lastCountry;
                if (!country) return;
                if (showAll || visible.indexOf(country) !== -1) {
                  row.classList.remove('country-hidden');
                } else {
                  row.classList.add('country-hidden');
                }
              });
            });
          })();
        ", visible_json, show_all))),
        div(class = "excel-table", HTML(section$table_html))
      ))
    }
    
    # ---- Helper: build a select-input filter function for Country ----
    # This creates a native <select> dropdown inside reactable's filter row
    # using only reactable's built-in reactable::JS — no external wiring needed.
    country_select_filter <- function(countries) {
      opts <- sort(unique(countries))
      opts_js <- paste0('"', opts, '"', collapse = ", ")
      reactable::JS(sprintf(
        'function(column, state) {
           var opts = [%s];
           var onChange = function(e) {
             column.setFilter(e.target.value || undefined);
           };
           return React.createElement("select", {
             value: column.filterValue || "",
             onChange: onChange,
             style: {
               width: "100%%",
               padding: "4px 6px",
               border: "1px solid #ccc",
               borderRadius: "4px",
               fontSize: "12px",
               fontFamily: "%s",
               background: "#fff",
               cursor: "pointer"
             }
           }, [
             React.createElement("option", { value: "", key: "_all" }, "All"),
             opts.map(function(o) {
               return React.createElement("option", { value: o, key: o }, o);
             })
           ]);
         }', opts_js, plotly_font_family
      ))
    }
    
    # Exact-match filter method for the dropdown (not substring)
    exact_filter_method <- reactable::JS(
      'function(rows, columnId, filterValue) {
         if (!filterValue) return rows;
         return rows.filter(function(row) {
           return row.values[columnId] === filterValue;
         });
       }'
    )
    
    # ---- Helper: build reactable ----
    build_reactable <- function(data, merge_country = FALSE) {
      
      is_all_bonuses_table <- identical(breakdown_type, "component") &&
        identical(component_filter, "bonuses_and_benefits") &&
        identical(bonus_component, "all_bonuses")
      
      table_class <- "all-bonuses-table"
      # .all-bonuses-table CSS lives in www/labor.css
      table_default_coldef <- reactable::colDef(
        html        = TRUE,
        minWidth    = 140,
        #maxWidth    = 260,
        align       = "left",
        headerStyle = list(textAlign = "center", fontWeight = "600"),
        style       = list(
          whiteSpace = "normal",
          lineHeight = "1.35",
          fontSize   = "12px",
          padding    = "6px",
          textAlign  = "left",
          fontFamily = plotly_font_family
        )
      )
      first_col <- names(data)[1]
      if (!is.null(first_col) && nzchar(first_col)) {
        table_columns <- list()
        table_columns[[first_col]] <- reactable::colDef(
          align       = "center",
          headerStyle = list(textAlign = "center", fontWeight = "600"),
          style       = list(fontWeight = "600", textAlign = "center")
        )
      }
      table_theme <- reactable::reactableTheme(
        style       = list(fontFamily = plotly_font_family),
        headerStyle = list(fontFamily = plotly_font_family, textAlign = "center")
      )
    
      
      # ---- "All Bonuses" special styling ----
      # .all-bonuses-table CSS lives in www/labor.css
      if (is_all_bonuses_table) {
        table_class <- "all-bonuses-table"
        table_default_coldef <- reactable::colDef(
          html        = TRUE,
          minWidth    = 140,
          #maxWidth    = 260,
          align       = "left",
          headerStyle = list(textAlign = "center", fontWeight = "600"),
          style       = list(
            whiteSpace = "normal",
            lineHeight = "1.35",
            fontSize   = "12px",
            padding    = "6px",
            textAlign  = "left",
            fontFamily = plotly_font_family
          )
        )
        first_col <- names(data)[1]
        if (!is.null(first_col) && nzchar(first_col)) {
          table_columns <- list()
          table_columns[[first_col]] <- reactable::colDef(
            align       = "center",
            headerStyle = list(textAlign = "center", fontWeight = "600"),
            style       = list(fontWeight = "600", textAlign = "center")
          )
        }
        table_theme <- reactable::reactableTheme(
          style       = list(fontFamily = plotly_font_family),
          headerStyle = list(fontFamily = plotly_font_family, textAlign = "center")
        )
      }
      
      # ---- Visual merge of Country column (TL Pt) ----
      if (merge_country && "Country" %in% names(data)) {
        if (is.null(table_columns)) table_columns <- list()
        
        countries <- data$Country
        n         <- length(countries)
        span_vec  <- integer(n)
        i <- 1
        while (i <= n) {
          run <- 1L
          while (i + run <= n && !is.na(countries[i + run]) &&
                 countries[i + run] == countries[i]) {
            run <- run + 1L
          }
          span_vec[i] <- run
          if (run > 1) span_vec[(i + 1):(i + run - 1)] <- 0L
          i <- i + run
        }
        spans_json <- jsonlite::toJSON(span_vec, auto_unbox = FALSE)
        
        js_style <- htmlwidgets::JS(sprintf(
          "function(rowInfo) {
             var spans = %s;
             var idx   = rowInfo.index;
             if (spans[idx] === 0) return { display: 'none' };
             return { fontWeight: '600', textAlign: 'center', verticalAlign: 'middle' };
           }",
          spans_json
        ))
        
        table_columns[["Country"]] <- reactable::colDef(
          filterable    = TRUE,
          filterInput   = country_select_filter(data$Country),
          filterMethod  = exact_filter_method,
          align         = "center",
          headerStyle   = list(textAlign = "center", fontWeight = "600"),
          style         = js_style
        )
        if (is.null(table_class)) table_class <- "merged-country-table"
        
        # ---- Standard Country column with dropdown filter ----
      } else if ("Country" %in% names(data)) {
        if (is.null(table_columns)) table_columns <- list()
        table_columns[["Country"]] <- reactable::colDef(
          filterable    = TRUE,
          filterInput   = country_select_filter(data$Country),
          filterMethod  = exact_filter_method,
          sticky        = "left",
          minWidth      = 130,
          #maxWidth      = 160,
          align         = "center",
          headerStyle   = list(
            textAlign  = "center",
            fontWeight = "600",
            position   = "sticky",
            left       = "0",
            background = "#fff",
            zIndex     = "1"
          ),
          style = list(
            fontWeight = "600",
            textAlign  = "center",
            position   = "sticky",
            left       = "0",
            background = "#fff",
            zIndex     = "1"
          )
        )
      }
      
      # .tbl-scroll-wrap CSS lives in www/labor.css

      tagList(
        title_ui,
        tags$div(
          class = "tbl-scroll-wrap",
          style = "display:flex; justify-content:center; width:100%;",
          reactable::reactable(
            data,
            width           = "100%",
            defaultColDef   = table_default_coldef,
            columns         = table_columns,
            theme           = table_theme,
            class           = table_class,
            bordered        = TRUE,
            striped         = TRUE,
            highlight       = TRUE,
            resizable       = TRUE,
            pagination      = FALSE,
            defaultPageSize = nrow(data)
          )
        )
      )
    }
    
    # =========================================================================
    # For downloads
    # =========================================================================
    cost_category <- safe_value(selected_cost_category(), "all")  
    
    show_table <- (breakdown_type == "component") ||
      (cost_category == "social") ||
      (cost_category == "payroll_taxes")
    
    if (!show_table) return()
    if (component_filter == "all_component" && cost_category == "all") return()
    
    data <- NULL
    
    if (component_filter == "bonuses_and_benefits") {
      if (bonus_component == "all_bonuses") {
        data <- load_and_filter("TL All B")
      } else if (bonus_component == "ab") {
        data <- load_and_filter("TL ab")
      } else if (bonus_component == "pl") {
        data <- load_and_filter("TL pl")
      } else if (bonus_component == "up") {
        data <- load_and_filter("TL up")
      } else if (bonus_component == "ob") {
        data <- load_and_filter("TL ob")
      }
    } else if (cost_category == "payroll_taxes") {
      data <- load_and_filter("TL Pt")
    } else if (cost_category == "social" && social_subcomponent == "health") {
      data <- load_and_filter("TL H")
    } else if (cost_category == "social" && social_subcomponent == "pensions") {
      data <- load_and_filter("TL All P")
    } else if (cost_category == "social" && social_subcomponent == "occupational_risk") {
      data <- load_and_filter("TL Or")
    }
    
    if (is.null(data)) return(NULL)
    table_visible(TRUE)
    
    needs_merge <- identical(social_subcomponent, "payroll_taxes")
    build_reactable(data, merge_country = needs_merge)
  })

  # Button renderUIs 
  output$option2_buttons <- renderUI({
    if (!option1_selected()) {
      return(div(style = "display:none;"))
    }

    cost_category <- selected_cost_category()
    valid_choices <- option2_choices_for_group(cost_category)
    if (cost_category == "social" &&
        safe_value(selected_social_subcomponent(), "pensions") == "occupational_risk") {
      valid_choices <- setdiff(valid_choices, "payer")
    }
    button_style <- paste(
      "background-color: #e6f4ff;",
      "color: #0f3b66;",
      "border: 1px solid #0f3b66;",
      "border-radius: 20px;",
      "padding: 6px 18px;",
      "font-weight: 600;"
    )

    option_button <- function(id, label, value, title) {
      btn_class <- if (identical(selected_breakdown_type(), value)) {
        "pill-button subcomponent-btn active"
      } else {
        "pill-button subcomponent-btn"
      }

      tags$div(
        style = "display: flex; flex-direction: column; gap: 4px;",
        actionButton(ns(id), label, class = btn_class, title = title, style = button_style)
      )
    }

    tags$div(
      class = "option2-group",
      style = "display: flex; flex-direction: column; gap: 8px;",
      tags$span("2. Explore by:", class = "labor-filter-label"),
      if ("total" %in% valid_choices) option_button("btn_total", "TOTAL", "total", "Show total non-salary costs."),
      if ("payer" %in% valid_choices) option_button("btn_payer", "BY PAYER", "payer",
                                                     "Split costs by payer (employer vs. employee)."),
      if ("component" %in% valid_choices && cost_category != "social") {
        option_button("btn_component", "BY COMPONENT", "component", "Break down costs by component.")
      }
    )
  })
  
  
  # --- Components ----
  output$component_buttons <- renderUI({
    cost_category <- selected_cost_category()
    
    if (cost_category != "social" || identical(selected_social_subcomponent(), "occupational_risk")) {
      return(NULL)
    }
    button_class <- function(value) {
      if (identical(selected_social_subcomponent(), value)) {
        "component-btn active"
      } else {
        "component-btn"
      }
    }
    
    div(
      style = "margin-top: 6px; margin-left: 18px;",
      div(
        class = "horizontal-container",
        style = "display:flex; align-items:flex-start; justify-content:flex-start; width:100%;",
        div(
          class = "component-buttons-container",
          style = "display:flex; flex-wrap:wrap; gap:8px;",
        actionButton(
          ns("pensions"),
          "Pensions",
          class = button_class("pensions")
        ),

          actionButton(
            ns("health"),
            "Health",
            class = button_class("health")
          )
        )
      )
    )
  })

  output$table_country_buttons <- renderUI({
    mode <- safe_value(input$compare_mode, "country")
    if (!identical(mode, "country")) {
      return(NULL)
    }
    sheet_name <- cross_table_sheet_for_selection()
    if (is.null(sheet_name) || is.null(table_partial_id_for_sheet(sheet_name))) {
      return(NULL)
    }
    countries <- table_country_codes(sheet_name)
    active_country <- current_table_country(sheet_name)
    if (length(countries) == 0 || is.null(active_country)) {
      return(NULL)
    }
    tags$div(
      class = "labor-table-country-selector",
      build_country_flag_buttons(countries, active_country, "table_country_button")
    )
  })

  output$country_buttons <- renderUI({
    if (!isTRUE(enable_tenure)) {
      return(NULL)
    }
    countries <- ns_variables$countries
    countries <- countries[!is.na(countries) & countries != "All"]
    if (length(countries) == 0) {
      return(NULL)
    }

    current <- ns_variables$country_sel
    if (is.null(current) || length(current) == 0 || "All" %in% current) {
      current <- countries[1]
    }
    active_country <- current[1]

    build_country_flag_buttons(countries, active_country, "country_button")
  })
  
  output$bonus_buttons <- renderUI({
    cost_category <- selected_cost_category()
    breakdown_type <- selected_breakdown_type()
    
    if(cost_category != "bonuses_and_benefits"){
      return(div(style="visibility:hidden;"))
    }
    else if (breakdown_type =="component") {
      bonus_class <- function(value) {
        if (identical(selected_bonus_component(), value)) {
          "component-btn active"
        } else {
          "component-btn"
        }
      }
      div(
        class = "horizontal-container",
        style = "margin-top: 6px; margin-left: 24px;",
        div(
          class = "component-buttons-container labor-subcategory-buttons",
          style = "display:flex; flex-direction:column; align-items:flex-start; gap:8px;",
          
          actionButton(
            ns("all_bonuses"),
            "All Bonuses",
            class = bonus_class("all_bonuses")
          ),
          
          actionButton(
            ns("ab"),
            "Annual and other periodic bonuses",
            class = bonus_class("ab")
          ),
          
          actionButton(
            ns("pl"),
            "Paid Leave",
            class = bonus_class("pl")
          ),
          
          actionButton(
            ns("up"),
            "Unemployment Protection",
            class = bonus_class("up")
          ),
          
          actionButton(
            ns("ob"),
            "other bonuses and benefits",
            class = bonus_class("ob")
          )
        )
      )
    }
  })

  #  Download handlers 
  output$download_df <- downloadHandler(
    filename = function() {
      paste0("Regulatory_Frameworks_Data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(ns_variables$df_final, file, row.names = FALSE)
    },
    contentType = "text/csv"
  )

  output$download_table_ui <- renderUI({
    df_table <- ns_variables$df_final_tabla
    if (!is.data.frame(df_table) || ncol(df_table) == 0) {
      return(NULL)
    }
    tagList(
      tags$p(
        "You can download the regulation details by clicking the button below:",
        style = "font-size: 12px; color: #555; margin: 14px 0 10px 0;"
      ),
      downloadButton(
        outputId = ns("download_table"),
        label = "DOWNLOAD REGULATION DETAIL",
        style = paste(
          "background-color: #1e3a5f; color: white; border-radius: 25px;",
          "padding: 10px 20px; font-weight: bold; border: none;"
        )
      )
    )
  })
  
  output$download_table <- downloadHandler(
    filename = function() {
      paste0("Regulatory_Frameworks_Legislation_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      openxlsx::write.xlsx(ns_variables$df_final_tabla, file, overwrite = TRUE)
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
