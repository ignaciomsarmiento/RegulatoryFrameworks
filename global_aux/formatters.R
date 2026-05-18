# =============================================================================
# formatters.R - Shared display helpers
# =============================================================================

country_display_name <- function(country_code) {
  if (is.null(country_code) || country_code == "") {
    return(country_code)
  }
  code <- toupper(country_code)
  mapped <- COUNTRY_NAME_MAP[[code]]
  if (!is.null(mapped)) {
    return(mapped)
  }
  country_code
}

format_wage_label <- function(wage_code) {
  if (is.null(wage_code) || length(wage_code) == 0) {
    return(character(0))
  }
  wage_code <- wage_code[!is.na(wage_code)]
  if (length(wage_code) == 0) {
    return(character(0))
  }
  paste0(substr(wage_code, 1, nchar(wage_code) - 2), " MW")
}

format_wage_phrase <- function(wage_code) {
  if (is.null(wage_code) || length(wage_code) == 0) {
    return("selected minimum wage levels")
  }
  wage_code <- wage_code[!is.na(wage_code)]
  if (length(wage_code) == 0) {
    return("selected minimum wage levels")
  }
  if (length(wage_code) > 1) {
    return("selected minimum wage levels")
  }
  wage_value <- suppressWarnings(as.integer(sub("sm", "", wage_code)))
  wage_word <- switch(
    as.character(wage_value),
    "1" = "one",
    "2" = "two",
    "5" = "five",
    "10" = "ten",
    "15" = "fifteen",
    as.character(wage_value)
  )
  if (is.na(wage_value)) {
    return(format_wage_label(wage_code))
  }
  if (wage_value == 1) {
    return(paste(wage_word, "minimum wage"))
  }
  paste(wage_word, "minimum wages")
}

format_country_phrase <- function(countries) {
  if (is.null(countries) || length(countries) == 0 || "All" %in% countries) {
    return("across countries")
  }
  if (length(countries) == 1) {
    return(paste0("in ", country_display_name(countries[1])))
  }
  "across selected countries"
}

format_tenure_label <- function(tenure_value) {
  if (is.null(tenure_value) || length(tenure_value) == 0 || is.na(tenure_value)) {
    return("")
  }
  tenure_value <- as.integer(tenure_value[1])
  if (is.na(tenure_value)) {
    return("")
  }
  if (tenure_value == 1) {
    return("1 year")
  }
  paste0(tenure_value, " years")
}

format_tenure_phrase <- function(tenure_value) {
  label <- format_tenure_label(tenure_value)
  if (label == "") {
    return("")
  }
  paste0("(tenure: ", label, ")")
}
