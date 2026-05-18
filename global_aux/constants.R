# =============================================================================
# constants.R - Palettes, display mappings, and app-level constants
# =============================================================================

WAGE_LEVELS <- c("1sm", "2sm", "5sm", "10sm", "15sm")
WAGE_LABELS <- paste0(sub("sm", "", WAGE_LEVELS), " MW")
WAGE_CHOICES <- stats::setNames(WAGE_LEVELS, WAGE_LABELS)

COUNTRIES_LIST <- c("All", unique(DATA_NON_SALARY$country))
COUNTRIES_LIST_WITHIN <- c("All", unique(DATA_NON_SALARY_WITHIN$country))
COUNTRIES_LIST_WITHIN_NO_TENURE <- c(
  "All",
  unique(DATA_NON_SALARY_WITHIN_NO_TENURE$country)
)
TENURE_LEVELS <- sort(unique(DATA_NON_SALARY_WITHIN$tenure))

PLOTLY_FONT_FAMILY <- paste(
  "National Park, 'Source Sans Pro',",
  "-apple-system, BlinkMacSystemFont, sans-serif"
)

COMPONENT_PALETTE <- c(
  "Pensions" = "#00C1FF",
  "Health" = "#002244",
  "Occupational Risk" = "#B9BAB5",
  "Bonuses and Benefits" = "#335B8E",
  "Payroll Taxes" = "#726AA8"
)



COMPONENT_STACK_ORDER <- c(
  "Bonuses and Benefits",
  "Pensions",
  "Health",
  "Occupational Risk",
  "Payroll Taxes"
)

COMPONENT_LEGEND_ORDER <- c(
  "Bonuses and Benefits",
  "Pensions",
  "Health",
  "Occupational Risk",
  "Payroll Taxes"
)

BONUS_PALETTE <- c(
  "Annual and other periodic bonuses" = "#002244",
  "Paid Leave" = "#8EA2BF",
  "Unemployment Protection" = "#B9BAB5",
  "Other bonuses" = "#6F6779"
)

BONUS_STACK_ORDER <- c(
  "Annual and other periodic bonuses",
  "Paid Leave",
  "Unemployment Protection",
  "Other bonuses"
)

COUNTRY_NAME_MAP <- c(
  ARG = "Argentina",
  BOL = "Bolivia",
  BRA = "Brazil",
  CHL = "Chile",
  COL = "Colombia",
  CRI = "Costa Rica",
  DOM = "Dominican Republic",
  ECU = "Ecuador",
  ESP = "Spain",
  SLV = "El Salvador",
  GTM = "Guatemala",
  HND = "Honduras",
  MEX = "Mexico",
  NIC = "Nicaragua",
  PAN = "Panama",
  PRY = "Paraguay",
  PER = "Peru",
  URY = "Uruguay",
  US4 = "US4",
  VEN = "Venezuela"
)

COUNTRY_FLAG_MAP <- c(
  ARG = "ar",
  BOL = "bo",
  BRA = "br",
  CHL = "cl",
  COL = "co",
  CRI = "cr",
  DOM = "do",
  ECU = "ec",
  ESP = "es",
  SLV = "sv",
  GTM = "gt",
  HND = "hn",
  MEX = "mx",
  NIC = "ni",
  PAN = "pa",
  PRY = "py",
  PER = "pe",
  URY = "uy",
  US4 = "us",
  VEN = "ve"
)
