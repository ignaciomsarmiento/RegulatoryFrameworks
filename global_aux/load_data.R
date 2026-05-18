# =============================================================================
# load_data.R - Preload RDS and Excel data for the Shiny app
# =============================================================================

# --- Paths ---
P_CROSS <- "data/non_salary/cross"
P_WT    <- "data/non_salary/within/wage_tenure_variation"
P_W     <- "data/non_salary/within/wage_variation"
EXCEL_TABLES_PATH <- "data/non_salary/tables/excel/tables.xlsx"
EXCEL_TABLE_SHEETS <- c("TL All B","TL ab","TL pl","TL up","TL ob","TL Or","TL H","TL Pt","TL All P")

# --- Helpers (fail loud) ---
load_rds <- function(path, label) {
  if (!file.exists(path)) {
    stop(sprintf("[load_data_v2] Missing RDS for %s at: %s", label, path))
  }
  obj <- readRDS(path)
  if (is.null(obj)) {
    stop(sprintf("[load_data_v2] Loaded NULL for %s from: %s", label, path))
  }
  obj
}

load_excel_sheet <- function(path, sheet, label) {
  if (!file.exists(path)) {
    stop(sprintf("[load_data_v2] Missing Excel file for %s at: %s", label, path))
  }
  df <- readxl::read_excel(path, sheet = sheet)
  if (is.null(df)) {
    stop(sprintf("[load_data_v2] Failed to read %s (sheet '%s') from: %s", label, sheet, path))
  }
  df
}

ensure_total_type_column <- function(df) {
  if (!"type" %in% names(df) && "min_max_total" %in% names(df)) {
    df <- dplyr::mutate(df, type = min_max_total)
  }
  df
}

# --- Cross / main bare vars ---
DATA_NON_SALARY           <- ensure_total_type_column(load_rds(file.path(P_CROSS, "total_non_salary_costs.rds"),        "DATA_NON_SALARY"))
DATA_NON_SALARY_PAYER     <- load_rds(file.path(P_CROSS, "total_ns_costs_by_payer.rds"),       "DATA_NON_SALARY_PAYER")
DATA_NON_SALARY_COMPONENT <- load_rds(file.path(P_CROSS, "total_ns_costs_by_component.rds"),   "DATA_NON_SALARY_COMPONENT")
DATA_TABLA                <- load_rds(file.path(P_CROSS, "bonuses_and_benefits_component.rds"), "DATA_TABLA")

# --- Within (wage + tenure) bare vars ---
DATA_NON_SALARY_WITHIN           <- ensure_total_type_column(load_rds(file.path(P_WT, "total_non_salary_costs_within_tenure.rds"),      "DATA_NON_SALARY_WITHIN"))
DATA_NON_SALARY_PAYER_WITHIN     <- load_rds(file.path(P_WT, "total_ns_costs_by_payer_within_tenure.rds"),     "DATA_NON_SALARY_PAYER_WITHIN")
DATA_NON_SALARY_COMPONENT_WITHIN <- load_rds(file.path(P_WT, "total_ns_costs_by_component_within_tenure.rds"), "DATA_NON_SALARY_COMPONENT_WITHIN")

# --- Within (wage only) bare vars ---
DATA_NON_SALARY_WITHIN_NO_TENURE           <- ensure_total_type_column(load_rds(file.path(P_W, "total_non_salary_costs_within.rds"),      "DATA_NON_SALARY_WITHIN_NO_TENURE"))
DATA_NON_SALARY_PAYER_WITHIN_NO_TENURE     <- load_rds(file.path(P_W, "total_ns_costs_by_payer_within.rds"),     "DATA_NON_SALARY_PAYER_WITHIN_NO_TENURE")
DATA_NON_SALARY_COMPONENT_WITHIN_NO_TENURE <- load_rds(file.path(P_W, "total_ns_costs_by_component_within.rds"), "DATA_NON_SALARY_COMPONENT_WITHIN_NO_TENURE")

# --- DATA_BY_GROUP (cross) ---
DATA_BY_GROUP <- list(
  pensions             = load_rds(file.path(P_CROSS, "pensions_all.rds"),              "DATA_BY_GROUP$pensions"),
  health               = load_rds(file.path(P_CROSS, "health_all.rds"),                "DATA_BY_GROUP$health"),
  occupational_risk    = load_rds(file.path(P_CROSS, "occupational_risk_all.rds"),     "DATA_BY_GROUP$occupational_risk"),
  bonuses_and_benefits = load_rds(file.path(P_CROSS, "bonuses_and_benefits_all.rds"),  "DATA_BY_GROUP$bonuses_and_benefits"),
  payroll_taxes        = load_rds(file.path(P_CROSS, "payroll_taxes_all_distinct.rds"), "DATA_BY_GROUP$payroll_taxes")
)

# --- DATA_BY_GROUP_WITHIN (wage + tenure) ---
DATA_BY_GROUP_WITHIN <- list(
  pensions             = load_rds(file.path(P_WT, "pensions_within_tenure.rds"),             "DATA_BY_GROUP_WITHIN$pensions"),
  health               = load_rds(file.path(P_WT, "health_within_tenure.rds"),               "DATA_BY_GROUP_WITHIN$health"),
  occupational_risk    = load_rds(file.path(P_WT, "occupational_risk_within_tenure.rds"),    "DATA_BY_GROUP_WITHIN$occupational_risk"),
  bonuses_and_benefits = load_rds(file.path(P_WT, "bonuses_and_benefits_within_tenure.rds"), "DATA_BY_GROUP_WITHIN$bonuses_and_benefits"),
  payroll_taxes        = load_rds(file.path(P_WT, "payroll_taxes_within_tenure.rds"),        "DATA_BY_GROUP_WITHIN$payroll_taxes")
)

# --- DATA_BY_GROUP_WITHIN_NO_TENURE (wage only) ---
DATA_BY_GROUP_WITHIN_NO_TENURE <- list(
  pensions             = load_rds(file.path(P_W, "pensions_within.rds"),             "DATA_BY_GROUP_WITHIN_NO_TENURE$pensions"),
  health               = load_rds(file.path(P_W, "health_within.rds"),               "DATA_BY_GROUP_WITHIN_NO_TENURE$health"),
  occupational_risk    = load_rds(file.path(P_W, "occupational_risk_within.rds"),    "DATA_BY_GROUP_WITHIN_NO_TENURE$occupational_risk"),
  bonuses_and_benefits = load_rds(file.path(P_W, "bonuses_and_benefits_within.rds"), "DATA_BY_GROUP_WITHIN_NO_TENURE$bonuses_and_benefits"),
  payroll_taxes        = load_rds(file.path(P_W, "payroll_taxes_within.rds"),        "DATA_BY_GROUP_WITHIN_NO_TENURE$payroll_taxes")
)

# --- DATA_BY_COMPONENT (cross) ---
DATA_BY_COMPONENT <- list(
  bonuses_and_benefits = DATA_TABLA
)

# --- DATA_BY_COMPONENT_WITHIN (wage + tenure) ---
DATA_BY_COMPONENT_WITHIN <- list(
  bonuses_and_benefits = load_rds(file.path(P_WT, "bonuses_and_benefits_by_component_within_tenure.rds"), "DATA_BY_COMPONENT_WITHIN$bonuses_and_benefits"),
  payroll_taxes        = load_rds(file.path(P_WT, "payroll_taxes_by_component_within_tenure.rds"),        "DATA_BY_COMPONENT_WITHIN$payroll_taxes")
)

# --- DATA_BY_COMPONENT_WITHIN_NO_TENURE (wage only) ---
DATA_BY_COMPONENT_WITHIN_NO_TENURE <- list(
  bonuses_and_benefits = load_rds(file.path(P_W, "bonuses_and_benefits_by_component_within.rds"), "DATA_BY_COMPONENT_WITHIN_NO_TENURE$bonuses_and_benefits"),
  payroll_taxes        = load_rds(file.path(P_W, "payroll_taxes_by_component_within.rds"),        "DATA_BY_COMPONENT_WITHIN_NO_TENURE$payroll_taxes")
)

# --- DATA_BY_PAYER (cross) ---
DATA_BY_PAYER <- list(
  pensions      = load_rds(file.path(P_CROSS, "pensions_payer.rds"),                "DATA_BY_PAYER$pensions"),
  health        = load_rds(file.path(P_CROSS, "health_payer.rds"),                  "DATA_BY_PAYER$health"),
  payroll_taxes = load_rds(file.path(P_CROSS, "payroll_taxes_by_payer_distinct.rds"), "DATA_BY_PAYER$payroll_taxes")
)

# --- DATA_BY_PAYER_WITHIN (wage + tenure) ---
DATA_BY_PAYER_WITHIN <- list(
  pensions      = load_rds(file.path(P_WT, "pensions_by_payer_within_tenure.rds"),      "DATA_BY_PAYER_WITHIN$pensions"),
  health        = load_rds(file.path(P_WT, "health_by_payer_within_tenure.rds"),        "DATA_BY_PAYER_WITHIN$health"),
  payroll_taxes = load_rds(file.path(P_WT, "payroll_taxes_by_payer_within_tenure.rds"), "DATA_BY_PAYER_WITHIN$payroll_taxes")
)

# --- DATA_BY_PAYER_WITHIN_NO_TENURE (wage only) ---
DATA_BY_PAYER_WITHIN_NO_TENURE <- list(
  pensions      = load_rds(file.path(P_W, "pensions_by_payer_within.rds"),      "DATA_BY_PAYER_WITHIN_NO_TENURE$pensions"),
  health        = load_rds(file.path(P_W, "health_by_payer_within.rds"),        "DATA_BY_PAYER_WITHIN_NO_TENURE$health"),
  payroll_taxes = load_rds(file.path(P_W, "payroll_taxes_by_payer_within.rds"), "DATA_BY_PAYER_WITHIN_NO_TENURE$payroll_taxes")
)

# --- Excel tables ---
EXCEL_TABLES <- setNames(
  lapply(EXCEL_TABLE_SHEETS, function(s) {
    load_excel_sheet(EXCEL_TABLES_PATH, s, sprintf("EXCEL_TABLES['%s']", s))
  }),
  EXCEL_TABLE_SHEETS
)

get_excel_table <- function(sheet_name) EXCEL_TABLES[[sheet_name]]

message("Loaded 39 RDS objects + 9 Excel sheets.")
