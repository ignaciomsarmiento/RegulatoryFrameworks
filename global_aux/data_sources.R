# =============================================================================
# data_sources.R - Labor module data source bundles
# =============================================================================

LABOR_DATA_SOURCES_ACROSS <- list(
  tabla = DATA_TABLA,
  non_salary = DATA_NON_SALARY,
  non_salary_payer = DATA_NON_SALARY_PAYER,
  non_salary_component = DATA_NON_SALARY_COMPONENT,
  group_data = DATA_BY_GROUP,
  component_data = DATA_BY_COMPONENT,
  payer_data = DATA_BY_PAYER,
  countries = COUNTRIES_LIST,
  bonus_hover_source = DATA_BY_GROUP$bonuses_and_benefits
)

LABOR_DATA_SOURCES_WITHIN <- list(
  tabla = DATA_TABLA,
  non_salary = DATA_NON_SALARY_WITHIN,
  non_salary_payer = DATA_NON_SALARY_PAYER_WITHIN,
  non_salary_component = DATA_NON_SALARY_COMPONENT_WITHIN,
  group_data = DATA_BY_GROUP_WITHIN,
  component_data = DATA_BY_COMPONENT_WITHIN,
  payer_data = DATA_BY_PAYER_WITHIN,
  countries = COUNTRIES_LIST_WITHIN,
  bonus_hover_source = DATA_BY_GROUP_WITHIN$bonuses_and_benefits
)

LABOR_DATA_SOURCES_WITHIN_NO_TENURE <- list(
  tabla = DATA_TABLA,
  non_salary = DATA_NON_SALARY_WITHIN_NO_TENURE,
  non_salary_payer = DATA_NON_SALARY_PAYER_WITHIN_NO_TENURE,
  non_salary_component = DATA_NON_SALARY_COMPONENT_WITHIN_NO_TENURE,
  group_data = DATA_BY_GROUP_WITHIN_NO_TENURE,
  component_data = DATA_BY_COMPONENT_WITHIN_NO_TENURE,
  payer_data = DATA_BY_PAYER_WITHIN_NO_TENURE,
  countries = COUNTRIES_LIST_WITHIN_NO_TENURE,
  bonus_hover_source = DATA_BY_GROUP_WITHIN_NO_TENURE$bonuses_and_benefits
)
