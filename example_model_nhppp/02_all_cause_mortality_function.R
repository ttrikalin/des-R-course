#' Generate death from other causes
#' 
#' @param population A data.table with the following columns:
#'   - `id`: Individual identifier
#'   - `birth_cohort`: Year of birth
#'   - `sex`: Sex
#' @param annual_mortality_rates A data.table with annual mortality rates by birth_cohort and sex 
#' @param range_t A numeric vector with the min and max years in the annual_mortality_rates

generate_death_from_other_causes <- function (
  population, 
  annual_mortality_rates, 
  range_t, 
  subinterval = range_t) {
  lambdas <- annual_mortality_rates[
    population, 
    on = c("birth_cohort", "sex")
  ]
  setindex(lambdas, "id")
  lambda_matrix <- as.matrix(lambdas[, c(paste0("age_", 0:109), "age_110+"), with = FALSE])
  return(
    nhppp::vztdraw_sc_step_regular_cpp(
      lambda_matrix = lambda_matrix,
      range_t = range_t,
      subinterval = subinterval,
      atmost1 = TRUE
    )
  )
}
