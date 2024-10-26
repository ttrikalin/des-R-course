#' Generate death from other causes
#' 
#' @param population A data.table with the following columns:
#'   - `id`: Individual identifier
#'   - `birth_cohort`: Year of birth
#'   - `sex`: Sex
#' @param annual_mortality_rates A data.table with annual mortality rates by birth_cohort and sex 
#' @param rate_matrix_t_min A numeric vector with the minimum age of the rate matrix
#' @param rate_matrix_t_max A numeric vector with the maximum age of the rate matrix
#' @param t_min A numeric vector with the lower bound from which to start sampling. t_min >= rate_matrix_t_min 
#' @param t_max A numeric vector with the upper bound from which to start sampling. t_max <= rate_matrix_t_max


generate_death_from_other_causes <- function (
  population, 
  annual_mortality_rates, 
  rate_matrix_t_min, 
  rate_matrix_t_max, 
  t_min = rate_matrix_t_min, 
  t_max = rate_matrix_t_max){ 
  lambdas <- annual_mortality_rates[
    population, 
    on = c("birth_cohort", "sex")
  ]
  setindex(lambdas, "id")
  lambda_matrix <- as.matrix(lambdas[, c(paste0("age_", 0:109), "age_110+"), with = FALSE])
  return(
    nhppp::vdraw_sc_step_regular(
      lambda_matrix = lambda_matrix,
      rate_matrix_t_min = rate_matrix_t_min,
      rate_matrix_t_max = rate_matrix_t_max,
      t_min = t_min, 
      t_max = t_max,
      atmost1 = TRUE, 
      atleast1 = TRUE
    )
  )
}
