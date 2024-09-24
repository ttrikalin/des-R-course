#' Nonparametric sampling of time to events from a discrete nonhomogeneous 
#' Poisson point process
#'
#' \code{nps_nhppp} samples states for multiple individuals simultaneously.
#' 
#' The elements of each row of the matrix `m_probs` should sum up to 1. In case
#' this does not happens, all the rows where this condition is not met will be
#' normalized.
#'
#' @param m_probs matrix with probabilities for each category. Each row 
#' represents one individual and each column an specific category. 
#' @param v_categories An optional argument. It is a vector containing the name of 
#' the categories to sample from. The length of this vector must be equal to 
#' the number of columns of `m_probs`.
#' @return A vector filled with sampled categories.
#' @export
nps_nhppp <- function(m_probs, 
                      v_categories = NULL, 
                      correction = c("none", "uniform")) {
  
  if (!is.numeric(m_probs)) {
    stop("`m_probs` must be a matrix filled with numeric elements")
  }
  
  if (!isTRUE(all.equal(target = rep(1, nrow(m_probs)), 
                        current = as.numeric(rowSums(m_probs))))) {
    
    #* Find rows where the sum of their elements is not equal to 1.
    #* We use a very small value to check that they are equal to 1 since
    #* == and != sometimes do not work as desired. The value 1.5e-8
    #* was taken as the same level of tolerance as the `all.equal` function.
    v_rows_to_norm <- which(abs(1 - rowSums(m_probs)) > 1.5e-8)
    
    warning(
      "The rows: ", 
      paste(as.character(v_rows_to_norm), collapse = ", "), 
      " in `m_probs` do not sum up to 1. The values within these rows will be normalized and then used in the sampling process. In case this behaviour is not desired modify the content of `m_probs`.")
    
    # Normalize rows
    if (length(v_rows_to_norm) == 1) {
      m_probs[v_rows_to_norm, ] <- m_probs[v_rows_to_norm, ]/sum(m_probs[v_rows_to_norm, ])
    }
    if (length(v_rows_to_norm) > 1) {
      m_probs[v_rows_to_norm, ] <- m_probs[v_rows_to_norm, ]/rowSums(m_probs[v_rows_to_norm, ])
    }
  }
  
  # Number of categories to sample from
  n_cat <- ncol(m_probs)
  
  if (is.null(v_categories)) {
    #' Generate the numeric categories based on the number of columns of 
    #' `m_probs`
    v_categories <- seq(0, (n_cat - 1))
  }
  
  correction <- match.arg(correction)
  
  # Number of time to events to draw
  n_samp <- nrow(m_probs)
  
  v_unif <- runif(n_samp, min = 0, max = 1)
  v_sum_p <- matrixStats::rowCumsums(m_probs)
  v_time_to_event <- v_categories[max.col(v_sum_p >= v_unif, 
                                          ties.method = "first")]
  
  if (correction == "uniform") {
    v_time_to_event <- v_time_to_event + v_unif
  }
  return(v_time_to_event)
  # 
  # d <- dim(m_prob)
  #   n <- d[1]
  #   k <- d[2]
  #   lev <- dimnames(m_prob)[[2]]
  #   if (!length(lev)) 
  #     lev <- 1:k
  #   v_time_to_event <- matrix(lev[1], ncol = m, nrow = n)
  #   U <- t(m_prob)
  #   for(i in 2:k) {
  #     U[i, ] <- U[i, ] + U[i - 1, ]
  #   }
  #   if (any((U[k, ] - 1) > 1e-05))
  #     stop("error in multinom: probabilities do not sum to 1")
  #   
  #   for (j in 1:m) {
  #     un <- rep(runif(n), rep(k, n))
  #     v_time_to_event[, j] <- lev[1 + colSums(un > U)]
  #   }
}

#' Obtain probabilities of events by Age, and Sex from lifetable rates
#'
#' \code{obtain_rates_des} obtaien probabilities of events by Age, and Sex from lifetable rates
#' 
#' The elements of each row of the data.frame should sum up to 1. 
#'
#' @param df_mortality_rate_long data.frame with mortality rates by Age and Sex
#' @return A data.frame with probabilities of events by Age and Sex
#' @export
obtain_probs_des <- function(df_mortality_rate_long){
  max_age <- max(df_mortality_rate_long$Age)
  
  df_mortrate_expanded <- data.frame()
  
  for (sex in unique(df_mortality_rate_long$Sex)) {
    # Load and filter base dataset --------------------------------------------
    df_mortrate <- df_mortality_rate_long %>%
      # Sex has to be changed in each iteration
      filter(Sex == sex) %>%
      group_by(Year) %>% 
      mutate(death_prob_cum = 1 - exp(-cumsum(death_rate)),
             death_prob = death_prob_cum - lag(death_prob_cum)
             # death_prob = c(death_prob_cum[1], diff(death_prob_cum))
             # death_prob = diff(c(0, death_prob_cum))
      ) %>%
      ungroup() %>%
      mutate(death_prob = if_else(condition = Age == 0,
                                  true = death_prob_cum,
                                  false = death_prob))
    
    # Create a dataframe with the transposed death probabilities
    df_DP <- matrix(data = df_mortrate$death_prob,
                    ncol = length(unique(df_mortrate$Age)),
                    byrow = TRUE) %>%
      as.data.frame() %>%
      # Repeat eachrow by the total number of ages (110)
      slice(rep(1:n(),
                each = length(unique(df_mortrate$Age))))
    
    # Rename columns
    colnames(df_DP) <- paste0("DP", 0:max_age)
    
    # Create a upper triangular matrix, then df, for the 110 ages
    df_tri <- (upper.tri(x = matrix(data = 1,
                                    nrow = length(unique(df_mortrate$Age)),
                                    ncol = length(unique(df_mortrate$Age))
    ),
    diag = TRUE)*1) %>%
      as.data.frame()
    
    colnames(df_tri) <- paste0("V", 0:max_age)
    
    # stack arrays vertically
    df_tri_exp <- do.call("rbind",
                          replicate(n = (nrow(df_DP)/length(unique(df_mortrate$Age))),
                                    expr = df_tri,
                                    simplify = FALSE))
    
    # Check that all dataframes have the correct dimensions
    dim(df_mortrate)
    dim(df_DP)
    dim(df_tri_exp)
    
    name_last_call_DP <- colnames(df_DP)[max_age + 1]
    name_last_call_tri <- colnames(df_tri)[max_age + 1]
    # Data Manipulation -------------------------------------------------------
    df_mortrate_onesex <- df_mortrate %>%
      bind_cols(df_DP, df_tri_exp) %>%
      # Make multiplication to obtain
      mutate(across(DP0:eval(name_last_call_DP)) * across(V0:eval(name_last_call_tri))) %>%
      # Remove the upper triangle matrix
      select(-c(V0:eval(name_last_call_tri))) %>%
      # Obtain proportion of probability in relation to the rowsum
      mutate(across(.cols = DP0:eval(name_last_call_DP)) / rowSums(across(.cols = DP0:eval(name_last_call_DP)))) %>%
      # Replace the NA's by 0
      replace(is.na(.), 0)
    df_mortrate_expanded <- bind_rows(df_mortrate_expanded, df_mortrate_onesex)
  }
  
  return(df_mortrate_expanded)
}

#' Calculate discounted outcome
#'
#' \code{calc_discount} computes discounted outcome.
#'
#' @param x Quantity to be discounted.
#' @param d_factor Annual discount rate.
#' @param elapsed_time Elapsed time.
#' @return 
#' Discounted quantity
#' @export
calc_discount <- function(x, disc_factor, time_init, time_fin){
  if (disc_factor > 0) {
    # x * (1-exp(-disc_factor*(time_fin - time_init)))/disc_factor
    x * (exp(-disc_factor*time_init) - exp(-disc_factor*time_fin) )/disc_factor
  } else{
    x * (time_fin - time_init)
  }
}