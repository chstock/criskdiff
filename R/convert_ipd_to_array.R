#' @title
#' Convert individual patient-level data to 3-dimensional array
#'
#' @description
#' An auxiliary function to preprocess binomial data collected in 2 groups
#' across different strata. The input data are \emph{individual patient-level
#' data} collected across multiple strata, represented in a data frame.
#' The function returns a 3-dimensional array that is the required
#' input format for other package functions that estimate common risk
#' differences.
#'
#' @param data Data frame containing a stratification variable (factor),
#'   a treatment indicator variable (binary) and an endpoint variable (binary).
#' @param group_names Character of length 2, indicating names of treatment
#'   groups, defaults to `c("pbo", "act")`.
#'
#' @return
#' A 3-dimensional array of stratified 2x2 tables with binomial data obtained
#' in two independent groups.
#' The first column of the table contains the outcome \code{y} (e.g. number of
#' successes or responses) and the second column contains the number of trials
#' \code{n} (e.g. the number of patients). Each row in the table represents a
#' sample (e.g. a treatment group).
#'
#' @export
#' @examples
#' data(trialdat)
#' dat <- trialdat
#' head(dat)
#' arr <- convert_ipd_to_array(
#'   data = dat,
#'   stratification = c("x1", "x2"),
#'   treatment = "z",
#'   endpoint = "y"
#' )
#' arr
#'
convert_ipd_to_array <-
  function(data,
           stratification,
           treatment,
           endpoint,
           group_names = c("pbo", "act")) {
    # Check arguments
    assert_that(is.data.frame(data))
    assert_that(all(c(stratification, treatment, endpoint) %in% colnames(data)))
    assert_that(is.character(group_names))
    assert_that(length(group_names) == 2)

    # Stratification variable
    data$strat <- data[, stratification, drop = FALSE] |>
      interaction() |>
      factor() |>
      as.integer()
    strat_levels <- unique(data$strat)

    # Subset
    data <- data[, c("strat", treatment, endpoint)]

    # Initialize the result matrix
    result_matrix <-
      matrix(0, nrow = length(strat_levels), ncol = 4)
    colnames(result_matrix) <- c("y1", "n1", "y2", "n2")

    # Loop through each stratum
    for (i in seq_along(strat_levels)) {
      strat_value <- strat_levels[i]

      # Subset data for the current stratum
      subset_data <- data[data$strat == strat_value, ]

      # Calculate counts for treatment z = 0
      y1 <- sum(subset_data$y[subset_data$z == 0])
      n1 <- sum(subset_data$z == 0)

      # Calculate counts for treatment z = 1
      y2 <- sum(subset_data$y[subset_data$z == 1])
      n2 <- sum(subset_data$z == 1)

      # Store the results in the matrix
      result_matrix[i, ] <- c(y1, n1, y2, n2)
    }

    # Return array
    return(result_matrix)

  }
