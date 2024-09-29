#' @title
#' Score-based common risk difference and interval estimation
#'
#' @description
#' Computes a score-based common risk difference estimate and confidence
#' limits based on stratum risk differences estimated according to
#' \insertCite{Miettinen1985;textual}{criskdiff} and
#' \insertCite{Mee1984;textual}{criskdiff}.
#'
#' @param arr Array with 3 dimensions containing 2x2 tables, as returned by
#'    \code{\link{convert_matrix_to_array}()}.
#' @param alpha Numeric of length 1, level \eqn{\alpha} to compute
#'   \eqn{(1-\alpha)}-confidence intervals.
#'
#' @return
#' A numeric vector with the following elements:
#'   \itemize{
#'     \item \code{est} - the score-based estimate of the common risk
#'       difference (risk in group 1 (first row) minus risk in group 2 (second
#'       row)),
#'     \item \code{var} - the variance of \code{est},
#'     \item \code{se} - the standard error of \code{est},
#'     \item \code{lcl} - the lower \eqn{100(1-\alpha)\%}-confidence interval
#'     limit,
#'     \item \code{ucl} - the upper \eqn{100(1-\alpha)\%}-confidence interval
#'     limit, and
#'     \item \code{pval} - the p-value from the (two-sided) test of
#'       \eqn{H_0:} risk difference \eqn{= 0} vs. risk difference \eqn{\ne 0};
#'       here \code{NA}.
#'   }
#'
#' @export
#'
#' @references
#'   \insertAllCited{}
#'
#' @examples
#' data(myel)
#' criskdiff_score(myel)
#'
criskdiff_score <- function(arr, alpha = 0.05) {
  # Check arguments
  assert_that(is.array(arr))
  assert_that(alpha > 0 & alpha < 1)

  # Function to compute score-based confidence limits for one stratum
  score_mn <- function(mat) {
    # Check arguments
    assert_that(all(dim(mat) == c(2, 2)))

    # Score interval for the risk difference in one stratum
    score_ci <- PropCIs::diffscoreci(
      x1 = mat[1, 1],
      n1 = mat[1, 2],
      x2 = mat[2, 1],
      n2 = mat[2, 2],
      conf.level = 1 - alpha
    )$conf.int
    lcl <- score_ci[1]
    ucl <- score_ci[2]

    # Additional quantities (to compute the summary score estimate)
    d_prime <- (ucl + lcl) / 2
    s_prime <- (ucl - lcl) / (2 * qnorm(alpha / 2))

    # Return results
    return(c(
      "d_prime" = d_prime,
      "s_prime" = s_prime,
      "lcl" = lcl,
      "ucl" = ucl,
      "alpha" = alpha
    ))

  }

  # Stratum-specific results
  stratum_riskdiffs <- apply(X = arr,
                             MARGIN = 3,
                             FUN = score_mn) |>
    t()
  d_prime_h <- stratum_riskdiffs[, c("d_prime")]
  s_prime_h <- stratum_riskdiffs[, c("s_prime")]

  # Summary score estimate
  w_prime_h <- (1 / s_prime_h ^ 2) / sum(1 / s_prime_h ^ 2)
  d_S <- sum(d_prime_h * w_prime_h)
  var_d_S <- 1 / sum(1 / s_prime_h ^ 2)
  lcl <- d_S + qnorm(alpha / 2) * sqrt(var_d_S)
  ucl <- d_S - qnorm(alpha / 2) * sqrt(var_d_S)

  # Return results
  return(c(
    "est" = d_S,
    "var" = var_d_S,
    "se" = sqrt(var_d_S),
    "lcl" = lcl,
    "ucl" = ucl,
    "pval" = NA
  ))

}
