#' @title
#' Common risk difference using the Klingenberg method
#'
#' @description
#' Computes the Mantel-Haenszel estimate of the common risk difference with
#' variance estimation according to
#' \insertCite{Klingenberg2014;textual}{criskdiff}.
#'
#' @param arr Array with 3 dimensions containing 2x2 tables, as returned by
#'    \code{\link{convert_matrix_to_array}()}
#' @param alpha Numeric of length 1, level \eqn{\alpha} to compute
#'   \eqn{(1-\alpha)}-confidence intervals.
#'
#' @return
#' A numeric vector with the following elements:
#'   \itemize{
#'     \item \code{est} - the Mantel-Haenszel estimate of the common risk
#'       difference,
#'     \item \code{var} - the variance of \code{est}; \code{NA} for this method,
#'     \item \code{se} - the pseudo-standard error of \code{est},
#'     \item \code{lcl} - the lower \eqn{(1-\alpha)}-confidence interval limit,
#'     \item \code{ucl} - the upper \eqn{(1-\alpha)}-confidence interval limit,
#'      and
#'     \item \code{pval} - the p-value from the test of \eqn{H_0:} risk
#'      difference \eqn{= 0} vs. risk difference \eqn{\ne 0}; \code{NA} for this
#'      method.
#'   }
#'
#' @details
#' The confidence interval may be advantageous when the distribution of risk
#' differences is skewed. It is not symmetric around the Mantel-Haenszel
#' estimate of the common risk difference.
#'
#' @export
#'
#' @references
#'   \insertAllCited{}
#'
#' @seealso [convert_matrix_to_array()] and [criskdiff_mh_sato()].
#'
#' @examples
#' data(myel)
#' criskdiff_mh_klingenberg(myel)
#'
criskdiff_mh_klingenberg <- function(arr, alpha = 0.05) {
  # Check arguments
  assert_that(is.array(arr))
  assert_that(alpha > 0 & alpha < 1)

  # Compute CMH risk difference

  ## Cochran weights
  w <- apply(
    X = arr,
    MARGIN = 3,
    FUN = function(m) prod(m[, "n"]) / sum(m[, "n"])
  )

  ## Stratum specific risk differences
  rho_hat <- apply(
    X = arr,
    MARGIN = 3,
    FUN = function(m) (m[1, 1] / m[1, 2]) - (m[2, 1] / m[2, 2])
  )
  
  ## Common MH risk difference
  rho_mh_hat <- sum(w * rho_hat) / sum(w)

  # Compute confidence interval following Klingenberg (2014)

  P <- apply(
    X = arr,
    MARGIN = 3,
    FUN = function(tab) {
      x <- tab[1, 1]
      n <- tab[1, 2]
      y <- tab[2, 1]
      m <- tab[2, 2]
      num <- n ^ 2 * y - m ^ 2 * x + n * m * (m - n) / 2
      denom <- (n + m) ^ 2
      num / denom
    }
  ) |>
    sum()

  Q <- apply(
    X = arr,
    MARGIN = 3,
    FUN = function(tab) {
      x <- tab[1, 1]
      n <- tab[1, 2]
      y <- tab[2, 1]
      m <- tab[2, 2]
      (x * (m - y) / (m + n) + y * (n - x) / (m + n)) / 2
    }
  ) |>
    sum()
  
  ## Common mid-p risk difference
  rho_midp_hat <-
    rho_mh_hat + 0.5 * stats::qchisq(1 - alpha, df = 1) * (P / sum(w) ^ 2)
  
  ## Margin of error
  me <-
    sqrt(rho_midp_hat ^ 2 - rho_mh_hat ^ 2 + stats::qchisq(1 - alpha, df = 1) *
           Q / sum(w) ^ 2)
  
  ## Confidence interval
  lcl <- rho_midp_hat - me
  ucl <- rho_midp_hat + me
  pseudo_se <- ((ucl - lcl)/2) / qnorm(1 - alpha / 2)

  # Return results
  return(c(
    "est" = rho_mh_hat,
    "var" = NA,
    "se" = pseudo_se, #2 *me / (qnorm(1 - alpha / 2)), 
    "lcl" = lcl,
    "ucl" = ucl,
    "pval" = NA
  ))

}
