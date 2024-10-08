#' @title
#' Common risk difference with variance estimation using the Sato method
#'
#' @description
#' Computes the Mantel-Haenszel estimate of the common risk difference with
#' variance estimation according to \insertCite{Sato1989;textual}{criskdiff}.
#'
#' @param arr Array with 3 dimensions containing 2x2 tables, as returned by
#'    \code{\link{convert_aggregated_matrix_to_array}()} or
#'    \code{\link{convert_ipd_to_array}()}.
#' @param alpha Numeric of length 1, level \eqn{\alpha} to compute
#'   \eqn{(1-\alpha)}-confidence intervals.
#'
#' @return
#' A numeric vector with the following elements:
#'   \itemize{
#'     \item \code{est} - the Mantel-Haenszel estimate of the common risk
#'       difference (risk in group 1 (first row) minus risk in group 2 (second
#'       row)),
#'     \item \code{var} - the variance of \code{est},
#'     \item \code{se} - the standard error of \code{est},
#'     \item \code{lcl} - the lower \eqn{100(1-\alpha)\%}-confidence interval
#'     limit,
#'     \item \code{ucl} - the upper \eqn{100(1-\alpha)\%}-confidence interval
#'     limit, and
#'     \item \code{pval} - the p-value from the (two-sided) \emph{z}-test of
#'       \eqn{H_0:} risk difference \eqn{= 0} vs. risk difference \eqn{\ne 0}.
#'   }
#'
#' @export
#'
#' @references
#'   \insertAllCited{}
#'
#' @seealso [convert_aggregated_matrix_to_array()], 
#'  [criskdiff_mh_klingenberg()] and
#'  [criskdiff_score()].
#'
#' @examples
#' data(myel)
#' criskdiff_mh_sato(myel)
#'
criskdiff_mh_sato <- function(arr, alpha = 0.05) {
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
  delta_hat <- apply(
    X = arr,
    MARGIN = 3,
    FUN = function(m) (m[1, 1] / m[1, 2]) - (m[2, 1] / m[2, 2])
  )

  ## Common risk difference
  delta_mh_hat <- sum(w * delta_hat) / sum(w)

  # Compute variance following Sato (1989)

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
  )

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
  )

  denom <- apply(
    X = arr,
    MARGIN = 3,
    FUN = function(tab) {
      n <- tab[1, 2]
      m <- tab[2, 2]
      n * m / (n + m)
    }
  ) |>
    (\(x) sum(x) ^ 2)()

  var_sato <- (delta_mh_hat * sum(P) + sum(Q)) / denom

  # Return results
  return(c(
    "est" = delta_mh_hat,
    "var" = var_sato,
    "se" = sqrt(var_sato),
    "lcl" = delta_mh_hat - qnorm(1 - alpha / 2) * sqrt(var_sato),
    "ucl" = delta_mh_hat + qnorm(1 - alpha / 2) * sqrt(var_sato),
    "pval" =  2 * (1 - pnorm(abs(delta_mh_hat / sqrt(var_sato))))
  ))

}
