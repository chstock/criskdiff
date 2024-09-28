#' Miettinen-Nurminen (Score) Confidence Limits for a Single Risk Difference
#'
#' @description
#' Computes score-based confidence limits for a single risk difference
#' according to \insertCite{Miettinen1985;textual}{criskdiff}.
#'
#' @param mat A 2x2 matrix
#' @param alpha Numeric of length 1, level \eqn{\alpha} to compute
#'   \eqn{(1-\alpha)}-confidence intervals.
#'
#' @return
#' A vector
#' 
#' @export
#'
#' @references
#'   \insertAllCited{}
#'
#' @examples
#' mat <- matrix(data = c(5, 15, 10, 20),
#'               nrow = 2,
#'               byrow = TRUE)
#' mn_score(mat)
#' 
mn_score <- function(mat, alpha = 0.05){
  # Check arguments
  assert_that(is.array(mat))
  assert_that(all(dim(mat) == c(2, 2)))
  assert_that(alpha > 0 & alpha < 1)
  
  # Compute Miettinen-Nurminen (score) confidence limits
  
  ## Risks and risk differences
  p1 <- mat[1, 1] / mat[1, 2]
  p2 <- mat[2, 1] / mat[2, 2]
  d_hat <- p1 - p2
  
  ## Score-based test statistic
  
}