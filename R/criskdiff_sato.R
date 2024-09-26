#' Computing risk difference using Sato method
#'
#' @param arr array
#'
#' @details
#' \insertCite{Sato1989;textual}{criskdiff}
#' 
#'
#' @return results
#' @export
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @examples
#' data(myel)
#' criskdiff_sato(myel)
#' 
criskdiff_sato <- function(arr) {
  # Check arguments
  assert_that(is.array(arr))
  
  # Compute CMH risk difference
  
  ## Cochran weights
  w <- apply(
    X = arr,
    MARGIN = 3,
    FUN = function(m)
      prod(m[, "n"]) / sum(m[, "n"])
  )
  
  ## Stratum specific risk differences
  rho_hat <- apply(
    X = arr,
    MARGIN = 3,
    FUN = function(m) (m[1, 1] / m[1, 2]) - (m[2, 1] / m[2, 2])
  )
  
  ## Common risk difference
  rho_mh_hat <- sum(w * rho_hat) / sum(w)
  
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
      (x * (m-y) / (m+n) + y*(n-x)/ (m+n) ) / 2
    }
  )
  
  denom <- apply(
    X = arr,
    MARGIN = 3,
    FUN = function(tab) {
      n <- tab[1, 2]
      m <- tab[2, 2]
      n*m/(n+m)
    }
  ) |>
    (\(x) sum(x) ^ 2)()
  
  var_sato <- (rho_mh_hat * sum(P) + sum(Q)) / denom
  
  # Return results
  return(c(
    "est" = rho_mh_hat,
    "var" = var_sato,
    "se" = sqrt(var_sato)
  ))
  
}
