#' Convert matrix to array
#'
#' @param mat A matrix
#'
#' @return
#' An array
#'
#' @export
#' @references
#' \insertRef{Freedman2008}{criskdiff}
#'
#' @examples
#' matrix(
#'   data = c(3,  7,  1,  4,
#'            3,  8,  8,  12,
#'            2,  3,  2,  4,
#'            2,  5,  2,  6),
#'   byrow = TRUE,
#'   ncol = 4
#' ) |>
#'   convert_matrix_to_array()
#'
convert_matrix_to_array <- function(mat) {
  # check arguments
  assert_that(is.matrix(mat))

  # auxiliary function
  convert_to_2x2_mat <- function(row) {
    matrix(c(row[1], row[2], row[3], row[4]),
           nrow = 2,
           byrow = TRUE)
  }

  # Convert matrix to array
  arr <-  array(
    data = unlist(apply(mat, 1, convert_to_2x2_mat)),
    dim = c(2, 2, nrow(mat)),
    dimnames = list(
      "group" = c("pbo", "act"),
      "outcome" = c("y", "n"),
      "stratum" = seq_along(1:nrow(mat))
    )
  )

  # return array
  return(arr)

}
