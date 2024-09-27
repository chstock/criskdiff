#' @title
#' Convert matrix to 3-dimensional array
#'
#' @description
#' An auxiliary function to preprocess binomial data collected in 2 groups
#' across different strata, and represented in a 2-dimensional matrix.
#' It returns a 3-dimensional array that is the required input format for
#' other package functions that estimate common risk differences.
#'
#' @param mat Matrix with 4 columns.
#' Each row contains the data collected in a stratum; hence the number of rows
#' equals the number of strata.
#' The data in the columns are required to be as follows:
#'  \itemize{
#'    \item Column 1 - Number of successes in group1,
#'    \item Column 2 - Number of trials in group1,
#'    \item Column 3 - Number of successes in group2, and
#'    \item Column 4 - Number of trials in group2.
#'  }
#' @param group_names Character of length 2, indicating names of treatment
#'   groups, defaults to `c("pbo", "act")`.
#'
#' @return
#' A 3-dimensional array of stratified 2x2 tables.
#'
#' @export
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
convert_matrix_to_array <-
  function(mat, group_names = c("pbo", "act")) {
    # check arguments
    assert_that(is.matrix(mat))
    assert_that(is.character(group_names))
    assert_that(length(group_names) == 2)

    # auxiliary function
    convert_to_2x2_mat <- function(row) {
      matrix(c(row[1], row[2], row[3], row[4]),
             nrow = 2,
             byrow = TRUE)
    }

    # convert matrix to array
    arr <-  array(
      data = unlist(apply(mat, 1, convert_to_2x2_mat)),
      dim = c(2, 2, nrow(mat)),
      dimnames = list(
        "group" = group_names,
        "outcome" = c("y", "n"),
        "stratum" = seq_len(nrow(mat))
      )
    )

    # return array
    return(arr)

  }
