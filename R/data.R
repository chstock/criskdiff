#' @title
#' Myeloma dataset
#'
#' @description
#' Data from a randomized controlled trial used in
#' \insertCite{Klingenberg2014;textual}{criskdiff}.
#' The trial compared two chemotherapy treatments with respect to survival
#' (alive or dead by the end of the study; i.e. a binary endpoint) in patients
#' with multiple myeloma.
#' It was was carried out in \eqn{K=21} institutions (strata), with an average
#' of 7.4 patients per institution.
#'
#' The data was obtained from the source given below and processed via
#' \code{\link{convert_matrix_to_array}()}.
#'
#' @docType data
#' @keywords datasets
#' @name myel
#' @usage data(myel)
#' @format A 3-dimensional array as returned by
#'   \code{\link{convert_matrix_to_array}()}
#' @references
#' \insertAllCited{}
#' @source \url{https://sites.williams.edu/bklingen/files/2013/06/myel.txt}
NULL
