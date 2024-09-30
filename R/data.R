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


#' @title
#' Simulated trial dataset
#'
#' @description
#' Simulated (hypothetical) individual patient data from a clinical trial with
#' 90 rows (patients) and 5 columns (variables). Columns 1 and 2 (\code{x1} and
#' \code{x2}) represent binary baseline stratification variables. Column 3
#' (\code{x3}) represents a continuous baseline variable. Column 4 (\code{z})
#' is a binary treatment indicator and column 5 (\code{y}) is the binary
#' response endpoint.
#'
#' The marginal proportions are as follows:
#'
#' | Treatment | Count | Marginal proportion (\code{y}=1) |
#' | :-: | :-: | :-: |
#' |  \code{z = 0}  |  30 | 0.23 |
#' |  \code{z = 1}  |  60 | 0.55 |
#'
#' @docType data
#' @keywords datasets
#' @name trialdat
#' @format Object of class "tibble" (90 x 5).
#' @usage data(trialdat)
NULL
