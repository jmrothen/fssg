#' Pseudo: Simulated data for experimentation
#'
#' Data is entirely fabricated. The source code for creating this data set can be found in the data-raw folder.
#'
#' @format
#' \describe{
#'  \item{time}{Time until event.}
#'  \item{death}{Indicator for death. If TRUE, the patient died at corresponding time.}
#'  \item{gender, age, region, surgery, drug, comorb, comorb_cat}{Arbitrary covariates.}
#' }
#' @source fssg
"pseudo"
