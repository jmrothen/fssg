#' Pseudo: Simulated data for experimentation
#'
#' This data is entirely fabricated. The source code for creating this data set can be found in the data-raw folder of the github repository.
#'
#'
#' @format
#' \describe{
#'  \item{time}{Time until event.}
#'  \item{death}{Indicator for death. If TRUE, the patient died at corresponding time.}
#'  \item{gender, age, region, surgery, drug, comorb, comorb_cat}{Arbitrary covariates.}
#' }
#' @source fssg
#' @examples head(pseudo)
"pseudo"
