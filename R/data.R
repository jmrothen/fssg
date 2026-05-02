#' Pseudo: Simulated data for experimentation
#'
#' Data is entirely fabricated. The source code for creating this data set can be found in the data-raw folder.
#'
#' @format
#' \describe{
#'  \item{time}{Time until event.}
#'  \item{censor}{Indicator for censorship. If TRUE, the patient was censored at corresponding time.}
#'  \item{gender, age, region, surgery, drug, comorb, comorb_cat}{Arbitrary covariates.}
#' }
#' @source <authors>
"pseudo"
