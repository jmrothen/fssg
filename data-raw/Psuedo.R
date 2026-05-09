# File to generate the sample dataset which is included in the package

set.seed(90837)

nrows <- 10000

times   <- rpois(nrows, 100)
times2  <- rnbinom(nrows, 100, .5)
censor  <- times < times2
atimes  <- ifelse(times<times2, times, times2)

gender  <- ifelse(rpois(nrows, 1) == 0, 'M', 'F')
age     <- rpois(nrows, 50)
region  <- sample(c('West','South','Northeast','Midwest'), nrows, replace = T)
surgery <- sample(c('Invasive','Non-invasive','None'), nrows, replace=T, prob=c(.2,.3,.5))
drug    <- sample(c('Toxinitrib', 'Curacadil','Placebomab', 'None'), nrows, replace=T, prob=c(.24, .27, .30, .19))
comorb  <- rpois(nrows, 2) + rpois(nrows,1) + rnbinom(nrows, 1, .9)
comorb_cat <- dplyr::case_when(
  comorb <= 5 ~ as.character(comorb),
  comorb >5 ~ '6+'
)


dataset <- data.frame(
  time = as.numeric(atimes),
  death = !as.logical(censor),
  # censor = as.logical(censor),
  gender= as.factor(gender),
  age = as.numeric(age),
  region = as.factor(region),
  surgery = as.factor(surgery),
  drug = as.factor(drug),
  comorb = as.factor(comorb),
  comorb_cat = as.factor(comorb_cat)
)

pseudo <- dataset

usethis::use_data(pseudo, overwrite = TRUE)
