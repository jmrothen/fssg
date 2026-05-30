### CHI SQUARED
fssg_chisq <- list(
  name='chisq',
  pars=c('df'),
  location=c('df'),
  transforms=c(log),
  inv.transforms=c(exp),
  inits = function(t){c(mean(t))},
  d = stats::dchisq,
  p = stats::pchisq,
  q = quantilify(stats::pchisq),
  h = hazardify(stats::dchisq, stats::pchisq),
  H = cumhazardify(stats::pchisq),
  fullname='chi_squared'
)

### F distribution
fssg_f <- list(
  name='f',
  pars = c('df1','df2'),
  location = ('df1'),
  transforms = c(log,log),
  inv.transforms = c(exp,exp),
  inits=function(t){c(stats::median(t),2)},
  d = stats::df,
  p = stats::pf,
  q = quantilify(stats::pf),
  h = hazardify(stats::df, stats::pf),
  H = cumhazardify(stats::pf),
  fullname='f'
)


fssg_fatigue_shape <- list(
  name='fatigue',
  pars = c('alpha','beta'), # shape, scale,  (mu is omitted but can be location. set to zero by default)
  location= 'alpha',  # shape, PH effect?
  transforms = c(log,log),
  inv.transforms = c(exp,exp),
  inits = function(t){c(mean(t)/2,1)},
  d = extraDistr::dfatigue,
  p = extraDistr::pfatigue,
  q = quantilify(extraDistr::pfatigue),
  h = hazardify(extraDistr::dfatigue,extraDistr::pfatigue),
  H = cumhazardify(extraDistr::pfatigue),
  fullname='birnbaum_saunders_shape'
)

fssg_fatigue_shape_loc <- list(
  name='fatigue',
  pars = c('alpha','beta', 'mu'), # shape, scale, location
  location= 'alpha',  # we'll use shape: PH Effect?
  transforms = c(log,log, identity),
  inv.transforms = c(exp,exp, identity),
  inits = function(t){c(mean(t)/2,1,0)},
  d = extraDistr::dfatigue,
  p = extraDistr::pfatigue,
  q = quantilify(extraDistr::pfatigue),
  h = hazardify(extraDistr::dfatigue,extraDistr::pfatigue),
  H = cumhazardify(extraDistr::pfatigue),
  fullname='birnbaum_saunders_shape_location'
)


### non central chi squared
fssg_ncchisq <- list(
  name='chisq',
  pars=c('df','ncp'),  # shape scale
  location=c('ncp'),
  transforms=c(log, log),
  inv.transforms=c(exp,exp),
  inits = function(t){c(mean(t), 1)},
  d = stats::dchisq,
  p = stats::pchisq,
  q = quantilify(stats::pchisq),
  h = hazardify(stats::dchisq, stats::pchisq),
  H = cumhazardify(stats::pchisq),
  fullname='non_central_chi_squared'
)

### noncentral F
fssg_ncf <- list(
  name='f',
  pars = c('df1','df2', 'ncp'),
  location = ('ncp'),
  transforms = c(log,log,log),
  inv.transforms = c(exp,exp,exp),
  inits=function(t){c(stats::median(t),2,.1)},
  d = stats::df,
  p = stats::pf,
  q = quantilify(stats::pf),
  h = hazardify(stats::df, stats::pf),
  H = cumhazardify(stats::pf),
  fullname='noncentral_f'
)

# log cauchy scale
fssg_logcauchy <- list(
  name='logcauchy',
  pars=c('mu','sigma'),
  location='sigma',
  transforms=c(identity, log),
  inv.transforms=c(identity, exp),
  inits=function(t){c(1,1)},
  d = dlogcauchy,
  p = plogcauchy,
  q = quantilify(plogcauchy),
  h = hazardify(dlogcauchy, plogcauchy),
  H = cumhazardify(plogcauchy),
  fullname='log_cauchy'
)


### Truncated Pareto
fssg_tpareto <- list(
  name='truncpareto',
  pars=c('lower','upper','shape'),
  location='shape',
  transforms=c(identity,identity, log),
  inv.transforms=c(identity, identity, exp),
  inits=function(t){c(min(t)-.1,max(t)+1, 1)},
  d = VGAM::dtruncpareto,
  p = VGAM::ptruncpareto,
  q = quantilify(VGAM::ptruncpareto),
  h = hazardify(VGAM::dtruncpareto, VGAM::ptruncpareto),
  H = cumhazardify(VGAM::ptruncpareto),
  fullname='truncated_pareto'
)


#' Quick QQ-plot for Flexsurv outputs
#'
#' @param flexsurv_output Output from a flexsurvreg model. Can also pass along output from fssg, particularly from <fssg output>$models$<model of interest>
#' @param ... Any additional arguments for the qqplot function.
#'
#' @details Currently only works for simple models.
#'
#' @examples
#' library(survival)
#' library(flexsurv)
#'
#' flexsurvreg(Surv(time,status) ~ 1, data=cancer, dist= get_fssg_dist('gamma_gompertz')) -> model
#' fssg_qqplot(model)
#'
#' @returns Nothing, but prints the QQplot.
fssg_qqplot <- function(flexsurv_output, ...){
  q_func <- flexsurv_output$dlist$q
  times <- sort(flexsurv_output$data$m[,1])
  if(dim(flexsurv_output$res)[1]<=1){
    params <- list(flexsurv_output$res[,1])
    names(params) <- rownames(flexsurv_output$res)
  }else{
    params <- as.list(flexsurv_output$res[,1])
  }

  p_vec <- list(stats::ppoints(length(times)))

  q <- do.call(q_func, c(p=p_vec, params))
  stats::qqplot(
    q,
    sort(times[,1]),
    xlab='Theoretical Quantiles',
    ylab='Sample Quantiles',
    main=paste('QQ-Plot for', flexsurv_output$dlist$fullname[1]),
    conf.level=.95,
    conf.args=list(col='lightgrey'),
    ...
  )
  graphics::abline(0,1)
}



# @rdname check_inits
bulk_check_inits <- function(times){
  for(i in fssg_dist_list()){
    print(i$name)
    tryCatch({
      check_inits(times, i) %>% print()
    },
    error=function(e){print(e)}
    )
  }
}

