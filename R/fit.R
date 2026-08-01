#' Collect fit statistics for a parametric survival model
#'
#' @param model Model object. Currently formatted to work with `flexsurvreg` objects, and may work for other types of models.
#' @param ibs Logical. If True, calculates the integrated Brier Score, which is a helpful fit statistic but is *much* slower to calculate than all other statistics.
#'
#' @returns List of fit statistics for the model.
#'
#' Please note that for concordance and AUC statistics, the ranks are arbitrarily sorted in one direction regardless of PH/AFT specification of the model.
#' To account for this, the statistics returned are the max of (statistic, 1-statistic), which should always provide the correct value regardless of rank sort order.
#'
#' @examples
#' library(survival)
#' library(flexsurv)
#'
#' flexsurvreg(Surv(time,status) ~ age +sex, data=cancer, dist= 'weibull') -> model
#' get_fit_stats(model = model, ibs = FALSE)
#'
#' @details
#' For a table of fit statistics and their sources, please see the vignette \code{vignette("Fit_Statistics")} for more details.
#'
#' @export
get_fit_stats <- function(model, ibs=FALSE){
  # Rely on these three packages for statistics
  requireNamespace('survival')
  requireNamespace('survAUC')
  requireNamespace('SurvMetrics')

  Surv_object <- model$data$m[[1]]

  # quick sub-function for deconstructing {predict.flexsurvreg(type='survival')}
  debulk_survprob <- function(list_of_tibbles){
    base_times <- list_of_tibbles$.pred[[1]]$.eval_time
    same_grid <- all(
      sapply(list_of_tibbles$.pred, function(x)
        identical(x$.eval_time, base_times))
    )
    if(!same_grid){
      stop("Not all patients share identical .eval_time grid")
    }
    surv_mat <- do.call(
      rbind,
      lapply(list_of_tibbles$.pred, function(x) x$.pred_survival)
    ) %>% as.matrix()
    return(list(
      sp_matrix = surv_mat,
      IRange = base_times
    ))
  }

  # Four time-flavors: observed times, IQR, 10-90, 1:max (aka full)
  quants <- stats::quantile(Surv_object, c(.1, .9, .25, .75))$quantile %>% as.numeric()

  times       <- Surv_object[,1] %>% as.numeric() %>% unique() %>% sort()
  max_time    <- max(times)
  med_time    <- stats::median(times)
  avg_time    <- mean(times)

  # alternative time frames
  times_iqr   <- seq(quants[3], quants[4], 1)
  times_wide  <- seq(quants[1], quants[2], 1)
  times_full  <- seq(1, max_time, 1)

  # Old prediction method
  # preds      <- tryCatch(as.vector(unlist(stats::predict(model))), error=function(e){
  #   as.vector(unlist(stats::predict(model, type='rmst')$.pred_rmst)) # use RMST if there are errors in predictions
  # })

  preds <- as.vector(unlist(stats::predict(model, type='quantile', p=.5)$.pred_quantile))

  # survival rate predictions at {times}, used in IAE, ISE, IBS
  survprob   <- debulk_survprob(stats::predict(model, type='survival', times= times))
  survprob2  <- debulk_survprob(stats::predict(model, type='survival', times= times_iqr))
  survprob3  <- debulk_survprob(stats::predict(model, type='survival', times= times_wide))
  survprob4  <- debulk_survprob(stats::predict(model, type='survival', times= times_full))

  ### NOTE: general rule, if model is AFT, use preds. If PH, use -preds. Could refactor this to depend on distribution specification.
  # But for the time being, we will simply take the max of concordance, 1-concordance. (same for AUC).


  ### original survival package methods
  harrel_c   <- survival::concordance(Surv_object ~ preds)$concordance
  uno_c      <- survival::concordance(Surv_object ~ preds, timewt = 'n/G2')$concordance

  ### survAUC functions
  uno_c_auc  <- tryCatch(survAUC::UnoC(Surv_object, Surv_object, lpnew = preds), error=function(e){NA})   # NOTE: WILL BE ZERO IF ALL PREDICTIONS EQUAL
  iauc       <- tryCatch(survAUC::AUC.uno(Surv_object, Surv_object, lpnew = preds, times = times)$iauc, error=function(e){NA})
  iauc2      <- tryCatch(survAUC::AUC.uno(Surv_object, Surv_object, lpnew = preds, times = times_iqr)$iauc, error=function(e){NA})   # IQR version
  iauc3      <- tryCatch(survAUC::AUC.uno(Surv_object, Surv_object, lpnew = preds, times = times_wide)$iauc, error=function(e){NA})  # Wider Version (10/90)
  iauc4      <- tryCatch(survAUC::AUC.uno(Surv_object, Surv_object, lpnew = preds, times = times_full)$iauc, error=function(e){NA})  # Full version
  iauc5      <- tryCatch(survAUC::AUC.uno(Surv_object, Surv_object, lpnew = preds, times = c(med_time))$iauc, error=function(e){NA}) # AUC at median
  iauc6      <- tryCatch(survAUC::AUC.uno(Surv_object, Surv_object, lpnew = preds, times = c(avg_time))$iauc, error=function(e){NA}) # AUC at mean

  ### metrics
  c_index    <- tryCatch(SurvMetrics::Cindex(Surv_object, predicted = as.vector(unlist(stats::predict(model)))), error=function(e){NA}) %>% as.numeric()
  mae        <- tryCatch(SurvMetrics::MAE(Surv_object, pre_time= as.vector(unlist(stats::predict(model)))), error=function(e){NA}) %>% as.numeric()
  i_stats    <- tryCatch(SurvMetrics::IAEISE(Surv_object, sp_matrix = survprob$sp_matrix,  IRange=survprob$IRange), error=function(e){NA}) %>% as.numeric()  ### vastly underestimates I stats?
  i_stats2   <- tryCatch(SurvMetrics::IAEISE(Surv_object, sp_matrix = survprob2$sp_matrix, IRange=survprob2$IRange), error=function(e){NA})%>% as.numeric()
  i_stats3   <- tryCatch(SurvMetrics::IAEISE(Surv_object, sp_matrix = survprob3$sp_matrix, IRange=survprob3$IRange), error=function(e){NA})%>% as.numeric()
  i_stats4   <- tryCatch(SurvMetrics::IAEISE(Surv_object, sp_matrix = survprob4$sp_matrix, IRange=survprob4$IRange), error=function(e){NA})%>% as.numeric()
  brier_med  <- tryCatch(SurvMetrics::Brier(Surv_object, pre_sp = as.vector(unlist(stats::predict(model, type='survival', times=med_time)$.pred_survival)), t_star = med_time), error=function(e){NA}) %>% as.numeric()
  brier_avg  <- tryCatch(SurvMetrics::Brier(Surv_object, pre_sp = as.vector(unlist(stats::predict(model, type='survival', times=avg_time)$.pred_survival)), t_star = avg_time), error=function(e){NA}) %>% as.numeric()

  # output
  out <- list(
    'Harrel.C.Index'      = max(harrel_c, 1-harrel_c),
    'Uno.C.Index'         = max(uno_c, 1-uno_c),
    # 'C.Index.Uno'         = max(uno_c_auc, 1-uno_c_auc),
    'iAUC'                = max(iauc,  1-iauc),
    'iAUC.IQR'            = max(iauc2, 1-iauc2),
    'iAUC.Q10.Q90'        = max(iauc3, 1-iauc3),
    'iAUC.Full'           = max(iauc4, 1-iauc4),
    'AUC.Median'          = max(iauc5, 1-iauc5),
    'AUC.Mean'            = max(iauc6, 1-iauc6),
    'C.Index'             = max(c_index, 1-c_index),
    'MAE'                 = mae,
    'IAE'                 = i_stats[1],
    'ISE'                 = i_stats[2],
    'IAE.IQR'             = i_stats2[1],
    'ISE.IQR'             = i_stats2[2],
    'IAE.Q10.Q90'         = i_stats3[1],
    'ISE.Q10.Q90'         = i_stats3[2],
    'IAE.Full'            = i_stats4[1],
    'ISE.Full'            = i_stats4[2],
    'Brier.Median'        = brier_med,
    'Brier.Mean'          = brier_avg
  )

  if(ibs){
    ibs1       <- SurvMetrics::IBS(Surv_object, sp_matrix = survprob$sp_matrix,  IBSrange = survprob$IRange) %>% as.numeric()  ### Vastly underestimates
    #ibs2       <- SurvMetrics::IBS(Surv_object, sp_matrix = survprob2$sp_matrix, IBSrange = survprob2$IRange)%>% as.numeric()
    #ibs3       <- SurvMetrics::IBS(Surv_object, sp_matrix = survprob3$sp_matrix, IBSrange = survprob3$IRange)%>% as.numeric()
    ibs4       <- SurvMetrics::IBS(Surv_object, sp_matrix = survprob4$sp_matrix, IBSrange = survprob4$IRange)%>% as.numeric()
    out <- c(
      out,
      'IBS'                 = ibs1,
      # 'IBS.IQR'             = ibs2,
      # 'IBS.Q10.Q90'         = ibs3,
      'IBS.Full'            = ibs4
    )
  }

  return(out)
}
