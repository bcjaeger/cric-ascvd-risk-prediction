##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param data
##' @param predictor
##' @param time
performance <- function(data,
                        predictor_new,
                        predictor_ref,
                        outcomes,
                        nri_cut_points = 0.50,
                        predict_horizon = 1500){



  formula <- as.formula(
    glue("Surv({outcomes$time}, {outcomes$status}) ~ 1")
  )

  score <- Score(
    object = list(predictor_new),
    formula = formula,
    data = data,
    times = predict_horizon,
    summary = c('IPA')
  )

  # go to extraordinary lengths to make this function shut up.
  invisible(
    capture.output(
      suppressMessages(
        nri_scores <- nricens(
          time = data$time,
          event = data$status,
          p.std = as.numeric(predictor_ref),
          p.new = as.numeric(predictor_new),
          cut = nri_cut_points,
          t0 = predict_horizon,
          niter = 0,
          msg = FALSE)
      )
    )
  )


  tibble(auc = score$AUC$score$AUC,
         ipa = score$Brier$score$IPA[2],
         nri_ovrl = nri_scores$nri$Estimate[1],
         nri_plus = nri_scores$nri$Estimate[2],
         nri_minus = nri_scores$nri$Estimate[3])

}
