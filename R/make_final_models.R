##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param data
##' @param outcomes
##' @param predict_horizon
##' @param predictors
make_final_models <- function(data,
                              outcomes,
                              predictors,
                              pre_process_recipe,
                              acc_aha_vars,
                              predict_horizon) {

  fits <- map(
    .x = predictors,
    .f = ~ {
      ma_cph_backwards(train = data,
                       predictors = .x,
                       outcomes = outcomes,
                       vars_required = acc_aha_vars,
                       predict_horizon = predict_horizon)
    })

  coefs <- map(
    .x = fits,
    .f = ~ .x %>%
      pull(fit) %>%
      map_dfr(tidy, .id = 'fit', conf.int = TRUE, exponentiate = TRUE)
  )

  list(fits = fits, coefs = coefs)

}
