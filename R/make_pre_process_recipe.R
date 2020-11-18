##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param outcomes
##' @param cric_data
make_pre_process_recipe <- function(predictors, outcomes, cric_data) {

  preds_collapsed <- glue_collapse(predictors$expanded, sep = ' + ')

  recipe_formula <- glue(
    "{outcomes$time} + {outcomes$status} ~ {preds_collapsed}"
  )

  recipe(formula = recipe_formula, x = cric_data) %>%
    step_mutate_at(all_nominal(), fn = as.factor) %>%
    step_knnimpute(all_predictors())

}
