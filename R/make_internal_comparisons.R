##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param cric_cv_repeated
make_internal_comparisons <- function(cric_cv_repeated,
                                      formula_1,
                                      predictors_1,
                                      formula_2,
                                      predictors_2,
                                      metric) {

  algo_1 <- paste(predictors_1, formula_1, sep = '..')
  algo_2 <- paste(predictors_2, formula_2, sep = '..')

  model_data <- cric_cv_repeated %>%
    unite(col = 'algo', predictors, formula, sep = '..') %>%
    filter(algo %in% c(algo_1, algo_2)) %>%
    select(starts_with("id"), algo, all_of(metric))

  stan_formula <- as.formula(glue("{metric} ~ algo + (1 | id2/id)"))
  stan_fit <- stan_lmer(formula = stan_formula, data = model_data)

  posterior_predict(
    stan_fit,
    newdata = data.frame(algo = unique(model_data$algo)),
    re.form = ~0
  ) %>%
    as.matrix() %>%
    apply(1, diff) %>%
    quantile(probs = c(0.025, 0.50, 0.975))

}
