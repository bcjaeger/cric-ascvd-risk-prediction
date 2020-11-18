##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##'
##' @param train
##' @param outcomes
##' @param predictors
##' @param test
##' @param predict_horizon
##' @param vars_required
##' @param block_count_1
##' @param block_count_2
##' @param block_count_3
##'
ma_cph_backwards <- function(train,
                             test = NULL,
                             outcomes,
                             predictors,
                             predict_horizon,
                             vars_required,
                             block_count_1 = 2,
                             block_count_2 = 2,
                             block_count_3 = 2){

  predictors_collapsed <- glue_collapse(predictors, sep = ' + ')

  formula_maximal <- as.formula(
    glue("Surv({outcomes$time}, {outcomes$status}) ~ {predictors_collapsed}")
  )

  fit_initial <- coxph(
    formula = formula_maximal,
    data = train,
    x = TRUE
  )

  vars_required_collapsed <- glue_collapse(vars_required, sep = ' + ')

  formula_minimal <- as.formula(glue(
    "Surv({outcomes$time}, {outcomes$status}) ~ {vars_required_collapsed}"
  ))

  fit <- MASS::stepAIC(
    object = fit_initial,
    scope = list(upper = formula_maximal,
                 lower = formula_minimal),
    direction = 'both',
    trace = 0,
    k = 1e5
  )

  variables_dropped <- fit$anova$Step %>%
    str_remove('\\- ') %>%
    setdiff('') %>%
    rev()

  block_index_1 <- seq(block_count_1)

  block_index_2 <- seq(max(block_index_1) + 1,
                       max(block_index_1) + block_count_2)

  block_index_3 <- seq(max(block_index_2) + 1,
                       max(block_index_2) + block_count_3)

  variable_blocks <- list(
    block_0 = NULL,
    block_1 = variables_dropped[block_index_1],
    block_2 = variables_dropped[block_index_2],
    block_3 = variables_dropped[block_index_3],
    block_full = variables_dropped[1:max(block_index_3)]
  ) %>%
    map(~c(vars_required_collapsed, .x)) %>%
    map(paste, collapse = ' + ')

  fit_formulas <- map(
    .x = variable_blocks,
    .f = ~ as.formula(glue(
      "Surv({outcomes$time}, {outcomes$status}) ~ {vars_required_collapsed} + {.x}"
    ))
  )

  fit_objects <- map(
    .x = fit_formulas,
    .f = ~ coxph(formula = .x, data = train, x = TRUE)
  )

  # if no testing data, then use training data for predictions
  if(is.null(test)) test <- train

  fit_preds <- map(
    .x = fit_objects,
    .f = ~predictRisk(.x, newdata = test, times = predict_horizon)
  )

  tibble(
    formula = fit_formulas,
    fit = fit_objects,
    prediction = fit_preds
  )

}
