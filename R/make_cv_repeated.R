##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param cric_data
make_cv_repeated <- function(cric_data,
                             outcomes,
                             predictors,
                             pre_process_recipe,
                             acc_aha_vars = c('trt','age','sex'),
                             n_repeats = 2, # change to 10 for final analysis
                             n_folds = 5, # change to 10 for final analysis
                             predict_horizon = 1500, # change to 10 for cric
                             block_count_1 = 2,
                             block_count_2 = 2,
                             block_count_3 = 2,
                             nri_cut_points = 0.50
                             ) {


  folds <- vfold_cv(data = cric_data,
                    v = n_folds,
                    repeats = n_repeats,
                    strata = outcomes$status)

  # apply recipe to training/testing folds ----------------------------------

  folds_processed <- folds %>%
    mutate(
      trn_raw = map(splits, training),
      tst_raw = map(splits, testing),
      recipe_prepped = map(trn_raw, ~prep(pre_process_recipe, training = .x)),
      trn_cooked = map(recipe_prepped, juice),
      tst_cooked = map2(recipe_prepped, tst_raw, ~bake(.x, new_data = .y))
    ) %>%
    select(starts_with('id'), ends_with('cooked'))

  message("Running stepwise PH regression on all folds")

  folds_fitted <- map_dfr(
    .x = predictors,
    .id = 'predictors',
    .f = ~ folds_processed %>%
      mutate(
        cph_backwards = map2(
          .x = trn_cooked,
          .y = tst_cooked,
          .f = ma_cph_backwards,
          outcomes = outcomes,
          predictors = .x,
          predict_horizon = predict_horizon,
          vars_required = acc_aha_vars,
          block_count_1 = block_count_1,
          block_count_2 = block_count_2,
          block_count_3 = block_count_3
        )
      )
  )

  message("Done!")

  # unnest the data to get predictions

  folds_unnested <- folds_fitted %>%
    unnest_wider(cph_backwards) %>%
    unnest(cols = c(formula, fit, prediction)) %>%
    mutate(formula = names(formula)) %>%
    group_by(predictors, id, id2) %>%
    mutate(prediction_reference = prediction[formula == 'block_0']) %>%
    ungroup()

  message("Computing AUC, IPA, and NRI for all folds")
  folds_scored <- folds_unnested %>%
    mutate(
      scores = pmap(
        .l = list(tst_cooked, prediction, prediction_reference),
        .f = performance,
        predict_horizon = predict_horizon,
        outcomes = outcomes,
        nri_cut_points = nri_cut_points
      )
    ) %>%
    select(starts_with('id'), predictors, formula, scores) %>%
    unnest_wider(scores)
  message("Done!")

  folds_scored


}
