
the_plan <- drake_plan(

  # define two sets of predictors
  # -- clinically available predictors
  # -- all predictors available in CRIC data
  predictors = make_predictors(),

  # change to age/sex/etc for cric
  acc_aha_vars = c('trt','age','sex'),

  predict_horizon = 1500, # change to 10 yrs for cric

  # specify column names of the two outcomes
  outcomes = list(time = 'time', status = 'status'),

  # load the CRIC data for analysis
  cric_data = clean_cric(file_path = 'data/cric.csv',
                         predictors = predictors,
                         outcomes = outcomes),

  # load validation data for analysis
  validation_data = clean_validation(
    file_path_mesa = 'data/mesa.csv',
    file_path_aric = 'data/aric.csv',
    predictors = predictors,
    outcomes = outcomes
  ),

  pre_process_recipe = make_pre_process_recipe(
    predictors,
    outcomes,
    cric_data
  ),

  # run internal validation of stepwise PH models
  cric_cv_repeated = make_cv_repeated(
    cric_data = cric_data,
    outcomes = outcomes,
    predictors = predictors,
    pre_process_recipe = pre_process_recipe,
    acc_aha_vars = acc_aha_vars,
    predict_horizon = predict_horizon,
    n_repeats = 2,          # change to 10 for final analysis
    n_folds = 5,            # change to 10 for final analysis
    nri_cut_points = 0.50,  # change to 0.10 for cric
    block_count_1 = 2,
    block_count_2 = 2,
    block_count_3 = 2
  ),

  internal_comparisons = make_internal_comparisons(
    cric_cv_repeated,
    formula_1 = 'block_0',
    predictors_1 = 'clinic',
    formula_2 = 'block_2',
    predictors_2 = 'expanded',
    metric = 'auc'
  ),

  cph_backward_final = pre_process_recipe %>%
    prep(training = cric_data) %>%
    juice() %>%
    make_final_models(outcomes = outcomes,
                      predictors = predictors,
                      acc_aha_vars = acc_aha_vars,
                      predict_horizon = predict_horizon),

  # create tables summarizing internal validation
  tbl_cv_repeated = tabulate_cv_repeated(cric_cv_repeated),
  tbl_cph_backward_final = tabulate_final_cph(cph_backward_final$coefs),

  tbl_model_development = tabulate_model_development(
    tbl_cv_repeated = tbl_cv_repeated,
    tbl_cph_backward_final = tbl_cph_backward_final
  )


)
