##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param cric_cv_repeated
##' @param cric_data
##' @param predictors
tabulate_cv_repeated <- function(cric_cv_repeated) {

  cric_cv_repeated %>%
    group_by(predictors, formula) %>%
    summarize(
      across(
        .cols = c(auc:nri_minus),
        .fns = list(lwr = ~quantile(.x, probs = 0.025),
                    est = ~quantile(.x, probs = 0.500),
                    upr = ~quantile(.x, probs = 0.975))
      )
    ) %>%
    transmute(
      formula,
      auc = table_glue("{auc_est}\n({auc_lwr}, {auc_upr})"),
      ipa = table_glue("{ipa_est}\n({ipa_lwr}, {ipa_upr})"),
      nri = table_glue("{nri_ovrl_est}\n({nri_ovrl_lwr}, {nri_ovrl_upr})"),
      nri = recode(nri, '0.00\n(0.00, 0.00)' = '0 (reference)')
    ) %>%
    pivot_wider(names_from = formula, values_from = c(auc:nri)) %>%
    pivot_longer(-predictors) %>%
    mutate(term = str_sub(name, start = 1, end = 3),
           name = str_remove(name, '^auc_|^ipa_|^nri_'),
           .before = 1) %>%
    pivot_wider(names_from = name, values_from = value)

}
