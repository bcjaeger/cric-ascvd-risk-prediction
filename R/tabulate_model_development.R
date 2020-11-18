##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param tbl_cv_repeated
##' @param tbl_cph_backward_final
tabulate_model_development <- function(tbl_cv_repeated,
                                       tbl_cph_backward_final) {

  tbl_data_recoded <- tbl_cph_backward_final %>%
    select(-tbl_p) %>%
    pivot_wider(names_from = fit, values_from = starts_with('tbl')) %>%
    bind_rows(tbl_cv_repeated) %>%
    mutate(
      group = if_else(str_detect(term, '^auc$|^ipa$|^nri$'),
                      true = 'Model performance (95% CI)',
                      false = 'Model hazard ratio (95% CI)'),
      term = recode(
        term,
        trt = 'Treatment group',
        age = 'Age, 1 year'
        #oldname = newname
      )
    )

  tbl_data_recoded %>%
    split(f = tbl_data_recoded$predictors) %>%
    map(
      ~ .x %>%
        select(-predictors) %>%
        as_grouped_data(groups = 'group') %>%
        as_flextable(hide_grouplabel = TRUE) %>%
        theme_box() %>%
        height(height = 1.5, part = 'header') %>%
        set_header_labels(
          term = '',
          block_0 = 'ACC/AHA variables only',
          block_1 = 'some more'
        ) %>%
        width(width = 1.15) %>%
        align(align = 'center', part = 'all') %>%
        align(j = 1, align = 'left', part = 'all')
    )

}
