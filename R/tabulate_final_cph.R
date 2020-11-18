##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param cph_backward_final
tabulate_final_cph <- function(cph_backward_final) {

  cph_backward_final %>%
    bind_rows(.id = 'predictors') %>%
    mutate(
      tbl_value = table_glue("{estimate}\n({conf.low}, {conf.high})"),
      tbl_p = table_pvalue(p.value)
    ) %>%
    select(predictors, fit, term, starts_with('tbl'))

}
