##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param file_path
clean_cric <- function(file_path = "data/cric.csv",
                       predictors,
                       outcomes) {

  read_csv(file_path) %>%
    select(
      all_of(unlist(outcomes)),
      all_of(predictors$expanded)
    )

}
