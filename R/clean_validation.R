##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param file_path_mesa
##' @param file_path_aric
clean_validation <- function(file_path_mesa = "data/mesa.csv",
                             file_path_aric = "data/aric.csv",
                             predictors,
                             outcomes) {

  # read the data in
  mesa <- read_csv(file_path_mesa)
  aric <- read_csv(file_path_aric)

  # do cleaning (if needed)

  # create overall data
  overall <- bind_rows(mesa = mesa, aric = aric, .id = 'study')

  # select the outcomes and predictors in each validation set
  # to match the columns selected in the derivation set
  list(
    mesa = mesa,
    aric = aric,
    overall = overall
  ) %>%
    map(
      ~select(.x,
              all_of(unlist(outcomes)),
              all_of(predictors$expanded))
    )


}
