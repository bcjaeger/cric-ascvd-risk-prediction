##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title

make_predictors <- function() {

  predictors_expanded <- c(
    "trt",
    "age",
    "sex",
    "ascites",
    "hepato",
    "spiders",
    "edema",
    "bili",
    "chol",
    "albumin",
    "copper",
    "alk.phos",
    "ast",
    "trig",
    "platelet",
    "protime",
    "stage"
  )

  predictors_clinic <- setdiff(
    predictors_expanded,
    # remove these predictors b/c they are not
    # usually available in the clinical setting.
    c("edema",
      "bili",
      "chol",
      "albumin")
  )

  if(!(all(predictors_clinic %in% predictors_expanded))){
    stop("All predictors in the clinic set should be in the expanded set")
  }

  list(
    clinic = predictors_clinic,
    expanded = predictors_expanded
  )

}
