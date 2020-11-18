## library() calls go here
library(conflicted)
library(dotenv)
library(drake)

library(tidyverse)
library(tidymodels)
library(nricens)
library(riskRegression)
library(obliqueRSF)
library(survival)
library(glue)
library(table.glue) # may need to install from CRAN
library(flextable)  # may need to install from CRAN
library(rstanarm)

conflicted::conflict_prefer("filter",    "dplyr")
conflicted::conflict_prefer('summarise', 'dplyr')
conflicted::conflict_prefer("gather",    "tidyr")
conflicted::conflict_prefer("set_names", "purrr")


# # NOTE: the code below creates fake data stored on the repo
# pbc <- as_tibble(survival::pbc)
#
# glimpse(pbc)
#
# # clean pbc data ----------------------------------------------------------
#
# # original status is 0/1/2 for censored, transplant, dead
# # recode this so that 0 is censored/transplant, 1 is dead
#
# pbc_clean <- pbc %>%
#   mutate(status = if_else(status >= 1, true = status-1, false = 0))
#
# table(clean = pbc_clean$status, original = pbc$status)
#
# write_csv(pbc_clean, 'data/cric.csv')
# write_csv(pbc_clean, 'data/mesa.csv')
# write_csv(pbc_clean, 'data/aric.csv')
