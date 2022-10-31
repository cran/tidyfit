## ---- include = FALSE, eval=TRUE----------------------------------------------
use_saved_results <- TRUE

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  echo = TRUE,
  eval = !use_saved_results,
  message = FALSE,
  warning = FALSE
)

if (use_saved_results) {
  library(pls)
  results <- readRDS("vignette_bci.rds")
  estimates <- results$estimates
}

## ---- eval=TRUE---------------------------------------------------------------
library(tidyverse)   # Data wrangling
library(tidyfit)     # Model fitting

## ---- eval=TRUE---------------------------------------------------------------
data <- MASS::Boston %>% 
  scale %>% 
  as_tibble

## -----------------------------------------------------------------------------
#  model_frame <- data %>%
#    regress(medv ~ ., m("plsr", ncomp = 5), m("pcr", ncomp = 5),
#            .cv = "bootstraps", .cv_args = list(times = 100),
#            .return_slices = TRUE)

## -----------------------------------------------------------------------------
#  estimates <- coef(model_frame,
#                    .add_bootstrap_interval = TRUE,
#                    .bootstrap_alpha = 0.05)

## ---- eval=TRUE---------------------------------------------------------------
estimates

## ---- eval=TRUE---------------------------------------------------------------
estimates <- estimates %>% 
  unnest(model_info)
estimates

## ---- fig.width=7, fig.height=3.5, fig.align="center", eval=TRUE--------------
estimates %>% 
  ggplot(aes(term, estimate, color = model)) +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = .lower, ymax = .upper), position = position_dodge()) +
  theme_bw(8)

