## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  echo = TRUE,
  message = FALSE,
  warning = FALSE
)

## -----------------------------------------------------------------------------
library(tidyverse) # Data wrangling
library(tidyfit)   # Auto-ML modeling

## -----------------------------------------------------------------------------
data <- MASS::Boston
mod_frame <- data %>% 
  regress(medv ~ ., m("hfr", kappa = c(0.25, 0.5, 0.75, 1))) %>% 
  unnest(settings)
mod_frame

## -----------------------------------------------------------------------------
mod_frame$model_object[[1]]

## ---- fig.width=7, fig.height=6, fig.align='center'---------------------------
mod_frame %>% 
  filter(kappa == 1) %>% 
  pull(model_object) %>% 
  .[[1]] %>% 
  plot(kappa = 1)

## -----------------------------------------------------------------------------
mod_frame <- mod_frame %>% 
  mutate(mod = map(model_object, ~.$object))
mod_frame

## ---- fig.width=7, fig.height=6, fig.align='center'---------------------------
# Store current par before editing
old_par <- par()

par(mfrow = c(2, 2))
par(family = "sans", cex = 0.7)
mod_frame %>% 
  arrange(desc(kappa)) %>% 
  select(model_object, kappa) %>% 
  pwalk(~plot(.x, kappa = .y, 
              max_leaf_size = 2, 
              show_details = FALSE))

## -----------------------------------------------------------------------------
# Restore old par
par(old_par)

