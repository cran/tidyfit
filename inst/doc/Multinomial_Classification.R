## ---- include = FALSE---------------------------------------------------------
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
  results <- readRDS("vignette_mc.rds")
  pred <- results$pred
}

## ---- eval=TRUE---------------------------------------------------------------
library(tidyverse) # Data wrangling
library(tidyfit)   # Auto-ML modeling

## ---- eval=TRUE---------------------------------------------------------------
data("iris")

# For reproducibility
set.seed(42)
ix_tst <- sample(1:nrow(iris), round(nrow(iris)*0.2))

data_trn <- iris[-ix_tst,]
data_tst <- iris[ix_tst,]

as_tibble(iris)

## -----------------------------------------------------------------------------
#  fit <- data_trn %>%
#    classify(Species ~ .,
#             m("lasso"),
#             m("ridge"),
#             m("enet"),
#             m("adalasso"),
#             ols = m("ridge", lambda = 1e-5),
#             .cv = "vfold")
#  
#  pred <- fit %>%
#    predict(data_tst)

## ---- fig.width=6, fig.height=3, fig.align="center", eval=TRUE----------------
metrics <- pred %>% 
  group_by(model, class) %>% 
  mutate(row_n = row_number()) %>% 
  spread(class, prediction) %>% 
  group_by(model) %>% 
  yardstick::mn_log_loss(truth, setosa:virginica)

metrics %>% 
  ggplot(aes(model, .estimate)) +
  geom_col(fill = "darkblue") +
  theme_bw() +
  theme(axis.title.x = element_blank())

