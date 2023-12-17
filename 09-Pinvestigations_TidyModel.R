# https://cran.r-project.org/web/packages/textmineR/vignettes/f_tidytext_example.html

load("~/R_Workflow/pinvestigations/data/finalModel.Rdata")

library(tidytext)
library(textmineR)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(RColorBrewer)

# below is equivalent to tidy_beta <- tidy(x = m, matrix = "beta")
tidy_beta <- data.frame(topic = as.integer(stringr::str_replace_all(rownames(model$phi), "t_", "")), 
                        model$phi, 
                        stringsAsFactors = FALSE) %>%
  gather(term, beta, -topic) %>% 
  tibble::as_tibble()

# проверка слова
tidy_beta %>% 
  filter(term == "благо") %>% 
  arrange(-beta)

# below is equivalent to tidy_gamma <- tidy(x = m, matrix = "gamma")
tidy_gamma <- data.frame(document = rownames(model$theta),
                         model$theta,
                         stringsAsFactors = FALSE) %>%
  gather(topic, gamma, -document) %>%
  tibble::as_tibble() %>% 
  mutate(topic = as.integer(str_remove(topic, "t_")))

# проверка  темы
tidy_gamma %>% 
  filter(topic == 3) %>% 
  arrange(-gamma)

