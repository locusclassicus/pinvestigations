library(tidyverse)
library(textmineR)
library(ldatuning)

# сначала смотрим coherence
# https://cran.r-project.org/web/packages/textmineR/vignettes/c_topic_modeling.html

# загружаем терм-документную матрицу
load("./data/Sparse.Rdata")

# список топиков
k_list <- seq(10,85, by=1)

# подгонка моделей
model_list <- TmParallelApply(X = k_list, FUN = function(k){
  message(paste0("trying k = ", k))
  m <- FitLdaModel(dtm = text_dtm,
                   k = k,
                   iterations = 500,
                   burnin = 180,
                   alpha = 0.1,
                   beta = colSums(text_dtm) / sum(text_dtm) * 100,
                   optimize_alpha = TRUE,
                   calc_likelihood = FALSE,
                   calc_coherence = TRUE,
                   calc_r2 = FALSE,
                   cpus = 1)
  m$k <- k

  m
},
cpus = 8)

save(model_list, file = "./data/ModelList.Rdata")
load("~/R_Workflow/pinvestigations/data/ModelList.Rdata")

# средняя coherence for each model
coherence_mat <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)), 
                            coherence = sapply(model_list, function(x) mean(x$coherence)), 
                            stringsAsFactors = FALSE)


# график coherence, что чем больше топиков (85+), тем лучше
plot(coherence_mat, type = "o")

#  подбор количества топиков с использованием других критериев

# result <- FindTopicsNumber(
#   text_dtm,
#   topics = seq(from = 10, to = 85, by = 1),
#   metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
#   method = "Gibbs",
#   control = list(seed = 12345),
#   mc.cores = 8L,
#   verbose = TRUE
# )
# # 
save(result, file = "./data/LDATuning.Rdata")
#load("~/R_Workflow/pinvestigations/data/LDATuning.Rdata")

FindTopicsNumber_plot(result)
