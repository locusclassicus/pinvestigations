# решение подсмотрено здесь https://towardsdatascience.com/beginners-guide-to-lda-topic-modelling-with-r-e57a5a8e7a25
# с некоторыми модификациями https://cran.r-project.org/web/packages/textmineR/vignettes/c_topic_modeling.html

library(dplyr)
library(tidytext)
library(textmineR)
library(purrr)
library(ggplot2)

# строим модель 
# set.seed(12345)
# 
# model <- FitLdaModel(dtm = text_dtm, 
#                      k = 85,
#                      iterations = 500, 
#                      burnin = 180,
#                      alpha = 0.1,
#                      beta = 0.05,
#                      optimize_alpha = TRUE,
#                      calc_likelihood = TRUE,
#                      calc_coherence = TRUE,
#                      calc_r2 = TRUE,
#                      cpus = 8) 
# 
# R-squared 
model$r2

# log Likelihood (does not consider the prior) 
plot(model$log_likelihood, type = "l")

# coherence
summary(model$coherence)

# смотрим на термины в топиках
model$top_terms <- GetTopTerms(phi = model$phi, M = 10)
top20_wide <- as.data.frame(model$top_terms)

model$prevalence <- colSums(model$theta) / sum(model$theta) * 100

# prevalence should be proportional to alpha
plot(model$prevalence, model$alpha, xlab = "prevalence", ylab = "alpha")

# labels
model$labels <- LabelTopics(assignments = model$theta > 0.05, 
                            dtm = text_dtm,
                            M = 2)

# put them together, with coherence into a summary table
model$summary <- data.frame(topic = rownames(model$phi),
                            label = model$labels,
                            coherence = round(model$coherence, 3),
                            prevalence = round(model$prevalence,3),
                            top_terms = apply(model$top_terms, 2, function(x){
                              paste(x, collapse = ", ")
                            }),
                            stringsAsFactors = FALSE)

summary_wide<- model$summary[ order(model$summary$prevalence, decreasing = TRUE) , ]
summary_wide <- summary_wide %>% rename(label_1 = label.label_1, label_2 = label.label_2)


# predictions with gibbs
# assignments <- predict(model, text_dtm,
#                        method = "gibbs", 
#                        iterations = 200,
#                        burnin = 180,
#                        cpus = 2)
# 
# barplot(assignments[3,])

# # кластеризация топиков
# topic_dist <- CalcHellingerDist(model$phi)
# topic_hclust <- hclust(as.dist(topic_dist), "ward.D")
# plot(topic_hclust)

