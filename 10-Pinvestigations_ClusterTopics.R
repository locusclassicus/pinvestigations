library(dendextend)

# близкие темы
phi <- model$phi

dist_mx <- CalcHellingerDist(phi)
dclust <- hclust(as.dist(dist_mx), method = "ward.D")

ddend <- as.dendrogram(dclust)

pal8 <- RColorBrewer::brewer.pal(8, "Dark2") # будет переработан до нужной длины

ddend %>% 
  set("branches_k_color", k = 15, value = pal8) %>% 
  set("labels_col", k=15, value = pal8) %>% 
  set("branches_lwd",  2) %>% 
  plot(horiz = F, main = "Тематические кластеры")

