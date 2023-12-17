# selected_ids <- sample(unique(tidy_gamma$document), 4)

selected_ids <- unique(tidy_gamma$document[str_detect(tidy_gamma$document, "2023")])

selected_docs <- tidy_gamma %>% 
  filter(document %in% selected_ids)

tidy_gamma %>% 
  filter(document %in% selected_ids) %>% 
  filter(topic == 21) %>% 
  arrange(-gamma)

selected_docs %>% 
  ggplot(aes(as.factor(topic), gamma, color = document)) + 
  geom_boxplot(show.legend = F) +
  facet_wrap(~document, nrow = 5, scales = "free") +
  xlab(NULL) +
  theme_bw()

selected_self <- unique(tidy_gamma$document[ str_detect(tidy_gamma$document, "Алиева") ])[1:4]

selected_docs <- tidy_gamma %>% 
  filter(document %in% selected_self) 

selected_docs %>% 
  ggplot(aes(as.factor(topic), gamma, color = document)) + 
  geom_boxplot(show.legend = F) +
  facet_wrap(~document, nrow = 2, scales = "free") +
  xlab(NULL) +
  theme_bw()
