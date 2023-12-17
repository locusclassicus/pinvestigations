load("~/R_Workflow/pinvestigations/data/TOC.Rdata")

sections <- all_tocs %>% 
  select(id, section) %>% 
  rename(document = id)

# tidy gamma + toc
tidy_gamma_toc <- tidy_gamma %>% 
  left_join(sections) %>% 
  select(-document) 

# which articles contribute to presence of a topic in section
tidy_gamma %>% 
  left_join(sections) %>% 
  filter(topic %in% c(6, 30),
         section == "Переводы и публикации") %>% 
  arrange(-gamma)
  


# relative share of cluster
gamma_sum_section <- tidy_gamma_toc %>%
  mutate(cluster = case_when(
    topic %in% c(3, 4, 26, 21) ~ "теоретическая философия",
    topic %in% c(18, 24) ~ "душа и тело",
    topic %in% c(28, 7) ~ "неоплатонизм и патристика",
    topic %in% c(9, 29) ~ "Платон и платоноведение",
    topic %in% c(25, 5, 15) ~ "Платон и Аристотель",
    topic %in% c(10, 23) ~ "разное",
    topic %in% c(19, 20) ~ "практическая философия",
    topic %in% c(30, 6) ~ "Сократ и сократики",
    topic %in% c(1) ~ "риторика",
    topic %in% c(2, 27, 13, 22) ~ "религия и культура",
    topic %in% c(17, 11, 12, 8) ~ "Платон в России",
    topic %in% c(14, 16) ~ "герменевтика",
  )) %>% 
  group_by(section, cluster) %>% 
  summarize(sum = sum(gamma))  

# total sum per section
total <- gamma_sum_section %>% 
  summarize(total = sum(sum))

gamma_sum_section_o <- gamma_sum_section %>% 
  left_join(total) %>% 
  filter(total > 5)
  
# plot
gamma_sum_section_o %>% 
  ggplot(aes(reorder(section, total), sum, fill = cluster)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = pal12) +
  guides(fill=guide_legend(title="Кластеры тем")) +
  coord_flip() +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw()
