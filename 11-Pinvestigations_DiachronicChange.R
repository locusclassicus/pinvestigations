library(pals)

# selected topics in movement

topics_sel <- tidy_gamma %>%
  mutate(year = str_extract(document, "\\d{4}"), .before = document) %>% 
  filter(topic %in% c(15, 5, 29, 25)) 

total_gamma <- tidy_gamma %>%
  mutate(year = str_extract(document, "\\d{4}"), .before = document) %>% 
  group_by(year) %>% 
  summarise(total_gamma = sum(gamma))

ratio_gamma <- topics_sel %>% 
  left_join(total_gamma) %>% 
  mutate(ratio = gamma / total_gamma) 

ratio_year <- ratio_gamma %>% 
  group_by(year, topic) %>% 
  summarise(total_topic = sum(ratio))

ratio_year %>% 
  ggplot(aes(year, total_topic, 
             color = as.factor(topic),
             group = as.factor(topic))) +
  geom_line(size = 0.9, alpha = 0.8) +
  xlab(NULL) +
  ylab(NULL) +
  guides(color=guide_legend(title="Темы")) +
  scale_color_manual(values = pals::brewer.dark2(4)) +
  theme_bw()


# relative share of cluster
gamma_sum <- tidy_gamma %>%
  mutate(year = str_extract(document, "\\d{4}"), .before = document) %>% 
  select(-document) %>% 
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
  group_by(year, cluster) %>% 
  summarize(sum = sum(gamma))  


# cluster colors
#pal13 <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928', '#8b3a62')
pal12 <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99', '#8b3a62')

# visualize (by years)
gamma_sum %>% 
  ggplot(aes(year, sum, fill = as.factor(cluster))) +
  geom_bar(stat = "identity", position = "fill", width = 1 ) +
  scale_fill_manual(values = pal12) +
  ylab(NULL) +
  xlab(NULL) +
  guides(fill=guide_legend(title="Кластеры тем")) +
  theme_test()

# try area geom
gamma_sum %>% 
  ggplot(aes(as.numeric(year), sum, fill = as.factor(cluster))) +
  geom_area(
    #position = "fill"
    ) +
  ylab(NULL) +
  xlab(NULL) +
  scale_fill_manual(values = pal12) +
  guides(fill=guide_legend(title="Кластеры тем")) +
  theme_test()


# theme colors 
pal30 <- pals::polychrome()[1:30]

# relative share of topic
gamma_sum <- tidy_gamma %>%
  mutate(year = str_extract(document, "\\d{4}"), .before = document) %>% 
  select(-document) %>% 
  group_by(year, topic) %>% 
  summarize(sum = sum(gamma)) 

# gamma_sum %>%
#   ggplot(aes(year, sum, fill = as.factor(topic))) +
#   geom_bar(stat = "identity", position = "fill", width = 1 ) +
#   ylab(NULL) +
#   guides(fill=guide_legend(title="Темы")) +
#   theme_test()
