scholars_gamma <- tidy_gamma %>% 
  mutate(author = str_remove(document, "_\\d{4}_\\d{1}")) %>% 
  group_by(topic, author) %>% 
  summarize(total_gamma = sum(gamma))

total_topic <- tidy_gamma %>% 
  group_by(topic) %>% 
  summarize(total_topic = sum(gamma))

scholars_share <- scholars_gamma %>% 
  left_join(total_topic) %>% 
  mutate(share = total_gamma / total_topic)

scholars_share_cropped <- scholars_share %>% 
  top_n(1, share) %>% 
  arrange(-share) %>% 
  select(-total_gamma, -total_topic) %>% 
  mutate(share = round(share, 1))

# с учетом prevalence
# prevalence_tbl <- tibble(topic = 1:30, 
#                          prevalence = unname(model$prevalence))
# 
# scholars_weighted <- scholars_share %>% 
#   left_join(prevalence_tbl) %>%
#   mutate(share_wt = (total_gamma / total_topic) * (prevalence / 100)) %>% 
#   group_by(topic) %>% 
#   top_n(1, share_wt)
