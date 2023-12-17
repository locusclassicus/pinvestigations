library(dplyr)
library(tidyr)
library(stringr)

# compare two erotic themes
beta_spread <- tidy_beta %>% 
  filter(topic %in% c(2, 6)) %>% 
  mutate(topic = paste0("topic_", topic)) %>%
  filter(!term %in% c("психея", "босх", "триптих")) %>% 
  mutate(term = case_when(term == "психей" ~ "психея",
                          term == "босха" ~ "босх",
                          term == "триптиха" ~ "триптих",
                          .default = term)) %>% 
  spread(topic, beta) %>% 
  filter(topic_6 > .003 | topic_2 > .003) %>%
  mutate(log_ratio = log2(topic_6 / topic_2))

beta_log_ratio <- beta_spread %>%
  mutate(sign = case_when(log_ratio > 0 ~ "pos",
                          log_ratio < 0 ~ "neg")) %>%
  select(-topic_2, -topic_6) %>% 
  group_by(sign) %>% 
  arrange(desc(abs(log_ratio))) %>% 
  slice_head(n = 10)

cols <- RColorBrewer::brewer.pal(4, "Accent")

beta_log_ratio %>% 
  ggplot(aes(reorder(term, log_ratio), log_ratio, fill = sign)) +
  geom_col(show.legend = FALSE) +
  xlab("термин") +
  ylab("log2 (beta_6 / beta_2)") +
  scale_x_discrete() +
  scale_fill_manual(values = c(cols[1], cols[3])) +
  coord_flip()

# compare to psycho-somatic themes
beta_spread <- tidy_beta %>% 
  filter(topic %in% c(18, 24)) %>% 
  mutate(topic = paste0("topic_", topic)) %>% 
  spread(topic, beta) %>% 
  filter(topic_24 > .001 | topic_18 > .001) %>%
  mutate(log_ratio = log2(topic_24 / topic_18))

beta_log_ratio <- beta_spread %>%
  mutate(sign = case_when(log_ratio > 0 ~ "pos",
                          log_ratio < 0 ~ "neg")) %>%
  select(-topic_18, -topic_24) %>% 
  group_by(sign) %>% 
  arrange(desc(abs(log_ratio))) %>% 
  slice_head(n = 10)

beta_log_ratio %>% 
  ggplot(aes(reorder(term, log_ratio), log_ratio, fill = sign)) +
  geom_col(show.legend = FALSE) +
  xlab("термин") +
  ylab("log2 (beta_24 / beta_18)") +
  coord_flip() +
  scale_x_discrete() +
  scale_fill_manual(values = c(cols[2], cols[4]))


# compare two hermeneutic themes
beta_spread <- tidy_beta %>% 
  filter(topic %in% c(14, 16)) %>% 
  mutate(topic = paste0("topic_", topic)) %>% 
  spread(topic, beta) %>% 
  filter(topic_14 > .003 | topic_16 > .003) %>%
  mutate(log_ratio = log2(topic_16 / topic_14))

beta_log_ratio <- beta_spread %>%
  mutate(sign = case_when(log_ratio > 0 ~ "pos",
                          log_ratio < 0 ~ "neg")) %>%
  select(-topic_14, -topic_16) %>% 
  group_by(sign) %>% 
  arrange(desc(abs(log_ratio))) %>% 
  slice_head(n = 10)

beta_log_ratio %>% 
  ggplot(aes(reorder(term, log_ratio), log_ratio, fill = sign)) +
  geom_col(show.legend = FALSE) +
  xlab("термин") +
  ylab("log2 (beta_16 / beta_14)") +
  coord_flip() +
  scale_x_discrete() +
  scale_fill_manual(values = c(cols[2], cols[4]))

# compare two philologic themes
beta_spread <- tidy_beta %>% 
  filter(topic %in% c(10, 11)) %>% 
  filter(!str_detect(term, "א")) %>% 
  mutate(topic = paste0("topic_", topic)) %>% 
  spread(topic, beta) %>% 
  filter(topic_10 > .003 | topic_11 > .003) %>%
  mutate(log_ratio = log2(topic_11 / topic_10))

beta_log_ratio <- beta_spread %>%
  mutate(sign = case_when(log_ratio > 0 ~ "pos",
                          log_ratio < 0 ~ "neg")) %>%
  select(-topic_11, -topic_10) %>% 
  group_by(sign) %>% 
  arrange(desc(abs(log_ratio))) %>% 
  slice_head(n = 10)

beta_log_ratio %>% 
  ggplot(aes(reorder(term, log_ratio), log_ratio, fill = sign)) +
  geom_col(show.legend = FALSE) +
  xlab("термин") +
  ylab("log2 (beta_11 / beta_10)") +
  coord_flip() +
  scale_x_discrete() +
  scale_fill_manual(values = c(cols[1], cols[3]))

# compare any other themes
beta_spread <- tidy_beta %>% 
  filter(topic %in% c(9, 29)) %>% 
  mutate(topic = paste0("topic_", topic)) %>% 
  spread(topic, beta) %>% 
  filter(topic_9 > .003 | topic_29 > .003) %>%
  mutate(log_ratio = log2(topic_29 / topic_9))

beta_log_ratio <- beta_spread %>%
  mutate(sign = case_when(log_ratio > 0 ~ "pos",
                          log_ratio < 0 ~ "neg")) %>%
  select(-topic_29, -topic_9) %>% 
  group_by(sign) %>% 
  arrange(desc(abs(log_ratio))) %>% 
  slice_head(n = 10)

beta_log_ratio %>% 
  ggplot(aes(reorder(term, log_ratio), log_ratio, fill = sign)) +
  geom_col(show.legend = FALSE) +
  xlab("термин") +
  ylab("log2 (beta_29 / beta_9)") +
  coord_flip() 

