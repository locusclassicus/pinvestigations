library(ggplot2)
library(dplyr)
library(tidyr)
library(showtext)

# всего материалов в номере
total <- all_tocs %>% 
  group_by(year) %>% 
  count() %>% 
  rename(total = n)

# языки по годам
for_lang <- all_tocs %>%
  filter(language != "ru") %>% 
  group_by(year) %>% 
  count() %>% 
  transmute(foreign = n) %>% 
  ungroup() 

# рубрики
top_sections <- all_tocs %>% 
  group_by(section) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  ungroup() %>% 
  top_n(n = 4)  %>% 
  pull(section)  
  
  
sections <- all_tocs %>%
  filter(section %in% top_sections) %>%
  count(year, section, .drop = F) %>% 
  complete(year, section) %>% 
  replace(is.na(.), 0)
  
lang_sect <- sections %>%  
  left_join(for_lang) %>% 
  replace(is.na(.), 0) #%>% 
  # left_join(total) %>% 
  # mutate(n = round((n / total), 1),
  #        foreign = round((foreign / total), 1))

# шрифт
font_add_google(name = "PT Sans Narrow", family = "PT Sans Narrow")


lang_sect %>%  
  ggplot(aes(year, n, fill = section, group = section)) +
  geom_col(position = position_dodge(width = 1)) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  geom_text(aes(label=n), position=position_dodge(width=0.9), 
            vjust=-0.25, color = "darkgrey") +
  guides(fill=guide_legend(title="Рубрика")) +
  geom_line(aes(year, foreign, 
                group = section), 
            stat = "identity",
            color = "grey30", 
            linetype = 2,
            linewidth = 0.9) +
  scale_x_discrete() +
  scale_fill_brewer(palette = "Accent") +
  theme(text=element_text(size = 12, family="PT Sans Narrow"))

ggsave("./images/sections.png")

# какие рубрики представлены в нескольких номерах

sect_distr <- all_tocs %>% 
  group_by(section, year, number, issue) %>% 
  count() %>% 
  ungroup() %>% 
  unite(issue, c(year, number, issue), sep = "_") %>% 
  group_by(section) %>% 
  count() %>% 
  arrange(-n)

# переводчки
all_tocs %>% 
  filter(section == "Переводы и публикации") %>% 
  group_by(author) %>% 
  count() %>% 
  arrange(-n)

# авторы
authors <- all_tocs %>% 
  group_by(author) %>% 
  summarise(n = n()) %>% 
  arrange(-n)

# редакторы
authors %>% 
  filter(grepl("Протопоп|Гарадж", author)) %>% 
  summarise(sum = sum(n))
  

