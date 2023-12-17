library(pdftools)
library(purrr)

# 02 прочесть pdf и записать файлы
pdfs_test <- list.files("./PInXIX/", pattern = "pdf")
pdf_test_names <- paste0("./PInXIX/", pdfs_test)
texts_test <- map(pdf_test_names, pdf_text)
names(texts_test) <- pdfs_test

text_names <- str_replace_all(names(texts_test), "pdf", "txt")
text_names <- paste0("./PInXIX/", text_names)
map2(texts_test, text_names, writeLines)

# 03 почистить
source("clean_test.R")
texts_clean <- map(text_names, clean_test)
names(texts_clean) <- str_remove_all(pdfs_test, "\\.pdf")

# записываем файлы
file_names <- paste0("./PInXIX/test_clean/", names(texts_clean), ".txt")
map2(texts_clean, file_names, writeLines)
# дочищала руками

# сделать dtm
all_texts <- map(file_names[-1], readLines)
names(all_texts) <- names(texts_clean[-1])

texts_tbl <- all_texts %>%
  stack() %>%
  rename(text = values) %>%
  transmute(id = str_remove(ind, ".txt"),
            text = text)

# лемматизируем
russian_syntagrus <- udpipe_load_model(file = "russian-syntagrus-ud-2.5-191206.udpipe")

id_text <- texts_tbl %>%
  mutate(text = str_remove_all(text, "[«»]")) %>%
  mutate(text = str_replace_all(text, "/", " ")) %>%
  mutate(text = str_remove_all(text, "\\(\\d{4}-\\d{4}\\)"))

id_text_ann <- udpipe_annotate(russian_syntagrus,
                               id_text$text,
                               doc_id = id_text$id) %>%
  as_tibble() %>%
  mutate(lemma = tolower(lemma))

# удалить стоп-слова и мусор
sw_ru <- stopwords("ru")
#
id_text_ann <- id_text_ann %>%
  filter(nchar(lemma) > 2) %>%
  filter(!lemma %in% sw_ru) %>%
  select(doc_id, lemma, upos) %>%
  mutate(lemma = case_when(str_detect(lemma, "платона|платно") ~ "платон",
                           str_detect(lemma, "тимя|тимий") ~ "тимей",
                           str_detect(lemma, "августина") ~ "августин",
                           str_detect(lemma, "мария") ~ "марий",
                           str_detect(lemma, "божия") ~ "божий",
                           str_detect(lemma, "отц") ~ "отец",
                           str_detect(lemma, "даймония") ~ "даймоний",
                           str_detect(lemma, "одисся") ~ "одиссей",
                           str_detect(lemma, "федра") ~ "федр",
                           str_detect(lemma, "семень") ~ "семя",
                           str_detect(lemma, "палам") ~ "палама",
                           str_detect(lemma, "алкивиада") ~ "алкивиад",
                           .default = lemma)) %>%
  filter(!str_detect(lemma, "[[:punct:]]")) %>%
  filter(!str_detect(lemma, "^ср$")) %>%
  filter(!lemma %in% c("самый", "многое", "случай", "статья",
                       "рис", "год", "фокин", "ный", "метр",
                       "ние", "орт", "факт", "член", "дело",
                       "часть", "глава", "суть", "вопрос",
                       "что́", "розан")) %>%
  filter(upos %in% c("ADJ", "NOUN", "PROPN"))


# считаем частотность
text_count <- id_text_ann %>%
  group_by(doc_id, lemma) %>%
  count(lemma)
# 
# удаляем слова, которые встречаются меньше двух раз
total <- text_count %>%
  group_by(lemma) %>%
  summarise(total = sum(n))
# 
text_count_pruned <- text_count %>%
  left_join(total) %>%
  filter(total > 1) %>%
  select(-total)
# 
# меняем формат
text_dtm <- text_count_pruned %>%
  cast_sparse(doc_id, lemma, n)

# predict
assignments <- predict(model, text_dtm,
                       method = "gibbs", 
                       iterations = 200,
                       burnin = 180,
                       cpus = 8)

# tidy and explore 
tidy_ass <- assignments %>% 
  as_tibble(rownames = "author") %>% 
  pivot_longer(-1, names_to = "topic", values_to = "gamma") %>% 
  mutate(topic = as.numeric(str_remove(topic, "t_")))

# всего гамма
total_ass <- tidy_ass %>% 
  summarise(total = sum(gamma))

# доля отдельных топиков
topic_ratio <- tidy_ass %>% 
  group_by(topic) %>% 
  summarise(total_topic = sum(gamma)) %>% 
  mutate(ratio = (total_topic / 15) * 100) %>% 
  arrange(-ratio)

# доля отдельных кластеров
topic_ratio %>% 
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
  group_by(cluster) %>% 
  summarize(sum = sum(ratio)) %>% 
  arrange(-sum)

# в каких статьях представлена тема
tidy_ass %>% 
  filter(topic == 11) %>% 
  arrange(-gamma)

# какие темы превалируют в статьях?
tidy_ass %>% 
  group_by(author) %>% 
  top_n(gamma, n = 1) %>% 
  arrange(-gamma)
  
tidy_ass %>% 
  group_by(topic) %>% 
  summarize(sum = sum(gamma)) %>% 
  arrange(-sum)

tidy_ass %>% 
  ggplot(aes(as.factor(topic), gamma, color = author)) + 
  geom_boxplot(show.legend = F) +
  facet_wrap(~author, nrow = 4, scales = "free") +
  xlab(NULL) +
  theme_bw()

tidy_ass %>% 
  group_by(topic) %>% 
  top_n(gamma, n =1) %>% 
  arrange(-gamma)
