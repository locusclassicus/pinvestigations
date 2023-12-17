library(purrr)
library(udpipe)
library(dplyr)
library(stringr)
library(stopwords)
library(reshape2)
library(wordcloud)
library(showtext)

files <- list.files("texts_clean_")
file_paths <- paste0("texts_clean_/", files)

all_texts <- map(file_paths, readLines)
names(all_texts) <- files

texts_tbl <- all_texts %>% 
  stack() %>% 
  rename(text = values) %>% 
  transmute(id = str_remove(ind, ".txt"), 
            text = text)

# оставляем только рубрику и текст
toc_text <- all_tocs %>% 
  left_join(texts_tbl) %>% 
  filter(!is.na(text)) %>%
  select(section, text)

# лемматизируем
russian_syntagrus <- udpipe_load_model(file = "russian-syntagrus-ud-2.5-191206.udpipe")

toc_text <- toc_text %>% 
  mutate(text = str_remove_all(text, "[«»]")) %>% 
  mutate(text = str_replace_all(text, "/", " ")) %>% 
  mutate(text = str_remove_all(text, "\\(\\d{4}-\\d{4}\\)"))

# аннотируем 
toc_text_ann <- udpipe_annotate(russian_syntagrus, 
                              toc_text$text, 
                              doc_id = toc_text$section) %>% 
  as_tibble() %>% 
  mutate(lemma = tolower(lemma))

# save(toc_text_ann, file = "./data/PInvestigationsAnn.Rdata")

# удалить стоп-слова и мусор
sw_ru <- stopwords("ru")

toc_text_ann_pruned <- toc_text_ann %>% 
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
                           .default = lemma)) %>% 
  filter(!str_detect(lemma, "[[:punct:]]")) %>% 
  filter(!str_detect(lemma, "^ср$")) %>% 
  filter(!lemma %in% c("самый", "многое", "случай", "статья", 
                       "рис", "год", "фокин", "ный", "метр", 
                       "ние", "орт", "факт", "член", "дело",
                       "часть", "глава", "суть", "вопрос",
                       "что́", "розан")) %>% 
  filter(upos %in% c("ADJ", "NOUN", "PROPN"))

# save(toc_text_ann_pruned, file = "./data/SectionAnn.Rdata")

# наиболее частотные слова в каждой рубрике
section_names1 <- c("Платон и платоноведение", 
                   "Сократ и сократики", "Платонизм в России", 
                   "Неоплатонизм и патристика")

section_names2 <- c("Рецепция платонизма", 
                    "Хроника и рецензии", "Платон и Аристотель", 
                    "Платонизм и традиция")

section_lemma1 <- toc_text_ann_pruned %>% 
  group_by(doc_id) %>% 
  count(lemma) %>% 
  slice_max(order_by = n, n = 500) %>% 
  filter(doc_id %in% section_names1) 

section_lemma2 <- toc_text_ann_pruned %>% 
  group_by(doc_id) %>% 
  count(lemma) %>% 
  slice_max(order_by = n, n = 500) %>% 
  filter(doc_id %in% section_names2) 



# подготавливаем шрифты
font_add_google(name = "PT Sans Narrow", family = "PT Sans Narrow")

# сравнительное облако слов
par(mar = c(1,1,1,1))

# строим график 
section_lemma1 %>%
  acast(lemma ~ doc_id, fill = 0) %>%
  comparison.cloud(scale=c(6,0.8),
                   max.words = 600,
                   random.order=F,
                   rot.per=.1,
                   use.r.layout=F,
                   title.size=1,
                   title.colors=NULL,
                   match.colors=F,
                   family = "PT Sans Narrow",
                   title.bg.colors = "grey"
                   )



section_lemma2 %>%
  acast(lemma ~ doc_id, fill = 0) %>%
  comparison.cloud(scale=c(6,0.8),
                   max.words = 600,
                   random.order=F,
                   rot.per=.1,
                   use.r.layout=F,
                   title.size=1,
                   title.colors=NULL,
                   match.colors=F,
                   family = "PT Sans Narrow",
                   title.bg.colors = "grey"
  )

