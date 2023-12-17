library(udpipe)
library(dplyr)
library(stringr)
library(stopwords)
library(showtext)
library(tidytext) 
library(topicmodels)
library(ggplot2)
library(text)
library(textmineR)


# files <- list.files("texts_clean_")
# file_paths <- paste0("texts_clean_/", files)
# 
# all_texts <- map(file_paths, readLines)
# names(all_texts) <- files
# 
# texts_tbl <- all_texts %>% 
#   stack() %>% 
#   rename(text = values) %>% 
#   transmute(id = str_remove(ind, ".txt"), 
#             text = text)


# лемматизируем
# russian_syntagrus <- udpipe_load_model(file = "russian-syntagrus-ud-2.5-191206.udpipe")
# 
# id_text <- texts_tbl %>% 
#   mutate(text = str_remove_all(text, "[«»]")) %>% 
#   mutate(text = str_replace_all(text, "/", " ")) %>% 
#   mutate(text = str_remove_all(text, "\\(\\d{4}-\\d{4}\\)"))
# 
# id_text_ann <- udpipe_annotate(russian_syntagrus, 
#                                id_text$text, 
#                                doc_id = id_text$id) %>% 
#   as_tibble() %>% 
#   mutate(lemma = tolower(lemma))

# удалить стоп-слова и мусор
# sw_ru <- stopwords("ru")
# 
# id_text_ann <- id_text_ann %>% 
#   filter(nchar(lemma) > 2) %>% 
#   filter(!lemma %in% sw_ru) %>% 
#   select(doc_id, lemma, upos) %>% 
#   mutate(lemma = case_when(str_detect(lemma, "платона|платно") ~ "платон",
#                            str_detect(lemma, "тимя|тимий") ~ "тимей",
#                            str_detect(lemma, "августина") ~ "августин",
#                            str_detect(lemma, "мария") ~ "марий",
#                            str_detect(lemma, "божия") ~ "божий",
#                            str_detect(lemma, "отц") ~ "отец",
#                            str_detect(lemma, "даймония") ~ "даймоний",
#                            str_detect(lemma, "одисся") ~ "одиссей",
#                            str_detect(lemma, "федра") ~ "федр",
#                            str_detect(lemma, "семень") ~ "семя",
#                            str_detect(lemma, "палам") ~ "палама",
#                            str_detect(lemma, "алкивиада") ~ "алкивиад",
#                            .default = lemma)) %>% 
#   filter(!str_detect(lemma, "[[:punct:]]")) %>% 
#   filter(!str_detect(lemma, "^ср$")) %>% 
#   filter(!lemma %in% c("самый", "многое", "случай", "статья", 
#                        "рис", "год", "фокин", "ный", "метр", 
#                        "ние", "орт", "факт", "член", "дело",
#                        "часть", "глава", "суть", "вопрос",
#                        "что́", "розан")) %>% 
#   filter(upos %in% c("ADJ", "NOUN", "PROPN"))

# save(id_text_ann, file = "./data/IdAnn.Rdata")

# считаем частотность
# text_count <- id_text_ann %>% 
#   group_by(doc_id, lemma) %>% 
#   count(lemma)
# 
# # удаляем слова, которые встречаются меньше двух раз 
# total <- text_count %>% 
#   group_by(lemma) %>% 
#   summarise(total = sum(n))
# 
# text_count_pruned <- text_count %>% 
#   left_join(total) %>% 
#   filter(total > 1) %>% 
#   select(-total)
# 
# # меняем формат
# text_dtm <- text_count_pruned %>% 
#   cast_sparse(doc_id, lemma, n) 

# save(text_dtm, file =  "./data/Sparse.Rdata")


