library(stringr)
library(purrr)

clean_text <- function(file_name){
  # читает файл
  text_file <- readLines(paste0("./texts_raw/", file_name))
  text_file_collapsed <- str_c(text_file, collapse = "\n")
  
  # удаляет running heads
  text_clean1 <- str_remove_all(text_file_collapsed, "\\d{1,3}\n\n *?.+\n\n?\n?")
  
  # удаляет переносы слов и копирайт с аффилиацией
  text_clean2 <- str_remove_all(text_clean1, "-\n") %>% 
    str_remove_all("©[\\S\\s]+?\n")
  
  # достает источники и литературу и сохраняет в отдельный файл
  bib <- unlist(str_extract_all(text_clean2, "(Источники|Литература)(\n+.*)+"))
  writeLines(bib, con = paste0("./bibs/bib_", file_name))
  
  # удаляет источники и литературу из текста
  text_clean3 <- str_remove_all(text_clean2, "(Источники|Литература)(\n*.*)+")
  
  # заменяет перенос строки на пробел, удаляет ссылки на гранты и все до DOI
  text_clean4 <- str_replace_all(text_clean3, "\n+", " ") %>% 
    str_remove("[\\s\\S]+(DOI)|(doi)") %>% 
    str_remove("Исследование (выполнено|осуществлено|проведено)[\\S\\s]+?\\.\\W")
  
  # удаляет верхние индексы
  text_clean5 <- str_remove_all(text_clean4, "\\p{No}")
  
  # удаляет цифры
  text_clean6 <- str_remove_all(text_clean5, "\\d+?")
  
  # удаляет греческий
  text_clean7 <- str_remove_all(text_clean6, "[[\u0370-\u03FF][\U1F00-\U1FFF]]")
  
  # удаляет латиницу
  text_clean8 <- str_remove_all(text_clean7, "[[\U0041-\U007F][\U0080-\U00FF]]")
  
  # удаляет лишнюю пунктуацию, лишние пробелы и др.
  text_clean9 <- str_remove_all(text_clean8, "(?<=\\W)[[:punct:]]") %>% 
    str_remove_all("\\)") %>% 
    str_remove_all("(?<=\\s|^)[Сс]м\\.") %>% 
    str_remove_all("(?<=\\s|^)[Сс]р\\.") %>% 
    str_remove_all("[Нн]апр\\.") %>% 
    str_remove_all("@") %>% 
    str_remove_all("[“”]") %>% 
    str_squish()
  
  # результат
  return(text_clean9)
}

# применяем ко всем файлам
texts_clean <- map(files, clean_text)
names(texts_clean) <- files

# записываем файлы
file_names <- paste0("./texts_clean/", files)

map2(texts_clean, file_names, writeLines)

# тут надо будет поправить руками (не везде есть слово "Литература" или "Источники")


