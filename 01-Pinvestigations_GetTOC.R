library(rvest)
library(tidyverse)
library(purrr)

my_url <- "http://www.pinvestigations.ru/content/Archive/issueList.aspx"
html <- read_html(my_url)

# достаем ненумерованный список со ссылками на номера журналов
list <- html %>% 
  html_elements("ul") %>% 
  pluck(3) 

# достаем ссылки на номера журналов
links <- list %>% 
  html_elements("li") %>% 
  html_elements("a") %>% 
  html_attr("href")


# создаем список url-адресов
urls <- str_c("http://www.pinvestigations.ru", links) %>% 
  str_replace_all("\\\\", "/")

urls <- urls[-1]

# функция для извлечения метаданных
get_toc <- function(my_url) {
  
  html <- read_html(my_url) 
  
  # все оглавление
  TOC <- html %>% 
    html_element(".IssueTOC") %>% 
    html_table()
  
  # рубрика
  TOC <- TOC %>%
    mutate_at(c(2), ~na_if(., '')) %>% 
    fill(X2) %>% 
    filter(!X1 == X2) %>% 
    select(-X5) %>% 
    rename(author = X1, section = X2, title = X3, pages = X4)
  
  # язык
  images <- html %>% 
    html_elements("img") %>% 
    html_attr("src") 
  
  lang <- images %>% 
    str_subset("flags") %>% 
    str_extract("(?<=flags/)\\w+")
  
  TOC <- TOC %>% 
    mutate(language = lang)
  
  # ссылка на скачивание 
  stable_urls <- html %>% 
    html_elements("nobr") %>% 
    html_elements("a") %>% 
    html_attr("href") %>% 
    str_subset("stable")
  
  TOC <- TOC %>% 
    mutate(stable_url = stable_urls)
  
  # выпуск, год
  issue <- html %>% 
    html_elements("#ctl00_content_lblIssueNo") %>% 
    html_text2()
  
  TOC <- TOC %>% 
    mutate(issue = issue, .before = author) %>% 
    mutate(year = str_extract(issue, pattern = "(?<=\\()\\d{4}"), 
           .after = issue) %>% 
    mutate(number = str_extract(issue, pattern = "(?<=Вып\\.\\s)\\d{1,2}"), 
           .before = issue) %>% 
    mutate(issue = str_sub(issue, -2, -2))
  
  return(TOC)
}

# запускаем 
all_tocs <- map_df(urls, get_toc)

# чистим
all_tocs <- all_tocs %>% 
  mutate(section = str_remove(section, "\\d\\.")) %>% 
  mutate(section = str_squish(section))

# переименовать очень редкие рубрики
all_tocs <- all_tocs %>% 
  mutate(section = case_when(
    str_detect(section, "[Рр]ецепц") ~ "Рецепция платонизма",
    str_detect(section, "[Рр]усс?к") ~ "Платонизм в России",
    str_detect(section, "платоновед") ~ "Платон и платоноведение",
    str_detect(section, "[Сс]окра") ~ "Сократ и сократики",
    str_detect(section, "христиан") ~ "Неоплатонизм и патристика",
    str_detect(section, "[Пп]еревод") ~ "Переводы и публикации",
    str_detect(section, "[Хх]роника|[Рр]еценз") ~ "Хроника и рецензии",
    str_detect(section, "Платон.* и традиция") ~ "Платонизм и традиция",
    .default = section
      )
    )

# почистить имена (убрать латиницу там, где есть кириллица)

all_tocs <- all_tocs %>% 
  mutate(
    author = str_remove_all(
      author, "(?<=[\u0400-\u04FF]\\.)[^[\u0400-\u04FF]]+$"))
  
  
# добавить id
all_tocs <- all_tocs %>% 
  arrange(year, number, issue) %>% 
  mutate(id = paste0(author, "_", year, "_", issue)) %>% 
  mutate(id = str_remove_all(id, pattern = "[\\s\\.]")) %>% 
  mutate(id = str_replace_all(id, pattern = ",", "_"))

all_tocs <- all_tocs %>% 
  relocate(id, .before = number)

save(all_tocs, file = "./data/TOC.Rdata")



