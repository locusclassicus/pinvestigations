library(pdftools)
library(udpipe)
library(tesseract)

# статьи на русском языке
ru_id <- all_tocs %>% 
  filter(language == "ru") %>% 
  pull(id)

ru_url <- all_tocs %>% 
  filter(language == "ru") %>% 
  pull(stable_url)

# скачать
map2(ru_url, 
     paste0("./pdfs/", ru_id, ".pdf"),
     download.file)

# прочесть нормальные pdf
pdfs_norm <- list.files("./pdfs/")
pdf_norm_names <- paste0("./pdfs/", pdfs_norm)
texts_norm <- map(pdf_norm_names, pdf_text)
names(texts_norm) <- pdfs_norm

# прочесть кривые pdf
# tesseract_download("rus")
# tesseract_download("grc")
pdfs_crooked <- list.files("./pdf_ocr")
pdf_crooked_names <- paste0("./pdf_ocr/", pdfs_crooked)
texts_crooked <- map(pdf_crooked_names, pdf_ocr_text, language = "rus+eng+grc")
names(texts_crooked) <- pdfs_crooked

# записать файлы
texts_all <- c(texts_norm, texts_crooked)
text_names <- str_replace_all(names(texts_all), "pdf", "txt")
text_names <- paste0("./texts_raw/", text_names)
map2(texts_all, text_names, writeLines)

