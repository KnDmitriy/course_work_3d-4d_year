install.packages("dplyr", "janeaustenr", "tidytext", "ggplot2" )  # не работает
library(dplyr)
library(janeaustenr)
library(tidytext)
library(stringr)

# install.packages("textstem")
# library(textstem)
install.packages("koRpus")
library(koRpus)
install.packages("NLP")
install.packages("RColorBrewer")
install.packages("corpus")
install.packages("text2vec")
# install.packages("spacyr")
install.packages("remotes")
remotes::install_github("quanteda/spacyr")
library("stringr")
library("tm")
library("SnowballC")
library("wordcloud") 
library("RColorBrewer")
library("spacyr")
library(qdap)  # для Bag of words 
install.packages('ru_core_news_md')
spacy_install()
# скачивание языковой модели для русского языка из пакета spacyr
spacy_download_langmodel("ru_core_news_sm")
spacy_initialize(model = "ru_core_news_sm")

# считывание набора данных, состоящего из размеченных текстов в виде датафрейма
banks_df <- read.csv("banks.csv", sep = '\t')
mystem <- function(doc) {
  
  # sdoc <- system('mystem -nl -e utf-8', intern=T, input=doc)
  # При получении нескольких вариантов mystem разделяет их
  # вертикальной чертой. Удалим черту и варианты.
  sdoc <- doc
  sdoc <- str_replace(sdoc, '\\|.*$', '')
  # Если mystem сомневается в результате, он добавляет знак вопроса. Удаляем.
  sdoc <- str_replace(sdoc, '\\?', '')
  sdoc <- paste(sdoc, collapse=" ")  # а это для чего?
  attributes(sdoc) <- attributes(doc)
  sdoc
}
feedback <- Corpus(VectorSource(banks_df[1, "Text"])) 
# feedback <- tm_map(feedback, iconv, 'cp1251', 'UTF-8')
feedback <- tm_map(feedback, stripWhitespace)
feedback <- tm_map(feedback, tolower)
feedback <- tm_map(feedback, removeNumbers)
feedback <- tm_map(feedback, removePunctuation)
feedback <- tm_map(feedback, mystem)
feedback <- tokenize_word_stems(feedback[[1]])
feedback <- tm_map(feedback, removeWords, c(stopwords("russian"),
                                            "это", "также",
                                            "быть", "мочь"))
library(quanteda)
feedback_tokens <- tokens(feedback[[1]][["content"]])
# banks_df$preprocessed_list_of_words <- tokens(feedback[["1"]][["content"]])['text1']
# banks_df$preprocessed_list_of_words[1]
freq_terms(2,feedback[[1]][["content"]] )

# bind_tf_idf(banks_df, )

# 
# book_words <- austen_books() %>%
#   unnest_tokens(word, text) %>%
#   count(book, word, sort = TRUE)
# 
# unnest_tokens(word, text)
# 
# total_words <- book_words %>% 
#   group_by(book) %>% 
#   summarize(total = sum(n))
# 
# book_words <- left_join(book_words, total_words)
# 
# book_words
# plot(book_words)
# 
# library(ggplot2)
# 
# ggplot(book_words, aes(n/total, fill = book)) +
#   geom_histogram(show.legend = FALSE) +
#   xlim(NA, 0.0009) +
#   facet_wrap(~book, ncol = 2, scales = "free_y")
# 
# 
# freq_by_rank <- book_words %>% 
#   group_by(book) %>% 
#   mutate(rank = row_number(), 
#          `term frequency` = n/total) %>%
#   ungroup()
# 
# freq_by_rank

