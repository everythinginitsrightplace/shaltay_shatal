library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("quanteda")
library(readtext)
setwd("/Volumes/Sony_16GR/Анализ данных в R часть 2")

g <- readtext("ЛСА ШАЛТАЙ БАБАЙ.txt")
g <- g$text
h <- readtext("ЛСА АЙСИН БО.txt")
h <- h$text
text_super <- c(g, h)
media <- factor(rep(c("Шалтай Бабай", "Статьи в БО")))
df <- data.frame(text_super, media, stringsAsFactors=FALSE)
corpus <- Corpus(VectorSource(df$text_super))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, function(x) removeWords(x, stopwords("russian")))
#corpus <- tm_map(corpus, stemDocument, language = "russian")

corpus <- corpus(corpus)

news_toks <- tokens(corpus, remove_punct = TRUE) 
news_dfm <- dfm(news_toks)

key <- textstat_keyness(news_dfm, target =  "text1")
attr(key, 'documents') <- c('Шалтай Бабай', 'Статья в БО')

textplot_keyness(key)
