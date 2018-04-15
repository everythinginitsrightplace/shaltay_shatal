library(readtext)
library(quanteda)
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
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

inaug_toks <- tokens(corpus)
inaug_dfm <- dfm(inaug_toks, remove = stopwords('russian'))
lexdiv <- textstat_lexdiv(inaug_dfm)
tail(lexdiv, 5)

plot(lexdiv$TTR, type = 'l', xaxt = 'n', xlab = NULL, ylab = "TTR")
grid()
axis(1, at = seq_len(nrow(lexdiv)), labels = docvars(inaug_dfm, field = NULL))
