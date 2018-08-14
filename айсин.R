library("rtweet")
library(httpuv)
library(readr)
library(twitteR)
library(corpora)
create_token(app = "**************", "*******************", "**********************",
             set_renv = TRUE)
aysin <- get_timeline(user = "ruslanaysin",n = 3200)

aysin <- janitor::remove_empty(aysin, "cols")
library(janitor)
knitr::kable(head(aysin))
library("tidytext")
library("dplyr")
library("rcorpora")
library("lubridate")
library("ggplot2")
library("viridis")
library(quanteda)

aysin <- subset(aysin, created_at < "2018-01-10 22:54:11" & created_at > "2017-04-26 00:00:00")
new.variable <- as.vector(aysin$text) 
write_as_csv(aysin, "2017-04-2018-01-aysintweets.csv", fileEncoding = "UTF-8")
aysin <- readr::read_csv("2017-04-2018-01-aysintweets.csv")


# Here I extract necessary column in order to save it as plain text
new.variable <- as.vector(aysin$text) 
cat(new.variable,file="MAC text aysin twitter.txt", encoding = "utf-8")

text <- readLines(file.choose(), encoding = "UTF-8")
docs <- Corpus(VectorSource(text))
options(max.print=999999)
inspect(docs)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

docs <- tm_map(docs, toSpace, "/")
#docs <- tm_map(docs, toSpace, "\")
docs <- tm_map(docs, toSpace, "—")
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, "–")

docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove russian common stopwords
docs <- tm_map(docs, removeWords, stopwords("russian"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("российский", "сколько","рт", "татарский", "татарстан", "тысруб", "татарстана", "татнефти", "большой", "никакой", "просто", "однако", "нашапочта", "деньга", "нампишут", "добрый", "день", "лицо", "лишь", "ваш", "https", "tme", "сообщает", "банка", "хотя", "тд", "млн", "миннихан", "причём", "менее", "считаю", "вид", "иметь", "самый", "смочь", "многий", "стать", "счёт", "должный", "наш", "ха", "свой", "свой", "часто", "идти", "часть", "весь", "уровень", "дать", "хотеть", "людей", "ещё", "мочь", "своей", "стран", "года", "лет", "таким", "хочу", "счет", "других", "также", "числе", "пока", "является", "свои", "этих", "образом", "лет", "годы", "таких", "будут", "тех", "будет", "году", "год","наша", "наши", "эта", "раза", "прежде", "раза", "нужно", "именно", "очень", "своих", "своим", "нашим", "необходимо", "будем", "наших", "те", "такую", "нашей", "я", "мой", "это", "должны", "должен", "должна", "которая", "которые", "который", "россии", "россия", "страны", "страна", "нам")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
docs <- unlist(docs, recursive = TRUE, use.names = TRUE)
text.tmp <- system2("/Users/aidarzinnatullin/Downloads/mystem", c("-c", "-l", "-d"), input = docs, stdout = TRUE)
text.tmp
cat(text.tmp,file="название файла после майстема.txt", encoding = "utf-8")



######## The next step is to use script for Putin


stopwords <- corpora("words/stopwords/ru")$stopWords

g <- get_trends("Kazan")

aysin_words <- aysin %>% unnest_tokens(aysin, text) %>% count(aysin, sort = TRUE) %>% filter(!aysin %in% stopwords("russian")) %>% filter(!aysin %in% c("t.co", "https"))
knitr::kable(head(timurshin_words, n = 20))
aysin <- mutate(aysin, wday = as.factor(wday(created_at, label = TRUE)))
aysin <- mutate(aysin, hour = as.factor(hour(created_at)))
aysin <- mutate(aysin, week = week(created_at))
aysin <- mutate(aysin, day = as.Date(created_at))
weekday_dat <- aysin %>%
  group_by(week, wday) %>%
  summarize(n = n(), created_at = created_at[1]) 

arrange(weekday_dat, desc(n)) %>%
  head() %>%
  knitr::kable()
ggplot(weekday_dat) +
  geom_boxplot(aes(wday, n),
               outlier.shape = NA) +
  scale_y_continuous(limits =  quantile(weekday_dat$n, c(0, 0.9)))
min(aysin$created_at)
## [1] "2016-12-14 19:06:23 UTC"
max(aysin$created_at)
## [1] "2017-02-28 20:00:42 UTC"
ggplot(aysin) + geom_histogram(aes(favorite_count))
ggplot(aysin) + geom_histogram(aes(retweet_count))
arrange(aysin, desc(retweet_count)) %>% head() %>% knitr::kable()
ggplot(aysin) +
  geom_point(aes(retweet_count, favorite_count))


rweekly_org <- get_timeline(user = "rweekly_org",
                            n = 1000)

ggplot(rweekly_org) +
  geom_point(aes(retweet_count, favorite_count)) +
  ggtitle("rweekly_org's timeline")
model <- lm(favorite_count ~ retweet_count, data = rbloggers)
broom::tidy(model) %>% knitr::kable()
model2 <- lm(favorite_count ~ retweet_count, data = rweekly_org)
broom::tidy(model2) %>% knitr::kable()
