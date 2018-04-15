library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("quanteda")
library(readtext)
library(ggplot2)

#### Для начала определим то, какие коллокации в анализируемых массивах текстов являются
#### наиболее часто встречающимися

# Slaltay
text <- readLines(file.choose())
collocations <- textstat_collocations(text, size = 2:3)
wordcloud(words = collocations$collocation, freq = collocations$count,
          scale=c(2,.2), min.freq = 10, max.words=Inf, 
          random.order=FALSE, rot.per=0.1, ordered.colors=FALSE, 
          random.color=TRUE, colors=brewer.pal(8, "Dark2"))
write.csv(collocations, file = "collocations_shaltay.csv")

# BO
text_1 <- readLines(file.choose())
collocations_2 <- textstat_collocations(text_1, size = 2:3)
wordcloud(words = collocations_2$collocation, freq = collocations_2$count,
          scale=c(2,.2), min.freq = 5, max.words=Inf, 
          random.order=FALSE, rot.per=0.1, ordered.colors=FALSE, 
          random.color=TRUE, colors=brewer.pal(8, "Dark2"))

write.csv(collocations_2, file = "collocations_bo.csv")

# Twitter
text_3 <- readLines(file.choose())
collocations_3 <- textstat_collocations(text_3, size = 2:3)
collocations_3 <- collocations_3[-c(1, 2, 3, 4,5 , 6, 7, 9, 10, 11, 12, 14, 24, 27, 34, 40, 105, 154, 159, 161, 171, 173, 174, 177, 181, 183, 184, 189, 202, 225, 226, 238, 239, 242), ]
wordcloud(words = collocations_3$collocation, freq = collocations_3$z,
          scale=c(2,.2), min.freq = 8.5, max.words=Inf, 
          random.order=FALSE, rot.per=0.1, ordered.colors=FALSE, 
          random.color=TRUE, colors=brewer.pal(8, "Dark2"))


#####  Далее приступаем к латентному семантическому анализу
library(readtext)
g <- readtext("ЛСА ШАЛТАЙ БАБАЙ.txt")
g <- g$text
h <- readtext("ЛСА АЙСИН БО.txt")
h <- h$text
j <- readtext("ЛСА АЙСИН ТВИТТЕР.txt")
j <- j$text
text_super <- c(g, h, j)
media <- factor(rep(c("Шалтай Бабай", "Статьи в БО", "Твиттер")))
df <- data.frame(text_super, media, stringsAsFactors=FALSE)
corpus <- Corpus(VectorSource(df$text_super))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, function(x) removeWords(x, stopwords("russian")))
#corpus <- tm_map(corpus, stemDocument, language = "english")
#corpus # check corpus
td.mat <- as.matrix(TermDocumentMatrix(corpus))

library(lsa)
library(reshape2)
lsa_space<-lsa(td.mat, dims=dimcalc_share()) 
tdm_lsa<- as.textmatrix(lsa_space)
lsa.distances<-cosine(tdm_lsa) # косинусное расстояние между текстами в LSA-пространстве
rownames(lsa.distances) <- df$media 
colnames(lsa.distances) <- df$media 
lsa.distances[upper.tri(lsa.distances)] <- NA 
diag(lsa.distances)=NA
lsa.matrix <- melt(lsa.distances)
colnames(lsa.matrix) <- c("Source","Target", "Weight")
lsa.matrix<-lsa.matrix[lsa.matrix$Weight > 0, ] 
lsa.matrix<-lsa.matrix[!(is.na(lsa.matrix$Weight)), ]
lsa.matrix$Type <- "Undirected" 
write.csv(lsa.matrix, "sh_tw_bo.csv")
write.csv(df$media, "labels_media.csv")
lsa_word_space<-lsa(td.mat, dims=dimcalc_share()) 
tdm_word_lsa<-as.textmatrix(lsa_word_space)
tdm_word_lsa




########
td.mat
td.mat.lsa <- lw_bintf(td.mat) * gw_idf(td.mat) # weighting
lsaSpace <- lsa(td.mat.lsa) # create LSA space
dist.mat.lsa <- dist(t(as.textmatrix(lsaSpace))) # compute distance matrix
dist.mat.lsa # check distance mantrix
fit <- cmdscale(dist.mat.lsa, eig=TRUE, k=2)
points <- data.frame(x=fit$points[, 1], y=fit$points[, 2])
ggplot(points,aes(x=x, y=y)) + 
  geom_point(data=points,aes(x=x, y=y, color=df$media), size = 5) + 
  geom_text(data=points,aes(x=x, y=y-0.2, label=row.names(df)))

library(scatterplot3d)
fit <- cmdscale(dist.mat.lsa, eig=TRUE, k=2)
colors <- rep(c("blue", "green", "red"))
scatterplot3d(fit$points[, 1], fit$points[, 2], fit$points[, 2], color=colors, pch=16, 
              main="Семантическое пространство в 3D", xlab="x", ylab="y", zlab="z", type="h")
#########