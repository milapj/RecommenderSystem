---
## title: "422_HW4"
## author: "milap"
## date: "11/29/2017"

library(cluster)
library(factoextra)
library(fpc)
library(textreuse)
library(magrittr)
rm(list=ls())
setwd("/Users/Milap/Desktop/CS422_DataMining/")
data_countries <- read.csv("lang1.csv", sep=",", header = T, row.names = 'X')

hc.complete <- eclust(data_countries, "hclust", hc_method="complete")
fviz_dend(hc.complete)

hc.single <- eclust(data_countries,"hclust", hc_method = "single")
fviz_dend(hc.single)

hc.average <- eclust(data_countries,"hclust",hc_method = "average")
fviz_dend(hc.average)

average_cut <- cutree(hc.average,h=125)
table(average_cut)


hc.average_1 <- eclust(data_countries,"hclust",k = 7,hc_method = "average")
fviz_dend(hc.average_1)
hc.single_1 <- eclust(data_countries,"hclust",k = 7,hc_method = "single")
fviz_dend(hc.single_1)
hc.complete_1 <- eclust(data_countries,"hclust",k = 7,hc_method = "complete")
fviz_dend(hc.complete_1)


library(fpc)

stats_single <- cluster.stats(dist(data_countries), hc.single_1$cluster, silhouette=TRUE)
stats_single$dunn
stats_single$avg.silwidth

stats_complete <- cluster.stats(dist(data_countries), hc.complete_1$cluster, silhouette=TRUE)
stats_complete$dunn
stats_complete$avg.silwidth

stats_average<- cluster.stats(dist(data_countries), hc.average_1$cluster,silhouette=TRUE)
stats_average$dunn
stats_average$avg.silwidth


input_files <- list.files("/Users/Milap/Downloads/corpus", full.names = T)
minhash <- minhash_generator(n=160, seed=100)
corpus <- TextReuseCorpus(input_files, tokenizer = tokenize_ngrams, n = 5,
                          minhash_func = minhash, keep_tokens = TRUE)
length(unlist(tokens(corpus)))


library(magrittr)
totaltokens <- tokens(corpus)
corpusMat <- list.files("/Users/Milap/Downloads/corpus", full.names=F)
doc_dict <- unlist(tokens(corpus)) %>% unique()
Matr <- lapply(totaltokens, function(set, dict) {   as.integer(dict %in% set)}, dict = doc_dict) %>% data.frame() 
tempSetName <-setNames( Matr, paste( corpusMat, 1:length(corpusMat)) )
rownames(Matr) <- doc_dict
dim(Matr)

tokens(corpus[["orig_taske"]])[1:5]


lsh_probability(h = 240, b =  20, s = 0.3)
lsh_probability(h = 240, b =  30, s = 0.3)
lsh_probability(h = 240, b =  60, s = 0.3)
lsh_probability(h = 240, b =  80, s = 0.3)


buckets <- lsh(corpus, bands = 80)
candidates <- lsh_candidates(buckets)
candidatepair<-nrow(candidates)
candidatepair
 

lshResult <- lsh_compare(candidates, corpus, jaccard_similarity)
lshResult[order(lshResult$score,decreasing = TRUE),][1:5,]



data_full <- read.csv("/Users/Milap/Desktop/CS422_DataMining/u.csv", sep="\t", header=T, comment.char = "#")
data_item <- read.csv("/Users/Milap/Desktop/CS422_DataMining/u_item.csv", sep="|", header=T, comment.char = "#")

user200_rownum <- which(data_full$user== 200)
user50_rownum <- which(data_full$user == 50)
user200 <- data_full[user200_rownum,]
user50 <- data_full[user50_rownum,]

movies200 <- data_item[user200[,2],]
movies50 <- data_item[user50[,2],]

movie.matrix200 <- movies200[,6:24] 
genre200 <- apply(movie.matrix200,2,mean)
vector200 <- as.vector(genre200)

movie.matrix50 <- movies50[,6:24]
genre50 <- apply(movie.matrix50,2,mean)
vector50 <- as.vector(genre50)

cosine <- function(x, y) {sum(x*y)/(norm(x, type="2") * norm(y, type="2"))}


cosine(vector200,vector50)

movie127 <- data_item[127,]
vector127 <- as.vector(movie127[,6:24])



cosine(vector127,vector200)


cosine(vector127,vector50)

user1_rownum_1 <- which(data_full$user== 1 & data_full$iditem == 1)
user1_rownum_2 <- which(data_full$user== 1 & data_full$iditem == 2)
user1_rownum_3 <- which(data_full$user== 1 & data_full$iditem == 3)
user1_rownum_4 <- which(data_full$user== 1 & data_full$iditem == 4)
user1_rownum_5 <- which(data_full$user== 1 & data_full$iditem == 5)
user1_rownum_6 <- which(data_full$user== 1 & data_full$iditem == 6)
user1_1 <- data_full[user1_rownum_1,3]
user1_2 <- data_full[user1_rownum_2,3]
user1_3 <- data_full[user1_rownum_3,3]
user1_4 <- data_full[user1_rownum_4,3]
user1_5 <- data_full[user1_rownum_5,3]
user1_6 <- data_full[user1_rownum_6,3]
user1_ratings <- c(user1_1,user1_2,user1_3,user1_4,user1_5,user1_6)

user21_rownum_1 <- which(data_full$user== 21 & data_full$iditem == 1)
user21_rownum_2 <- which(data_full$user== 21 & data_full$iditem == 2)
user21_rownum_3 <- which(data_full$user== 21 & data_full$iditem == 3)
user21_rownum_4 <- which(data_full$user== 21 & data_full$iditem == 4)
user21_rownum_5 <- which(data_full$user== 21 & data_full$iditem == 5)
user21_rownum_6 <- which(data_full$user== 21 & data_full$iditem == 6)
user21_1 <- data_full[user21_rownum_1,3]
user21_2 <- data_full[user21_rownum_2,3]
user21_3 <- data_full[user21_rownum_3,3]
user21_4 <- data_full[user21_rownum_4,3]
user21_5 <- data_full[user21_rownum_5,3]
user21_6 <- data_full[user21_rownum_6,3]
user21_ratings <- c(user21_1,0,0,0,user21_5,0)

user44_rownum_1 <- which(data_full$user== 44 & data_full$iditem == 1)
user44_rownum_2 <- which(data_full$user== 44 & data_full$iditem == 2)
user44_rownum_3 <- which(data_full$user== 44 & data_full$iditem == 3)
user44_rownum_4 <- which(data_full$user== 44 & data_full$iditem == 4)
user44_rownum_5 <- which(data_full$user== 44 & data_full$iditem == 5)
user44_rownum_6 <- which(data_full$user== 44 & data_full$iditem == 6)
user44_1 <- data_full[user44_rownum_1,3]
user44_2 <- data_full[user44_rownum_2,3]
user44_3 <- data_full[user44_rownum_3,3]
user44_4 <- data_full[user44_rownum_4,3]
user44_5 <- data_full[user44_rownum_5,3]
user44_6 <- data_full[user44_rownum_6,3]
user44_ratings <- c(user44_1,0,0,0,user44_5,0)

user59_rownum_1 <- which(data_full$user== 59 & data_full$iditem == 1)
user59_rownum_2 <- which(data_full$user== 59 & data_full$iditem == 2)
user59_rownum_3 <- which(data_full$user== 59 & data_full$iditem == 3)
user59_rownum_4 <- which(data_full$user== 59 & data_full$iditem == 4)
user59_rownum_5 <- which(data_full$user== 59 & data_full$iditem == 5)
user59_rownum_6 <- which(data_full$user== 59 & data_full$iditem == 6)
user59_1 <- data_full[user59_rownum_1,3]
user59_2 <- data_full[user59_rownum_2,3]
user59_3 <- data_full[user59_rownum_3,3]
user59_4 <- data_full[user59_rownum_4,3]
user59_5 <- data_full[user59_rownum_5,3]
user59_6 <- data_full[user59_rownum_6,3]
user59_ratings <- c(user59_1,0,user59_3,user59_4,0,0)

user72_rownum_1 <- which(data_full$user== 72 & data_full$iditem == 1)
user72_rownum_2 <- which(data_full$user== 72 & data_full$iditem == 2)
user72_rownum_3 <- which(data_full$user== 72 & data_full$iditem == 3)
user72_rownum_4 <- which(data_full$user== 72 & data_full$iditem == 4)
user72_rownum_5 <- which(data_full$user== 72 & data_full$iditem == 5)
user72_rownum_6 <- which(data_full$user== 72 & data_full$iditem == 6)
user72_1 <- data_full[user72_rownum_1,3]
user72_2 <- data_full[user72_rownum_2,3]
user72_3 <- data_full[user72_rownum_3,3]
user72_4 <- data_full[user72_rownum_4,3]
user72_5 <- data_full[user72_rownum_5,3]
user72_6 <- data_full[user72_rownum_6,3]
user72_ratings <- c(user72_1,user72_2,0,0,user72_5,0)

user82_rownum_1 <- which(data_full$user== 82 & data_full$iditem == 1)
user82_rownum_2 <- which(data_full$user== 82 & data_full$iditem == 2)
user82_rownum_3 <- which(data_full$user== 82 & data_full$iditem == 3)
user82_rownum_4 <- which(data_full$user== 82 & data_full$iditem == 4)
user82_rownum_5 <- which(data_full$user== 82 & data_full$iditem == 5)
user82_rownum_6 <- which(data_full$user== 82 & data_full$iditem == 6)
user82_1 <- data_full[user82_rownum_1,3]
user82_2 <- data_full[user82_rownum_2,3]
user82_3 <- data_full[user82_rownum_3,3]
user82_4 <- data_full[user82_rownum_4,3]
user82_5 <- data_full[user82_rownum_5,3]
user82_6 <- data_full[user82_rownum_6,3]
user82_ratings <- c(user82_1,0,user82_3,0,0,0)

user102_rownum_1 <- which(data_full$user== 102 & data_full$iditem == 1)
user102_rownum_2 <- which(data_full$user== 102 & data_full$iditem == 2)
user102_rownum_3 <- which(data_full$user== 102 & data_full$iditem == 3)
user102_rownum_4 <- which(data_full$user== 102 & data_full$iditem == 4)
user102_rownum_5 <- which(data_full$user== 102 & data_full$iditem == 5)
user102_rownum_6 <- which(data_full$user== 102 & data_full$iditem == 6)
user102_1 <- data_full[user102_rownum_1,3]
user102_2 <- data_full[user102_rownum_2,3]
user102_3 <- data_full[user102_rownum_3,3]
user102_4 <- data_full[user102_rownum_4,3]
user102_5 <- data_full[user102_rownum_5,3]
user102_6 <- data_full[user102_rownum_6,3]
user102_ratings <- c(user102_1,user102_2,0,user102_4,user102_5,0)


user234_rownum_1 <- which(data_full$user== 234 & data_full$iditem == 1)
user234_rownum_2 <- which(data_full$user== 234 & data_full$iditem == 2)
user234_rownum_3 <- which(data_full$user== 234 & data_full$iditem == 3)
user234_rownum_4 <- which(data_full$user== 234 & data_full$iditem == 4)
user234_rownum_5 <- which(data_full$user== 234 & data_full$iditem == 5)
user234_rownum_6 <- which(data_full$user== 234 & data_full$iditem == 6)
user234_1 <- data_full[user234_rownum_1,3]
user234_2 <- data_full[user234_rownum_2,3]
user234_3 <- data_full[user234_rownum_3,3]
user234_4 <- data_full[user234_rownum_4,3]
user234_5 <- data_full[user234_rownum_5,3]
user234_6 <- data_full[user234_rownum_6,3]
user234_ratings <- c(user234_1,user234_2,0,user234_4,user234_5,0)

user268_rownum_1 <- which(data_full$user== 268 & data_full$iditem == 1)
user268_rownum_2 <- which(data_full$user== 268 & data_full$iditem == 2)
user268_rownum_3 <- which(data_full$user== 268 & data_full$iditem == 3)
user268_rownum_4 <- which(data_full$user== 268 & data_full$iditem == 4)
user268_rownum_5 <- which(data_full$user== 268 & data_full$iditem == 5)
user268_rownum_6 <- which(data_full$user== 268 & data_full$iditem == 6)
user268_1 <- data_full[user268_rownum_1,3]
user268_2 <- data_full[user268_rownum_2,3]
user268_3 <- data_full[user268_rownum_3,3]
user268_4 <- data_full[user268_rownum_4,3]
user268_5 <- data_full[user268_rownum_5,3]
user268_6 <- data_full[user268_rownum_6,3]
user268_ratings <- c(user268_1,user268_2,user268_3,user268_4,0,0)


user409_rownum_1 <- which(data_full$user== 409 & data_full$iditem == 1)
user409_rownum_2 <- which(data_full$user== 409 & data_full$iditem == 2)
user409_rownum_3 <- which(data_full$user== 409 & data_full$iditem == 3)
user409_rownum_4 <- which(data_full$user== 409 & data_full$iditem == 4)
user409_rownum_5 <- which(data_full$user== 409 & data_full$iditem == 5)
user409_rownum_6 <- which(data_full$user== 409 & data_full$iditem == 6)
user409_1 <- data_full[user409_rownum_1,3]
user409_2 <- data_full[user409_rownum_2,3]
user409_3 <- data_full[user409_rownum_3,3]
user409_4 <- data_full[user409_rownum_4,3]
user409_5 <- data_full[user409_rownum_5,3]
user409_6 <- data_full[user409_rownum_6,3]
user409_ratings <- c(0,0,0,0,0,user409_6)


user486_rownum_1 <- which(data_full$user== 486 & data_full$iditem == 1)
user486_rownum_2 <- which(data_full$user== 486 & data_full$iditem == 2)
user486_rownum_3 <- which(data_full$user== 486 & data_full$iditem == 3)
user486_rownum_4 <- which(data_full$user== 486 & data_full$iditem == 4)
user486_rownum_5 <- which(data_full$user== 486 & data_full$iditem == 5)
user486_rownum_6 <- which(data_full$user== 486 & data_full$iditem == 6)
user486_1 <- data_full[user486_rownum_1,3]
user486_2 <- data_full[user486_rownum_2,3]
user486_3 <- data_full[user486_rownum_3,3]
user486_4 <- data_full[user486_rownum_4,3]
user486_5 <- data_full[user486_rownum_5,3]
user486_6 <- data_full[user486_rownum_6,3]
user486_ratings <- c(user486_1,0,user486_3,0,0,user486_6)


m <- cbind(user1_ratings,user21_ratings,user44_ratings,user59_ratings,user72_ratings,user82_ratings,user102_ratings,user234_ratings,user268_ratings,user409_ratings,user486_ratings)
m[m==0] <- NA
movie1 <- user1_ratings[1]+ user21_ratings[1]+user44_ratings[1]+user59_ratings[1]+user72_ratings[1]+user82_ratings[1]+user102_ratings[1]+user234_ratings[1]+user268_ratings[1]+user409_ratings[1]+user486_ratings[1]
movie2 <- user1_ratings[2]+ user21_ratings[2]+user44_ratings[2]+user59_ratings[2]+user72_ratings[2]+user82_ratings[2]+user102_ratings[2]+user234_ratings[2]+user268_ratings[2]+user409_ratings[2]+user486_ratings[2]
movie3 <- user1_ratings[3]+ user21_ratings[3]+user44_ratings[3]+user59_ratings[3]+user72_ratings[3]+user82_ratings[3]+user102_ratings[3]+user234_ratings[3]+user268_ratings[3]+user409_ratings[3]+user486_ratings[3]
movie4 <- user1_ratings[4]+ user21_ratings[4]+user44_ratings[4]+user59_ratings[4]+user72_ratings[4]+user82_ratings[4]+user102_ratings[4]+user234_ratings[4]+user268_ratings[4]+user409_ratings[4]+user486_ratings[4]
movie5 <- user1_ratings[5]+ user21_ratings[5]+user44_ratings[5]+user59_ratings[5]+user72_ratings[5]+user82_ratings[5]+user102_ratings[5]+user234_ratings[5]+user268_ratings[5]+user409_ratings[5]+user486_ratings[5]
movie6 <- user1_ratings[6]+ user21_ratings[6]+user44_ratings[6]+user59_ratings[6]+user72_ratings[6]+user82_ratings[6]+user102_ratings[6]+user234_ratings[6]+user268_ratings[6]+user409_ratings[6]+user486_ratings[6]


m[1,] <- (m[1,] - movie1/10)
m[2,] <- (m[2,] - movie2/5)
m[3,] <- (m[3,] - movie3/5)
m[4,] <- (m[4,] - movie4/5)
m[5,] <- (m[5,] - movie5/6)
m[6,] <- (m[6,] - movie6/3)
m[is.na(m)] <- 0
a <- cosine(m[5,],m[1,])
b <- cosine(m[5,],m[2,])
c <- cosine(m[5,],m[3,])
d <- cosine(m[5,],m[4,])
e <- cosine(m[5,],m[5,])
f <- cosine(m[5,],m[6,])
m <- cbind(user1_ratings,user21_ratings,user44_ratings,user59_ratings,user72_ratings,user82_ratings,user102_ratings,user234_ratings,user268_ratings,user409_ratings,user486_ratings)
m[m==0] <- NA

