
library(NLP)
library(tm)
library(udpipe)
library(dplyr)
library(syuzhet)
library(ggplot2)


setwd("D:/GitHub/Latin_Text_Sentiment_Analysis/")

#Latin text preprocessing

alltext<-paste(scan(file ="files/00 alltext.txt",what='character'),collapse=" ")
prologus<-paste(scan(file ="files/01 prologus.txt",what='character'),collapse=" ")
historia_g<-paste(scan(file ="files/02 historia_g.txt",what='character'),collapse=" ")
recapitulatio<-paste(scan(file ="files/03 recapitulatio.txt",what='character'),collapse=" ")
historia_w<-paste(scan(file ="files/04 historia_w.txt",what='character'),collapse=" ")
historia_s<-paste(scan(file ="files/05 historia_s.txt",what='character'),collapse=" ")


alltext <- tolower(alltext)
prologus <- tolower(prologus)
historia_g <- tolower(historia_g)
recapitulatio <- tolower(recapitulatio)
historia_w <- tolower(historia_w)
historia_s <- tolower(historia_s)

alltext <- stripWhitespace(alltext)
prologus <- stripWhitespace(prologus)
historia_g <- stripWhitespace(historia_g)
recapitulatio <- stripWhitespace(recapitulatio)
historia_w <- stripWhitespace(historia_w)
historia_s <- stripWhitespace(historia_s)

alltext <- removeNumbers(alltext)
prologus <- removeNumbers(prologus)
historia_g <- removeNumbers(historia_g)
recapitulatio <- removeNumbers(recapitulatio)
historia_w <- removeNumbers(historia_w)
historia_s <- removeNumbers(historia_s)

alltext<-data.frame(texts=alltext)
prologus<-data.frame(texts=prologus)
historia_g<-data.frame(texts=historia_g)
recapitulatio<-data.frame(texts=recapitulatio)
historia_w<-data.frame(texts=historia_w)
historia_s<-data.frame(texts=historia_s)

#alltext$book<-"Alltext"
#prologus$book<-"Prologus"
#historia_g$book<-"Historia_Gothorum"
#recapitulatio$book<-"Recapitulatio"
#historia_w$book<-"Historia_Wandalorum"
#historia_s$book<-"Historia_Suevorum"


#fivebooks<-rbind(prologus,historia_g,recapitulatio,historia_w,historia_s)

#udmodel_latin <- udpipe_load_model(file = "latin-ittb-ud-2.4-190531.udpipe")

#x <- udpipe_annotate(udmodel_latin, x = fivebooks$texts, doc_id = fivebooks$book, tagger = "default", parser = "default", trace = TRUE)
#x <- as.data.frame(x)


# Text pre-processing by UDPipe package.

udmodel_latin <- udpipe_load_model(file = "latin-ittb-ud-2.4-190531.udpipe")

# All text
x <- udpipe_annotate(udmodel_latin, x = alltext$texts)
x <- as.data.frame(x)

#library(dplyr)

alltext <- as.character(select(x, lemma))
alltext <- stripWhitespace(alltext)
alltext <- removePunctuation(alltext)
write(alltext, file = "text_lemma.txt")


# Prologus

x <- udpipe_annotate(udmodel_latin, x = prologus$texts)
x <- as.data.frame(x)

text_prologus <- as.character(select(x, lemma))
text_prologus <- stripWhitespace(text_prologus)
text_prologus <- removePunctuation(text_prologus)
write(text_prologus, file = "text_prologus_lemma.txt")


# Historia gothorum

x <- udpipe_annotate(udmodel_latin, x = historia_g$texts)
x <- as.data.frame(x)

text_historia_g <- as.character(select(x, lemma))
text_historia_g <- stripWhitespace(text_historia_g)
text_historia_g <- removePunctuation(text_historia_g)
write(text_historia_g, file = "text_historia_g_lemma.txt")

# Recapitulatio

x <- udpipe_annotate(udmodel_latin, x = recapitulatio$texts)
x <- as.data.frame(x)


text_recapitulatio <- as.character(select(x, lemma))
text_recapitulatio <- stripWhitespace(text_recapitulatio)
text_recapitulatio <- removePunctuation(text_recapitulatio)
write(text_recapitulatio, file = "text_recapitulatio_lemma.txt")

# Historia wandalorum

x <- udpipe_annotate(udmodel_latin, x = historia_w$texts)
x <- as.data.frame(x)


text_historia_w <- as.character(select(x, lemma))
text_historia_w <- stripWhitespace(text_historia_w)
text_historia_w <- removePunctuation(text_historia_w)
write(text_historia_w, file = "text_historia_w_lemma.txt")

# Historia Suevorum

x <- udpipe_annotate(udmodel_latin, x = historia_s$texts)
x <- as.data.frame(x)

text_historia_s <- as.character(select(x, lemma))
text_historia_s <- stripWhitespace(text_historia_s)
text_historia_s <- removePunctuation(text_historia_s)
write(text_historia_s, file = "text_historia_s_lemma.txt")



# Sentiment analysis

# All text

text_lemma <- get_text_as_string("text_lemma.txt")
text_tokens <- get_tokens(text_lemma)

nrc_data <- get_nrc_sentiment(text_tokens, language="latin")


#simple_plot(nrc_data)

#change result from a list to a data frame and transpose it 

result<-data.frame(t(nrc_data))


#rowSums computes column sums across rows for each level of a 
#grouping variable.

new_result <- data.frame(rowSums(result))

#name rows and columns of the dataframe
names(new_result)[1] <- "count"
new_result <- cbind("sentiment" = rownames(new_result), new_result)
rownames(new_result) <- NULL



#plot the first 8 rows,the distinct emotions
qplot(sentiment, data=new_result[1:8,], weight=count, geom="bar",fill=sentiment)+ggtitle("Весь текст 'Истории готов, вандалов и свевов'")

#plot the last 2 rows ,positive and negative
qplot(sentiment, data=new_result[9:10,], weight=count, geom="bar",fill=sentiment)+ggtitle("Весь текст 'Истории готов, вандалов и свевов'")


trust_items <- which(nrc_data$trust > 0)
text_tokens[trust_items]


angry_items <- which(nrc_data$anger > 0)
text_tokens[angry_items]


sadness_items <- which(nrc_data$sadness > 0)
text_tokens[sadness_items]


joy_items <- which(nrc_data$joy > 0)
text_tokens[joy_items]



# Prologus

text_lemma <- get_text_as_string("text_prologus_lemma.txt")
text_tokens <- get_tokens(text_lemma)

nrc_data <- get_nrc_sentiment(text_tokens, language="latin")


#change result from a list to a data frame and transpose it 

result<-data.frame(t(nrc_data))


#rowSums computes column sums across rows for each level of a 
#grouping variable.

new_result <- data.frame(rowSums(result))

#name rows and columns of the dataframe
names(new_result)[1] <- "count"
new_result <- cbind("sentiment" = rownames(new_result), new_result)
rownames(new_result) <- NULL

#plot the first 8 rows,the distinct emotions
qplot(sentiment, data=new_result[1:8,], weight=count, geom="bar",fill=sentiment)+ggtitle("Пролог 'Истории готов, вандалов и свевов'")

#plot the last 2 rows ,positive and negative
qplot(sentiment, data=new_result[9:10,], weight=count, geom="bar",fill=sentiment)+ggtitle("Пролог 'Истории готов, вандалов и свевов'")


# Historia gothorum

text_lemma <- get_text_as_string("text_historia_g_lemma.txt")
text_tokens <- get_tokens(text_lemma)

nrc_data <- get_nrc_sentiment(text_tokens, language="latin")


#change result from a list to a data frame and transpose it 

result<-data.frame(t(nrc_data))


#rowSums computes column sums across rows for each level of a 
#grouping variable.

new_result <- data.frame(rowSums(result))

#name rows and columns of the dataframe
names(new_result)[1] <- "count"
new_result <- cbind("sentiment" = rownames(new_result), new_result)
rownames(new_result) <- NULL

#plot the first 8 rows,the distinct emotions
qplot(sentiment, data=new_result[1:8,], weight=count, geom="bar",fill=sentiment)+ggtitle("Истории готов")

#plot the last 2 rows ,positive and negative
qplot(sentiment, data=new_result[9:10,], weight=count, geom="bar",fill=sentiment)+ggtitle("Истории готов")


# Recapitulati

text_lemma <- get_text_as_string("text_recapitulatio_lemma.txt")
text_tokens <- get_tokens(text_lemma)

nrc_data <- get_nrc_sentiment(text_tokens, language="latin")


#change result from a list to a data frame and transpose it 

result<-data.frame(t(nrc_data))


#rowSums computes column sums across rows for each level of a 
#grouping variable.

new_result <- data.frame(rowSums(result))

#name rows and columns of the dataframe
names(new_result)[1] <- "count"
new_result <- cbind("sentiment" = rownames(new_result), new_result)
rownames(new_result) <- NULL

#plot the first 8 rows,the distinct emotions
qplot(sentiment, data=new_result[1:8,], weight=count, geom="bar",fill=sentiment)+ggtitle("Выводы 'Истории готов, вандалов и свевов'")

#plot the last 2 rows ,positive and negative
qplot(sentiment, data=new_result[9:10,], weight=count, geom="bar",fill=sentiment)+ggtitle("Выводы 'Истории готов, вандалов и свевов'")


# Historia wandalorum

text_lemma <- get_text_as_string("text_historia_w_lemma.txt")
text_tokens <- get_tokens(text_lemma)

nrc_data <- get_nrc_sentiment(text_tokens, language="latin")


#change result from a list to a data frame and transpose it 

result<-data.frame(t(nrc_data))


#rowSums computes column sums across rows for each level of a 
#grouping variable.

new_result <- data.frame(rowSums(result))

#name rows and columns of the dataframe
names(new_result)[1] <- "count"
new_result <- cbind("sentiment" = rownames(new_result), new_result)
rownames(new_result) <- NULL

#plot the first 8 rows,the distinct emotions
qplot(sentiment, data=new_result[1:8,], weight=count, geom="bar",fill=sentiment)+ggtitle("История вандалов")

#plot the last 2 rows ,positive and negative
qplot(sentiment, data=new_result[9:10,], weight=count, geom="bar",fill=sentiment)+ggtitle("История вандалов")


# Historia suevorum

text_lemma <- get_text_as_string("text_historia_s_lemma.txt")
text_tokens <- get_tokens(text_lemma)

nrc_data <- get_nrc_sentiment(text_tokens, language="latin")


#change result from a list to a data frame and transpose it 

result<-data.frame(t(nrc_data))


#rowSums computes column sums across rows for each level of a 
#grouping variable.

new_result <- data.frame(rowSums(result))

#name rows and columns of the dataframe
names(new_result)[1] <- "count"
new_result <- cbind("sentiment" = rownames(new_result), new_result)
rownames(new_result) <- NULL

#plot the first 8 rows,the distinct emotions
qplot(sentiment, data=new_result[1:8,], weight=count, geom="bar",fill=sentiment)+ggtitle("История свевов")

#plot the last 2 rows ,positive and negative
qplot(sentiment, data=new_result[9:10,], weight=count, geom="bar",fill=sentiment)+ggtitle("История всевов")




=======
library(NLP)
library(tm)
library(udpipe)
library(dplyr)
library(syuzhet)
library(ggplot2)


setwd("D:/GitHub/Latin_Text_Sentiment_Analysis/")

#Latin text preprocessing

alltext<-paste(scan(file ="files/00 alltext.txt",what='character'),collapse=" ")
prologus<-paste(scan(file ="files/01 prologus.txt",what='character'),collapse=" ")
historia_g<-paste(scan(file ="files/02 historia_g.txt",what='character'),collapse=" ")
recapitulatio<-paste(scan(file ="files/03 recapitulatio.txt",what='character'),collapse=" ")
historia_w<-paste(scan(file ="files/04 historia_w.txt",what='character'),collapse=" ")
historia_s<-paste(scan(file ="files/05 historia_s.txt",what='character'),collapse=" ")


alltext <- tolower(alltext)
prologus <- tolower(prologus)
historia_g <- tolower(historia_g)
recapitulatio <- tolower(recapitulatio)
historia_w <- tolower(historia_w)
historia_s <- tolower(historia_s)

alltext <- stripWhitespace(alltext)
prologus <- stripWhitespace(prologus)
historia_g <- stripWhitespace(historia_g)
recapitulatio <- stripWhitespace(recapitulatio)
historia_w <- stripWhitespace(historia_w)
historia_s <- stripWhitespace(historia_s)

alltext <- removeNumbers(alltext)
prologus <- removeNumbers(prologus)
historia_g <- removeNumbers(historia_g)
recapitulatio <- removeNumbers(recapitulatio)
historia_w <- removeNumbers(historia_w)
historia_s <- removeNumbers(historia_s)

alltext<-data.frame(texts=alltext)
prologus<-data.frame(texts=prologus)
historia_g<-data.frame(texts=historia_g)
recapitulatio<-data.frame(texts=recapitulatio)
historia_w<-data.frame(texts=historia_w)
historia_s<-data.frame(texts=historia_s)

#alltext$book<-"Alltext"
#prologus$book<-"Prologus"
#historia_g$book<-"Historia_Gothorum"
#recapitulatio$book<-"Recapitulatio"
#historia_w$book<-"Historia_Wandalorum"
#historia_s$book<-"Historia_Suevorum"


#fivebooks<-rbind(prologus,historia_g,recapitulatio,historia_w,historia_s)

#udmodel_latin <- udpipe_load_model(file = "latin-ittb-ud-2.4-190531.udpipe")

#x <- udpipe_annotate(udmodel_latin, x = fivebooks$texts, doc_id = fivebooks$book, tagger = "default", parser = "default", trace = TRUE)
#x <- as.data.frame(x)


# Text pre-processing by UDPipe package.

udmodel_latin <- udpipe_load_model(file = "latin-ittb-ud-2.4-190531.udpipe")

# All text
x <- udpipe_annotate(udmodel_latin, x = alltext$texts)
x <- as.data.frame(x)

#library(dplyr)

alltext <- as.character(select(x, lemma))
alltext <- stripWhitespace(alltext)
alltext <- removePunctuation(alltext)
write(alltext, file = "text_lemma.txt")


# Prologus

x <- udpipe_annotate(udmodel_latin, x = prologus$texts)
x <- as.data.frame(x)

text_prologus <- as.character(select(x, lemma))
text_prologus <- stripWhitespace(text_prologus)
text_prologus <- removePunctuation(text_prologus)
write(text_prologus, file = "text_prologus_lemma.txt")


# Historia gothorum

x <- udpipe_annotate(udmodel_latin, x = historia_g$texts)
x <- as.data.frame(x)

text_historia_g <- as.character(select(x, lemma))
text_historia_g <- stripWhitespace(text_historia_g)
text_historia_g <- removePunctuation(text_historia_g)
write(text_historia_g, file = "text_historia_g_lemma.txt")

# Recapitulatio

x <- udpipe_annotate(udmodel_latin, x = recapitulatio$texts)
x <- as.data.frame(x)


text_recapitulatio <- as.character(select(x, lemma))
text_recapitulatio <- stripWhitespace(text_recapitulatio)
text_recapitulatio <- removePunctuation(text_recapitulatio)
write(text_recapitulatio, file = "text_recapitulatio_lemma.txt")

# Historia wandalorum

x <- udpipe_annotate(udmodel_latin, x = historia_w$texts)
x <- as.data.frame(x)


text_historia_w <- as.character(select(x, lemma))
text_historia_w <- stripWhitespace(text_historia_w)
text_historia_w <- removePunctuation(text_historia_w)
write(text_historia_w, file = "text_historia_w_lemma.txt")

# Historia Suevorum

x <- udpipe_annotate(udmodel_latin, x = historia_s$texts)
x <- as.data.frame(x)

text_historia_s <- as.character(select(x, lemma))
text_historia_s <- stripWhitespace(text_historia_s)
text_historia_s <- removePunctuation(text_historia_s)
write(text_historia_s, file = "text_historia_s_lemma.txt")



# Sentiment analysis

# All text

text_lemma <- get_text_as_string("text_lemma.txt")
text_tokens <- get_tokens(text_lemma)

nrc_data <- get_nrc_sentiment(text_tokens, language="latin")


#simple_plot(nrc_data)

#change result from a list to a data frame and transpose it 

result<-data.frame(t(nrc_data))


#rowSums computes column sums across rows for each level of a 
#grouping variable.

new_result <- data.frame(rowSums(result))

#name rows and columns of the dataframe
names(new_result)[1] <- "count"
new_result <- cbind("sentiment" = rownames(new_result), new_result)
rownames(new_result) <- NULL



#plot the first 8 rows,the distinct emotions
qplot(sentiment, data=new_result[1:8,], weight=count, geom="bar",fill=sentiment)+ggtitle("Весь текст 'Истории готов, вандалов и свевов'")

#plot the last 2 rows ,positive and negative
qplot(sentiment, data=new_result[9:10,], weight=count, geom="bar",fill=sentiment)+ggtitle("Весь текст 'Истории готов, вандалов и свевов'")


trust_items <- which(nrc_data$trust > 0)
text_tokens[trust_items]


angry_items <- which(nrc_data$anger > 0)
text_tokens[angry_items]


sadness_items <- which(nrc_data$sadness > 0)
text_tokens[sadness_items]


joy_items <- which(nrc_data$joy > 0)
text_tokens[joy_items]



# Prologus

text_lemma <- get_text_as_string("text_prologus_lemma.txt")
text_tokens <- get_tokens(text_lemma)

nrc_data <- get_nrc_sentiment(text_tokens, language="latin")


#change result from a list to a data frame and transpose it 

result<-data.frame(t(nrc_data))


#rowSums computes column sums across rows for each level of a 
#grouping variable.

new_result <- data.frame(rowSums(result))

#name rows and columns of the dataframe
names(new_result)[1] <- "count"
new_result <- cbind("sentiment" = rownames(new_result), new_result)
rownames(new_result) <- NULL

#plot the first 8 rows,the distinct emotions
qplot(sentiment, data=new_result[1:8,], weight=count, geom="bar",fill=sentiment)+ggtitle("Пролог 'Истории готов, вандалов и свевов'")

#plot the last 2 rows ,positive and negative
qplot(sentiment, data=new_result[9:10,], weight=count, geom="bar",fill=sentiment)+ggtitle("Пролог 'Истории готов, вандалов и свевов'")


# Historia gothorum

text_lemma <- get_text_as_string("text_historia_g_lemma.txt")
text_tokens <- get_tokens(text_lemma)

nrc_data <- get_nrc_sentiment(text_tokens, language="latin")


#change result from a list to a data frame and transpose it 

result<-data.frame(t(nrc_data))


#rowSums computes column sums across rows for each level of a 
#grouping variable.

new_result <- data.frame(rowSums(result))

#name rows and columns of the dataframe
names(new_result)[1] <- "count"
new_result <- cbind("sentiment" = rownames(new_result), new_result)
rownames(new_result) <- NULL

#plot the first 8 rows,the distinct emotions
qplot(sentiment, data=new_result[1:8,], weight=count, geom="bar",fill=sentiment)+ggtitle("Истории готов")

#plot the last 2 rows ,positive and negative
qplot(sentiment, data=new_result[9:10,], weight=count, geom="bar",fill=sentiment)+ggtitle("Истории готов")


# Recapitulati

text_lemma <- get_text_as_string("text_recapitulatio_lemma.txt")
text_tokens <- get_tokens(text_lemma)

nrc_data <- get_nrc_sentiment(text_tokens, language="latin")


#change result from a list to a data frame and transpose it 

result<-data.frame(t(nrc_data))


#rowSums computes column sums across rows for each level of a 
#grouping variable.

new_result <- data.frame(rowSums(result))

#name rows and columns of the dataframe
names(new_result)[1] <- "count"
new_result <- cbind("sentiment" = rownames(new_result), new_result)
rownames(new_result) <- NULL

#plot the first 8 rows,the distinct emotions
qplot(sentiment, data=new_result[1:8,], weight=count, geom="bar",fill=sentiment)+ggtitle("Выводы 'Истории готов, вандалов и свевов'")

#plot the last 2 rows ,positive and negative
qplot(sentiment, data=new_result[9:10,], weight=count, geom="bar",fill=sentiment)+ggtitle("Выводы 'Истории готов, вандалов и свевов'")


# Historia wandalorum

text_lemma <- get_text_as_string("text_historia_w_lemma.txt")
text_tokens <- get_tokens(text_lemma)

nrc_data <- get_nrc_sentiment(text_tokens, language="latin")


#change result from a list to a data frame and transpose it 

result<-data.frame(t(nrc_data))


#rowSums computes column sums across rows for each level of a 
#grouping variable.

new_result <- data.frame(rowSums(result))

#name rows and columns of the dataframe
names(new_result)[1] <- "count"
new_result <- cbind("sentiment" = rownames(new_result), new_result)
rownames(new_result) <- NULL

#plot the first 8 rows,the distinct emotions
qplot(sentiment, data=new_result[1:8,], weight=count, geom="bar",fill=sentiment)+ggtitle("История вандалов")

#plot the last 2 rows ,positive and negative
qplot(sentiment, data=new_result[9:10,], weight=count, geom="bar",fill=sentiment)+ggtitle("История вандалов")


# Historia suevorum

text_lemma <- get_text_as_string("text_historia_s_lemma.txt")
text_tokens <- get_tokens(text_lemma)

nrc_data <- get_nrc_sentiment(text_tokens, language="latin")


#change result from a list to a data frame and transpose it 

result<-data.frame(t(nrc_data))


#rowSums computes column sums across rows for each level of a 
#grouping variable.

new_result <- data.frame(rowSums(result))

#name rows and columns of the dataframe
names(new_result)[1] <- "count"
new_result <- cbind("sentiment" = rownames(new_result), new_result)
rownames(new_result) <- NULL

#plot the first 8 rows,the distinct emotions
qplot(sentiment, data=new_result[1:8,], weight=count, geom="bar",fill=sentiment)+ggtitle("История свевов")

#plot the last 2 rows ,positive and negative
qplot(sentiment, data=new_result[9:10,], weight=count, geom="bar",fill=sentiment)+ggtitle("История всевов")





