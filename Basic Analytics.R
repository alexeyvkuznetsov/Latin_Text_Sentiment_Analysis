library(lattice)
library(tm) 
library(stringr)
library(udpipe) 
library(igraph)
library(ggraph)
library(ggplot2)
library(qgraph)
library(textrank)

setwd("D:/GitHub/Latin_Text_Sentiment_Analysis/")


text <- paste(readLines("files/00 alltext.txt"), collapse=" ")
tolower(text)
text <- tolower(text)
stripWhitespace(text)
text <- stripWhitespace(text)
text <- removePunctuation(text)
text <- removeNumbers(text)


#library(udpipe)

txt <- c(text)
udmodel_latin <- udpipe_load_model(file = "latin-ittb-ud-2.4-190531.udpipe")
x <- udpipe_annotate(udmodel_latin, x = txt, tagger = "default", parser = "default", trace = TRUE)
x <- as.data.frame(x)


# Basic Analytics

#library(lattice)
#library(textrank)

#stats <- txt_freq(x$upos)
#stats$key <- factor(stats$key, levels = rev(stats$key))

#barchart(key ~ freq, data = stats, col = "cadetblue", main = "UPOS (Universal Parts of Speech)\n frequency of occurrence", xlab = "Freq")

# Статистика встречаемости частей речи
stats <- txt_freq(x$upos)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = stats, col = "cadetblue", main = " ", xlab = " ")


#library(lattice)
#library(textrank)

stats <- subset(x, upos %in% c("NOUN", "ADJ"))
stats <- txt_freq(x = stats$lemma, exclude = c("annus", "aer", "romanus", "gothus", "suevus"))
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, par.strip.text=list(fontsize = 14), data = head(stats, 20), col = "cadetblue")

#barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", main = "Наиболее часто встречающиеся существительные и прилагательные", xlab = "История короля Вамбы")



## Using RAKE
stats <- keywords_rake(x = x, term = "lemma", group = "doc_id", relevant = x$upos %in% c("NOUN", "ADJ"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 3), 20), col = "cadetblue", 
         main = "Keywords identified by RAKE", 
         xlab = "Rake")



##Co-occurrences

cooc <- cooccurrence(x = subset(x, upos %in% c("NOUN")), term = "lemma", group = c("doc_id", "paragraph_id", "sentence_id"))
head(cooc)

cooc <- cooccurrence(x = subset(x, upos %in% c("NOUN", "ADJ")), term = "lemma", group = c("doc_id", "paragraph_id", "sentence_id"))
head(cooc)


library(igraph)
library(ggraph)
library(ggplot2)
wordnetwork <- head(cooc, 25)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "cadetblue") +
  geom_node_text(aes(label = name), col = "black", size = 6) +
  theme_graph(base_family = "Arial Narrow") +
  theme(legend.position = "none")

  labs(title = "Cooccurrences within sentence", subtitle = "Nouns & Adjective")

library(igraph)
library(ggraph)
library(ggplot2)
wordnetwork <- head(cooc, 30)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "pink") +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  theme(legend.position = "none") +
  labs(title = "Cooccurrences within sentence", subtitle = "Nouns & Adjective")



##Nouns / adjectives which follow one another


cooc <- cooccurrence(x$lemma, relevant = x$upos %in% c("NOUN", "ADJ"), skipgram = 1)
head(cooc)

library(igraph)
library(ggraph)
library(ggplot2)
wordnetwork <- head(cooc, 15)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc)) +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  labs(title = "Words following one another", subtitle = "Nouns & Adjective")




stats <- textrank_keywords(x$lemma, 
                          relevant = x$upos %in% c("NOUN", "ADJ"), 
                          ngram_max = 8, sep = " ")
stats <- subset(stats$keywords, ngram > 1 & freq >= 5)
library(wordcloud)
wordcloud(words = stats$keyword, freq = stats$freq)




