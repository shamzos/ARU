install.packages("textmineR")

library(textmineR)

# load nih_sample data set from textmineR
aadl <- df[df$Type=="aadl",]

aadl$Text <- gsub(aadl$Text,pattern = "Lire la vidéo>",replacement = "")
aadl$Text <- stri_replace_all(aadl$Text,replacement = "",regex = "\\d")
aadl$Text <- lapply(aadl$Text, function(x) gsub("^\\s+|\\s+$", "", x))
aadl[aadl$Text =="",] <- NA
aadl <- aadl[!is.na(aadl$Text),]

# create a document term matrix 
dtm <- CreateDtm(doc_vec = aadl$Text, # character vector of documents
                 doc_names = aadl$Title, # document names
                 ngram_window = c(1, 2), # minimum and maximum n-gram length
                 stopword_vec = c(tm::stopwords("french"), # stopwords from tm
                                  tm::stopwords("SMART")), # this is the default value
                 lower = TRUE, # lowercase - this is the default value
                 remove_punctuation = TRUE, # punctuation - this is the default
                 remove_numbers = TRUE, # numbers - this is the default
                 verbose = FALSE, # Turn off status bar for this demo
                 cpus = 1) # default is all available cpus on the system

# construct the matrix of term counts to get the IDF vector
tf_mat <- TermDocFreq(dtm)

# TF-IDF and cosine similarity
tfidf <- t(dtm[ , tf_mat$term ]) * tf_mat$idf

tfidf <- t(tfidf)

csim <- tfidf / sqrt(rowSums(tfidf * tfidf))

csim <- csim %*% t(csim)

cdist <- as.dist(1 - csim)

hc <- hclust(cdist, "ward.D")

clustering <- cutree(hc, 10)

plot(hc, main = "Hierarchical clustering of 118 aadl Articles",
     ylab = "", xlab = "", yaxt = "n")

rect.hclust(hc, 10, border = "red")



#############################
p_words <- colSums(dtm) / sum(dtm)

cluster_words <- lapply(unique(clustering), function(x){
  rows <- dtm[ clustering == x , ]
  
  # for memory's sake, drop all words that don't appear in the cluster
  rows <- rows[ , colSums(rows) > 0 ]
  
  colSums(rows) / sum(rows) - p_words[ colnames(rows) ]
})


# create a summary table of the top 5 words defining each cluster
cluster_summary <- data.frame(cluster = unique(clustering),
                              size = as.numeric(table(clustering)),
                              top_words = sapply(cluster_words, function(d){
                                paste(
                                  names(d)[ order(d, decreasing = TRUE) ][ 1:5 ], 
                                  collapse = ", ")
                              }),
                              stringsAsFactors = FALSE)
cluster_summary


# plot a word cloud of one cluster as an example
wordcloud::wordcloud(words = names(cluster_words[[ 1 ]]), 
                     freq = cluster_words[[ 1 ]], 
                     max.words = 500, 
                     random.order = FALSE, 
                     main = "Top words in cluster 100")
