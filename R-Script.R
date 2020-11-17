# Install
install.packages("NLP") 
install.packages("tm")  
install.packages("SnowballC") 
install.packages("wordcloud") 
install.packages("RColorBrewer")
install.packages("syuzhet")
install.packages("ggplot2")
#Load
library("NLP")
library("tm")
library("SnowballC")
library("RColorBrewer")
library("wordcloud")
library("syuzhet")
library("ggplot2")

# Read the text file from local machine , choose file interactively
text <- readLines(file.choose())
# Load the data as a corpus
TextDoc <- Corpus(VectorSource(text))

#Replacing "/", "@" and "|" with space
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
TextDoc <- tm_map(TextDoc, toSpace, "/")
TextDoc <- tm_map(TextDoc, toSpace, "@")
TextDoc <- tm_map(TextDoc, toSpace, "\\|")
# Convert the text to lower case
TextDoc <- tm_map(TextDoc, content_transformer(tolower))
# Remove numbers
TextDoc <- tm_map(TextDoc, removeNumbers)
# Remove english common stopwords
TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))
# Remove punctuations
TextDoc <- tm_map(TextDoc, removePunctuation)
# Eliminate extra white spaces
TextDoc <- tm_map(TextDoc, stripWhitespace)

# Build a term-document matrix
TextDoc_dtm <- TermDocumentMatrix(TextDoc)
dtm_m <- as.matrix(TextDoc_dtm)
dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
head(dtm_d, 5)

# Plot the most frequent words
barplot(dtm_d[1:5,] $freq, las = 2, names.arg = dtm_d[1:5,]$word, col ="lightgreen", main ="Top 5 most frequent words", ylab = "Word frequencies")

#generate word cloud
set.seed(1234)
wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = 5, max.words=100, random.order=FALSE, rot.per=0.40, colors=brewer.pal(8, "Dark2"))

# Find associations 
findAssocs(TextDoc_dtm, terms = c("govt","india","new","police"), corlimit = 0.06)

# Syuzhet Method
syuzhet_vector <- get_sentiment(text, method="syuzhet")
head(syuzhet_vector)
summary(syuzhet_vector)

# Bing Method
bing_vector <- get_sentiment(text, method="bing")
head(bing_vector)
summary(bing_vector)

# Affin Method
afinn_vector <- get_sentiment(text, method="afinn")
head(afinn_vector)
summary(afinn_vector)

# Compare the first row of each vector using sign function
rbind( sign(head(syuzhet_vector)), sign(head(bing_vector)), sign(head(afinn_vector)))

# NRC Lexicon 
d<-get_nrc_sentiment(text)
# head(d,10) - to see top 10 lines of the get_nrc_sentiment dataframe
head (d,10)

#TLC needed 
td<-data.frame(t(d))
td_new <- data.frame(rowSums(td[2:253]))
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]

#Plot One - count of words associated with each sentiment
quickplot(sentiment, data=td_new2, weight=count, geom="bar", fill=sentiment, ylab="count")+ggtitle("Survey sentiments")
#Plot two - count of words associated with each sentiment, expressed as a percentage
barplot( sort(colSums(prop.table(d[, 1:8]))), horiz = TRUE, cex.names = 0.7, las = 1, main = "Emotions in Text", xlab="Percentage")
