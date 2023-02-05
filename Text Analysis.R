# Install
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
# install.packages("syuzhet") # for sentiment analysis
install.packages("ggplot2") # for plotting graphs

# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
#library("syuzhet")
library("ggplot2")


# Read the text file from local machine , choose file interactively
text <- readLines(file.choose())


# Load the data as a corpus
TextDoc <- Corpus(VectorSource(text))
TextDoc

# Remove numbers  

TextDoc = tm_map(TextDoc, removeNumbers)

writeLines(as.character(TextDoc[[1]])) # inspect doc number one


# Remove punctuation


TextDoc = tm_map(TextDoc, removePunctuation)

writeLines(as.character(TextDoc[[1]])) # inspect doc number one


# Strip whitespace

TextDoc = tm_map(TextDoc, stripWhitespace)

writeLines(as.character(TextDoc[[1]])) # inspect doc number one

writeLines(as.character(TextDoc[[2]])) # inspect doc number one


# # Remove english common stopwords


TextDoc = tm_map(TextDoc,removeWords, stopwords("english"))
writeLines(as.character(TextDoc[[1]])) # inspect doc number one

# Text stemming - which reduces words to their root form
# the stem of the words eating, eats, eaten is eat.


TextDoc = tm_map(TextDoc, stemDocument)

writeLines(as.character(TextDoc[[1]])) # inspect doc number one


# Convert the text to lower case
TextDoc <- tm_map(TextDoc, content_transformer(tolower))
writeLines(as.character(TextDoc[[1]])) # inspect doc number one


# Remove your own stop word
# specify your custom stopwords as a character vector
TextDoc <- tm_map(TextDoc, removeWords, c("s", "company", "team")) 
TextDoc



# Sort by descearing value of frequency
dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
# Display the top 5 most frequent words
head(dtm_d, 5)

# Plot the most frequent words
barplot(dtm_d[1:5,]$freq, las = 2, names.arg = dtm_d[1:5,]$word,
        col ="lightgreen", main ="Top 5 most frequent words",
        ylab = "Word frequencies")


# Build a term-document matrix
TextDoc_dtm <- TermDocumentMatrix(TextDoc)
dtm_m <- as.matrix(TextDoc_dtm)
dtm_m



# https://www.red-gate.com/simple-talk/databases/sql-server/bi-sql-server/text-mining-and-sentiment-analysis-with-r/



#generate word cloud
#A word cloud is one of the most popular ways to 
#visualize and analyze qualitative data. It’s
#of text, where the size of each word indicates 
#its frequency in that body of text. 


set.seed(1234)
wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = 5,
          max.words=100, random.order=FALSE, rot.per=0.40, 
          colors=brewer.pal(8, "Dark2"))

# words – words to be plotted

#freq – frequencies of words

#min.freq – words whose frequency is at or above this threshold 
#value is plotted 

#max.words – the maximum number of words to display on the plot 

#random.order – set it to FALSE, so the words are
#plotted in order of decreasing frequency

#rot.per – the percentage of words that are displayed as
# vertical text (with 90-degree rotation). 

#colors – changes word colors going from lowest to highest frequencies



# Word Association


# Find associations 
findAssocs(TextDoc_dtm, terms = c("shine","place","move"), corlimit = 0.25)		



# Sentiment Score



#Replacing "/", "@" and "|" with space
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
TextDoc <- tm_map(TextDoc, toSpace, "/")
TextDoc <- tm_map(TextDoc, toSpace, "@")
TextDoc <- tm_map(TextDoc, toSpace, "\\|")

# Convert the text to lower case
TextDoc <- tm_map(TextDoc, content_transformer(tolower))


print(TextDoc)

