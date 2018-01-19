library(syuzhet)
library(tm)
library(ggplot2)
library(readr)



transcripts <- read_csv("~/Downloads/transcripts.csv")
View(transcripts)

#read the transcript column from transcripts
newdata<- iconv(transcripts$transcript, "ASCII", "UTF-8", sub="")

#extract text from the data frame build your own corpus(a corpus is a collection of text files)
mydata <- Corpus(VectorSource(newdata))

# convert to lower case
mydata <- tm_map(mydata, content_transformer(tolower))

#remove ������ what would be emojis
mydata<-tm_map(mydata, content_transformer(gsub), pattern="\\W",replace=" ")

# remove URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
mydata <- tm_map(mydata, content_transformer(removeURL)
)
# remove anything other than English letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
mydata <- tm_map(mydata, content_transformer(removeNumPunct))

# remove stopwords
mydata <- tm_map(mydata, removeWords, stopwords("english"))


# remove extra whitespace
mydata <- tm_map(mydata, stripWhitespace)

# Remove numbers
mydata <- tm_map(mydata, removeNumbers)

# Remove punctuations
mydata <- tm_map(mydata, removePunctuation)

# keep a copy for stem completion later
mydataCopy <- mydata

#carryout sentiment mining using the get_nrc_sentiment()function log the findings under a variable result
result <- get_nrc_sentiment(as.character(mydataCopy))

#change result from a list to a dataframe and transpose it 
result1<-data.frame(t(result))

#rowSums computes column sums across rows for each level of a grouping variable.
new_result <- data.frame(rowSums(result1))


#name rows and columns of the dataframe
names(new_result)[1] <- "count"
new_result <- cbind("sentiment" = rownames(new_result), new_result)
rownames(new_result) <- NULL

#plot the first 8 rows  ,distict emotions
qplot(sentiment, data=new_result[1:8,], weight=count, geom="bar",fill=sentiment)+ggtitle("TedTalk Sentiments")

#plot the positive and negative
qplot(sentiment, data=new_result[9:10,], weight=count, geom="bar",fill=sentiment)+ggtitle("TedTalk Sentiments")

