library(dplyr)
library(ggplot2)
library(tokenizers)
library(tidytext)
library(SnowballC)
library(tm)
library(stringi)
library(ggrepel)

data <-read.csv("~/University/DSMA Master/Text Analytics/Assignments/Womens Clothing E-Commerce Reviews.csv")
data <- data[,c(1,5)]
data$Review.Text <- as.character(data$Review.Text)

#display first two lrows of data for observation
head(data, n=2)

########METHOD 1####################################################################
data2 <- data
data2$Review.Text <- as.character(data2$Review.Text)  %>% 
  tolower() %>% 
  {gsub(":( |-|o)*\\("," SADSMILE ", .)} %>%       # Find :( or :-( or : ( or :o(
  {gsub(":( |-|o)*\\)"," HAPPYSMILE ", .)} %>%     # Find :) or :-) or : ) or :o)
  {gsub("(\"| |\\$)-+\\.-+"," NUMBER", .)} %>%     # Find numbers
  {gsub("([0-9]+:)*[0-9]+ *am"," TIME_AM", .)} %>%         # Find time AM
  {gsub("([0-9]+:)*[0-9]+ *pm"," TIME_PM", .)} %>%         # Find time PM
  {gsub("-+:-+","TIME", .)} %>%                    # Find general time
  {gsub("\\$ ?[0-9]*[\\.,]*[0-9]+"," DOLLARVALUE ", .)} %>%           # Find Dollar values
  {gsub("[0-9]*[\\.,]*[0-9]+"," NUMBER ", .)} %>%           # Find remaining numbers
  {gsub("-"," ", .)} %>%                           # Remove all -
  {gsub("&"," and ", .)} %>%                       # Find general time
  {gsub("\"+"," ", .)} %>%                         # Remove all "
  {gsub("\\|+"," ", .)} %>%                        # Remove all |
  {gsub("_+"," ", .)} %>%                          # Remove all _
  {gsub(";+"," ", .)} %>%                          # Remove excess ;
  {gsub(" +"," ", .)} %>%                          # Remove excess spaces
  {gsub("\\.+","\\.", .)}                          # Remove excess .

# Creating the full review from the cleaned+stemmed words
j<-1
for (j in 1:nrow(data2)) {
  stemmed_description<-  anti_join((data2[j,] %>% unnest_tokens(word,Review.Text, drop=FALSE,to_lower=TRUE) ),stop_words)
  
  stemmed_description<-(wordStem(stemmed_description[,"word"], language = "porter"))
  
  data2[j,"Review.Text"]<-paste((stemmed_description),collapse = " ")
  
}

#cut text into words by splitting on spaces and punctuation & convert all words to lower case
data_words <- data2 %>% unnest_tokens(word,Review.Text,to_lower=TRUE) 
print("number of words after stemming and without stop words") 
nrow(data_words) #453421

#Count the number of times each word occurs
counts <- data_words %>%count(word, sort=TRUE) # sort = TRUE for sorting in descending order of n. 
print("number of unique words after stemming and without stop words")
nrow(counts) #10591

#####------------MDS EMILIA------------------------
library(quanteda)
library(smacof)

set.seed(123)
reviews_corp <- corpus(data2, docid_field = "X", text_field = "Review.Text")

# feature cooccurrence matrix : fcm()
co_occurrence_matrix <- fcm(x = reviews_corp, context = "document", count = "frequency", tri=FALSE)

#Need number of documents with each word on the diagonal
reviews_dfm <- dfm(reviews_corp) # get document frequency matrix
counts <- colSums(as.matrix(reviews_dfm)) 
co_occurrence_matrix <- as.matrix(co_occurrence_matrix)
diag(co_occurrence_matrix) <- counts

sortedcount <- counts%>% sort(decreasing=TRUE)
sortednames <- names(sortedcount)
nwords<-200
subset_words<-as.matrix(sortedcount[1:nwords])

co_occurrence_matrix <- co_occurrence_matrix[sortednames[1:nwords],sortednames[1:nwords]]
co_occurrence_matrix[1:15,1:15]

set.seed(123)
#converting co-occurrences into distances 
distances <- sim2diss(co_occurrence_matrix, method = "cooccurrence") # Transform similarities to distances.
distances[1:15,1:15]

set.seed(123)
MDS_map <- smacofSym(distances) # run the routine that finds the best matching coordinates in a 2D mp given the distances
ggplot(as.data.frame(MDS_map$conf), aes(D1, D2, label = rownames(MDS_map$conf))) +
  geom_text(check_overlap = TRUE) + theme_minimal(base_size = 15) + xlab('') + ylab('') +
  scale_y_continuous(breaks = NULL) + scale_x_continuous(breaks = NULL)
# the conf element in the MDS output contains the coordinates with as names D1 and D2.

#words that are beside each other, since window=1
co_occurrence_matrix <- fcm(x = reviews_corp, context = "window", window=1, count = "frequency", tri=FALSE)

co_occurrence_matrix<-co_occurrence_matrix[sortednames[1:nwords],sortednames[1:nwords]]

diag(co_occurrence_matrix) <- counts[sortednames[1:nwords]]
co_occurrence_matrix[1:15,1:15]

distances <- sim2diss(co_occurrence_matrix, method = "cooccurrence") # Transform similarities to distances.
min(distances) #check whethet minimum distance is positive. Sometimes the counting procedure did something unexpected.
max(distances) #check whethet maximum distance is positive. Sometimes the counting procedure did something unexpected.
MDS_map <- smacofSym(distances) # run the routine that finds the best matching coordinates in a 2D mp given the distances
ggplot(as.data.frame(MDS_map$conf), aes(D1, D2, label = rownames(MDS_map$conf))) +
  geom_text(check_overlap = TRUE) + theme_minimal(base_size = 15) + xlab('') + ylab('') +
  scale_y_continuous(breaks = NULL) + scale_x_continuous(breaks = NULL)
# the conf element in the MDS output contains the coordinatis with as names D1 and D2.

#words that are within 5 words distances from each other whether before or after, count is in boolean!
co_occurrence_matrix <- fcm(x = reviews_corp, context = "window", window=5, count = "boolean", tri=FALSE)

co_occurrence_matrix<-co_occurrence_matrix[sortednames[1:nwords],sortednames[1:nwords]]

diag(co_occurrence_matrix) <- counts[sortednames[1:nwords]]
co_occurrence_matrix[1:15,1:15]

distances <- sim2diss(co_occurrence_matrix, method = "cooccurrence") # Transform similarities to distances.
MDS_map <- smacofSym(distances) # run the routine that finds the best matching coordinates in a 2D map given the distances
ggplot(as.data.frame(MDS_map$conf), aes(D1, D2, label = rownames(MDS_map$conf))) +
  geom_text(check_overlap = TRUE) + theme_minimal(base_size = 15) + xlab('') + ylab('') +
  scale_y_continuous(breaks = NULL) + scale_x_continuous(breaks = NULL)
# the conf element in the MDS output contains the coordinatis with as names D1 and D2.

#######------------
counts %>% 
  mutate(word = reorder(word,n)) %>% 
  top_n(20, word) %>%
  ggplot(aes(word,n)) +  
  geom_col() + 
  labs(x = NULL, y = "Number of occurences") + 
  coord_flip() + 
  theme(text = element_text(size = 17)) + 
  ggtitle("Word Frequency Histogram")

#building term-document matrix
corpus <- Corpus(VectorSource(data_words$word))
tdm<-TermDocumentMatrix(corpus)
tdm.reviews.m<-as.matrix(tdm) #this line doesnt work in my comp (Error: cannot allocate vector of size 34.3 Gb), but works for method 2
dim(tdm.reviews.m)
tdm.reviews.m[1:25,1:25] #check how it looks like

#the below code is in the Rmd file by Bas but i dont get how it works?
stri_replace_all_regex('123|456|789', '([0-9]).([0-9])', '$2-$1' )

############METHOD 2#############################################################
data <-read.csv("~/University/DSMA Master/Text Analytics/Assignments/Womens Clothing E-Commerce Reviews.csv")
data$Review.Text <- as.character(data$Review.Text)

#Using the tm package to create a cleaned document term matrix faster.
#Create corpus
#Add ID to dataframe to keep track of reviews
reviews <- data.frame(doc_id=seq(1:nrow(data)),text=data$Review.Text)

# Return NA instead of tolower error (tolower is the function for lower case, it gives error when it encounters special char)
tryTolower <- function(x){
  # return NA when there is an error
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error = function(e) e)
  # if not an error
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

# Function to clean corpus
clean.corpus <-function(corpus){
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, custom.stopwords)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  return(corpus)}

# List of stopwords to remove from corpus, note that I added clothes and wear as stopwords
custom.stopwords <- c(stopwords('english'), 'clothes','wear','wore')

# pre-cleaned corpus
corpus <- VCorpus(DataframeSource(reviews))

# Clean corpus
corpus_new<-clean.corpus(corpus)

# Stem corpus
corpus_stemmed <- tm_map(corpus_new, stemDocument)

# See how many characters are still inside the corpus for the first row
# Pre-cleaned & pre-stemmed corpus
inspect(corpus[[1]])
# Cleaned & pre-stemmed corpus
inspect(corpus_new[[1]])
# Cleaned & stemmed corpus
inspect(corpus_stemmed[[1]])

# Create TermDocumentMatrix 
tdm<-TermDocumentMatrix(corpus_stemmed,control=list(weighting=weightTf))
tdm.reviews.m<-as.matrix(tdm)
# Dimensions: 11743 (words) x 23486 (documents)
dim(tdm.reviews.m)
tdm.reviews.m[1:25,1:25] #check how it looks like

#the below code is in the Rmd file by Bas but i dont get how it works?
stri_replace_all_regex('123|456|789', '([0-9]).([0-9])', '$2-$1' )
