#Trying out R for text analysis on survey data
#loading neccessary packages -- these should be installed before loading
instainstall.packages("wordcloud")ll.packages("tm")
install.packages("tidytext")
install.packages("topicmodels")
install.packages("rvest")
install.packages("natur")
install.packages("wordcloud2")
install.packages("word_associate")

library(tm)
library(ggplot2)
library(wordcloud)
library(wordcloud2)
library (plyr)
library(tidytext)
library(cluster)
library(dplyr)
library(tidyr)
library(topicmodels)
library(xml2)
library(rvest)
library(natur)
library(magrittr)
library(gridExtra)
library(xtable)
Library(word_associate)
assign("last.warning", NULL, envir = baseenv())

abstract_text<-read_xml("/Users/gamageperera/Desktop/Text_mining/BiodiversityConservation_data/metadata/journal-article-10.1086_678127.xml")
abstract<-xml_find_all(abstract_text,".//abstract")
#using rvest library
html_text(abstract)

directory_list<-list.files("/Users/gamageperera/Desktop/Text_mining/BiodiversityConservation_data/metadata",pattern="*.xml")
allabstract<-""
for (filename in directory_list){
  print(paste("/Users/gamageperera/Desktop/Text_mining/BiodiversityConservation_data/metadata/",filename, sep =""))
  abstract_text<-read_xml(paste("/Users/gamageperera/Desktop/Text_mining/BiodiversityConservation_data/metadata/",filename, sep =""))
  abstract<-xml_find_all(abstract_text,".//abstract")
  #using rvest library
  allabstract<-append(allabstract, html_text(abstract))
}
print("loop finish")

assign("last.warning", NULL, envir = baseenv())

corpus<-Corpus(VectorSource(allabstract))

#Cleaning up data
corpus2 <- VCorpus(VectorSource(allabstract)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(tolower)  %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(stripWhitespace) %>%
  tm_map(PlainTextDocument)
corpus2
inspect(corpus2[32])

dtm3 <- TermDocumentMatrix(corpus2)
m2 <- as.matrix(dtm3)
v2 <- sort(rowSums(m2),decreasing=TRUE)
d2 <- data.frame(word = names(v2),freq=v2)
head(d2, 10)
wordcloud2(data = d2, size = 1.6, shape = 'diamond')

#Cleaning up data - Sarah's version
#Removing punctuation
corpus <- tm_map(corpus, removePunctuation)
#making all lower case
corpus <- tm_map(corpus, content_transformer(tolower))
#Removing English stopwords. Might prefer to custom list for project, removes all version of not -- could be problematic
head(sample(stop_words$word, 15),15)

corpus_nostopwords <-corpus #saving a version of the corpus without stopwords for later use
myStopwords <- c(stopwords('english')) #Add other words you want to exclude to this list
corpus <- tm_map(corpus, removeWords, myStopwords)

#striping extra whitespace
corpus <- tm_map(corpus, stripWhitespace)
#stemming words -- may not want to do - I left it out because it didn't seem helpful. Uncomment the next line to try it out.
#corpus <- tm_map(corpus, stemDocument)
#checking cleaned data
inspect(corpus_nostopwords[32])
inspect(corpus[32])

assign("last.warning", NULL, envir = baseenv())

#original code from Sarah
#Creating the corpus
# abstracts <- read.csv("data.csv", header=TRUE)
# corpus <- Corpus(VectorSource(abstracts$AB))
# corpus

#Adding metadata (ids) to corpus -- need to come back to this. 
ids <- as.list(levels(survey$DB_ID))
meta(corpus[[1]])


# #Cleaning up data
# #Removing punctuation
# corpus <- tm_map(corpus, removePunctuation)
# #making all lower case
# corpus <- tm_map(corpus, content_transformer(tolower))
# #Removing English stopwords. Might prefer to custom list for project, removes all version of not -- could be problematic
# corpus_nostopwords <-corpus #saving a version of the corpus without stopwords for later use
# myStopwords <- c(stopwords('english'), "college") #Add other words you want to exclude to this list
# corpus <- tm_map(corpus, removeWords, myStopwords)
# #striping extra whitespace
# corpus <- tm_map(corpus, stripWhitespace)
# #stemming words -- may not want to do - I left it out because it didn't seem helpful. Uncomment the next line to try it out.
# #corpus <- tm_map(corpus, stemDocument)
# #checking cleaned data
# inspect(corpus_nostopwords[32])
# inspect(corpus[32])


#Creating a Term Document Matrix
dtm <- DocumentTermMatrix(corpus)
inspect(dtm[])
dtm

#Build a term-document matrix
dtm2 <- TermDocumentMatrix(corpus)
m <- as.matrix(dtm2)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, scale= c(4,.5), rot.per=0.35, 
          colors=brewer.pal(8,"Dark2"))



#Finding frequent terms -Nu
findFreqTerms(dtm, lowfreq=150) #only returns words that appear 150 times or more
frequentterms<-findFreqTerms(dtm, lowfreq=150)
frequentterms

freqr <- colSums(as.matrix(dtm))
freqr

wordcloud(frequentterms, freqr, scale=c(4,.5), min.freq = 30, max.words = 500, random.order = TRUE, random.color = FALSE,
          colors=brewer.pal(max(3,ncol(freqr)),"Dark2"))

#Trying to plot - Sarah
#putting data in a plottable format
freqr <- colSums(as.matrix(dtm))
freqr
class(freqr)

#Create bar chart of most frequent terms - Sarah
wf=data.frame(term=names(freqr), occurrences=freqr)
p <- ggplot(subset(wf, freqr>1000), aes(term, occurrences))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p

#Creating a wordcloud
#limit words by specifying minimum frequency
wordcloud(names(freqr), freqr, min.freq = 200, colors=brewer.pal(4, "Dark2"))


#Trying out clustering
#Removing sparse terms
dtm_rm_sparse <- removeSparseTerms(dtm, 0.75) #removes terms that only in less than 75% of the documents -- have to do this or cluster will be unreadable
dtm_rm_sparse


#Hierarchal Clustering
dtm_rm_sparse
d <- dist(t(dtm_rm_sparse), method="euclidian")
fit <- hclust(d=d, method="complete")
fit

plot(fit, hang=-1)

#identifing clusters
plot.new()
plot(fit, hang=-1)
groups <- cutree(fit, k=10) #k is the number of cluster
rect.hclust(fit, k=10, border="red")

#finding Associations - words that often appear near other words
findAssocs(dtm, "species", 0.2)
findAssocs(dtm, "conservation", 0.2)
findAssocs(dtm, "habitat", 0.2)
findAssocs(dtm, "change", 0.2)
findAssocs(dtm, "climate", 0.2)

#create word network
word_associate(
  dtm,
  match.string
)

#Trying out Topic Models
#getting rid of empty entries in dtm
rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm.new   <- dtm[rowTotals> 0, ]           #remove all docs without words

#set a seed so that the output of model is predictable
corpus_lda <-LDA(dtm.new, k=5, control = list(seed =150)) # Setting the seed makes this random process repeatable -- Probably want to do more reading on this.

corpus_topics <-tidy(corpus_lda, matrix="beta")
corpus_topics

#pulling out top 10 terms most common within each topic
corpus_top_terms <- corpus_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

#Plotting those terms
corpus_top_terms %>%
  mutate(term= reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#Looking at terms with the greatest difference in beta between topics 1 and 2
beta_spread <- corpus_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2/topic2))

beta_spread

#Examining per document per topic, called gamma
corpus_documents <- tidy(corpus_lda, matrix = "gamma")
corpus_documents

tidy(dtm.new) %>%
  filter(document == 5) %>%
  arrange(desc(count))

#Using some of the terms to pull assign cateogories to entries. Using regex patterns with keywords that might apply
wind = "wind|turbine"


#Writing a loop to crawl through the assigned terms
inspect(corpus_nostopwords)
corpus_length = length(corpus)

#Checking wind words
total_wind = 0
for (doc in 1:corpus_length){
  keywords = wind
  response <- corpus_nostopwords[[doc]]
  status <-grepl(keywords, response)
  if (status == TRUE){
    total_wind = total_wind + 1
    print(paste0(doc, ": ", corpus_nostopwords[[doc]]))
  }
}
print(total_wind)

