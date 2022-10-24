#--------------------------------------------------------
# LDA example 2
# Read a csv file then LDA training
#--------------------------------------------------------

#### DATA PREPROCESSING

#set working directory
setwd("C:/Users/user/workspaceR/Datamining")


#load text mining library
library(tm)
#stemDocument
library(SnowballC)

#read a csv file 윕스온 특허 데이터 읽어오기 
patents_raw <- read.csv("WipsonData20_Siemens.csv", stringsAsFactors = FALSE, encoding="utf-8")

#convert character encoding 문자 변환
#ref: http://www.astrostatistics.psu.edu/datasets/R/html/utils/html/iconv.html
for(i in 1:length(patents_raw$abstract)){patents_raw$abstract[[i]] = iconv(patents_raw$abstract[[i]], "latin1", "ASCII", " ")}

#create corpus from vector 벡터를 corpus
docs <- VCorpus(VectorSource(patents_raw$abstract))

#inspect a particular document in corpus
writeLines(as.character(docs[[20]]))

#start preprocessing
#Transform to lower case 
docs <-tm_map(docs, content_transformer(tolower))

#remove potentially problematic symbols
toSpace <- content_transformer(function(x, pattern) 
  { return (gsub(pattern, " ", x))})

docs <- tm_map(docs, toSpace, "-")

#remove punctuation
docs <- tm_map(docs, removePunctuation)
#Strip digits
docs <- tm_map(docs, removeNumbers)
#remove stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
#remove whitespace
docs <- tm_map(docs, stripWhitespace)

#Good practice to check every now and then
#writeLines(as.character(docs[[30]]))


#Stem document
docs <- tm_map(docs,stemDocument)

#replace word with what you want
docs <- tm_map(docs, content_transformer(gsub), pattern = "organiz", replacement = "organ")

#custom stopwords
myStop <- c("can", "say","one","way","use","also","howev","tell",
                 "will", "much","need","take","tend","even","like","particular","rather","said",
        "get","well","make","ask","come","end","first","two","help","often","may",
        "might","see","someth","thing","point","post","look","right","now","think",
        "'re ","anoth","put","set","new","good","want","sure","kind","larg","yes,","day","etc",
        "quit","sinc","attempt","lack","seen","awar","littl","ever","moreov","though","found",
        "abl","enough","far","earli","away","achiev","draw","last","never","brief","bit","entir",
        "brief","great","lot","degree","image","modified","oil")

#remove stopwords
docs <- tm_map(docs, removeWords, myStop)



#Create document-term matrix
dtm <- DocumentTermMatrix(docs)
#convert rownames to titles
rownames(dtm) <- patents_raw$title
#collapse matrix by summing over columns
freq <- colSums(as.matrix(dtm))
#length should be total number of terms
length(freq)
#create sort order (descending)
ord <- order(freq,decreasing=TRUE)
#List all terms in decreasing order of freq and write to disk
freq[ord]
write.csv(freq[ord],"word_freq20_Schneider.csv")




#### LDA

#load topic models library
library(topicmodels)

#Set parameters for Gibbs sampling
burnin <- 4000      
iter <- 2000       
thin <- 500        

nstart <- 5         
seed <-list(2589,5,63,100001,765) 
best <- TRUE

#Number of topics
k <- 10

#Run LDA using Gibbs sampling (Not working)
ldaOut <-LDA(dtm, k, method="Gibbs",control=list(nstart=nstart, seed = seed, best=best,burnin = burnin, iter = iter, thin=thin))

#run the LDA model 2
#ldaOut <- LDA(dtm,k, method="Gibbs", control=
#                list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))



#write out results
#docs to topics
ldaOut.topics <- as.matrix(topics(ldaOut))
write.csv(ldaOut.topics,file=paste("LDAGibbs",k,"DocsToTopics20_Mitsubishi.csv"))

#top 100 terms in each topic
ldaOut.terms <- as.matrix(terms(ldaOut,100))
write.csv(ldaOut.terms,file=paste("LDAGibbs",k,"TopicsToTerms20_Mitsubishi.csv"))

#ouputs in ldaOut 
str(ldaOut)

#document-topic distributions
ldaOut@gamma

#topic-term distributions
t(posterior(ldaOut)$terms)
(posterior(ldaOut)$terms)

#topic-term distributions (from 1th to 10th words)
t(posterior(ldaOut)$terms)[1:10,]
(posterior(ldaOut)$terms)[,1:5]

#save probabilities associated with each topic assignment
topicProbabilities <- as.data.frame(ldaOut@gamma)
write.csv(topicProbabilities,file=paste("LDAGibbs",k,"TopicProbabilities20_Mitsubishi.csv"))


#Find relative importance of top 2 topics
topic1ToTopic2 <- lapply(1:nrow(dtm),function(x)
  sort(topicProbabilities[x,])[k]/sort(topicProbabilities[x,])[k-1])


#Find relative importance of second and third most important topics
topic2ToTopic3 <- lapply(1:nrow(dtm),function(x)
  sort(topicProbabilities[x,])[k-1]/sort(topicProbabilities[x,])[k-2])


#write to file
write.csv(topic1ToTopic2,file=paste("LDAGibbs",k,"Topic1ToTopic2_20_Mitsubishi.csv"))
write.csv(topic2ToTopic3,file=paste("LDAGibbs",k,"Topic2ToTopic3_20_Mitsubishi.csv"))




# Visualization
#install.packages("LDAvis")
#install.packages('stringi')
#install.packages("servr")

library(LDAvis)
library(stringi)
library(servr)

# D*K matrix (doc-topic)
theta <- posterior(ldaOut)$topics

# K*W matrix (topic-word)
phi <- posterior(ldaOut)$terms

# vocabulary 
vocab <- colnames(phi)

# word count in each document
doc_length <- vector()

for (i in 1:length(docs)) {
  temp <- paste(docs[[i]]$content, collapse = ' ')
  doc_length <- c(doc_length, stri_count(temp, regex = '\\S+'))
}


# freq_matrix : each word frequency in all documents
#temp_frequency <- inspect(dtm)    
temp_frequency <- as.matrix(dtm)  
freq_matrix <- data.frame(ST = colnames(temp_frequency),
                          Freq = colSums(temp_frequency))


# visualization result
json_lda <- createJSON(phi = phi, theta = theta,
                               vocab = vocab,
                               doc.length = doc_length,
                               term.frequency = freq_matrix$Freq)
# show the result
serVis(json_lda, out.dir = 'vis', open.browser = TRUE)


##### Probabilities of sorted words per topic

# p(w|t) of topic3 for w='data'
phi[3,'data']

# p(w|t) of topic3 for words from index 1 to 10
phi[3,1:10]

# sorted phi
sorted_phi <- 
  lapply(1:nrow(phi),function(x) sort(phi[x,], decreasing = TRUE))

# p(w|t) of topic3 for top 10 words in the topic
sorted_phi[[3]][1:10]



##### distance between documents using doc-topic distribution

# distance between doc1 and doc30
dist(rbind(ldaOut@gamma[1,], ldaOut@gamma[30,]))
# distance for all document pairs
dist_matrix <- sapply(1:length(docs), function(x,y)
                dist(rbind(ldaOut@gamma[x,], ldaOut@gamma[y,])))[1:length(docs),]

# distance between doc1 and (doc1, doc2, doc3 ....)
dist_matrix[1,]

# name of document
rownames(dist_matrix) <- rownames(theta)
colnames(dist_matrix) <- rownames(theta)

# most similar document of doc1 (by document name)
sort(dist_matrix[1,])[1:4]

# index of document
rownames(dist_matrix) <- c(1:length(docs))
colnames(dist_matrix) <- c(1:length(docs))

# most similar document of doc1 (by document index)
sort(dist_matrix[1,])[1:2]






