#Packages
install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("tidytext")
#install.packages(c('ROAuth','RCurl'))
#install.packages("rtweet")
#install.packages("tm", dependencies=TRUE)
#install.packages("stringi")
#install.packages("stringr")
#install.packages("wordcloud")
#install.packages("topicmodels") 
library("wordcloud")
library("tm")
library("stringi")
library("stringr")
library(ggplot2)
library(dplyr)
library(tidytext)
library(ROAuth)
library(RCurl)
library(rtweet)
library(igraph)
library("sentimentr")
library(topicmodels)
library(tidyverse)
library(rvest)
library(reshape2)
library(magrittr)
library(data.table)

###########################################################################
#---------------Create the app and set the keys for twitter---------------#
###########################################################################

appname <- "Social_Media_Sentiment_analysis"

## API key 
key <- ""

## API secret 
secret <- ""

twitter_token <- create_token(app = appname,consumer_key = key, consumer_secret = secret, access_token = "", access_secret = "")

###########################################################################
#----------------------Collect the data from Twitter----------------------#
###########################################################################

query = "Volkswagen OR volkswagen OR VOLKSWAGEN"
number.of.tweets = 18000

vw = search_tweets(
  query,
  n = number.of.tweets,
  include_rts = FALSE, #No retweets, only original tweets!
  geocode = NULL,
  max_id = NULL,
  parse = TRUE,
  token = twitter_token,
  retryonratelimit = FALSE,
  verbose = TRUE,
  lang = "en",
  tweet_mode = "extended" 
)

############################################################################
#----------------Check retweets and favorite tweets-----------------------#
############################################################################

vw.dt = setDT(vw)

ret = vw.dt[,.(retweet_count), by= .(full_text)][order(-retweet_count)]

fav = vw.dt[,.(favorite_count), by= .(full_text)][order(-favorite_count)]

############################################################################
#----------------------Daily Tweet Count Visualization---------------------#
############################################################################

ts_plot(vw, "days")+
  ggplot2::theme_minimal()+
  ggplot2::theme(plot.title=ggplot2::element_text(face="bold"))+
  ggplot2::labs(x=NULL,y=NULL,
                title="Frequency of Volkswagen Twitter statuses",
                subtitle="Twitter status counts daily intervals",
                caption="\nSource: Data collected from Twitter's API"
  )

###########################################################################
#--------------------Corpus Creation and Data Cleaning--------------------#
###########################################################################

## Removing special characters in non latin language

usableText <- iconv(vw$text, to = "ASCII", sub="")
vw_corpus<-Corpus(VectorSource(usableText))
vw_corpus<-tm_map(vw_corpus,tolower)
vw_corpus<-tm_map(vw_corpus,removePunctuation)
vw_corpus<-tm_map(vw_corpus,removeNumbers)
vw_corpus<-tm_map(vw_corpus,function(x)removeWords(x,stopwords()))
vw_corpus<-tm_map(vw_corpus,removeWords,"volkswagen") # it appears in every tweet and affects the visualization
vw_corpus<-tm_map(vw_corpus,removeWords,"amp")
vw_corpus<-tm_map(vw_corpus,
                  function(x)removeWords(x,
                                         stopwords("english")))
vw_corpus<-tm_map(vw_corpus,
                  function(x)removeWords(x,
                                         stopwords("french")))

vw_corpus<-tm_map(vw_corpus,
                  function(x)removeWords(x,
                                         stopwords("italian")))

vw_corpus<-tm_map(vw_corpus,
                  function(x)removeWords(x,
                                         stopwords("spanish")))

text_corpus <- tm_map(vw_corpus,
                      content_transformer(function(x)
                        iconv(x,to='ASCII',sub='byte')))


###########################################################################
#---------------------Frequency of words and hashtags---------------------#
###########################################################################


#----------------------Document Term Matrix----------------------#
vw.tdm <- TermDocumentMatrix(text_corpus) 
m <- as.matrix(vw.tdm) 
m[1:20,1:5] 

v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10) 

#----------------------Word Frequency----------------------#
barplot(d[1:20,]$freq, las = 3, 
        names.arg = d[1:20,]$word,col ="lightblue", 
        main ="Most frequent words",
        ylab = "Word frequencies") 

#------------Words that appear at least 50 times-----------#

findFreqTerms(vw.tdm, highfreq=50)[1:20]


#--------------------Word Cloud----------------------------#

wordcloud(words = d$word, freq = d$freq, min.freq = 10, max.words=100, 
          random.order=FALSE, rot.per=0.35, colors=brewer.pal(6, "Dark2"))

## Keep words that appear in 95% of tweets

vw1.tdm<-removeSparseTerms(vw.tdm,sparse=0.95)


## Convert the term-document matrix to a data frame

vw.df <- as.data.frame(as.matrix(vw1.tdm))

## Scale the data 

vw.df.scale <- scale(vw.df)

#------------------------Distance Matrix------------------------#
vw.dist <- dist(vw.df.scale,method = "euclidean")

#----------------------------Clusters---------------------------#
vw.fit<-hclust(vw.dist, method="ward.D2")

## Visualize the result
plot(vw.fit, main="Cluster-Volkswagen")

## Plotting clusters with visualization 

groups <- cutree(vw.fit, k=3)
plot(vw.fit, main="Cluster-Volkswagen")
rect.hclust(vw.fit, k=3, border="red")

#------------------Relationship between Hashtags-----------------#
##  Define a tag extractor function

tags<-function(x) toupper(grep("#",strsplit(x," +")[[1]],value=TRUE))

# Create a list of tag sets for each tweet

l <- nrow(vw)
taglist <- vector(mode = "list", l)
texts <- vector(mode = "character", length = l)

## Extract the tweet text from each tweet status and populate it

for (i in 1:l) texts[i] <- vw$text[i]
texts <- iconv(texts, to = "ASCII", sub="")

j<-0
for(i in 1:l){
  if(is.na(str_match(texts[i],"#"))[1,1]==FALSE){
    j<-j+1
    taglist[[j]]<-str_squish(removePunctuation(tags(ifelse(is.na(str_match(texts[i],                                                               "[\n]")[1,1])==TRUE,texts[i],gsub("[\n]"," ",texts[i])))))
  }
}
alltags <- NULL
for (i in 1:l) alltags<-union(alltags,taglist[[i]])


## Create an empty graph and populate it with nodes and edges

hash.graph <- graph.empty(directed = T)
hash.graph <- hash.graph + vertices(alltags)

for (tags in taglist){
  if (length(tags)>1){
    for (pair in combn(length(tags),2,simplify=FALSE,
                       FUN=function(x) sort(tags[x]))){
      if (pair[1]!=pair[2]) {
        if (hash.graph[pair[1],pair[2]]==0) 
          hash.graph<-hash.graph+edge(pair[1],
                                      pair[2])
      }
    }
  }
}


## Network construction

V(hash.graph)$color <- "black"
  E(hash.graph)$color <- "black"
    V(hash.graph)$name <- paste("#",V(hash.graph)$name,sep = "")
    V(hash.graph)$label.cex = 0.5
    V(hash.graph)$size <- 15
    V(hash.graph)$size2 <- 2
    hash.graph_simple<-delete.vertices(simplify(hash.graph),degree(hash.graph)<=70)
    
    ## Network construction
    
    plot(hash.graph_simple, edge.width = 2, 
         edge.color = "black", vertex.color = "SkyBlue2",
         vertex.frame.color="black", label.color = "black",
         vertex.label.font=1, edge.arrow.size=0.05) 
    
    
    ###########################################################################
    #---------------------------Sentiment Analysis----------------------------#
    ###########################################################################
    
    plain.text<-vector()
    
    for(i in 1:dim(vw)[1]){
      plain.text[i]<-vw_corpus[[i]][[1]]
    }
    
    sentence_sentiment<-sentiment(get_sentences(plain.text))
    
    sentence_sentiment
    
    ## Average sentiment of the tweets
    
    average_sentiment<-mean(sentence_sentiment$sentiment)
    average_sentiment
    
    sd_sentiment<-sd(sentence_sentiment$sentiment)
    sd_sentiment
    
    extract_sentiment_terms(get_sentences(plain.text))
    
    ## Set the confidence interval 
    ci_upper= average_sentiment + ((1.96*sd_sentiment)/sqrt(100))
    ci_lower= average_sentiment - ((1.96*sd_sentiment)/sqrt(100))
    ci_upper
    ci_lower
    if(average_sentiment<ci_upper && average_sentiment>ci_lower){
      "Value is similar to 0"
    }
    
    #-----------------------the top 10 and last 10 tweets--------------------#
    
    vw.dt[,.(full_text,average_sentiment)][order(-average_sentiment)] %>% tail(10)
    vw.dt[,.(full_text,average_sentiment)][order(-average_sentiment)] %>% head(10)
    
    ###########################################################################
    #--------------------------Topic Modeling: LDA----------------------------#
    ###########################################################################
    
    text_corpus2<-text_corpus[1:200]
    doc.lengths<-rowSums(as.matrix(DocumentTermMatrix(text_corpus2)))
    dtm <- DocumentTermMatrix(text_corpus2[doc.lengths > 0])
    
    # Pick a random seed for replication
    SEED = sample(1:1000000, 1)  
    
    # 3 topics
    k = 3  
    Topics_results<-LDA(dtm, k = k, control = list(seed = SEED))
    terms(Topics_results,15)
    topics(Topics_results)
    
    ## Topic Modeling
    tidy_model_beta<-tidy(Topics_results, matrix = "beta")
    
    tidy_model_beta %>%
      group_by(topic) %>%
      top_n(10, beta) %>%
      ungroup() %>%
      arrange(topic, -beta) %>%
      ggplot(aes(reorder(term, beta),beta,fill=factor(topic)))+
      geom_col(show.legend = FALSE) +
      facet_wrap(~ topic, scales = "free") +
      scale_fill_viridis_d() + 
      coord_flip() + 
      labs(x = "Topic", 
           y = "beta score", 
           title = "Topic modeling")
    
    
    