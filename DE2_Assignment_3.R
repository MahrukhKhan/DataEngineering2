
library(aws.s3)
library(httr)
library(jsonlite)
library(aws.transcribe)
library(plyr)
library(aws.comprehend)
library(data.table)
library(tidyverse)
library(wordcloud)
library(tm)
library(dplyr)


rm(list=ls())

keyfile = list.files(path=".", pattern="accessKeys.csv", full.names=TRUE)
if (identical(keyfile, character(0))){
  stop("ERROR: AWS key file not found")
}

keyTable <- read.csv(keyfile, header = T) # *accessKeys.csv == the CSV downloaded from AWS containing your Access & Secret keys
AWS_ACCESS_KEY_ID <- as.character(keyTable$Access.key.ID)
AWS_SECRET_ACCESS_KEY <- as.character(keyTable$Secret.access.key)

#activate
Sys.setenv("AWS_ACCESS_KEY_ID" = AWS_ACCESS_KEY_ID,
           "AWS_SECRET_ACCESS_KEY" = AWS_SECRET_ACCESS_KEY,
           "AWS_DEFAULT_REGION" = "eu-west-1") 


#Creating List For MP3
list_of_names <- c('malcolm-cbc-1965', 'malcolm-police-1965', 'malcolm-TV-1963', 'malcolm-grassroot-1963')

list_of_sites <- c('https://2103239-mahrukh.s3.eu-west-1.amazonaws.com/Malcolm_X_1965_CBC.mp3',
                   'https://2103239-mahrukh.s3.eu-west-1.amazonaws.com/Malcolm_X_Interview_1965.mp3',
                   'https://2103239-mahrukh.s3.eu-west-1.amazonaws.com/Malcolm_X_JBritish_TV_1963.mp3',
                   'https://2103239-mahrukh.s3.eu-west-1.amazonaws.com/Malcolm_GrassRoot_1963.mp3')

  
# AWS Transcription
transcription <- function(list_of_names, list_of_sites) {
  start_transcription(list_of_names, list_of_sites)
  get_transcription(list_of_names)$Transcriptions
}

transcription_func <- mapply(transcription, list_of_names, list_of_sites)


# Get Sentiments for Text more than 5000 character
get_sentiments <- function(long_text){
  long_text = paste(long_text, collapse = '')
                    
  # Breaking the input text into character vectors of length.segm characters each
  char.segments <- function(x, segm.length){
    byte.counter <- nchar(x, type = 'bytes')
    f <- c(1, rep(0, segm.length - 1))
    f <- cumsum(rep(f, length.out = byte.counter))
    s <- split(unlist(strsplit(x,'')), f)
    unname(sapply(s, paste, collapse = ''))
  }
  
  five.thousand.byte.chunk <- char.segments(long_text, 5000)

  # Iterating through the chunks 
  for (i in 1:length(five.thousand.byte.chunk)) { 
    current_chunk = five.thousand.byte.chunk[i]
    if (current_chunk > "") {  
      # Some cats so that you can see the chunks and their byte sum
      
      df <- detect_sentiment(current_chunk)
      df$text = current_chunk
      
      if (!exists('sentiments_df')){
        sentiments_df = df
      } else {
        sentiments_df = rbind(sentiments_df, df)
      }
    }
  }
  return(sentiments_df)
}

list_of_sentiments <- lapply(transcription_func, get_sentiments)
sentiments <- rbindlist(list_of_sentiments)
sentiments$Index <- seq.int(nrow(sentiments)) 
sentiments$total_char <- nchar(sentiments$text)

## Taking Index 4 out as it's entirely on Muhammad Elijah and not relevant to the perspective we're looking at
sentiments <- sentiments %>% slice(-c(4))

#Averaging Sentiment for A Single Speech

sentiments_1 <- sentiments %>% 
                    filter(Index==1 |Index==2) %>% 
                    summarise('Neutral' = weighted.mean(Neutral, total_char), 
                    'Negative' = weighted.mean(Negative, total_char),
                    'Positive' = weighted.mean(Positive, total_char),
                    'Mixed' = weighted.mean(Mixed,total_char),
                    'Index' = 19652) %>% 
                     select(Index, everything())
                    

sentiments_2 <- sentiments %>% 
                      filter(Index==3 | Index==5) %>% 
                      summarise('Neutral' = weighted.mean(Neutral, total_char), 
                                'Negative' = weighted.mean(Negative, total_char),
                                'Positive' = weighted.mean(Positive, total_char),
                                'Mixed' = weighted.mean(Mixed,total_char),
                                'Index' = 19651) %>% 
                                 select(Index, everything())
 
sentiments_3 <- sentiments %>% 
                      filter(Index==7 | Index==8) %>% 
                      summarise('Neutral' = weighted.mean(Neutral, total_char), 
                     'Negative' = weighted.mean(Negative, total_char),
                     'Positive' = weighted.mean(Positive, total_char),
                     'Mixed' = weighted.mean(Mixed,total_char),
                     'Index' = 19632) %>% 
                      select(Index, everything())


# Creating final table and sorting it

sentiments[6,1] = 19631
sentiment_rbind <- rbind(sentiments, sentiments_1, sentiments_2, sentiments_3, fill=TRUE)
final_sentiments <- sentiment_rbind %>% 
          filter(Index>=1963) %>% 
          select(Index, Sentiment, Mixed, Negative, Neutral, Positive) 

final_sentiments$Index <- as.character(final_sentiments$Index)
final_sentiments <- final_sentiments[order(final_sentiments$Index, decreasing = TRUE), ]
final_sentiments$maxsenti <- pmax(final_sentiments$Mixed, final_sentiments$Negative, final_sentiments$Neutral, final_sentiments$Positive)

for (i in 1:4){
    if(final_sentiments$maxsenti[i]==final_sentiments$Mixed[i]){
      final_sentiments$Sentiment[i] = "MIXED"
      }
    else if(final_sentiments$maxsenti[i]==final_sentiments$Negative[i]){
      final_sentiments$Sentiment[i] = "NEGATIVE"
       }
    else if(final_sentiments$maxsenti[i]==final_sentiments$Positive[i]){
      final_sentiments$Sentiment[i] = "POSITIVE"
      }
    else {
      final_sentiments$Sentiment[i] = "NEUTRAL"
      }
}

#Final Table 

final_sentiments <- final_sentiments %>% filter(Index>=1963) %>% 
  select(Index, Sentiment, Mixed, Negative, Neutral, Positive) 


##World Cloud 1963

speeches_1963 <- VCorpus(VectorSource(sentiments[5:7,7])) 
speeches_1963 <- tm_map(speeches_1963, removeWords, stopwords())
tdm_speeches_1963 <- DocumentTermMatrix(speeches_1963)
  
w_f_1963 = sort(colSums(as.matrix(tdm_speeches_1963)), decreasing=TRUE)
df_speech_1963 = data.frame(word = names(w_f_1963), freq=w_f_1963)

##World Cloud 1965 

speeches_1965 <- VCorpus(VectorSource(sentiments[1:4,7])) 
speeches_1965 <- tm_map(speeches_1965, removeWords, stopwords())
tdm_speeches_1965 <- DocumentTermMatrix(speeches_1965)

w_f_1965 = sort(colSums(as.matrix(tdm_speeches_1965)), decreasing=TRUE)
df_speech_1965 = data.frame(word = names(w_f_1965), freq=w_f_1965)

#WordClouds

par(mfrow=c(1,2))


wordcloud(words = df_speech_1963$word, freq =  df_speech_1963$freq, min.freq = 1,
          max.words=50, random.order=FALSE, rot.per=0.25,
          colors=c("lightsteelblue1","lightsteelblue2","lightsteelblue3","lightsteelblue"))


wordcloud(words = df_speech_1965$word, freq =  df_speech_1965$freq, min.freq = 1,
          max.words=50, random.order=FALSE, rot.per=0.25,
          colors= c("indianred1","indianred2","indianred3","indianred"))


## Converting to long format for GG PLOTS
long_sentiments <- melt(final_sentiments)
## GG PLOTS

ggplot(long_sentiments, aes(Index, value*100, fill = variable)) +
  geom_bar(position='stack', stat ='identity') +
  #facet_wrap(~Index, ncol = 2, scales = "free_x") +
  theme_bw()

# AWS Detect Entities
entities_speech <- data.frame(sentiments[1:4,7])
entities_speech <- detect_entities(entities_speech$text)

table(entities_speech$Type)
ok <- entities_speech %>% 
      filter(Type=='LOCATION')

ggplot(ok, aes(x=Score)) + geom_density(fill='salmon') + theme_classic()
ggplot(ok, aes(x=Text, y=Score)) + geom_point(shape='star', col='salmon') + theme_classic()

# AWS Detect Syn
syntax_speech <- data.frame(sentiments[1:4,7])
syntax_speech <- detect_syntax(syntax_speech$text)

syntax_gg <- syntax_speech %>% filter(PartOfSpeech.Tag=='ADJ', 
                    Text!='same', Text!='other', Text!='many', Text!='more', PartOfSpeech.Score > 0.99 )


ggplot(syntax_gg, aes(x=Text, y=PartOfSpeech.Score)) + 
                  geom_point(size=4, col='red') + geom_line(size=2) +
                  coord_flip() + 
                  scale_y_continuous(name="Score") +
                  scale_x_discrete(name="ADJ") +
                  theme(axis.text.x = element_text(face="bold", color="#008000",
                                   size=8, angle=0),
                        axis.text.y = element_text(face="bold", color="#008000",
                                   size=8, angle=0))
