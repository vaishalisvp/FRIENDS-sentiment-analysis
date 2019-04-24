###library load###
library(gridExtra)
library(fmsb)
library(udpipe)
library(data.table)
library(stringr)
library(twitteR)
library(purrr)
library(tidytext)
library(dplyr)
library(tidyr)
library(lubridate)
library(scales)
library(broom)
library(ggplot2)
library(rvest)
library(XML)
library(magrittr)
library(plyr)
library(ROAuth)
library(httr)
library(tm)
library(SnowballC)
library(wordcloud)
library(stargazer)
library(spacyr)
library(lettercase)

setwd("C:/Users/vaipr/Dropbox/FRIENDS tv show text mining")

friends <- read.csv("friends.csv")

count <- friends %>% dplyr::group_by(person) %>% dplyr::count(sort=TRUE)

View(count)

friends$person[friends$person=="mnca"] <- "monica"
friends$person[friends$person=="rach"] <- "rachel"
friends$person[friends$person=="phoe"] <- "phoebe"
friends$person[friends$person=="very funny. phoebe"] <- "phoebe"

friends$line <- toString(friends$line)
friends$linenum <- 1:nrow(friends)

friends$code <- paste0("S",friends$season, "E",friends$episodeNum)


homophobia <- friends %>%
  filter(str_detect(line, "gay|burlesque|lesbian|homosexual"))

fwrite(homophobia, "homophobia.csv")


female <- friends %>% 
  filter(str_detect(line, "woman|women|girl|girls")) %>% 
  filter(str_detect(line, "always|never"))


fwrite(female, "female.csv")

male <- friends %>% 
  filter(str_detect(line, "man|men|boy|boys|guy|guys")) %>% 
  filter(str_detect(line, "always|never"))

fwrite(male, "male.csv")


friends_bigrams <- friends %>%
  unnest_tokens(bigram, line, token = "ngrams", n = 2)
friends_bigrams


bigrams_separated <- friends_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigrams_separated

bigrams_separated <- read.csv("bigrams_separated.csv")


bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
bigrams_filtered

bigram_counts <- bigrams_filtered %>% # new bigram counts
  dplyr::count(person,word1, word2, sort = TRUE)
bigram_counts


####sentiment analysis
tokens <- friends %>% unnest_tokens(word, line, token = "ngrams", n = 1)

tokens <- read.csv("tokens.csv")
names(tokens)[names(tokens) == 'bigram'] <- 'word'


tokens_filtered <- tokens %>%
  filter(!word %in% stop_words$word)
tokens_filtered

tokens_counts <- tokens_filtered %>% 
  dplyr::count(person,word, sort = TRUE)
tokens_counts 


AFINN <- get_sentiments("afinn")

tokens_filtered <- tokens_filtered %>%  inner_join(AFINN, by = c(word = "word")) %>%
  dplyr::count(word, score, sort = TRUE) %>%
  ungroup()

tokens_filtered

tokens_filtered %>%
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  head(25) %>%
  mutate(word = reorder(word, contribution)) %>%
  ggplot(aes(word, n * score, fill = n * score > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Top 25 popular words from FRIENDS") +
  ylab("Sentiment score * number of occurrences") +
  coord_flip()


tokens_filtered %>%
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  filter(score<= -4) %>% 
  head(10) %>%
  mutate(word = reorder(word, contribution)) %>%
  ggplot(aes(word, n * score, fill = n * score > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Top 10 negative words uttered by characters") +
  ylab("Sentiment score * number of occurrences") +
  coord_flip()

tokens_filtered %>%
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  filter(score>=4) %>% 
  head(10) %>%
  mutate(word2 = reorder(word, contribution)) %>%
  ggplot(aes(word, n * score, fill = n * score > 0)) +
  geom_col(show.legend = FALSE, fill="#00BFC4") +
  xlab("Top 10 positive words uttered by characters") +
  ylab("Sentiment score * number of occurrences") +
  coord_flip()


###BING SENTIMENT ANALYSIS SCORING (https://www.kaggle.com/rtatman/tutorial-sentiment-analysis-in-r)
bing <- get_sentiments("bing") #assigns positive/negative
afinn <- get_sentiments("afinn") #assigns from scale from -5 to 5
nrc <- get_sentiments("nrc") # nrc - categorises each word into positive, negative, anger, anticipation, disgust, fear, joy, sadness, surprise and trust

tokens_filtered <- tokens %>%
  filter(!word %in% stop_words$word)
tokens_filtered ##211916 WORDS

tokensbing <- tokens_filtered %>%
  inner_join(bing) ##26856

ggplot(tokensbing, aes(x=as.factor(season), fill=sentiment)) + geom_bar(position="fill") 

#tokensafinn <- tokens %>%
  #inner_join(afinn) ##55870

tokensnrc <- tokens_filtered %>% inner_join(nrc)

tokensnrcseason1 <- tokensnrc %>% filter(season==1) %>% select(sentiment) %>% group_by(sentiment) %>% count() %>% mutate(perc=freq/sum(freq)) %>% select(sentiment,perc)

###location geographic
street <- bigrams_filtered %>%
  dplyr::filter(word2 == "street") %>%
  dplyr::count(season, episodeNum, person, word1, word2, sort = TRUE)

avenue <- bigrams_filtered %>%
  dplyr::filter(word2 == "avenue") %>%
  dplyr::count(season, episodeNum, person, word1, word2, sort = TRUE)

square <- bigrams_filtered %>%
  dplyr::filter(word2 == "square") %>%
  dplyr::count(season, episodeNum, person, word1, word2, sort = TRUE)

road <- bigrams_filtered %>%
  dplyr::filter(word2 == "road") %>%
  dplyr::count(season, episodeNum, person, word1, word2, sort = TRUE)


library(udpipe)
english <- udpipe_download_model(language = "english")
ud_english <- udpipe_load_model(english$file_model)

taggedwomen <- udpipe_annotate(ud_english, x = bigrams_separatedwomen2$word1)
taggedwomen <- as.data.frame(taggedwomen)

taggedmen <- udpipe_annotate(ud_english, x = bigrams_separatedmen2$word1)
taggedmen <- as.data.frame(taggedmen)

word1women <- taggedwomen %>%
  filter(upos == 'ADJ') %>% 
  select(sentence)

names(word1women)[names(word1women) == 'word'] <- 'word1'

word1men <- taggedmen %>%
  filter(upos == 'ADJ') %>% 
  select(sentence)

names(word1men)[names(word1men) == 'sentence'] <- 'word1'

bigrams_separatedwomen2 <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(word2 == "woman"|word2 == "women"|word2 == "female"|word2 =="girl"|word2 =="girls") %>% 
  dplyr::count(word1, word2, sort = TRUE) %>% 
  filter(word1 %in% word1women$word1)
View(bigrams_separatedwomen2)

bigrams_separatedmen2 <- bigrams_separated %>%
  
  filter(!word1 %in% stop_words$word) %>%
  filter(word2 == "man"|word2 == "men"|word2 == "male"|word2 =="boy"|word2 =="boys"|word2=="guy"|word2=="guys") %>% 
  dplyr::count(word1, word2, sort = TRUE) %>% 
  filter(word1 %in% word1men$word1)
View(bigrams_separatedmen2)

womenadj <- bigrams_separatedwomen2 %>% dplyr::group_by(word1) %>% dplyr::mutate(total=sum(n)) %>% distinct(word1,total)

menadj <- bigrams_separatedmen2 %>% dplyr::group_by(word1) %>% dplyr::mutate(total=sum(n)) %>% distinct(word1,total)


plot1 <- womenadj %>% arrange(desc(abs(total))) %>% head(10) %>% mutate(reorder(word1,total)) %>%  ggplot(aes(reorder(word1,total), total)) + geom_col(fill="lightpink2") + coord_flip() + ylab("Total # of occurences") + xlab("Female")


plot2 <- menadj %>% arrange(desc(abs(total))) %>% head(10) %>% mutate(reorder(word1,total)) %>%  ggplot(aes(reorder(word1,total), total)) + geom_col(fill="lightskyblue3") + coord_flip() + ylab("Total # of occurences") + xlab("Male")


grid.arrange(plot2,plot1, nrow=1,  top = "Top 10 adjectives used to describe each gender")
