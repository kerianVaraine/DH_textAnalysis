# set your working directory
setwd("F:/Projects/DH_Joyce/DH_textAnalysis")
# make sure these libraries are imported into Rstudio via Tools=>Install packages
library(tidyverse)
library(tidytext)
library(dplyr)
library(stringr)
library(stopwords)
data("stop_words")
library(SnowballC)

#stop_words <- as_tibble(stopwords("en"))

# import original text from conversion, with line numbers, and chapter, skipping empty rows due to encoding
text.original <- as_tibble(read_lines("data/Disappearance - Michael Joyce.txt", skip_empty_rows = TRUE, ))

# View(text.original)

# extract all chapter markings and apply them to the lines until the next chapter marking, and have these as a column
text.chapters <- text.original %>%
  mutate(linenumber = row_number(), chapter = cumsum(str_detect(as.matrix(text.original), regex("[\\d]", ignore_case = TRUE))))


# Lay each word out, with line number and chapter.
text.tidyish <- unnest_tokens(text.chapters, word, value)

# Remove end words from text. This does include I etc by default, need to alter the database for this perhaps?
text.tidyNoEndWords <- text.tidyish %>% anti_join(stop_words, by = c("word" = "word"))

##ORRRRR
text.tidrNoEndWords <- text.tidyish %>% filter(!word %in% stop_words$word)
##

unique_stopwords <- data.frame(word = c("i'm", "it's", "don't"))

text.tidyNoEndWords <- text.tidyNoEndWords %>% anti_join(unique_stopwords, by = "word")

# recurrance of words, not including stopWords.
frequency.noStopWords <- text.tidyNoEndWords %>% count(word, sort = TRUE)

# recurrance of words, including stopWords
frequency.IncludingstopWords <- text.tidyish %>% count(word, sort = TRUE)





########################################################
# word cloud
# Effective for beggining student work, not too difficult
library(wordcloud)

# define colour palette
pal <- brewer.pal(8, "Dark2")

frequency.noStopWords %>% with(wordcloud(word, n, random.order = FALSE, max.words = 50, colors = pal))







######################################
# sentiment analysis
######################################
library(textdata)
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

# sentiment in text, testing with joy from nrc dataset
nrc_joy <- get_sentiments("nrc") %>% filter(sentiment == "joy")
text.tidyNoEndWords %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)

# get sentiment per chapter???
#####################
# chapter1 sentiment:: create a function for this for plotting.
#####################

sentiment.ch1.raw <- filter(text.tidyNoEndWords, chapter == 1)

sentiment.ch1.bing <- sentiment.ch1.raw %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment) %>%
  spread(sentiment, n, fill = 0)
sentiment.ch1.positive <- sum(sentiment.ch1.bing$positive)
sentiment.ch1.negative <- sum(sentiment.ch1.bing$negative)


## Split text into chapters
chapters <- split(text.tidyNoEndWords, text.tidyNoEndWords$chapter)

## apply funtion to chapters listing
sentiment <- sapply(chapters, sentimentCount)

## function for sentiment calc per chapter
sentimentCount <- function(chap) {
  chap %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment = positive - negative)
}



## calc sentiment in general for each chapter. Wrap this in a function and create new dataset?
for (i in sentiment) {
  print(
    (if ("positive" %in% names(i)) {
      sum(i$positive)
    } else {
      0
    }) -
      (if ("negative" %in% names(i)) {
        sum(i$negative)
      } else {
        0
      })
  )
}

