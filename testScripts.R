#set your working directory
setwd("F:/Projects/DH_Joyce/DH_textAnalysis")
#make sure these libraries are imported into Rstudio via Tools=>Install packages
library(tidyverse)
library(tidytext) 
library(dplyr)
library(stringr)
library(stopwords)
# library(tm)
# library(textclean)

#import original text from conversion, with line numbers, and chapter, skipping empty rows due to encoding
text.original <- as_tibble(read_lines("src/Disappearance - Michael Joyce.txt", skip_empty_rows = TRUE, ))

  #View(text.original)

#extract all chapter markings and apply them to the lines until the next chapter marking, and have these as a column
text.chapters <- text.original %>% mutate(linenumber = row_number(), 
                                          chapter = cumsum(str_detect(as.matrix(text.original), regex("[\\d]", ignore_case = TRUE))))

                                          
#Lay each word out, with line number and chapter.
text.tidyish <- unnest_tokens(text.chapters, word, value)
