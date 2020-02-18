# install.packages("spacyr")
library(spacyr)
spacy_initialize()

library(tidytext)
library(tidyverse)
library(stringr)

# This needs to be done properly, with relative paths, etc
# Also done in init R script
library(here)
root_dir = here()
data_dir = here("data/")
plot_dir = here("plots/")
source_text_file = here("data/Disappearance - Michael Joyce.txt")
source_text = read_lines(source_text_file, skip_empty_rows = TRUE)

############# Huck Finn example
# source_text_file = here("data/huck.txt")
# source_text = read_lines(source_text_file, skip_empty_rows = TRUE)
# ### manual analysis and preprosessing happens here or in text editor.
# ### reload text if edited in editor
# # huck finn example, change all CHAPTER occurances to numbers
# m <- regexpr("CHAPTER \\w+.", source_text)
# regmatches(source_text,m) <- 1:length(m)
# # single occurance of chapter naming
# source_text[9113] <- 43
#########################################


# parse text with spacyr, and add chapter column
text_parsed <- spacy_parse(source_text, pos=TRUE, tag=TRUE)
text_parsed <- text_parsed %>% mutate(chapter = cumsum(str_detect(as.matrix(text_parsed$token), regex("(^0?[1-9]$)|(^1[0-2]$)", ignore_case = TRUE))))
#docid = line number  | sentence id = sentance within docid | tokenId = index in line | token = single word or punct | 

# Get number of chapters (not perfect for huck finn example)
no_of_chapters <- length(unique(text_parsed[["chapter"]]))

