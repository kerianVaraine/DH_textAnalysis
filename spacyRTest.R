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
source_text_file = here("data/Disappearance - Michael Joyce.txt")
source_text = read_lines(source_text_file, skip_empty_rows = TRUE)

# parse text with spacyr, and add chapter column
text_parsed <- spacy_parse(source_text)
text_parsed <- text_parsed %>% mutate(chapter = cumsum(str_detect(as.matrix(text_parsed$token), regex("[\\d]", ignore_case = TRUE))))
#docid = line number  | sentence id = sentance within docid | tokenId = index in line | token = single word or punct | 
# Get number of chapters

no_of_chapters <- length(unique(text_parsed[["chapter"]]))

#-----------------------------------------

#extract noun phrases
text_nounphrases <- spacy_extract_nounphrases(source_text)

#################################
# extract only the entities
text_entities <- spacy_extract_entity(source_text)
# Get only characters from text
text_characters <- text_entities %>% filter(ent_type == "PERSON")
#################################
# One Liner of above, easy function to make
# @TODO need to include all character names, like gypsy, ect... that wouldn't be picked up by pos PERSON:
# do that in filter with or statement running through a list of character names that aren't picked up. but entity search doesn't bring up gypsy!
text_characters <- source_text %>% spacy_extract_entity() %>% filter(ent_type %in% "PERSON")

# Return frequency of each character name, no filtering
character_frequency <- text_characters %>% count(text, sort = TRUE)

#################################
# trying cooccurance of characters across text
# Simple Corollation approach:
########################
# FROM : Skorinkin, 2017
# 1. Character co-occurrence at certain length. We assume there is an edge be-
# tween two character nodes if they appear together within the same sentence or
# paragraph or chapter or simply a text window of a given length. This is the
# most primitive and abstract formalization, which is nevertheless widely used
# due to its simplicity. The number of cooccurrences usually becomes the weight
# of an edge between the characters.
########################
#My Approach

# Step 1; Identify all the different names of each character, then group them together.
# Step 2: Identify at which parts of the text characters are in the same sentence, chapter, paragraph?

# First person speech? HARD. Maybe extract all Parts of speech with the pattern that matches 'I Said'
# within text_parsed, check for occurance of pronoun "I" along with verb within a few words of each other.
first_person_I <- text_parsed %>% filter(token == "I")
# Not needed #####   first_person_verb <- text_parsed %>% filter((pos == "VERB" & lemma == "say") & !str_detect(token, "say")) 
# start with said, need to do with other words used in 1st person conversational passages

# Thesaurus of said
said_thes <- c("said")
first_person_said <- text_parsed %>% filter(token == said_thes)

first_person_joined <- rbind(first_person_I, first_person_said)

#########Testing the function to check if I is followed by 'said' within 3 words?
testing_function <- first_person_joined %>% filter(doc_id == "text1012")
# go through dataframe and check if (token_id of token == "I") is proximate to token_id of token=="said"
test2 <- testing_function %>% group_by(doc_id)
#gather tokens from row below, and row below that
test3 <- test2 %>% mutate(lead = lead(token)) %>% mutate(lead2 = lead(token, 2))


###THIS FUNCTION IS VERTY CRUCIAL
test_if_verb_close <- function(token, token_id, doc_id) {
  #to be used inside mutate function? 
  #ie: test3 <- test2 %>% mutate(coocurrance = test_if_verb_close(token))
  #
  #get token if == 'I' 
  #get next and next token if == 'said' @TODO Make vector of words used after 'I' to denote speaking to someone
  #if I and said are present, get doc_id, if doc_id is the
  #same and token_id is within 2 of each other, return TRUE and add to
  #coocurrance column
  initial_token <- token
  initial_token_id <- token_id
  initial_id <- doc_id
  next_token <- lead(token)
  next_token_id <- lead(token_id)
  next_id <- lead(doc_id)
  third_token <- lead(token, 2)
  third_token_id <- lead(token_id, 2)
  third_id <- lead(doc_id, 2)
  return (((initial_token == "I" & next_token == 'said') & (initial_id == next_id) & (next_token_id - initial_token_id == 1)) 
          | (initial_token == "I" & third_token == 'said') & (initial_id == next_id) & (third_token_id - initial_token_id == 2))
}
# INITIAL TESTS WORK WITH SIMPLE DF!
test3 <- test2 %>% mutate(coocurrance = test_if_verb_close(token, token_id, doc_id))

# TEST WITH PARSED DOC ___WORKS!!!!!
test3 <- first_person_joined %>% data.table::setorder(cols = doc_id) %>% mutate(coocurrance = test_if_verb_close(token, token_id, doc_id)) %>% filter(coocurrance == TRUE)

# # At each point of text where I is followed by said within 2 words, change I to PROTAGONIST, and run correlations on that token.
# # turn this into a function to apply to text_parsed
# text_parsed_protagonist_labeled %>% filter(doc_id == test3[1,1], sentence_id == test3[1,2], token_id == test3[1,3]) %>% mutate(token = replace(token, token == "I", "PROTAGONIST"))

label_first_person_speak <- function(text) {
  #need to not filter exactly, but only apply the function to the part needed.
  text %>% filter(doc_id %in% test3$doc_id, sentence_id %in% test3$sentence_id, token_id %in% test3$token_id) %>% mutate(token = replace(token, token == "I", "PROTAGONIST"))
  
}

###########################################################
## SUBSET ATTEMPT
################ THIS IS THE ONE!!!
# I don't quite understand it, but it goes through the table, and changes values only if conditions are met.
# @TODO # Need to create an simpler function for this
# trying from https://stackoverflow.com/questions/34096162/dplyr-mutate-replace-several-columns-on-a-subset-of-rows
mutate_cond <- function(.data, condition, ..., new_init = NA, envir = parent.frame()) {
  # Initialize any new variables as new_init
  new_vars <- substitute(list(...))[-1]
  new_vars %<>% sapply(deparse) %>% names %>% setdiff(names(.data))
  .data[, new_vars] <- new_init
  
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data %>% filter(condition) %>% mutate(...)
  .data
}

# use mutate_cond to apply change only when condition met.
# https://stackoverflow.com/questions/10865095/why-do-i-get-warning-longer-object-length-is-not-a-multiple-of-shorter-object-l
# reference for use of %in% instead of == in conditional statement
text_parsed_protagonist_labeled <- text_parsed %>% mutate_cond((doc_id %in% test3[,1] & sentence_id %in% test3[,2] & token_id %in% test3[,3]), token = replace(token, token == "I", "PROTAGONIST"))

# Attempt to rename all characters to one name each::



###########END SUBSET
###########################################################

##################
# Dee, Mr D., or
characters_filtered.dee <- text_characters %>% filter(str_detect(text, "Dee"))

# Beckmesser
characters_filtered.beckmesser <- text_characters %>% filter(str_detect(text, "Beck"))

# Marco or Manco, [nr] means match either of those cases
characters_filtered.marco <- text_characters %>% filter(str_detect(text, "^Ma[nr]co"))

# Frankie, the boy, candido?
characters_filtered.frankie <- text_characters %>% filter(str_detect(text, "Frank"))
    #convert to same format as other character vectors, using rootword column.
characters_filered.boy <- text_nounphrases %>% filter(str_detect(text, regex("the boy$", ignore_case = TRUE)))

# Irene, irina, Gypsy(from main text_parsed),
characters_filtered.irene <- text_characters %>% filter(str_detect(text, "Ir[ie]n[ae]"))
characters_filered.gypsy <- text_nounphrases %>% filter(str_detect(text, regex("gypsy", ignore_case = TRUE))) 
irene_vec <- unique(sort(c(characters_filtered.irene$text)))



#######
# GOTO tidying_process.R
#######

#################
# from https://www.tidytextmining.com/ngrams.html
# Convert to wide data to count and correlationg pairs of words.

library(widyr)

text_parsed_word_pairs <- text_parsed_protagonist_labeled %>% filter(!token %in% stop_words$word, pos != "PUNCT") %>% group_by(token) %>% filter(n() >=1) %>% pairwise_cor(token, doc_id, sort = TRUE)
# Not quite sure what this has pulled out.... but here is a way to filter by word and correlation(correlation taken out used to have param ,correlation >= 0.1)
view(filter(text_parsed_word_pairs, item1 == "Beckmesser"))

view(filter(text_parsed_word_pairs, item1 == "Dee"))

view(filter(text_parsed_word_pairs, item1 == "boy"))

view(filter(text_parsed_word_pairs, item1 =="Cedric"))

# using protagonist label to identify correlation with other words.
view(filter(text_parsed_word_pairs, item1 %in% "PROTAGONIST"))


### select only unique calls to character names in text

marco_vec <- unique(sort(c(characters_filtered.marco$text)))
beck_vec <- unique(sort(c(characters_filtered.beckmesser$text)))
dee_vec <- unique(sort(c(characters_filtered.dee$text)))
frankie_vec <- unique(sort(c(characters_filtered.frankie$text))) 


# Join all character vectors into one main list of characters
all_chars_vec <- c(marco_vec, beck_vec, dee_vec, frankie_vec, "Rosaria", "PROTAGONIST")

#Filter by character name vector, containing various names for one character
character_correlations <-  text_parsed_word_pairs %>%
  filter(item1 %in% all_chars_vec & item2 %in% all_chars_vec)




### TESTING COR GRAPHING
library(ggplot2)
library(igraph)
library(ggraph)
character_correlations %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()
