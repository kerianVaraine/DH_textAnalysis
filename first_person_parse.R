# If not first person narrative, use this function below
# text_parsed_protagonist_labeled <- text_parsed

# If first person narrative, try these below
# Thesaurus of said
said_thes <- c('said', 'told', 'asked')


first_person_I <- text_parsed %>% filter(token == "I")

first_person_said <- text_parsed %>% filter(token %in% said_thes)

first_person_joined <- rbind(first_person_I, first_person_said)


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
  return (((initial_token == "I" & next_token %in% said_thes) & (initial_id == next_id) & (next_token_id - initial_token_id == 1)) 
          | (initial_token == "I" & third_token %in% said_thes) & (initial_id == next_id) & (third_token_id - initial_token_id == 2))
}

# TEST WITH PARSED DOC ___WORKS!!!!!
test3 <- first_person_joined %>% 
  data.table::setorder(cols = doc_id) %>%
  mutate(coocurrance = test_if_verb_close(token, token_id, doc_id)) %>%
  filter(coocurrance == TRUE)

# # At each point of text where I is followed by said_thesaurus within 2 words, change I to PROTAGONIST, and run correlations on that token.
# # turn this into a function to apply to text_parsed

label_first_person_speak <- function(text) {
  text %>%
    filter(doc_id %in% test3$doc_id, sentence_id %in% test3$sentence_id, token_id %in% test3$token_id) %>% 
    mutate(token = replace(token, token == "I", "protagonist"))
}

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
text_parsed_protagonist_labeled <- text_parsed %>%
  mutate_cond((doc_id %in% test3[,1] & sentence_id %in% test3[,2] & token_id %in% test3[,3] & token %in% "I"), token = replace(token, token == "I", "protagonist"),lemma = replace(lemma, lemma == "-PRON-", "protagonist"), pos = replace(pos, pos == "PRON", "PROPN")) 
  
#################################################################################
# Error rate testing
# Manually read through a chapter and compare with 1st person parsed method
#################################################################################
# convert parsed text to string for readability

readable.text.1stperson <- paste(text_parsed_protagonist_labeled$token, collapse = " ")
write.table(readable.text.1stperson, file = "firstPersText.txt", sep="")

