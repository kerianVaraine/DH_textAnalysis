library(quanteda)
library(tidyverse)
library(readtext)

# setting up text.chapter.sectioned as corpus object where doc id is chapterS?
q.text.chapters <- text.chapters.sectioned %>% 
  select(doc_id = chapter, text = token, section)

# q.c <- corpus(q.text.chapters, docid_field = "doc_id", text_field = "text")
# head(q.c)
# docvars(q.c)

#comparing quanteda and spacyr tokenising.
# q. == quanteda, q.s. == spacyr
# import in lower case
q.text <- char_tolower(source_text)

#tokenise text with quanteda
q.word <- tokens(q.text, remove_punct = TRUE) %>% as.character()

# tokenise with spacyr
q.s.word <- spacy_parse(source_text) %>% as.tokens() 

#doc-freq-matrix
q.dfm <- dfm(q.word, remove_punct = TRUE, remove = stopwords("english"))
textstat_frequency(q.dfm, n = 10)

q.s.dfm <- dfm(q.s.word, remove_punct = TRUE, remove = stopwords("english"))

# get top features ?
q.sorted.freqs <- topfeatures(q.dfm, n = nfeat(q.dfm))
q.s.sorted.freqs <- topfeatures(q.s.dfm, n = nfeat(q.dfm))

# check freq of chars in topfeatures
q.sorted.freqs[character_vector]
q.s.sorted.freqs[character_vector]

ntoken(q.dfm)
sum(q.sorted.freqs)
