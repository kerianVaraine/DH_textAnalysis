# Tutorial for Spacy :: following https://www.dataquest.io/blog/tutorial-text-classification-in-python-using-spacy/
import spacy
import random
from spacy.lang.en import English
from spacy.pipeline import Sentencizer
nlp = spacy.load("en_core_web_sm")

# Loading the tex†: make sure terminal is in the right directory!
source_file = open('./PythonScripts/Disappearance.txt', 'r')
source_text = source_file.read()
source_doc = nlp(source_text)

# Creating a list of tokens
text_token_list = []

for token in source_doc:
    text_token_list.append(token.text)


print (text_token_list[10])
# ## Getting used to python syntax:: woop
# demonstrates use of len() for list length and random number generators
# for x in range(0, 20):
#     print(text_token_list[random.randint(0,len(text_token_list))], end=' ')


## splitting text into sentances
# create sentencizer component
# sentencizer = Sentencizer()
# #add component to pipeline
# nlp.add_pipe(sentencizer)

# text_sentence_list = []

# for sent in source_doc.sents:
#     text_sentence_list.append(sent.text)

#################################################################
# Filter source document
# Removes stopwords, puntuation and '\n\n'
# assign stopwords to variable
spacy_stopwords = spacy.lang.en.stop_words.STOP_WORDS
# init list to hold filtered text
doc_stopwords_removed = []
# loop and check if the word is not on the list, then add to filtered list
for word in source_doc:
    if word.is_stop==False:
        doc_stopwords_removed.append(word)

# trying it nicer within declaration and removing puntuation and new line expressions(\n\n)
#this works very well, but returns a list, not a nlp(doc).
doc_filtered = [token.text for token in source_doc if token.is_stop !=True and token.is_punct != True and token.text != "\n\n" and token.text != "\ufeff1"]

# Lemmatization of entire text
lemma_tokens = [token.lemma_ for token in source_doc]

# Combining all filtering into one call. WORKED! just needs a better name, and a better way to remove random "\"+ *
doc_lemmad_filtered = [token.lemma_ for token in source_doc if token.is_stop !=True and token.is_punct != True and token.text != "\n\n" and token.text != "\ufeff1"]

# Only lemming original text;
doc_lemmad_only = [token.lemma_ for token in source_doc if token.is_punct != True and token.text != "\n\n" and token.text != "\ufeff1"]


## Final Expression to be turned into a single funtion with input params: using regex to filter out all wierd '/' calls
# Import regex module
import re

######ALMOST
# @TODO here
doc_filtered_oneshot= [token.lemma_ for token in source_doc if token.is_stop !=True and token.is_punct != True and not re.search(r'^\\', token.text)]



###########
# At this point, this list is ready to be saved as csv for frequency analysis in R.
###########
# Export as CSV
# Import csv module
import csv

# Create or overwrite to csv file
with open('./data/filtered_text.csv', 'w', newline='') as myfile:
     wr = csv.writer(myfile, quoting=csv.QUOTE_ALL)
     wr.writerow(doc_lemmad_filtered)

#################################################################
# Quick frequency analysis of lemmed list
# Import counter module
from collections import Counter
word_freq = Counter(doc_lemmad_filtered)
common_words = word_freq.most_common(50)
print(common_words)




#################################################################
#trying entity search
text_ent_list = []
for ent in source_doc.ents:
    text_ent_list.append(ent)

print(text_ent_list)

#################################################################
# search for all entities in text
enitities = [(i,i.label_, i.label) for i in source_doc.ents]

# make list of only .label_ == "PERSON"
persons = []
for ent in source_doc.ents:
    if ent.label_ == "PERSON":
        persons.append(ent.text)

# can be exported as .csv and brought into R for further analysis.
# can use as an entity list for network analysis data gathering??

# Quick frequency analysis
name_freq = Counter(persons)
common_names = name_freq.most_common(len(persons))
print(common_names)
######################################