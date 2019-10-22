# Tutorial for Spacy :: following https://www.dataquest.io/blog/tutorial-text-classification-in-python-using-spacy/
import spacy
import random
from spacy.lang.en import English
from spacy.pipeline import Sentencizer
nlp = spacy.load("en_core_web_sm")

# Loading the tex†: make sure terminal is in the right directory!
source_file = open('Disappearance.txt', 'r')
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
# Remove stopwords from source document
# assign stopwords to variable
spacy_stopwords = spacy.lang.en.stop_words.STOP_WORDS
# init list to hold filtered text
doc_stopwords_removed = []
# loop and check if the word is not on the list, then add to filtered list
for word in source_doc:
    if word.is_stop==False:
        doc_stopwords_removed.append(word)
#################################################################


#################################################################
#trying entity search
text_ent_list = []
for ent in source_doc.ents:
    text_ent_list.append(ent)

print(text_ent_list[3])

#################################################################
# search for all entities in text
enitities = [(i,i.label_, i.label) for i in source_doc.ents]

# make list of only .label_ == "PERSON"
persons = []
for ent in source_doc.ents:
    if ent.label_ == "PERSON":
        persons.append(ent.text)

# Count occurances of each name (rework this code to find all occurances, or just chuck it in R?)
def most_frequent(List): 
    counter = 0
    num = List[0] 
      
    for i in List: 
        curr_frequency = List.count(i) 
        if(curr_frequency > counter): 
            counter = curr_frequency 
            num = i 
  
    return num 