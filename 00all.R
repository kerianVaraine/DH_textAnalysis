library(dplyr)
library(readr)
library(stringr)
library(tidytext)

# Imports funtions from "R" folder
source(here::here("R", "01importAndTidyText.R"))

# Import text from "\data" folder
rawText <- importAsTibble("Disappearance - Michael Joyce.txt")

# Tokenise and add chapters
tokenText <- tokeniseAndAddChapters(rawText)
