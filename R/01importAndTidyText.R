
importAsTibble <- function(filename) {

  # import original text from conversion, with line numbers, and chapter, skipping empty rows due to encoding
  as_tibble(read_lines(here::here("data", filename), skip_empty_rows = TRUE, ))
}

tokeniseAndAddChapters <- function(textToTokenise) {
  # add column with chapter in which lines are found
  tmp <- textToTokenise %>%
    mutate(linenumber = row_number(), chapter = cumsum(str_detect(as.matrix(textToTokenise), regex("[\\d]", ignore_case = TRUE))))

  # Each word in its own row, with line number and chapter.
  unnest_tokens(tmp, word, value)
}
