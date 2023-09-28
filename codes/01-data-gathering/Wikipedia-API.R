## WIKIPEDIA API -- WORDCLOUD

library(rvest)
library(tm)
library(wordcloud2)

# define base URL
base_url <- "https://en.wikipedia.org/wiki/NBA_All-Star_Game_"

# define years
years <- setdiff(1980:2010, 1999)

# empty text corpus
corpus <- Corpus(VectorSource(""))

# loop through the years
for (year in years) {
  # make full URL
  url <- paste0("https://en.wikipedia.org/wiki/", year, "_NBA_All-Star_Game")
  
  # read HTML content from the URL
  webpage <- read_html(url)
  
  # extract the main content
  page_text <- webpage %>%
    html_nodes("p") %>%
    html_text() %>%
    paste(collapse = " ")
  
  # add the page text to the corpus
  corpus <- Corpus(VectorSource(page_text))
}

# create term doc matrix
tdm <- TermDocumentMatrix(corpus, control = list(removePunctuation = TRUE,
                                                 removeNumbers = TRUE,
                                                 stopwords = stopwords("en"),
                                                 tolower = TRUE))

# convert to a matrix
matrix <- as.matrix(tdm)

# get word frequencies
word_freq <- rowSums(matrix)

# create a word cloud
wordcloud2(data = data.frame(word = names(word_freq), freq = word_freq), 
           color = 'random-dark', backgroundColor = 'black')
