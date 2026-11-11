# Student: Gabriella Delgado
# Student Number: EDUV4815041
# Module: ITBDA4-14
# PROJECT 2
# QUESTION 2
library(tm)
library(topicmodels)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)

# Question 2.1
#Data Preparation and Preprocessing

# Import the ZIP file
zip_file <- "/Users/gaby/Desktop/news.zip"

if (!file.exists(zip_file)) {
  stop("ZIP file not found")
}

# Extract the news articles
folder <- tempfile("news")
dir.create(folder)
unzip(zip_file, exdir = folder)

# Find all the news articles
files <- list.files(folder, pattern = "\\.txt$", recursive = TRUE, full.names = TRUE)

if (length(files) == 0) stop("No text files found")

# Import the articles into a data frame
news_df <- data.frame(
  file_name = basename(files),
  content = unname(vapply(files, function(file) {
    paste(readLines(file, warn = FALSE, encoding = "UTF-8"), collapse = " ")
  }, character(1))),
  stringsAsFactors = FALSE
)

# Preprocess and denoise the text
corpus <- VCorpus(VectorSource(news_df$content))

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("en"))
corpus <- tm_map(corpus, stripWhitespace)

# Store the cleaned data in the data frame
news_df$content <- vapply(corpus, as.character, character(1))

# Display the data frame
str(news_df)


# Question 2.2
# LDA Topic Modelling

# Create the Document Term Matrix
dtm <- DocumentTermMatrix(corpus, control = list(wordLengths = c(2, Inf)))

# Set the number of topics and top words
n_topics <- 5
k_words <- 10

# Create the LDA model
lda_model <- LDA(dtm, k = n_topics, control = list(seed = 1234))

# Display the top 10 words per topic
top_words <- terms(lda_model, k_words)
print(top_words)

# Obtain the topic word probability distributions
topic_probabilities <- posterior(lda_model)$terms

# Create a table of all the words and probabilities per topic
topic_words_df <- do.call(rbind, lapply(1:n_topics, function(i) {
  data.frame(
    Topic = paste("Topic", i),
    Word = top_words[, i],
    Probability = as.numeric(topic_probabilities[i, top_words[, i]])
  )
}))

rownames(topic_words_df) <- NULL

# Display the probability distribution of the top words
print(transform(topic_words_df, Probability = round(Probability, 4)))

# Display the document topic distribution
document_topics <- posterior(lda_model)$topics
rownames(document_topics) <- news_df$file_name

print(round(document_topics, 3))

# Question 2.3 
# Visualisations

set.seed(1234)

for (i in 1:n_topics) {
  
  # Select the top words for the current topic
  topic_df <- topic_words_df[
    topic_words_df$Topic == paste("Topic", i), ]
  
  # Visualisation 1 (Word Cloud)
  wordcloud(
    words = topic_df$Word,
    freq = topic_df$Probability,
    min.freq = 0,
    max.words = k_words,
    random.order = FALSE,
    colors = brewer.pal(8, "Dark2")
  )
  
  title(main = paste("Topic", i, "- Word Cloud"))
  
  # Visualisation 2 (Bar Graph)
  p <- ggplot(topic_df,
              aes(x = reorder(Word, Probability),
                  y = Probability)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(title = paste("Topic", i, "- Top 10 Words"),
         x = "Word",
         y = "P(word | topic)") +
    theme_minimal()
  
  print(p)
}
