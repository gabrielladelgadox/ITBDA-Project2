# Student: Gabriella Delgado
# Student Number: EDUV4815041
# Module: ITBDA4-14
# PROJECT 2
# QUESTION 3

# Load libraries
library(rvest)
library(tm)
library(topicmodels)
library(ggplot2)

# Question 3.1
# Web Scraping

# Chosen website
url <- "https://science.nasa.gov/climate-change/evidence/"

# Read the webpage
page <- read_html(url)

# Extract webpage paragraphs and headings
paragraphs <- html_text2(html_elements(
  page, "main p, main h2, main h3, main h4"
))

# Create the data frame
web_df <- data.frame(
  Paragraph = seq_along(paragraphs),
  Text = paragraphs
)

# Display the data structure
str(web_df)
print(head(web_df, 3))

# Question 3.2
# Text Preprocessing

# Remove image credits and any trailing reference numbers
web_df$Text <- sub(
  "(?is)\\s*(image credit|credit):.*$",
  "", web_df$Text, perl = TRUE
)

web_df$Text <- sub(
  "([.!?])\\s*\\d+(,\\s*\\d+)*\\s*$",
  "\\1", web_df$Text, perl = TRUE
)

# Remove extra whitespace
web_df$Text <- trimws(web_df$Text)

# Identify the References section
ref_start <- which(grepl(
  "^references\\b|^(1\\.?\\s*)?IPCC Sixth Assessment Report",
  web_df$Text, ignore.case = TRUE
))

# Remove the References section and following content
if (length(ref_start) > 0 && ref_start[1] > 1) {
  web_df <- web_df[1:(ref_start[1] - 1), ]
} else {
  stop("References section not found. Check extracted webpage text.")
}

# Remove empty, short, duplicate and credit-related text
web_df <- web_df[
  nzchar(web_df$Text) &
    nchar(web_df$Text) >= 70 &
    !grepl("^image credit|^credit:|^image:|^header image",
           web_df$Text, ignore.case = TRUE),
]

web_df <- unique(web_df["Text"])

# Reset the paragraph numbers
web_df$Paragraph <- seq_len(nrow(web_df))
web_df <- web_df[, c("Paragraph", "Text")]
rownames(web_df) <- NULL

# Preprocessing function to clean the text
clean_text <- function(text) {
  
  # Remove any URLs before punctuation
  text <- gsub("https?://\\S+|www\\.\\S+", " ", text,
               ignore.case = TRUE, perl = TRUE)
  
  corpus <- VCorpus(VectorSource(text))
  
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  corpus <- tm_map(corpus, stripWhitespace)
  
  unname(vapply(corpus, as.character, character(1)))
}

# Clean the extracted webpage content
web_df$Clean_Text <- clean_text(web_df$Text)

# Remove empty records
web_df <- web_df[nzchar(web_df$Clean_Text), ]
rownames(web_df) <- NULL

# Display the preprocessed content
print(head(web_df[, c("Paragraph", "Clean_Text")], 3))

# Create a Document-Term Matrix
corpus <- VCorpus(VectorSource(web_df$Clean_Text))
dtm <- DocumentTermMatrix(
  corpus, control = list(wordLengths = c(3, Inf))
)

# Display the Document-Term Matrix
print(dtm)

# Check for remaining unwanted terms
print(Terms(dtm)[grepl("https|doi|credit", Terms(dtm))])

# Check the final extracted paragraphs
print(tail(web_df$Text, 3))

# Question 3.3
# Text Summarisation

# Split the original text into sentences
sentences <- unique(trimws(unlist(
  strsplit(web_df$Text, "(?<=[.!?])\\s+", perl = TRUE)
)))

sentences <- sentences[nchar(sentences) >= 60]

# Clean the sentences for analysis
sentence_clean <- clean_text(sentences)

keep <- nzchar(sentence_clean)
sentences <- sentences[keep]
sentence_clean <- sentence_clean[keep]

# Create a sentence level Document-Term Matrix
sentence_dtm <- DocumentTermMatrix(
  VCorpus(VectorSource(sentence_clean)),
  control = list(wordLengths = c(3, Inf))
)

# Remove sentences without usable terms
keep <- rowSums(as.matrix(sentence_dtm)) > 0
sentences <- sentences[keep]
sentence_dtm <- sentence_dtm[keep, ]

if (length(sentences) < 5) {
  stop("Not enough sentences for summarisation.")
}

sentence_counts <- as.matrix(sentence_dtm)


# Technique 1
# TF-IDF Extractive Summarisation

# Calculate TF-IDF weights
tfidf <- as.matrix(weightTfIdf(sentence_dtm))

# Calculate the sentence scores
tfidf_scores <- rowSums(tfidf)

# Select the five highest-scoring sentences
tfidf_selected <- sort(
  head(order(tfidf_scores, decreasing = TRUE), 5)
)

# Display the TF-IDF summary
cat("\nTF-IDF EXTRACT SUMMARY:\n")
cat(paste(sentences[tfidf_selected], collapse = " "), "\n")


# Technique 2
# LDA Topic Summary

# Set the number of topics
n_topics <- 3

# Remove empty documents
lda_dtm <- dtm[rowSums(as.matrix(dtm)) > 0, ]

if (nrow(lda_dtm) < n_topics) {
  stop("Not enough documents for LDA.")
}

# Create the LDA model using webpage paragraphs
lda_model <- LDA(lda_dtm, k = n_topics,
                 control = list(seed = 1234))

# Display the top 10 words per topic
top_terms <- terms(lda_model, 10)
print(top_terms)

# Obtain topic-word probabilities
lda_prob <- posterior(lda_model)$terms

# Identify words shared by sentences and topics
shared <- intersect(colnames(sentence_counts),
                    colnames(lda_prob))

if (length(shared) == 0) {
  stop("No common words for topic scoring.")
}

# Calculate sentence scores for each topic
lda_scores <- sentence_counts[, shared, drop = FALSE] %*%
  t(lda_prob[, shared, drop = FALSE])

lda_scores <- lda_scores /
  pmax(rowSums(sentence_counts), 1)

# Select one representative sentence per topic
chosen <- integer(0)

for (i in 1:n_topics) {
  scores <- lda_scores[, i]
  scores[chosen] <- -Inf
  chosen <- c(chosen, which.max(scores))
}

# Select two additional relevant sentences
extra <- setdiff(
  order(apply(lda_scores, 1, max), decreasing = TRUE),
  chosen
)

chosen <- sort(c(chosen, head(extra, 5 - n_topics)))

# Display the LDA summary
cat("\nLDA TOPIC SENTENCE SUMMARY:\n")
cat(paste(sentences[chosen], collapse = " "), "\n")


# Visualisation 1
# TF-IDF Bar Graph

# Select the 10 highest weighted terms
word_scores <- head(
  sort(colSums(tfidf), decreasing = TRUE), 10
)

word_df <- data.frame(
  Word = names(word_scores),
  Score = as.numeric(word_scores)
)

# Create the graph
print(
  ggplot(word_df,
         aes(x = reorder(Word, Score), y = Score)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(
      title = "Top 10 TF-IDF Terms in NASA Climate Evidence Web Page",
      x = "Word",
      y = "Total TF-IDF Score"
    ) +
    theme_minimal()
)


# Visualisation 2
# LDA Topic-Word Distributions

# Create a table of words and probabilities per topic
lda_words_df <- do.call(rbind, lapply(1:n_topics, function(i) {
  data.frame(
    Topic = paste("Topic", i),
    Word = top_terms[, i],
    Probability = as.numeric(lda_prob[i, top_terms[, i]])
  )
}))

# Create the graph
print(
  ggplot(lda_words_df,
         aes(x = reorder(Word, Probability),
             y = Probability)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    facet_wrap(~Topic, scales = "free_x") +
    labs(
      title = "Top 10 Words per LDA Topic",
      x = "Word",
      y = "P(Word | Topic)"
    ) +
    theme_minimal()
)

# Display the topic-word probabilities
print(transform(
  lda_words_df,
  Probability = round(Probability, 4)
))