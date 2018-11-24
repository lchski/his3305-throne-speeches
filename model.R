library(tidyverse)
library(dfrtopics)
library(mallet)

# Load base index data

index_throne <- read_csv("data/source/index-throne-speech.csv")

## Remove unnecessary columns
index_throne <- index_throne %>%
  select(
    parliament = `Parliament`,
    session = `Session`,
    date = `Debates`
  ) %>%
  mutate(
    parliament = as.numeric(str_extract(parliament, "(\\d+)")),
    session = as.numeric(str_extract(session, "(\\d+)")),
    date = as.Date(substr(date, 1, 10))
  )

## Metadata setup
speech_meta <- index_throne %>%
  mutate(
    id = paste("data/generated/speeches-throne//", parliament, "-", session, ".txt", sep = "")
  )



# Topic modelling

## Load the speeches
speeches <- mallet.read.dir("data/generated/speeches-throne/")
speeches <- tibble(id = speeches$id, text = speeches$text)

## Bring in the speeches
mallet.instances <- mallet.import(speeches$id, speeches$text, stoplist = "stoplist.txt")

## Set the number of topics
topic.model <- MalletLDA(num.topics = 10)

## Load the speeches into the model
topic.model$loadDocuments(mallet.instances)

## Get the vocabulary, and some statistics about word frequencies.
##  These may be useful in further curating the stopword list.
vocabulary <- topic.model$getVocabulary()
word.freqs <- mallet.word.freqs(topic.model)

## Optimize hyperparameters every 20 iterations,
##  after 50 burn-in iterations.
topic.model$setAlphaOptimization(20, 50)

## Train the model
topic.model$train(1000)

## NEW: run through a few iterations where we pick the best topic for each token,
##  rather than sampling from the posterior distribution.
topic.model$maximize(20)

## Get the probability of topics in documents and the probability of words in topics.
## By default, these functions return raw word counts. Here we want probabilities,
##  so we normalize, and add "smoothing" so that nothing has exactly 0 probability.
doc.topics <- mallet.doc.topics(topic.model, smoothed=T, normalized=T)
topic.words <- mallet.topic.words(topic.model, smoothed=T, normalized=T)

## Create a model for dfrtopics
m <- mallet_model(doc_topics = doc.topics, doc_ids = speeches$id, vocab = vocabulary, topic_words = topic.words, model = topic.model)

## Load metadata into model
metadata(m) <- speech_meta


# Analysis

## Print out the topics
topic_labels(m)

dd <- top_docs(m, n=3)