library(tidyverse)
library(dfrtopics)
library(mallet)

# Clear out
rm(list = ls())

# LOCAL CONFIG
conf_num_topics <- 40
conf_num_runs <- 10000

# Load base index data

index_throne <- read_csv("data/source/index-throne-speech.csv")
parliaments_by_governing_party <- read_csv("data/generated/parliaments-governing-parties.csv")

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
    date = as.Date(substr(date, 1, 10)),
    pubdate = date
  ) %>%
  inner_join(parliaments_by_governing_party)

## Filter to relevant parliaments (22 to 42, 1953-current) and sort
index_throne <- index_throne %>%
  filter(parliament > 21) %>%
  arrange(date) %>%
  mutate(
    num_id = row_number()
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
topic.model <- MalletLDA(num.topics = conf_num_topics)

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
topic.model$train(conf_num_runs)

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


# Create analysis variables

## Print out the topics
topic_labels <- tibble(label = topic_labels(m)) %>% mutate(id = row_number())

## Get the top 10 speeches corresponding to each topic
dd <- top_docs(m, n=10) %>%
  inner_join(metadata(m), by = c("doc" = "num_id")) %>%
  select(topic, weight, parliament, session, date, governing_party) %>%
  inner_join(topic_labels, by = c("topic" = "id"))

### Coefficients of variance for the topics across the ten speeches
### (higher values will be flatter lines when plotted)
dd_cov <- dd %>%
  select(topic, weight) %>%
  group_by(topic) %>%
  summarize(variance = sd(weight) / mean(weight))

## Topic probabilities per document
topic_probabilities_by_document <- doc_topics(m) %>%
  gather_matrix() %>%
  rename(doc_id = row_key, topic_id = col_key, weight = value) %>%
  inner_join(metadata(m), by = c("doc_id" = "num_id")) %>%
  inner_join(topic_labels, by = c("topic_id" = "id")) %>%
  select(topic_id, topic_label = label, weight, parliament, session, date, governing_party)

### Coefficients of variance again
topic_probabilities_by_document_cov <- topic_probabilities_by_document %>%
  group_by(topic_id) %>%
  summarize(cov = sd(weight) / mean(weight))

## Optional: save model; rename with num topics, num iterations, index once saved
save.image(paste("data/models/", conf_num_topics, "-", conf_num_runs, "-1.RData", sep=""))
