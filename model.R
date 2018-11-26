library(tidyverse)
library(dfrtopics)
library(mallet)

# Clear out
rm(list = ls())

source("visualize.R")

# LOCAL CONFIG
conf_index <- floor(runif(1, min=0, max=32000000)) # also the seed
conf_num_topics <- 30
conf_num_runs <- 1000

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
instance_list <- mallet.import(speeches$id, speeches$text, stoplist = "stoplist.txt")

## Train the model
m <- train_model(instance_list, n_topics=conf_num_topics,
                 n_iters=conf_num_runs,
                 seed=conf_index,
                 metadata=speech_meta,
                 n_hyper_iters=20,
                 n_burn_in = 50)

# Create analysis variables

## Print out the topics
topic_labels <- tibble(label = topic_labels(m)) %>%
  mutate(id = row_number()) %>%
  select(id, label)

topic_labels_short <- tibble(label = topic_labels(m, n = 16)) %>%
  mutate(id = row_number()) %>%
  select(id, label)

topic_labels_medium <- tibble(label = topic_labels(m, n = 20)) %>%
  mutate(id = row_number()) %>%
  select(id, label)

topic_labels_long <- tibble(label = topic_labels(m, n = 100)) %>%
  mutate(id = row_number()) %>%
  select(id, label)

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


# SAVE ALL THE THINGS

## Save model
model_filename <- paste0("data/models/", conf_num_topics, "-", conf_num_runs, "-", conf_index)
save.image(paste0(model_filename, ".RData"))
write_mallet_model(m, model_filename)

# Create a base filename for each summary file
base_filename <- paste0("data/models/summaries/", conf_num_topics, "-", conf_num_runs, "-", conf_index, "-")

# Save the topic labels
topic_labels %>% write_csv(paste0(base_filename, "topic-labels-8.csv"))

topic_labels_short %>% write_csv(paste0(base_filename, "topic-labels-16.csv"))

# Save the medium topic labels
topic_labels_medium %>% write_csv(paste0(base_filename, "topic-labels-20.csv"))

# Save the long topic labels
topic_labels_long %>% write_csv(paste0(base_filename, "topic-labels-100.csv"))

# Save the plot of average weights by decade by party
plot_avg_weights_by_decade_by_party(topic_probabilities_by_document) +
  ggsave(
    paste0(base_filename, "avg-weights-decade-party.pdf", sep=""),
    width = 17,
    height = 11,
    units = "in"
  )
