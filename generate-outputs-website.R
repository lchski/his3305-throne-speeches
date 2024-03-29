library(tidyverse)
library(mallet)
library(dfrtopics)

# Load the model
m <- load_mallet_model_directory(
  f = "data/models/30-1000-18480634/",
  load_topic_words = T,
  load_sampling_state = T
)

# Load metadata
speech_meta <- read_csv("data/speech-meta.csv")
metadata(m) <- speech_meta

# Set base vars

## Topics
topic_labels <- tibble(label = topic_labels(m), label_longer = topic_labels(m, 20)) %>%
  mutate(
    id = row_number(),
    label = str_remove(label, pattern = "^[0-9]{1,2} "),
    label_longer = str_remove(label_longer, pattern = "^[0-9]{1,2} ")
  ) %>%
  select(id, label, label_longer)

## Topic probabilities per document
topic_probabilities_by_document_normalized <- top_docs(m, n = 30) %>%
  rename(doc_id = doc, topic_id = topic, weight = weight) %>%
  inner_join(metadata(m), by = c("doc_id" = "num_id")) %>%
  inner_join(topic_labels, by = c("topic_id" = "id")) %>%
  select(topic_id, topic_label = label, weight, parliament, session, date, governing_party)

topic_probabilities_by_document <- doc_topics(m) %>%
  gather_matrix() %>%
  rename(doc_id = row_key, topic_id = col_key, weight = value) %>%
  inner_join(metadata(m), by = c("doc_id" = "num_id")) %>%
  inner_join(topic_labels, by = c("topic_id" = "id")) %>%
  select(topic_id, topic_label = label, weight, parliament, session, date, governing_party)

## Weights by speech
weight_by_speech <- topic_probabilities_by_document %>%
  select(topic_id, topic_label, date, weight, governing_party)

## Top docs for a speech
top_docs(m, 6) %>%
  filter(topic == 17) %>%
  mutate(weight = round(weight, 5)) %>%
  inner_join(speech_meta, by = c("doc" = "num_id")) %>%
  select(topic, weight, speech_id = doc, parliament, session, date, governing_party)

# Plotting

## Weight by speech (coloured by party) and a disaggregated trendline

### Function (for single plot)
plot_topic_weights <- function(topic_weights) {
  ggplot(topic_weights, mapping = aes(x = date, y = weight)) +
    geom_smooth(method = "loess", se = 0, colour = "black", linetype = "dotted") +
    geom_bar(mapping = aes(fill = governing_party), stat = "identity", width = 100) +
    geom_point(data = topic_weights %>% filter(weight > 0), mapping = aes(colour = governing_party)) +
    geom_point(data = topic_weights %>% filter(weight == 0), mapping = aes(colour = governing_party), alpha = 0.25) +
    scale_y_continuous(name = "Weight", expand = c(0, 0), limits = c(0, 1)) +
    scale_fill_manual(values = c("conservative" = "blue", "liberal" = "red")) +
    scale_colour_manual(values = c("conservative" = "blue", "liberal" = "red")) +
    scale_x_date(
      name = "Date",
      limits = as.Date(c("1954-07-01","2016-07-01")),
      breaks = as.Date(c(
        "1950-01-01",
        "1960-01-01",
        "1970-01-01",
        "1980-01-01",
        "1990-01-01",
        "2000-01-01",
        "2010-01-01",
        "2020-01-01"
      )),
      minor_breaks = NULL,
      date_labels = "%Y"
    ) +
    theme(
      text = element_text(family = "Helvetica"),
      strip.text.x = element_text(hjust = 0),
      legend.position = 0,
      plot.background = element_rect(fill = "transparent", colour = NA)
    )
}

ggplot(weight_by_speech %>% filter(topic_id == 17), mapping = aes(x = date, y = weight)) +
  geom_smooth(method = "loess", se = 0, colour = "black", linetype = "dotted") +
  geom_bar(mapping = aes(fill = governing_party), stat = "identity", width = 100) +
  geom_point(data = weight_by_speech %>% filter(topic_id == 17 & weight > 0), mapping = aes(colour = governing_party)) +
  geom_point(data = weight_by_speech %>% filter(topic_id == 17 & weight == 0), mapping = aes(colour = governing_party), alpha = 0.25) +
  scale_y_continuous(name = "Weight", expand = c(0, 0), limits = c(0, 1600)) +
  scale_fill_manual(values = c("conservative" = "blue", "liberal" = "red")) +
  scale_colour_manual(values = c("conservative" = "blue", "liberal" = "red")) +
  scale_x_date(
    name = "Date",
    limits = as.Date(c("1954-07-01","2016-07-01")),
    breaks = as.Date(c(
      "1950-01-01",
      "1960-01-01",
      "1970-01-01",
      "1980-01-01",
      "1990-01-01",
      "2000-01-01",
      "2010-01-01",
      "2020-01-01"
    )),
    minor_breaks = NULL,
    date_labels = "%Y"
  ) +
  theme(
    text = element_text(family = "Helvetica"),
    strip.text.x = element_text(hjust = 0),
    legend.position = 0,
    plot.background = element_rect(fill = "transparent", colour = NA)
  )


# OUTPUTTING

topic_labels %>% write_csv("data/generated/website/topic.csv")


## Plots
for (topic_to_output in topic_labels$id) {
  base_filename <- paste0("output/", topic_to_output, "-")
  
  ## ggsave...
  plot_topic_weights(weight_by_speech %>% filter(topic_id == topic_to_output)) +
    ggsave(
      paste0(base_filename, "topic-weights.png"),
      device = "png",
      width = 7,
      height = 3,
      units = "in",
      bg = "transparent",
      dpi = 150
    )
  
  plot_top_words(top_words(m, 10), topic_to_output) +
    xlab(label = "Weight") +
    labs(title = NULL) +
    theme(
      text = element_text(family = "Helvetica"),
      plot.background = element_rect(fill = "transparent",colour = NA)
    ) +
    ggsave(
      paste0(base_filename, "top-words.png"),
      device = "png",
      width = 3,
      height = 3,
      units = "in",
      bg = "transparent",
      dpi = 150
    )
}

topic_probabilities_by_document %>%
  group_by(topic_id, decade = as.Date(paste(substr(date, 1, 3), "0-01-01", sep="")), governing_party, topic_label) %>%
  summarize(avg_weight = mean(weight)) %>%
  ungroup() %>%
  mutate(topic_label = fct_reorder(topic_label, topic_id)) %>%
  filter(topic_id == 17) %>%
  ggplot(mapping = aes(x = decade, y = avg_weight)) +
  geom_col(mapping = aes(fill = governing_party), position = "dodge") +
  geom_smooth(method = "lm", se = 0, colour = "black", linetype = "dashed", size = 0.5) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  scale_fill_manual(values = c("conservative" = "blue", "liberal" = "red")) +
  scale_x_date(
    limits = as.Date(c("1945-01-01","2020-01-01")),
    breaks = as.Date(c(
      "1950-01-01",
      "1960-01-01",
      "1970-01-01",
      "1980-01-01",
      "1990-01-01",
      "2000-01-01",
      "2010-01-01"
    )),
    minor_breaks = NULL,
    date_labels = "%Y"
  ) +
  theme(
    strip.text.x = element_text(hjust = 0),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
  )
