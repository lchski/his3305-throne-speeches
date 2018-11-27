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

# Pull presentation info

## Specify topics
presentation_topics <- c(24, 6, 17, 3)

topic_series(m) %>%
  filter(topic %in% presentation_topics) %>%
  inner_join(speech_meta, by = c("topic" = ))
  plot_series() +
  labs(title = "test") +
  scale_x_date(
    limits = as.Date(c("1955-01-01","2015-01-01")),
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
  )
  
weight_by_speech <- topic_probabilities_by_document %>%
  select(topic_id, topic_label, date, weight) %>%
  filter(topic_id %in% presentation_topics)
  
avg_weight_by_decade_party <- topic_probabilities_by_document %>%
  filter(topic_id %in% presentation_topics) %>%
  group_by(topic_id, decade = as.Date(paste(substr(date, 1, 3), "0-01-01", sep="")), governing_party, topic_label) %>%
  summarize(avg_weight = mean(weight)) %>%
  ungroup() %>%
  mutate(topic_label = fct_reorder(topic_label, topic_id))
  
ggplot() +
  geom_col(data = avg_weight_by_decade_party, mapping = aes(x = decade, y = avg_weight, fill = governing_party), position = "dodge") +
  geom_smooth(data = weight_by_speech, mapping = aes(x = date, y = weight), method = "loess", se = 0, colour = "black", linetype = "dashed", size = 0.5) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  scale_fill_manual(values = c("conservative" = "blue", "liberal" = "red")) +
  scale_x_date(
    limits = as.Date(c("1945-01-01","2015-01-01")),
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
  facet_wrap(~ topic_label) +
  theme(
    strip.text.x = element_text(hjust = 0),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
  )
