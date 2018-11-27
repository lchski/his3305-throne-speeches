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
  
## Weights by speech
weight_by_speech <- topic_probabilities_by_document %>%
  select(topic_id, topic_label, date, weight, governing_party) %>%
  filter(topic_id %in% presentation_topics)
  
## Average weights by decade, organized by party
avg_weight_by_decade_party <- topic_probabilities_by_document %>%
  filter(topic_id %in% presentation_topics) %>%
  group_by(topic_id, decade = as.Date(paste(substr(date, 1, 3), "0-01-01", sep="")), governing_party, topic_label) %>%
  summarize(avg_weight = mean(weight)) %>%
  ungroup() %>%
  mutate(topic_label = fct_reorder(topic_label, topic_id))
  
## Column plot with average party weight by decade, and a trendline from the unaggregated data
ggplot() +
  geom_col(data = avg_weight_by_decade_party, mapping = aes(x = decade, y = avg_weight, fill = governing_party), position = "dodge") +
  geom_smooth(data = weight_by_speech, mapping = aes(x = date, y = weight), method = "lm", se = 0, colour = "black", linetype = "dashed", size = 0.5) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1000)) +
  scale_fill_manual(values = c("conservative" = "blue", "liberal" = "red")) +
  scale_x_date(
    limits = as.Date(c("1950-01-01","2020-01-01")),
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
  facet_wrap(~ topic_label) +
  theme(
    strip.text.x = element_text(hjust = 0),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
  )



## Weight by speech (coloured by party) and a disaggregated trendline

### Function (for single plot)
plot_topic_weights <- function(topic_weights) {
  ggplot(topic_weights, mapping = aes(x = date, y = weight)) +
    geom_smooth(method = "loess", se = 0, colour = "black", linetype = "dashed", size = 0.5) +
    geom_bar(mapping = aes(fill = governing_party), stat = "identity", width = 50) +
    geom_point(data = topic_weights %>% filter(weight > 0), mapping = aes(colour = governing_party), size = 3) +
    scale_y_continuous(name = "Weight", expand = c(0, 0), limits = c(0, 1000)) +
    scale_fill_manual(values = c("conservative" = "blue", "liberal" = "red")) +
    scale_colour_manual(values = c("conservative" = "blue", "liberal" = "red")) +
    scale_x_date(
      name = "Date",
      limits = as.Date(c("1950-01-01","2020-01-01")),
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
      axis.text.y = element_text(size = 18, margin = margin(r = 10)),
      axis.text.x = element_text(size = 18, margin = margin(t = 10)),
      axis.title.y = element_text(size = 32, margin = margin(r = 10)),
      axis.title.x = element_text(size = 32, margin = margin(t = 20)),
      plot.background = element_rect(fill = "transparent",colour = NA)
    )
}

### Non-function (all the plots)
ggplot(weight_by_speech, mapping = aes(x = date, y = weight)) +
  geom_smooth(method = "loess", se = 0, colour = "black", linetype = "dashed", size = 0.5) +
  geom_bar(mapping = aes(fill = governing_party), stat = "identity", width = 25) +
  geom_point(data = weight_by_speech %>% filter(weight > 0), mapping = aes(colour = governing_party)) +
  scale_y_continuous(name = "Weight", expand = c(0, 0), limits = c(0, 1000)) +
  scale_fill_manual(values = c("conservative" = "blue", "liberal" = "red")) +
  scale_colour_manual(values = c("conservative" = "blue", "liberal" = "red")) +
  scale_x_date(
    name = "Date",
    limits = as.Date(c("1950-01-01","2020-01-01")),
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
  facet_wrap(~ topic_label) +
  theme(
    text = element_text(family = "Helvetica"),
    strip.text.x = element_text(hjust = 0),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    legend.position = 0
  )



# OUTPUTTING

for (presentation_topic in presentation_topics) {
  base_filename <- paste0("output/", presentation_topic, "-")
  
  ## ggsave...
  plot_topic_weights(weight_by_speech %>% filter(topic_id == presentation_topic)) +
    ggsave(
      paste0(base_filename, "topic-weights.png"),
      width = 14.08,
      height = 8.92,
      units = "in",
      bg = "transparent"
    )
  
  plot_top_words(top_words(m, 10), presentation_topic) +
    xlab(label = "Weight") +
    labs(title = NULL) +
    theme(
      text = element_text(family = "Helvetica"),
      axis.text.y = element_text(size = 32, margin = margin(r = 10)),
      axis.text.x = element_text(size = 18, margin = margin(t = 10)),
      axis.title.x = element_text(size = 32, margin = margin(t = 20)),
      plot.background = element_rect(fill = "transparent",colour = NA)
    ) +
    ggsave(
      paste0(base_filename, "top-words.png"),
      width = 7.04,
      height = 8.92,
      units = "in",
      bg = "transparent"
    )
}

plot_top_words(top_words(m, 10), 24) +
  xlab(label = "Weight") +
  labs(title = NULL) +
  theme(
    axis.text.y = element_text(size = 32, margin = margin(r = 10)),
    axis.text.x = element_text(size = 18),
    axis.title.x = element_text(size = 32, margin = margin(t = 20))
  )
