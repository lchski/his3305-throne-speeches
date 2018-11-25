library(tidyverse)
library(dfrtopics)
library(mallet)

# NB: first, load a model: `load("data/models/[...].RData")`

# Visualize

## Plot the topics over time
topic_series(m) %>%
  plot_series(labels=topic_labels(m, 2))

## Topic average weights by party
topic_probabilities_by_document %>%
  group_by(topic_label, governing_party) %>%
  summarize(avg_weight = mean(weight)) %>%
  ggplot(mapping = aes(x = governing_party, y = avg_weight)) +
  geom_col(mapping = aes(fill = governing_party)) +
  scale_fill_manual(values = c("conservative" = "royalblue1", "liberal" = "tomato1")) +
  facet_wrap(~ topic_label) +
  theme(strip.text.x = element_text(hjust = 0), axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")

## Topic average weights by decade
topic_probabilities_by_document %>%
  group_by(topic_label, decade = substr(date, 1, 3)) %>%
  summarize(avg_weight = mean(weight)) %>%
  ggplot(mapping = aes(x = decade, y = avg_weight)) +
  geom_col() +
  facet_wrap(~ topic_label) +
  theme(strip.text.x = element_text(hjust = 0), axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")

## Topic average weights by decade by party
topic_probabilities_by_document %>%
  group_by(topic_label, decade = as.Date(paste(substr(date, 1, 3), "0-01-01", sep="")), governing_party) %>%
  summarize(avg_weight = mean(weight)) %>%
  ungroup() %>%
  ggplot(mapping = aes(x = decade, y = avg_weight)) +
  geom_col(mapping = aes(fill = governing_party), position = "dodge") +
  geom_smooth(method = "lm", se = 0, colour = "black", linetype = "dashed", size = 0.5) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  scale_fill_manual(values = c("conservative" = "blue", "liberal" = "red")) +
  facet_wrap(~ topic_label) +
  theme(
    strip.text.x = element_text(hjust = 0),
    axis.text.x = element_text(angle = 90, hjust = 1)
  ) +
  ggsave(
    paste("data/models/summaries/", conf_num_topics, "-", conf_num_runs, "-1.pdf", sep=""),
    width = 11,
    height = 8.5,
    units = "in"
  )
