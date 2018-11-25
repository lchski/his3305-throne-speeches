library(tidyverse)
library(dfrtopics)
library(mallet)

# NB: first, load a model: `load("data/models/[...].RData")`

# Visualize

## Topic average weights by party
plot_avg_weights_by_party <- function(topic_probabilities_by_document) {
  topic_probabilities_by_document %>%
    group_by(topic_label, governing_party) %>%
    summarize(avg_weight = mean(weight)) %>%
    ggplot(mapping = aes(x = governing_party, y = avg_weight)) +
    geom_col(mapping = aes(fill = governing_party)) +
    scale_fill_manual(values = c("conservative" = "royalblue1", "liberal" = "tomato1")) +
    facet_wrap(~ topic_label) +
    theme(strip.text.x = element_text(hjust = 0), axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")
}

## Topic average weights by decade
plot_avg_weights_by_decade <- function(topic_probabilities_by_document) {
  topic_probabilities_by_document %>%
    group_by(topic_label, decade = substr(date, 1, 3)) %>%
    summarize(avg_weight = mean(weight)) %>%
    ggplot(mapping = aes(x = decade, y = avg_weight)) +
    geom_col() +
    facet_wrap(~ topic_label) +
    theme(strip.text.x = element_text(hjust = 0), axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")
}

## Topic average weights by decade by party
plot_avg_weights_by_decade_by_party <- function(topic_probabilities_by_document) {
  topic_probabilities_by_document %>%
    group_by(topic_id, decade = as.Date(paste(substr(date, 1, 3), "0-01-01", sep="")), governing_party, topic_label) %>%
    summarize(avg_weight = mean(weight)) %>%
    ungroup() %>%
    mutate(topic_label = fct_reorder(topic_label, topic_id)) %>%
    ggplot(mapping = aes(x = decade, y = avg_weight)) +
    geom_col(mapping = aes(fill = governing_party), position = "dodge") +
    geom_smooth(method = "lm", se = 0, colour = "black", linetype = "dashed", size = 0.5) +
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
}

render_avg_weights_by_decade_by_party <- plot_avg_weights_by_decade_by_party(topic_probabilities_by_document)

### [single graph to experiment with]
render_exp_graph <- topic_probabilities_by_document %>%
  group_by(topic_label, decade = as.Date(paste(substr(date, 1, 3), "0-01-01", sep="")), governing_party) %>%
  summarize(avg_weight = mean(weight)) %>%
  ungroup() %>%
  filter(substr(topic_label, 1, 2) == "1 ") %>%
  ggplot(mapping = aes(x = decade, y = avg_weight)) +
  geom_col(mapping = aes(fill = governing_party), position = "dodge") +
  geom_smooth(method = "lm", se = 0, colour = "black", linetype = "dashed", size = 0.5) +
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
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
  )


# Save summaries

plot_avg_weights_by_decade_by_party +
  ggsave(
    paste("data/models/summaries/", conf_num_topics, "-", conf_num_runs, "-1-avg-weights-decade-party.pdf", sep=""),
    width = 17,
    height = 11,
    units = "in"
  )

topic_labels %>% write_csv(paste("data/models/summaries/", conf_num_topics, "-", conf_num_runs, "-1-topic-labels.csv", sep=""))

