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


# OUTPUTTING

topic_labels <- tibble(label = topic_labels(m), label_longer = topic_labels(m, 20)) %>%
  mutate(
    id = row_number(),
    label = str_remove(label, pattern = "^[0-9]{1,2} "),
    label_longer = str_remove(label_longer, pattern = "^[0-9]{1,2} ")
  ) %>%
  select(id, label, label_longer) %>%
  write_csv("data/generated/website/topic-labels.csv")


for (presentation_topic in presentation_topics) {
  base_filename <- paste0("output/", presentation_topic, "-")
  
  ## ggsave...
  plot_topic_weights(weight_by_speech %>% filter(topic_id == presentation_topic)) +
    ggsave(
      paste0(base_filename, "topic-weights.png"),
      width = 28.16,
      height = 17.84,
      units = "in",
      bg = "transparent"
    )
  
  plot_top_words(top_words(m, 10), presentation_topic) +
    xlab(label = "Weight") +
    labs(title = NULL) +
    theme(
      text = element_text(family = "Helvetica"),
      axis.text.y = element_text(size = 64, margin = margin(r = 20)),
      axis.text.x = element_text(size = 36, margin = margin(t = 20)),
      axis.title.x = element_text(size = 64, margin = margin(t = 40)),
      plot.background = element_rect(fill = "transparent",colour = NA)
    ) +
    ggsave(
      paste0(base_filename, "top-words.png"),
      width = 14.08,
      height = 17.84,
      units = "in",
      bg = "transparent"
    )
}
