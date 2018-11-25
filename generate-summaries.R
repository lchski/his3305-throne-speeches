library(tidyverse)
library(dfrtopics)
library(mallet)

# Bring in visualization functions
source("visualize.R")

# Create list of paths to the saved models
models <- tibble(file = dir(path = "data/models/", full.names = TRUE)) %>%
  filter(str_detect(file, ".RData")) %>%
  mutate(file = gsub("//", "/", file))

# Generate a summary of each model
by(models, 1:nrow(models), function(model_path) {
  # Load the model data into the global environment [wooo global variables, heh]
  load(as.character(model_path), envir = globalenv())
  
  # Create a base filename for each summary file
  base_filename <- paste0("data/models/summaries/", conf_num_topics, "-", conf_num_runs, "-1-")
  
  # Save the topic labels
  topic_labels %>% write_csv(paste0(base_filename, "topic-labels.csv"))
    
  # Save the plot of average weights by decade by party
  plot_avg_weights_by_decade_by_party(topic_probabilities_by_document) +
    ggsave(
      paste0(base_filename, "avg-weights-decade-party.pdf", sep=""),
      width = 17,
      height = 11,
      units = "in"
    )
})
