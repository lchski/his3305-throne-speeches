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
