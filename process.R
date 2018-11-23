library(tidyverse)

# Load base data
index_throne <- read_csv("data/source/index-throne-speech.csv")
dates_parliaments <- read_csv("data/source/dates-parliaments.csv")
dates_sessions <- read_csv("data/source/dates-parliaments-sessions.csv")

# Remove unnecessary columns
index_throne <- index_throne %>%
  select(
    parliament = `Parliament`,
    session = `Session`,
    date = `Debates`
  ) %>%
  mutate(
    parliament = as.numeric(str_extract(parliament, "(\\d+)")),
    session = as.numeric(str_extract(session, "(\\d+)")),
    date = as.Date(substr(date, 1, 10))
  )

# Filter to relevant parliaments (23 to 41, 1957-2015)
index_throne <- index_throne %>%
  filter(parliament > 22) %>%
  filter(parliament < 42)
