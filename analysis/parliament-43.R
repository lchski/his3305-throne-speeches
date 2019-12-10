library(tidytext)

source("lib/similarity.R")

speech_43 <- mallet.read.dir("data/source/new-speeches/") ## NB! Need to have all 51 speeches from original project plus the 43rd Parliament speech (speech 52, for our purposes)
speech_43 <- tibble(id = speech_43$id, text = speech_43$text)
s43_ilist <- mallet.import(as.character(speech_43$id), as.character(speech_43$text), stoplist.file = "stoplist.txt")

s43_inftop <- infer_topics(m, s43_ilist)

## See the top topics for the 52nd speech! (Parliament 43, session 1)
docs_top_topics(s43_inftop, n = 30) %>%
  as_tibble() %>%
  mutate(wt = round(weight, 4)) %>%
  left_join(topic_labels_short, by = c("topic" = "id")) %>%
  left_join(speech_meta, by = c("doc" = "num_id")) %>%
  select(-id) %>%
  filter(doc == 52) %>%
  View()

## (just for debugging, to compare doc rankings with the original model—seems to hold)
top_docs(s43_inftop, n =3) %>%
  as_tibble() %>%
  mutate(wt = round(weight, 4)) %>%
  left_join(speech_meta, by = c("doc" = "num_id")) %>%
  select(-id) %>%
  View()



## Determine similarity to other speeches!

### Calculate scores...
speech_similarity <- analyse_statement_similarity(
  statements = speech_43 %>%
    mutate(id = row_number()) %>%
    left_join(speech_meta, by = c("id" = "num_id")) %>%
    select(-id.y),
  similarity_threshold = 0.75
) %>% pluck("similarity_scores") %>%
  mutate(id.x = as.integer(id.x), id.y = as.integer(id.y))

### Compare!
get_scores_for_speech <- function(similarity_scores, speech_id) {
  similarity_scores %>%
    filter(id.x == speech_id | id.y == speech_id) %>%
    mutate(
      id.x = if_else(id.x == speech_id, NA_integer_, id.x),
      id.y = if_else(id.y == speech_id, NA_integer_, id.y)
    ) %>%
    mutate(
      base_id = speech_id,
      compared_id = coalesce(id.x, id.y)
    ) %>%
    select(base_id, compared_id, value) %>%
    left_join(speech_meta %>% select(-id), by = c("compared_id" = "num_id")) %>%
    arrange(-value)
}

speech_similarity %>%
  get_scores_for_speech(52)



## play with the tidytext package
tidy(m$model, matrix = "beta") %>% group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>% View()
