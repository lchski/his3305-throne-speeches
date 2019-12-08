library(tidytext)

source("lib/similarity.R")

speech_43 <- mallet.read.dir("data/source/new-speeches/")
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
) %>% pluck("similarity_scores")

### Compare!
speech_similarity %>%
  filter(id.x == 52 | id.y == 52) %>%
  mutate(id.x = as.integer(id.x)) %>%
  left_join(speech_meta, by = c("id.x" = "num_id")) %>%
  select(-id) %>%
  arrange(-value) %>%
  View()


## play with the tidytext package
tidy(m$model, matrix = "beta") %>% group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>% View()
