library(tidytext)

speech_43 <- mallet.read.dir("data/source/new-speeches/")
speech_43 <- tibble(id = speech_43$id, text = speech_43$text)
s43_ilist <- mallet.import(as.character(speech_43$id), as.character(speech_43$text), stoplist.file = "stoplist.txt")

s43_inftop <- infer_topics(m, s43_ilist)

docs_top_topics(s43_inftop, n = 30) %>%
  as_tibble() %>%
  filter(doc == 1) %>%
  mutate(wt = round(weight, 4)) %>%
  left_join(topic_labels_short, by = c("topic" = "id")) %>%
  View()


## play with the tidytext package
tidy(m$model, matrix = "beta") %>% group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>% View()
