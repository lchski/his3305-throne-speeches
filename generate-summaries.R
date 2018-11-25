source(visualize.R)

plot_avg_weights_by_decade_by_party +
  ggsave(
    paste("data/models/summaries/", conf_num_topics, "-", conf_num_runs, "-1-avg-weights-decade-party.pdf", sep=""),
    width = 17,
    height = 11,
    units = "in"
  )

topic_labels %>% write_csv(paste("data/models/summaries/", conf_num_topics, "-", conf_num_runs, "-1-topic-labels.csv", sep=""))

