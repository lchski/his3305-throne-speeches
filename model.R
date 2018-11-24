library(mallet)

speeches <- mallet.read.dir("data/generated/speeches-throne/")

mallet.instances <- mallet.import(speeches$id, speeches$text, stoplist = "stoplist.txt")

topic.model <- MalletLDA(num.topics = 10)

topic.model$loadDocuments(mallet.instances)

## Get the vocabulary, and some statistics about word frequencies.
##  These may be useful in further curating the stopword list.
vocabulary <- topic.model$getVocabulary()
word.freqs <- mallet.word.freqs(topic.model)

## Optimize hyperparameters every 20 iterations,
##  after 50 burn-in iterations.
topic.model$setAlphaOptimization(20, 50)

topic.model$train(1000)

## NEW: run through a few iterations where we pick the best topic for each token,
##  rather than sampling from the posterior distribution.
topic.model$maximize(20)

## Get the probability of topics in documents and the probability of words in topics.
## By default, these functions return raw word counts. Here we want probabilities,
##  so we normalize, and add "smoothing" so that nothing has exactly 0 probability.
doc.topics <- mallet.doc.topics(topic.model, smoothed=T, normalized=T)
topic.words <- mallet.topic.words(topic.model, smoothed=T, normalized=T)

mallet.topic.labels(topic.model, topic.words, 10)

# library(dfrtopics)

# m <- mallet_model(doc_topics = doc.topics, doc_ids = speeches$id, vocab = vocabulary, topic_words = topic.words, model = topic.model)
