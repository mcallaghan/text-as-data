# NMF has some annoying dependencies, you can install them like this
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("Biobase")



library(NMF)
library(quanteda)
library(topicmodels)
library(tidytext)
library(LDAvis)
library(stm)

texts <- c(
  "Cats elephants camels",
  "Hawks doves pigeons",
  "Cats pigeons"
)
dfmat <- texts %>%
  tokens() %>%
  dfm()
dfmat

V <- as.matrix(dfmat)

res <- nmf(V, 2)
W <- res@fit@W
W

H <- res@fit@H
H

WH <- W %*% H

error <- V - WH
error


library(readr)
library(dplyr)
df <- read_csv("../datasets/hertie_papers.csv")
df <- df %>% filter(!is.na(abstract)) %>%
  distinct(abstract)
dfmat <- df$abstract %>%
  tokens(remove_punct = T) %>%
  tokens_remove(pattern=stopwords("en")) %>%
  dfm()  %>%
  dfm_trim(min_termfreq = 5)

dim(dfmat)


lda <- LDA(dfmat, 15)

W <- lda@gamma
H <- lda@beta

topic_words <- tidy(lda, matrix="beta") %>%
  group_by(topic) %>%
  slice_max(beta, n = 5) %>% 
  ungroup() %>%
  arrange(topic, -beta)

library(ggplot2)
topic_words %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()


topicmodels2LDAvis <- function(x, ...){
  post <- topicmodels::posterior(x)
  if (ncol(post[["topics"]]) < 3) stop("The model must contain > 2 topics")
  mat <- x@wordassignments
  json <- LDAvis::createJSON(
    phi = post[["terms"]], 
    theta = post[["topics"]],
    vocab = colnames(post[["terms"]]),
    doc.length = slam::row_sums(mat, na.rm = TRUE),
    term.frequency = slam::col_sums(mat, na.rm = TRUE)
  )
  return(json)
}

json <- topicmodels2LDAvis(lda)
serVis(json)
