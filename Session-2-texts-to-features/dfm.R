#install.packages(c("quanteda","ggplot2","reshape2","tidytext"))

library(quanteda)

# First we define a list of texts

texts <- c(
  "System change not climate change",
  "The Current State of the Climate",
  "Mitigation pathways compatible with long-term goals",
  "Climate Change 2022: Impacts, Adaptation and Vulnerability"
)

# Now we tokenize the texts
toks <- tokens(texts)

# Now we pass the tokens to the dfm function
dfmat <- dfm(toks)

# We can do this all in one go with the pipe operator
dfmat <- texts %>%
  tokens() %>%
  dfm()


#################################
## We can plot a heatmap of the resulting matrix using ggplot2

library(ggplot2)
library(reshape2)
library(tidytext)

# To pass the data to ggplot, we need this in tidy format
# The tidy function from tidytext removes zeros, but in this case
# we want to show these! So we simply melt the matrix representation of our dfm
# (this is all tidytext is doing anyway...)
tidy_dfm <- tidy(dfmat)
tidy_dfm <- melt(as.matrix(dfmat))

# tidy_dfm is a "long" representation of our dfm, with each row representing a
# document-feature combination, and it's count as "value"

ggplot(tidy_dfm, aes(features, docs)) + # so we pass features and docs to x and y
  geom_tile(aes(fill=value), color="black") + # and fill with value
  geom_text(aes(label=value)) + # and write the values in as texts
  coord_fixed() + # we fix the aspect ratio so we have squares not rectangles
  scale_fill_distiller(direction=1, palette="Greys", limits=c(0,4)) + # define a colour scale
  scale_x_discrete(position="top") + # put the x labels on the top
  theme(axis.text.x = element_text(angle=60, vjust=0.5, hjust=0)) # and adjust their angles


texts <- c(
  "Poverty and inequality implications of carbon pricing",
  "Optimizing and Comparing Topic Models is Simple",
  "How to stop cities and companies causing planetary harm",
  "Contextualized Document Embeddings Improve Topic Coherence",
  "Optimal carbon taxation and horizontal equity"
)
dfmat <- texts %>%
  tokens() %>%
  tokens_wordstem() %>% 
  dfm() %>%
  dfm_tfidf()
pred <- (-1 + dfmat[,"document"]*0.5 + dfmat[,"topic"]*3)@x

pred

