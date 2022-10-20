library(readr)
library(uwot)
library(textdata)
library(coop)
library(quanteda)

N_DIM <- 300
#setwd("Session-7-Embeddings/")
glove <- embedding_glove6b(dimensions=N_DIM, dir="embeddings")
word_matrix <- as.matrix(glove[,-1])
rownames(word_matrix) <- glove$token

df <- read_csv("../datasets/uk_manifestos.csv")
corp <- corpus(df)
dfmat <- corp %>% tokens(remove_punc=TRUE) %>%
  tokens_remove(pattern=stopwords("en")) %>%
  dfm() 

common_features <- intersect(colnames(dfmat),rownames(word_matrix))
glove_dfmat <- dfmat[,common_features]
corpus_word_matrix <- word_matrix[common_features,]
doc_matrix <- glove_dfmat %*% corpus_word_matrix

embeddings <- umap(as.matrix(doc_matrix), n_neighbors = 10, min_dist=0.01)
df$x <- embeddings[,1]
df$y <- embeddings[,2]

colordict <- c(
  "Labour"="red","LibDems"="yellow",
  "Conservatives"="Blue","Greens"="green")

library(plotly)
p <- ggplot(df, aes(x, y, colour=party, label=text)) + 
  geom_point(size=0.5) + 
  scale_colour_manual(values=colordict) + 
  theme_bw() +
  coord_fixed()

ggplotly(p)