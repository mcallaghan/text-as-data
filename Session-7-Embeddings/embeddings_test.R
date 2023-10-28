pacman::p_load("quanteda","textdata","coop")

N_DIM <- 100
glove <- embedding_glove6b(dimensions=N_DIM, dir="embeddings")
word_matrix <- as.matrix(glove[,-1])
rownames(word_matrix) <- glove$token
words <- word_matrix[c("dog","cat","carbon"),]

words[,1:5]

words <- word_matrix[c("dog","cat","carbon"),]
sims <- cosine(t(words))
sims

vec_a <- word_matrix["cat",] 
sims <- apply(word_matrix, 1, function(x) cosine(x,vec_a))
sims %>% sort(decreasing=T) %>% head()

similar_words <- function(word, word_matrix) {
  vec_word <- word_matrix[word,]
  sims <- apply(word_matrix, 1, function(x) cosine(x,vec_word))
  return(sort(sims, decreasing=T))
}

similar_words("carbon", word_matrix) %>% head()



diff <- word_matrix["paris",]  - word_matrix["france",] 
vec_d <- word_matrix["berlin",] - diff
sims <- apply(word_matrix, 1, function(x) cosine(x,vec_d))
sims %>% sort(decreasing=T) %>% head()




docs <- c(
  "The acclaimed author penned novels based on her life",
  "Nobel prize-winning writer writes autobiographical fiction"
)
dfmat <- docs %>% tokens(remove_punc=TRUE) %>%
  tokens_remove(pattern=stopwords("en")) %>%
  dfm() 

print(cosine(as.vector(dfmat[1,]), as.vector(dfmat[2,])))

dfmat


common_features <- intersect(colnames(dfmat),rownames(word_matrix))
common_features <- c("author","novels","writer","writes")

glove_dfmat <- dfmat[,common_features]
print(glove_dfmat)
corpus_word_matrix <- round(word_matrix[common_features,1:5],1)
print(corpus_word_matrix)


doc_matrix <- glove_dfmat %*% corpus_word_matrix
doc_matrix



common_features <- intersect(colnames(dfmat),rownames(word_matrix))

glove_dfmat <- dfmat[,common_features]
corpus_word_matrix <- word_matrix[common_features,]
doc_matrix <- glove_dfmat %*% corpus_word_matrix
print(cosine(doc_matrix[1,], doc_matrix[2,]))

