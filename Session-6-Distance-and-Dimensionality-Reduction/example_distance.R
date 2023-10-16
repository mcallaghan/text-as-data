
pacman::p_load("quanteda","readr","lexicon","ggplot2","plotly","uwot","tidyr","quanteda.textstats")

df <- read_csv("Session-6-Distance-and-Dimensionality-Reduction/data/uk_manifestos.csv")
df$id <- as.character(seq(1,length(df$text)))

texts <- c(
  "apple orange pear", "apple pear quince",
  "apple pear orange quince peach avocado kiwi physalis"
)
dfmat <- texts %>% tokens() %>% dfm()
textstat_simil(dfmat, method="cosine")


corp <- corpus(df)
dfmat <- corp %>% tokens(remove_punc=TRUE) %>%
  tokens_remove(pattern=stopwords("en")) %>%
  dfm() %>%
  dfm_trim(min_termfreq=5) %>%
  dfm_tfidf()


embeddings <- umap(as.matrix(dfmat))
df$x <- embeddings[,1]
df$y <- embeddings[,2]

colordict <- c(
  "Labour"="red","LibDems"="yellow",
  "Conservatives"="Blue","Greens"="green")

p <- ggplot(df, aes(x, y, fill=party)) + 
  geom_point(color="grey", shape=21, size=0.5) + 
  scale_fill_manual(values=colordict) + 
  theme_bw() +
  coord_fixed()
p


p <- ggplot(df, aes(x, y, colour=party, label=text)) + 
  geom_point(size=0.5) + 
  scale_colour_manual(values=colordict) + 
  theme_bw() +
  coord_fixed()

ggplotly(p)