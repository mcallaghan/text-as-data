library(tidytext)
library(dplyr)
library(quanteda)
library(vader)
library(tidyr)

texts <- c(
  "Elon Musk is a champion of free speech",
  "It's a terrible shame to see mashed potato thrown at art"
)


lex <- get_sentiments("afinn")
sample_n(lex, 5)

lex <- read_tsv(
  "https://raw.githubusercontent.com/fnielsen/afinn/master/afinn/data/AFINN-en-165.txt",
  col_names=c("word","value")
)

dfmat <- texts %>%
  tokens %>%
  dfm()

text_tokens <- tidy(dfmat) %>% 
  inner_join(lex, by=c("term" = "word"))

doc_sentiments <- tidy(dfmat) %>% 
  inner_join(lex, by=c("term" = "word")) %>%
  mutate(value=value*count) %>%
  group_by(document) %>%
  summarise(value = sum(value))

doc_sentiments


yes <-read_csv("../datasets/YesScotlandTweets_cleaned.csv")
yes$campaign <- "YesScotland"
no <- read_csv("../datasets/UkTogetherTweets_cleaned.csv")
no$campaign <- "UkTogether"

tweets <- rbind(yes, no)


sentiments <- vader_df(tweets$text)

tweet_sentiment <- cbind(tweets, select(sentiments,-text))

pos <- tweet_sentiment %>% arrange(desc(compound)) %>% 
  head()

for( i in rownames(pos) ) {
  print(pos[i, "text"])
  print(pos[i, "compound"])
}

neg <- tweet_sentiment %>% arrange(compound) %>% 
  head()

for( i in rownames(neg) ) {
  print(neg[i, "text"])
  print(neg[i, "compound"])
}


tweet_sentiment$date <- as.Date(tweet_sentiment$created) 



daily_sentiment <- tweet_sentiment %>% 
  group_by(campaign, date) %>%
  summarise(score = mean(compound)) %>%
  pivot_wider(names_from=campaign, values_from=score)

days <- data.frame(date=seq(
  as.Date("2014-06-01"),
  as.Date("2014-09-18"),
  1
))

daily_sentiment <- days %>% 
  left_join(daily_sentiment) %>%
  pivot_longer(cols=-date, names_to="campaign", values_to="score") %>%
  group_by(campaign) %>%
  arrange(date) %>%
  mutate(score7 = data.table::frollmean(score, 7))


library(ggplot2)
ggplot(daily_sentiment, aes(date, colour=campaign)) + 
  geom_point(aes(y=score)) + 
  geom_line(aes(y=score7))


