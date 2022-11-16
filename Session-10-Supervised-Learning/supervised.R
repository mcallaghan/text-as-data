library(stats)
library(dplyr)

n <- 1000
t <- rlogis(n, location=qlogis(0.1))
noise <- rnorm(n,sd=1)
y <- plogis(t)
y_pred <- plogis(t+noise)

df <- data.frame(y=y,y_pred=y_pred) %>%
  arrange(y)

plot(df$y)
points(df$y_pred, col="red")
abline(v=which(df$y>0.5)[1])
abline(h=0.5)

dev.off()


library(ggplot2)
df <- data.frame(t=numeric(), p=numeric(), r=numeric())
for (thresh in seq(0,1,0.01)) {
  tp <- sum((y>=0.5)&(y_pred>=thresh))
  fn <- sum((y>=0.5)&(y_pred<=thresh))
  fp <- sum((y<=0.5)&(y_pred>=thresh))
  recall <- tp / (tp+fn)
  precision <- tp / (tp+fp)
  df <- df %>% add_row(t=thresh, p=precision, r=recall)
}
ggplot(df, aes(r, p, colour=t)) +
  geom_point()
ggsave("plots/precision-recall.png")



library(readr)
library(tidyr)
library(dplyr)
df <- read_csv("../datasets/uk_manifestos.csv")
df$climate <- 0
df$climate[df$cmp_code==501] <- 1
df$climate <- factor(df$climate)


library(textrecipes)
library(tidymodels)
library(caret)
library(parsnip)
library(rsample)
library(yardstick)

rec <-recipe(climate ~ text, data = df)

rec <- rec %>%
  step_tokenize(text) %>%
  step_tokenfilter(text, max_tokens = 1e3) %>%
  step_tfidf(text)

wf <- workflow() %>%
  add_recipe(rec)

model <- svm_linear(mode="classification")

df_split <- initial_split(df)
train_data <- training(df_split)
test_data <- testing(df_split)

model_fit <- wf %>%
  add_model(model) %>% 
  fit(train_data)

predict(model_fit, test_data)

