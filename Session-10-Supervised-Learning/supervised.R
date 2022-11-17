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
df<- df[sample(nrow(df)),]
df$env <- 0
df$env[df$cmp_code==501] <- 1
df$env <- factor(df$env) 

df <- df %>% select(-cmp_code) 


library(textrecipes)
library(tidymodels)
library(parsnip)
library(rsample)
library(yardstick)

df_split <- initial_split(df, prop=0.8)
train_data <- training(df_split)
test_data <- testing(df_split)

rec <-recipe(env ~ text, data = train_data)

rec <- rec %>%
  step_tokenize(text) %>%
  step_tokenfilter(text, max_tokens = 1e3) %>%
  step_tfidf(text)

model <- svm_linear(mode="classification")

wf <- workflow() %>%
  add_recipe(rec) %>%
  add_model(model)

model_fit <- wf %>% 
  fit(train_data)

test_data$prediction <- predict(model_fit, test_data)$.pred_1

tp <- sum((test_data$env==1)&(test_data$prediction==1))
fn <- sum((test_data$env==1)&(test_data$prediction==0))
fp <- sum((test_data$env==0)&(test_data$prediction==1))
tn <- sum((test_data$env==0)&(test_data$prediction==0))
recall <- tp / (tp+fn)
precision <- tp / (tp+fp)

scorer <- metric_set(
  yardstick::accuracy, 
  yardstick::precision, 
  yardstick::recall,
  yardstick::f_meas
)


scorer(test_data, truth=env, estimate=prediction, event_level="second")


model <- svm_poly(cost=tune()) %>%
  set_engine("kernlab") %>%
  set_mode("classification")

wf <- workflow() %>%
  add_recipe(rec) %>%
  add_model(model)

folds <- vfold_cv(train_data, v = 2)
svm_res <- tune_grid(
  wf, resamples = folds, grid = 2,
  metrics = metric_set(f_meas),
  control = control_grid(event_level="second")
  )
collect_metrics(svm_res)

best_model <- svm_res %>% select_best()

final_workflow <- wf %>% 
  finalize_workflow(best_model)



test_data$opt_prediction <- predict(model_fit, test_data)$.pred_class
scorer(test_data, truth=env, estimate=opt_prediction)


n_splits <- 3



k <- 1
k_split <- folds$splits[[k]]
k_train <- training(k_split)
k_test <- testing(k_split)
k_model <- final_workflow %>% fit(k_train)
k_test$pred <- predict(k_model, k_test, type="prob")$.pred_1
t <- 0.5
est <- factor(ifelse(k_test$pred>=t,1,0))



f1 <- f_meas_vec(k_test$env, est, event_level = "second")



res <- data.frame(k=numeric(),t=numeric(),f1=numeric())
n_splits=3
folds <- vfold_cv(train_data, v = n_splits)
for (k in 1:n_splits) {
  k_split <- folds$splits[[k]]
  k_train <- training(k_split)
  k_test <- testing(k_split)
  k_model <- final_workflow %>% fit(k_train)
  k_test$pred <- predict(k_model, k_test, type="prob")$.pred_1
  for (t in seq(0.1, 0.9, length.out=50)) {
    est <- factor(ifelse(k_test$pred>=t,1,0))
    f1 <- f_meas_vec(k_test$env, est, event_level = "second")
    res <- add_row(res,k=k,t=t,f1=f1)
  }
}

res <- res %>% 
  group_by(t) %>% summarise(f1 = mean(f1)) %>%
  arrange(desc(f1))

optimal_t <- res$t[1]
