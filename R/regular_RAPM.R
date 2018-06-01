library(readr)
library(dplyr)
library(caret)
library(ModelMetrics)

######################################################################
# train
######################################################################
# get stints_train and stints_test and remove unwanted features
stints_train <- read_csv('../data/stints_train_filtered.csv') %>%
  select(-game_id, -pt.differential, -minutes_played)
stints_test <- read_csv('../data/stints_test_filtered.csv') %>%
  select(-game_id, -pt.differential, -minutes_played)

######################################################################
# model fitting
######################################################################

### compute regular RAPM over the season
print('Fitting regular non-interaction RAPM model')

regular_RAPM_mod <- train(
  pt.diff.per.min ~ .,
  intercept=FALSE,
  data=stints_train,
  method='glmnet',
  trControl=trainControl(method='repeatedcv', repeats=1, verbose=TRUE),
  tuneGrid=expand.grid(alpha=0, lambda=seq(0,20,2)),
  metric='RMSE'
)

######################################################################
# training CV results
######################################################################
# plot RMSE versus lambda from cross validation
plot(regular_RAPM_mod)
# performance summary
regular_RAPM_mod$results

# 10-fold CV dummy prediction
dummy_rmses = c()
folds = createFolds(stints_train$pt.diff.per.min, k=10)
for (i in 1:length(folds)){
  X_train = stints_train$pt.diff.per.min[folds[-i] %>% unlist]
  X_test = stints_train$pt.diff.per.min[folds[i] %>% unlist]
  training_mean = mean(X_train)
  dummy_predictions = rep(training_mean, length(X_test))
  dummy_rmses[i] = rmse(X_test, dummy_predictions)
}
(dummy_CV_RMSE = mean(dummy_rmses))

# prediction plot

y_actual = stints_train$pt.diff.per.min
y_pred = fitted(regular_RAPM_mod)
data = data.frame(y_actual, y_pred)

colors = c("True Values"="red","Predicted Values"="#000000")
ggplot(data) +
  geom_line(aes(x=y_actual, y=y_actual, color='True Values')) +
  geom_point(aes(x=y_actual, y=y_pred, color='Predicted Values'), alpha=0.2) +
  labs(x='True Plus-Minus',
       y='Predicted Plus-Minus') +#,
       #title='RAPM True vs. Predicted Plus-Minus -- Training Data') +
  scale_colour_manual(name="",
                      values=colors,
                      guide=guide_legend(override.aes=list(linetype=c("blank", "solid"),
                                                           shape=c(16, NA))))

######################################################################
# test
######################################################################
# load test data
X_test <- stints_test %>% select(-pt.diff.per.min)
y_test <- stints_test %>% select(pt.diff.per.min)

# prediction plot
y_actual = y_test %>% unlist
y_pred = predict(regular_RAPM_mod, newdata = X_test)
data = data.frame(y_actual, y_pred)

colors = c("True Values"="red","Predicted Values"="#000000")
ggplot(data) +
  geom_line(aes(x=y_actual, y=y_actual, color='True Values')) +
  geom_point(aes(x=y_actual, y=y_pred, color='Predicted Values'), alpha=0.2) +
  labs(x='True Plus-Minus',
       y='Predicted Plus-Minus',
       title='RAPM True vs. Predicted Plus-Minus -- Test Data') +
  scale_colour_manual(name="",
                      values=colors,
                      guide=guide_legend(override.aes=list(linetype=c("blank", "solid"),
                                                           shape=c(16, NA))))

# predict on test data (bootstrap to get confidence intervals)
predict_95_CI_RMSE <- function(glmnet_model, X, y){
  ### X: test data
  ### y: test data response

  # get overall score
  y_test <- y %>% unlist()
  y_pred <- predict(glmnet_model, newdata=X_test) %>% unlist()
  score <- rmse(actual=y_test, predicted=y_pred)

  # get 95% CI for score via bootstrapping
  num_samples <- 100
  n <- nrow(X)
  scores <- numeric(num_samples)
  set.seed(123)
  for (t in 1:num_samples){
    sample_indices <- sample(1:n, n, replace=TRUE)
    X_test <- X[sample_indices,]
    y_test <- y[sample_indices,] %>% unlist()
    y_pred <- predict(glmnet_model, newdata=X_test) %>% unlist()
    scores[t] <- rmse(actual=y_test, predicted=y_pred)
  }
  return(c(score=score, quantile(scores, c(0.025, 0.975))))
}
predict_95_CI_RMSE(regular_RAPM_mod, X_test, y_test)
