library(readr)
library(dplyr)
library(glmnet)
library(ModelMetrics)


######################################################################
# train
######################################################################
# load data
stints_train = readMM('../data/stints_train_interaction_sparse.txt')
names = read_lines('../data/colnames_stints_train_interaction_sparse.txt')
colnames(stints_train) = names

# fit ridge regression model
interaction_RAPM_mod = cv.glmnet(stints_train[,-1],stints_train[,1], alpha=0)

# look at CV results
plot(interaction_RAPM_mod)

# dummy classifier training results
training_mean = mean(stints_train[,1])
dummy_predictions = rep(training_mean, length(stints_train[,1]))
(dummy_rmse = rmse(stints_train[,1], dummy_predictions))


######################################################################
# test
######################################################################
# load data
stints_test = readMM('../data/stints_test_interaction_sparse.txt')
names = read_lines('../data/colnames_stints_test_interaction_sparse.txt')
colnames(stints_test) = names


# give stints_test the same columns as stints_train with all zeros for cols only in stints_train
cols_to_add = setdiff(colnames(stints_train), colnames(stints_test))
for (col_name in cols_to_add){
  new_col = Matrix(0, nrow=nrow(stints_test), ncol=1, sparse = TRUE, dimnames=list(NULL, col_name))
  stints_test = cBind(stints_test, new_col)
}

# remove cols that are only in stints_test
cols_to_remove = setdiff(colnames(stints_test), colnames(stints_train))
cols_to_remove_logical = !(colnames(stints_test) %in% cols_to_remove)
stints_test = stints_test[, cols_to_remove_logical]

# predict 
y_pred = predict(interaction_RAPM_mod, newx = stints_test[,-1], s='lambda.min')
y_actual = stints_test[,1]

# compute RMSE
rmse(y_actual, y_pred)
# 2.505492

# compare to baseline mean prediction
y_pred_baseline = rep(mean(stints_train[,1]), length(y_actual))
rmse(y_actual, y_pred_baseline)
# worse than the baseline mean prediction

# plot results
plot(y_actual, y_actual, type='l', main='Interaction RAPM Predictions')
points(y_actual, y_pred)
text(5, -5, labels = paste0('RMSE=',rmse(y_actual, y_pred)))

# compute R^2
(r2 <- interaction_RAPM_mod$glmnet.fit$dev.ratio[which(
  interaction_RAPM_mod$glmnet.fit$lambda == interaction_RAPM_mod$lambda.min)])


