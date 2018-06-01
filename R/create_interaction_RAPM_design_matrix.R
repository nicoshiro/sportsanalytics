library(readr)
library(dplyr)


######################################################################
# training data
######################################################################
# get stints_train and stints_test
stints_train <- read_csv('../data/stints_train_filtered.csv') %>%
  select(-game_id, -pt.differential, -minutes_played)
stints_test <- read_csv('../data/stints_test_filtered.csv') %>%
  select(-game_id, -pt.differential, -minutes_played)

# function for special addition of columns
`%+%`<-function(x,y){ifelse((x+y)/2 < 0, ceiling((x+y)/2), floor((x+y)/2))}

# apply addition function to all pairs of indicators
pairs <- combn(names(stints_train %>% select(-pt.diff.per.min)),2)

# create new matrix with interaction columns
stints_train = Matrix(as.matrix(stints_train), sparse=TRUE)
pb <- txtProgressBar(min=0, max=ncol(pairs), style=3)
for (col in 1:ncol(pairs)){
  # get pair of players
  pair = pairs[,col]
  col_name = paste(pair, collapse='+')
  
  # create interaction column
  new_col = stints_train[,pair[1]] %+% stints_train[,pair[2]]
  new_col_logical = as.logical(new_col)
  new_col = Matrix(new_col, nrow=length(new_col), ncol=1, sparse = TRUE, dimnames=list(NULL, col_name))
  
  # add new column to stints_train only if it has one or more nonzero entries
  if (any(new_col_logical)){
    stints_train = cBind(stints_train, new_col)
  }
  # show progress
  setTxtProgressBar(pb, col)
}
close(pb)

# write sparse matrix to file along with colnames separately
writeMM(stints_train, '../data/stints_train_interaction_sparse.txt')
write_lines(colnames(stints_train), path='../data/colnames_stints_train_interaction_sparse.txt')


######################################################################
# test data
######################################################################
# apply addition function to all pairs of indicators
pairs <- combn(names(stints_test %>% select(-pt.diff.per.min)),2)
stints_test = Matrix(as.matrix(stints_test), sparse=TRUE)
pb <- txtProgressBar(min=0, max=ncol(pairs), style=3)
for (col in 1:ncol(pairs)){
  pair = pairs[,col]
  col_name = paste(pair, collapse='+')
  new_col = stints_test[,pair[1]] %+% stints_test[,pair[2]]
  new_col_logical = as.logical(new_col)
  new_col = Matrix(new_col, nrow=length(new_col), ncol=1, sparse = TRUE, dimnames=list(NULL, col_name))
  if (any(new_col_logical)){
    stints_test = cBind(stints_test, new_col)
  }
  setTxtProgressBar(pb, col)
}
close(pb)

# write sparse matrix to file along with colnames separately
writeMM(stints_test, '../data/stints_test_interaction_sparse.txt')
write_lines(colnames(stints_test), path='../data/colnames_stints_test_interaction_sparse.txt')