rm(list=ls(all=TRUE))
library(policytree)
data <- read.csv("letter_binvars_pred_comb.csv")

controls <- c('sex', 'ppethm1', 'ppethm2', 'ppethm3', 'ppethm4', 'ppeducat1', 'ppeducat2',
              'ppeducat3', 'ppincimp1', 'ppincimp2', 'ppincimp3', 'ppincimp4', 
              'ppincimp5', 'ppincimp6', 'ppincimp7', 'ppincimp8', 'ppincimp9',
              'ppincimp10', 'ppincimp11', 'ppincimp12', 'ppincimp13', 'ppincimp14',
              'ppincimp15', 'ppincimp16', 'ppincimp17', 'ppincimp18', 'ppincimp19',
              'ppincimp20', 'ppwork1', 'ppwork2', 'ppwork3', 'ppwork4', 'ppwork5',
              'ppwork6', 'xparty71', 'xparty72', 'xparty73', 'xparty74', 'xparty75',
              'xparty76')

X <- data[controls]
W <- data$method
Y_writing_bin <- -data$mse_writing_bin_opt
Y_writing_abortion_bin <- -data$mse_writing_abortion_bin_opt
Y_writing_minW_bin <- -data$mse_writing_minW_bin_opt

for (i in 1:100) {
  multi.forest <- multi_causal_forest(X = X, Y = Y_writing_bin, W = W)
  head(predict(multi.forest)$predictions)
  Gamma.matrix <- double_robust_scores(multi.forest)
  head(Gamma.matrix)
  train <- sample(1:4240, 3000)
  opt.tree <- policy_tree(X[train, ], Gamma.matrix[train, ], depth = 2)
  opt.tree
  head(predict(opt.tree, X))
  col <- paste("writing_bin_predict", i, sep = "")
  data[col]=predict(opt.tree, X)
  
  multi.forest <- multi_causal_forest(X = X, Y = Y_writing_abortion_bin, W = W)
  head(predict(multi.forest)$predictions)
  Gamma.matrix <- double_robust_scores(multi.forest)
  head(Gamma.matrix)
  train <- sample(1:4240, 3000)
  opt.tree <- policy_tree(X[train, ], Gamma.matrix[train, ], depth = 2)
  opt.tree
  head(predict(opt.tree, X))
  col <- paste("writing_abortion_bin_predict", i, sep = "")
  data[col]=predict(opt.tree, X)
  
  multi.forest <- multi_causal_forest(X = X, Y = Y_writing_minW_bin, W = W)
  head(predict(multi.forest)$predictions)
  Gamma.matrix <- double_robust_scores(multi.forest)
  head(Gamma.matrix)
  train <- sample(1:4240, 3000)
  opt.tree <- policy_tree(X[train, ], Gamma.matrix[train, ], depth = 2)
  opt.tree
  head(predict(opt.tree, X))
  col <- paste("writing_minW_bin_predict", i, sep = "")
  data[col]=predict(opt.tree, X)
  
  
  }
write.csv(data,'policytree_letter_binvars-output.csv')

rm(list=ls(all=TRUE))
library(policytree)
data <- read.csv("letter_contvars_pred_comb.csv")

controls <- c('sex', 'ppethm1', 'ppethm2', 'ppethm3', 'ppethm4', 'ppeducat1', 'ppeducat2',
              'ppeducat3', 'ppincimp1', 'ppincimp2', 'ppincimp3', 'ppincimp4', 
              'ppincimp5', 'ppincimp6', 'ppincimp7', 'ppincimp8', 'ppincimp9',
              'ppincimp10', 'ppincimp11', 'ppincimp12', 'ppincimp13', 'ppincimp14',
              'ppincimp15', 'ppincimp16', 'ppincimp17', 'ppincimp18', 'ppincimp19',
              'ppincimp20', 'ppwork1', 'ppwork2', 'ppwork3', 'ppwork4', 'ppwork5',
              'ppwork6', 'xparty71', 'xparty72', 'xparty73', 'xparty74', 'xparty75',
              'xparty76')

X <- data[controls]
W <- data$method
Y_writing <- -data$mse_writing_opt
Y_writing_abortion <- -data$mse_writing_abortion_opt
Y_writing_minW <- -data$mse_writing_minW_opt

for (i in 1:100) {
  multi.forest <- multi_causal_forest(X = X, Y = Y_writing, W = W)
  head(predict(multi.forest)$predictions)
  Gamma.matrix <- double_robust_scores(multi.forest)
  head(Gamma.matrix)
  train <- sample(1:4240, 3000)
  opt.tree <- policy_tree(X[train, ], Gamma.matrix[train, ], depth = 2)
  opt.tree
  head(predict(opt.tree, X))
  col <- paste("writing_predict", i, sep = "")
  data[col]=predict(opt.tree, X)
  
  multi.forest <- multi_causal_forest(X = X, Y = Y_writing_abortion, W = W)
  head(predict(multi.forest)$predictions)
  Gamma.matrix <- double_robust_scores(multi.forest)
  head(Gamma.matrix)
  train <- sample(1:4240, 3000)
  opt.tree <- policy_tree(X[train, ], Gamma.matrix[train, ], depth = 2)
  opt.tree
  head(predict(opt.tree, X))
  col <- paste("writing_abortion_predict", i, sep = "")
  data[col]=predict(opt.tree, X)
  
  
  multi.forest <- multi_causal_forest(X = X, Y = Y_writing_minW, W = W)
  head(predict(multi.forest)$predictions)
  Gamma.matrix <- double_robust_scores(multi.forest)
  head(Gamma.matrix)
  train <- sample(1:4240, 3000)
  opt.tree <- policy_tree(X[train, ], Gamma.matrix[train, ], depth = 2)
  opt.tree
  head(predict(opt.tree, X))
  col <- paste("writing_minW_predict", i, sep = "")
  data[col]=predict(opt.tree, X)
  
  
}
write.csv(data,'policytree_letter_contvars-output.csv')


