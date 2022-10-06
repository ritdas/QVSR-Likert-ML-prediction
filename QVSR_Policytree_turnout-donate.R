rm(list=ls(all=TRUE))
library(policytree)
data <- read.csv("turnout-gun-wall_binvars_pred_comb.csv")

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
Y_voted <- -data$mse_voted_yes_opt
Y_gun <- -data$mse_donate_gun_bin_opt
Y_wall <- -data$mse_donate_wall_bin_opt
Y_donate <- -data$mse_donate_bin_opt

for (i in 1:100) {
  multi.forest <- multi_causal_forest(X = X, Y = Y_voted, W = W)
  head(predict(multi.forest)$predictions)
  Gamma.matrix <- double_robust_scores(multi.forest)
  head(Gamma.matrix)
  train <- sample(1:4096, 3000)
  opt.tree <- policy_tree(X[train, ], Gamma.matrix[train, ], depth = 2)
  opt.tree
  head(predict(opt.tree, X))
  col <- paste("voted_predict", i, sep = "")
  data[col]=predict(opt.tree, X)
  
  multi.forest <- multi_causal_forest(X = X, Y = Y_gun, W = W)
  head(predict(multi.forest)$predictions)
  Gamma.matrix <- double_robust_scores(multi.forest)
  head(Gamma.matrix)
  train <- sample(1:4096, 3000)
  opt.tree <- policy_tree(X[train, ], Gamma.matrix[train, ], depth = 2)
  opt.tree
  head(predict(opt.tree, X))
  col <- paste("gun_bin_predict", i, sep = "")
  data[col]=predict(opt.tree, X)
  
  multi.forest <- multi_causal_forest(X = X, Y = Y_wall, W = W)
  head(predict(multi.forest)$predictions)
  Gamma.matrix <- double_robust_scores(multi.forest)
  head(Gamma.matrix)
  train <- sample(1:4096, 3000)
  opt.tree <- policy_tree(X[train, ], Gamma.matrix[train, ], depth = 2)
  opt.tree
  head(predict(opt.tree, X))
  col <- paste("wall_bin_predict", i, sep = "")
  data[col]=predict(opt.tree, X)
  
  multi.forest <- multi_causal_forest(X = X, Y = Y_donate, W = W)
  head(predict(multi.forest)$predictions)
  Gamma.matrix <- double_robust_scores(multi.forest)
  head(Gamma.matrix)
  train <- sample(1:4096, 3000)
  opt.tree <- policy_tree(X[train, ], Gamma.matrix[train, ], depth = 2)
  opt.tree
  head(predict(opt.tree, X))
  col <- paste("donate_bin_predict", i, sep = "")
  data[col]=predict(opt.tree, X)
  
  
  }
write.csv(data,'policytree_turnout-gun-wall_binvars-output.csv')

rm(list=ls(all=TRUE))
library(policytree)
data <- read.csv("turnout-gun-wall_contvars_pred_comb.csv")

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
Y_donate <- -data$mse_donate_opt
Y_donate_abs <- -data$mse_donate_abs_opt
Y_donate_wall <- -data$mse_donate_wall_opt
Y_donate_wall_abs <- -data$mse_donate_wall_abs_opt
Y_donate_gun <- -data$mse_donate_gun_opt
Y_donate_gun_abs <- -data$mse_donate_gun_abs_opt


for (i in 1:100) {
  multi.forest <- multi_causal_forest(X = X, Y = Y_donate, W = W)
  head(predict(multi.forest)$predictions)
  Gamma.matrix <- double_robust_scores(multi.forest)
  head(Gamma.matrix)
  train <- sample(1:4096, 3000)
  opt.tree <- policy_tree(X[train, ], Gamma.matrix[train, ], depth = 2)
  opt.tree
  head(predict(opt.tree, X))
  col <- paste("donate_predict", i, sep = "")
  data[col]=predict(opt.tree, X)
  
  multi.forest <- multi_causal_forest(X = X, Y = Y_donate_abs, W = W)
  head(predict(multi.forest)$predictions)
  Gamma.matrix <- double_robust_scores(multi.forest)
  head(Gamma.matrix)
  train <- sample(1:4096, 3000)
  opt.tree <- policy_tree(X[train, ], Gamma.matrix[train, ], depth = 2)
  opt.tree
  head(predict(opt.tree, X))
  col <- paste("donate_abs_predict", i, sep = "")
  data[col]=predict(opt.tree, X)
  
  multi.forest <- multi_causal_forest(X = X, Y = Y_donate_wall, W = W)
  head(predict(multi.forest)$predictions)
  Gamma.matrix <- double_robust_scores(multi.forest)
  head(Gamma.matrix)
  train <- sample(1:4096, 3000)
  opt.tree <- policy_tree(X[train, ], Gamma.matrix[train, ], depth = 2)
  opt.tree
  head(predict(opt.tree, X))
  col <- paste("donate_wall_predict", i, sep = "")
  data[col]=predict(opt.tree, X)
  
  multi.forest <- multi_causal_forest(X = X, Y = Y_donate_wall_abs, W = W)
  head(predict(multi.forest)$predictions)
  Gamma.matrix <- double_robust_scores(multi.forest)
  head(Gamma.matrix)
  train <- sample(1:4096, 3000)
  opt.tree <- policy_tree(X[train, ], Gamma.matrix[train, ], depth = 2)
  opt.tree
  head(predict(opt.tree, X))
  col <- paste("donate_wall_abs_predict", i, sep = "")
  data[col]=predict(opt.tree, X)
  
  multi.forest <- multi_causal_forest(X = X, Y = Y_donate_gun, W = W)
  head(predict(multi.forest)$predictions)
  Gamma.matrix <- double_robust_scores(multi.forest)
  head(Gamma.matrix)
  train <- sample(1:4096, 3000)
  opt.tree <- policy_tree(X[train, ], Gamma.matrix[train, ], depth = 2)
  opt.tree
  head(predict(opt.tree, X))
  col <- paste("donate_gun_predict", i, sep = "")
  data[col]=predict(opt.tree, X)
  
  multi.forest <- multi_causal_forest(X = X, Y = Y_donate_gun_abs, W = W)
  head(predict(multi.forest)$predictions)
  Gamma.matrix <- double_robust_scores(multi.forest)
  head(Gamma.matrix)
  train <- sample(1:4096, 3000)
  opt.tree <- policy_tree(X[train, ], Gamma.matrix[train, ], depth = 2)
  opt.tree
  head(predict(opt.tree, X))
  col <- paste("donate_gun_abs_predict", i, sep = "")
  data[col]=predict(opt.tree, X)

}
write.csv(data,'policytree_turnout-gun-wall_contvars-output.csv')


