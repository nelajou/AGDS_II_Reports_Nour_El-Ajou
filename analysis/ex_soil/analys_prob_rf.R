


## --------------------------------------------------------------------------------------
# re-train Random Forest model
rf_bor_prob <- ranger::ranger(
  y = df_train[, target],              # target variable
  x = df_train[, predictors_selected], # Predictor variables
  seed = 42,                           # Specify the seed for randomization to reproduce the same model again
  num.threads = parallel::detectCores() - 1,
  probability = TRUE,
  min.node.size = opt_min_node_size,
  mtry = opt_mtry) # Use all but one CPU core for quick model training



## --------------------------------------------------------------------------------------
# Need to load {ranger} because ranger-object is used in predict()
library(ranger) 

# Make predictions for validation sites
prediction_bor_prob <- predict(
  rf_bor_prob,           # RF model
  data = df_test_bor,   # Predictor data
  num.threads = parallel::detectCores() - 1
)

# Save predictions to validation df
df_test_bor_prob <- df_test_bor
df_test_bor_prob$pred <- prediction_bor_prob$predictions


## --------------------------------------------------------------------------------------
df_treshhold <- as.data.frame(cbind( 
  "waterlog.100" = as.numeric( as.character( df_test_bor_prob$waterlog.100 ) ), "P(waterlog = 1)" =   df_test_bor_prob$pred[,2]) )

treshholds  <-  (seq(from = 1, to = 0, -0.001) )


true_pos <- rep(NA,length(treshholds))
false_pos <- rep(NA,length(treshholds))

true_neg <- rep(NA,length(treshholds))
false_neg <- rep(NA,length(treshholds))

all_pos <- rep(70,length(treshholds))
all_neg <- rep(130, length(treshholds))


## --------------------------------------------------------------------------------------
for(i in 1:length(treshholds) ){
  
  df_treshhold$try <- df_treshhold$`P(waterlog = 1)`
  
  df_treshhold$try <- (df_treshhold$try >=treshholds[i])*1
  # df_treshhold$try[df_treshhold$try >= treshholds[i]] <- 1
  # df_treshhold$try[df_treshhold$try < treshholds[i]] <- 0
  
  
  # Make classification predictions
  # y_bor <- df_treshhold$waterlog.100 == 1
  # x_bor <- df_treshhold$try
  y_bor <- as.factor(df_treshhold$waterlog.100)
  x_bor <- as.factor(df_treshhold$try*1) # Use 0.5 as threshold
  
  # Change class names
  # levels(y_bor) <- levels(x_bor) <- c("no", "yes")
  y_bor <- factor(y_bor, levels = c(0,1), labels = c("no", "yes"))
  x_bor <- factor(x_bor, levels = c(0,1), labels = c("no", "yes"))
  
  
  
  # as.numeric(y_bor)
  # as.numeric(x_bor)
  
  # plot confusion matrix
  conf_matrix_bor <- caret::confusionMatrix(data = x_bor, reference = y_bor)
  # conf_matrix_bor$table
  
  # true positive rate
  true_pos[i] <- conf_matrix_bor$table[2,2]
  # False positive rate
  false_pos[i] <- conf_matrix_bor$table[2,1]
  
  # true negative rate
  true_neg[i] <- conf_matrix_bor$table[1,1]
  # False negative rate
  false_neg[i] <- conf_matrix_bor$table[1,2]
  
  
}

conf_table <- as.data.frame( round(cbind(treshholds, true_neg, false_neg,true_pos, false_pos ), 3)
)


## --------------------------------------------------------------------------------------
conf_table <- cbind(conf_table,
                    f1 = 
                      2*conf_table$true_pos/ (2*conf_table$true_pos + conf_table$false_pos + conf_table$false_neg) 
)



## --------------------------------------------------------------------------------------
true_pos_rate <- true_pos / all_pos
false_pos_rate <- false_pos / all_neg

png("../plots/ex_soil/aoc.png", width = 800, height = 600)
plot(false_pos_rate, true_pos_rate, col = "black", type = "l",
     main = "ROC Curve", xlab = "False positive Rate", 
     ylab = "True positive Rate", xlim = c(0,1), ylim = c(0,1) )
abline( v = 0.014, col = "red")
abline( v = 0.487, col = "red")
abline( v = 1, col = "black")
 # abline( h = 1, col = "red")

dev.off()
## --------------------------------------------------------------------------------------
# true_pos_rate
# false_pos_rate


## --------------------------------------------------------------------------------------
max_treshold_0_FN <- max(conf_table$treshholds[conf_table$false_neg == 0])


# cat ("The highest treshhold without any false negatives would be ", max_treshold_0_FN)


## --------------------------------------------------------------------------------------
max_treshold_F1 <- conf_table$treshholds[which.max(conf_table$f1)]

# cat ("The the treshhold returning the highest F1 would be ", max_treshold_F1, "with a a corresponding F1 of ",round(max(conf_table$f1),3) )

