

# Make classification predictions
y_bor <- as.factor(df_test_bor$waterlog.100)
x_bor <- df_test_bor$pred # Use 0.5 as threshold

# Change class names
levels(y_bor) <- levels(x_bor) <- c("no", "yes")

# plot confusion matrix
conf_matrix_bor <- caret::confusionMatrix(data = x_bor, reference = y_bor)
conf_matrix_bor$table

## --------------------------------------------------------------------------------------
tp_bor <- conf_matrix_bor$table[2,2]
fp_bor <- conf_matrix_bor$table[2,1]

tn_bor <- conf_matrix_bor$table[1,1]
fn_bor <- conf_matrix_bor$table[1,2]


## --------------------------------------------------------------------------------------
acc_bor <- na.omit( (tp_bor + tn_bor )/ sum(conf_matrix_bor$table) )
# cat("the accuracy for the boruta model is ",acc_bor)


## --------------------------------------------------------------------------------------
prec_bor <- na.omit(tp_bor / (tp_bor + fp_bor) )
# cat("the precision for the boruta model is ", prec_bor)


## --------------------------------------------------------------------------------------

f1_bor <-  na.omit(2*tp_bor / (2*tp_bor + fp_bor +fn_bor) )
# cat("the f1 for the boruta model is ", f1_bor)



## --------------------------------------------------------------------------------------
# OOB prediction error of the final model
obb_basic <- sqrt(rf_basic$prediction.error)
obb_bor <- sqrt(rf_bor$prediction.error)


## --------------------------------------------------------------------------------------
# cat("Looking at the prediction error, the Boruta Model would be preferred over the
# basic case since its Prediction error with", sqrt(rf_bor$prediction.error), "smaller than the one of the basic model"
# ) 



## --------------------------------------------------------------------------------------
diag_table <- rbind(diag_basic <- c(round(obb_basic,3), round(acc_basic,3),                                              round(prec_basic,3),round(f1_basic,3) ),
                    diag_bor <-  c(round(obb_bor,3), round(acc_bor,3),                                              round(prec_bor,3),round(f1_bor,3) )
)
diag_table <- as.data.frame(diag_table, row.names = c("Basic RF", "Boruta RF") )
colnames(diag_table) <- c("OBB", "Accuracy", "Precision", "F1")

preferred_model <- as.vector(
  c( rownames(diag_table)[which.min(diag_table$OBB)],
     rownames(diag_table ) [which.max(diag_table$Accuracy)],
     rownames(diag_table) [which.max(diag_table$Precision)],
     rownames(diag_table  )[which.max(diag_table$F1) ]  )  )


diag_table <- rbind(diag_table, preferred_model)

rownames(diag_table) <- c("Basic RF", "Boruta RF", "preferred Model")
diag_table




## --------------------------------------------------------------------------------------
summary(df_predict$prediction == df_predict_bor$prediction)


