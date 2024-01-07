


## --------------------------------------------------------------------------------------
# Make classification predictions
y_bor <- as.factor(df_test_bor$waterlog.100)
x_bor <- df_test_bor$pred_tuned # Use 0.5 as threshold

# Change class names
levels(y_bor) <- levels(x_bor) <- c("no", "yes")

# plot confusion matrix
conf_matrix_bor_tuned <- caret::confusionMatrix(data = x_bor, reference = y_bor)
conf_matrix_bor_tuned$table

tp_bor_tuned <- conf_matrix_bor_tuned$table[2,2]
fp_bor_tuned <- conf_matrix_bor_tuned$table[2,1]

tn_bor_tuned <- conf_matrix_bor_tuned$table[1,1]
fn_bor_tuned <- conf_matrix_bor_tuned$table[1,2]


## --------------------------------------------------------------------------------------


acc_bor_tuned <- na.omit( (tp_bor_tuned + tn_bor_tuned )/ sum(conf_matrix_bor_tuned$table) )
cat("the accuracy for the tuned Boruta model is ",acc_bor_tuned)


## --------------------------------------------------------------------------------------

prec_bor_tuned <- na.omit(tp_bor_tuned / (tp_bor_tuned + fp_bor_tuned) )
cat("the Precision for the tuned Boruta model is ",prec_bor_tuned)


## --------------------------------------------------------------------------------------

f1_bor_tuned <-  na.omit(2*tp_bor_tuned / (2*tp_bor_tuned + fp_bor_tuned +fn_bor_tuned) )

cat("the F1 for the tuned Boruta model is ",f1_bor_tuned)


## --------------------------------------------------------------------------------------
obb_bor_tuned <- sqrt(rf_bor_tuned$prediction.error)

cat("the F1 for the tuned Boruta model is ",obb_bor_tuned)



## --------------------------------------------------------------------------------------
diag_table_3 <- rbind( c(round(obb_basic,3), round(acc_basic,3),                                              round(prec_basic,3),round(f1_basic,3) ),
                       diag_bor <-  c(round(obb_bor,3), round(acc_bor,3),  
                                      round(prec_bor,3), round(f1_bor,3) ),
                       diag_bor_tuned <- c(round(obb_bor_tuned,3),  round(acc_bor_tuned,3),                                      round(prec_bor_tuned,3), round(f1_bor_tuned,3) )
)
diag_table_3 <- as.data.frame(diag_table_3)
rownames(diag_table_3) <- c("Basic RF", "Boruta RF","Boruta tuned")

colnames(diag_table_3) <- c("OBB", "Accuracy", "Precision", "F1")

best_model <- as.vector(
  c( rownames(diag_table_3)[which.min(diag_table_3$OBB)],
     rownames(diag_table_3 ) [which.max(diag_table_3$Accuracy)],
     rownames(diag_table_3) [which.max(diag_table_3$Precision)],
     rownames(diag_table_3  )[which.max(diag_table_3$F1) ]  ) 
)


diag_table_3 <- rbind(diag_table_3, best_model)

rownames(diag_table_3) <- c("Basic RF", "Boruta RF","Boruta tuned", "Best Model")


diag_table_3

