

## --------------------------------------------------------------------------------------
# Make classification predictions
y <- as.factor(df_test$waterlog.100)
x <- df_test$pred # Use 0.5 as threshold

# Change class names
levels(y) <- levels(x) <- c("no", "yes")

# plot confusion matrix
conf_matrix_basic <- caret::confusionMatrix(data = x, reference = y)
conf_matrix_basic


## --------------------------------------------------------------------------------------
tp_basic <- conf_matrix_basic$table[2,2]
fp_basic <- conf_matrix_basic$table[2,1]

tn_basic <- conf_matrix_basic$table[1,1]
fn_basic <- conf_matrix_basic$table[1,2]


## --------------------------------------------------------------------------------------
acc_basic <- na.omit( (tp_basic + tn_basic )/ sum(conf_matrix_basic$table) )
# cat("the accuracy for the basic model is ",acc_basic)


## --------------------------------------------------------------------------------------
prec_basic <- na.omit(tp_basic / (tp_basic + fp_basic) )
# cat("the precision for the basic model is ",prec_basic)


## --------------------------------------------------------------------------------------
f1_basic <-  na.omit(2*tp_basic / (2*tp_basic + fp_basic +fn_basic) )
# cat("the f1 for the basic model is ",f1_basic)



## --------------------------------------------------------------------------------------
