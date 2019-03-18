
M <- .2
N <- nrow(adult)
adult_miss <- adult
if (M > 0) {
  remove_ind <- sample(1:((ncol(adult)-1)*N), size = floor(M*N))
  column <- 1 + floor(remove_ind/nrow(adult))
  row <- remove_ind - (nrow(adult)*floor(remove_ind/nrow(adult)))
  for (i in 1:length(column)) {
    adult_miss[row[i], column[i]] <- NA        
  }
}
adult_cdf <- adult_miss %>%
  mutate_if(is.numeric,
            funs(apply_numeric_empirical_cdf(., class_variable = adult$exceeds_50k,
                                                         class_value = ">50K")))
adult_cdf <- adult_cdf %>%
  dplyr::select(-exceeds_50k) %>%
  mutate_if(is.character,
            funs(apply_categorical_empirical_cdf(.,
                                                 class_variable = adult$exceeds_50k,
                                                 class_value = ">50K"))) %>%
  mutate(exceeds_50k = adult_miss$exceeds_50k)

apply_categorical_empirical_pdf(adult$workclass,
                                class_variable = adult$exceeds_50k,
                                class_value = ">50K")

rf_output <- randomForest(x=predictor_data, y=target, importance = TRUE,
                       ntree = 10001, proximity=TRUE, sampsize=sampsizes)

library(ROCR)
predictions=as.vector(rf_output$votes[,2])
pred=prediction(predictions,target)

perf_AUC=performance(pred,"auc") #Calculate the AUC value
AUC=perf_AUC@y.values[[1]]

perf_ROC=performance(pred,"tpr","fpr") #plot the actual ROC curve
plot(perf_ROC, main="ROC plot")
text(0.5,0.5,paste("AUC = ",format(AUC, digits=5, scientific=FALSE)))

