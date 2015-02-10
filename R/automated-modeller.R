library(magrittr)
library(dplyr)
library(caret)

stratify <- function(data, response, frac=.5, seed=10, ...) {
  set.seed(seed)
  if(class(data[, response])=="factor"){
    by_data <- data %>% group_by_(response)
    train <- sample_frac(by_data, frac)
    test <- data[-as.numeric(rownames(train)),]
  } else {
    train <- sample_frac(data, frac)
    test <- data[-as.numeric(rownames(train)),]
  }
  return(list(train=train, test=test))
}

train_caret <- function(train, response, methods=c("rpart", "nnet", "svmRadial", "gam")) {
  control <- trainControl(method="repeatedcv", number=5, repeats=1,
                          index=createFolds(train[,response]))
  models <- Map(function(method) {
    model <- tryCatch(
      train(as.formula(paste(response, "~ .")), data=train, method=method, trControl=control),
      error=function(e) NULL)
    return(model)
  }, methods)
  
  return(models)
}

confusion_matrix <- function(test, response, models) {
  return(Map(function(model){
    fit_test <- predict(model, newdata=test[,!(names(iris) == response)])
    cm <- confusionMatrix(fit_test, test[,(names(iris) == response)])
  }, models))
}

# Example Usage:

data(iris)
response <- "Species"
get_samples <- list(train=iris, test=iris)

models <- train_caret(get_samples$train, response)

Map(function(model) {
  tryCatch(plot(model), error = function(e) NULL)
}, models)

bwplot(resamples(models))


