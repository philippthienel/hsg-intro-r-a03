RMSEPredict <- function(formula, data, alpha = 0.8){
  
  # create random index to determine test & training data
  i <- round(alpha * nrow(data))
  train <- sample(1:nrow(data),size = i)
  data.train <- data[train,]
  data.test <- data[-train,]
  
  # train model
  fit <- lm(formula, data=data.train)
  
  # predict values
  prediction <- predict(fit, data.test, interval="predict", na.action = na.omit)
  
  # get predicted values & actual values
  fit.test <- lm(formula, data=data.test)
  y <- model.frame(fit.test)[,1]
  y.hat <- prediction[,1]
  
  # get root mean squared error 
  RMSE <- sqrt(mean((y-y.hat)^2))
  
  return(RMSE)
}


