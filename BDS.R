
#Load the dataset and compare the attributes lstat and r
library(tidyr)
Boston
attach(Boston)
plot(lstat, medv, col='blue', pch=20)
plot(rm, medv, col='red', pch=20)


#Split the data into two equal parts randomly
data <- data.frame(lstat,rm,medv)
set.seed(203)
train <- sample(1:nrow(data), nrow(data)/2)
train_X <- data[train,1:2]
train_y <- data[train,3]
test_X <- data[-train,1:2]
test_y <- data[-train,3]

#TASK.1: DECISION STUMP

#Decision stump function
dec_s <- function(X,y) {
  #This function returns the predictions for a given data and its label.
  #It splits the space in two regions and calculates the mean of the label of each region.
  #The values that evalueates the best prediction are the minimum RSS and the correspond s = threshold.
  sort_X <- sort(X)
  rss_X <- c()
  yhat <- matrix(0, nrow=length(y), ncol=length(X))
    
  for (s in 1:length(sort_X)) {
    R1 <- X[X < sort_X[s]]
    R2 <- X[X >= sort_X[s]]
    indxR1 <- which(X %in% R1)
  
    yhat[indxR1,s] <- mean(y[indxR1])
    indxR2 <- which(X %in% R2)

    yhat[indxR2,s] <- mean(y[indxR2])
    
    rss <- sum((y[indxR1] - yhat[indxR1,s])^2) + sum((y[indxR2] - yhat[indxR2,s])^2)
    
    rss_X <- c(rss_X, rss)
  }
  
  min_rss <- min(rss_X)
  indx <- which(rss_X %in% min_rss)
  min_s <- sort_X[indx]
  min_yhat <- yhat[,indx]
  my_list <- list('rss' = min_rss, 's' = min_s, 'pred' = min_yhat) 
  return(my_list)
}


#Best Values for Decision Stump
rm_val <- dec_s(train_X[,2], train_y)
lstat_val <- dec_s(train_X[,1], train_y)

cat("\nMinimum RSS for rm:", rm_val$rss)
cat("\nBest s for rm:", rm_val$s)
cat("\nBest yhat for R1:", min(rm_val$pred))
cat("\nBest yhat for R2:", max(rm_val$pred))
cat("\n\nMinimum RSS for lstat:", lstat_val$rss)
cat("\nBest s for lstat:", lstat_val$s)
cat("\nBest yhat for R1:", min(lstat_val$pred))
cat("\nBest yhat for R2:", max(lstat_val$pred))


#Plot the predictions for the values in rm (best RSS)
plot(rm, medv, pch=20)
lines(x=c(6.758, 6.758), y=c(0,60), col='red')
text(x=5, y=19.38365, labels=19.38365, col='red')
text(x=8, y=36.63111, labels=36.63111, col='red')


#Function for mse and rss
mse <- function(min_X, X, y) {
  
  ##This function returns the train MSE and the test RSS and MSE.
  ##To find that, it takes as inputs the values of the best rss and the test data. 
  
  train_mse <- min_X$rss/length(train)
  R1 <- X[X < min_X$s]
  R2 <- X[X >= min_X$s]
  indxR1 <- which(X %in% R1)
  indxR2 <- which(X %in% R1)
  test_rss <- sum((y[indxR1] - min_X$pred[indxR1])^2) + sum((y[indxR2] - min_X$pred[indxR2])^2)
  test_mse <- test_rss/length(y)
  my_mse <- list('train_mse' = train_mse, 'test_rss' = test_rss, 'test_mse' = test_mse)
  return(my_mse)
}


#Train MSE, test RSS and test MSE
mse(rm_val, test_X[,2], test_y)



#TASK.2: BOOSTED DECISION STUMP

#Boosted Decision Tree function
bds <- function(learn_rate, B, train_y) {
  #This functions performs B decision stumps and keeps improving the predictions based on a given learning rate. 
  #It returns as an ouput the last rss and the last predictions.
  pred <- 0
  r <- train_y
  for (i in c(1:B)) {
    ds1 <- dec_s(train_X[,2], r)
    ds2 <- dec_s(train_X[,1], r)
    if (ds1$rss < ds2$rss) {
      fb <- ds1$pred
      rss <- ds1$rss
    }
    else {
      fb <- ds2$pred
      rss <- ds2$rss
    }
    pred <- pred + learn_rate*fb[1:253]
    r <- r - learn_rate*fb[1:253]
  }
  my_values <- list('rss' = rss, 'pred' = pred)
  
  return(my_values)
}


#RSS using BDS for learning rate (0.01) and number of trees (B = 1000)
bds_tree<- bds(0.01, 1000, train_y)
cat("\nRSS for bds:", bds_tree$rss)


#Predictions for the training set
plot(train_X[,1], train_y, pch=20)
points(train_X[,1], bds_tree$pred, pch=20, col='blue')
plot(train_X[,2], train_y, pch=20)
points(train_X[,2], bds_tree$pred, pch=20, col='blue')


#test MSE function
mse_bds <- function(min_X, y) {
  #This function returns the test mse
  test_mse <- sum((y - min_X$pred)^2)/length(y)
  return(test_mse)
}


#test MSE for B = 1000
mse_bds(bds_tree, train_y)

#TASK.3: PLOT MSE BOOSTED DECISION STUMP

#Test MSE loop
B <- 100
mse_list <- rep(0,B)
for (j in c(1:B)) {
  bds_1 <- bds(0.01, j, train_y)
  mse_list[j] <- mse_bds(bds_1, test_y)
}


#Test MSE performance plot
plot(c(1:100), mse_list, pch=20, col='red', xlab="B Trees", ylab="Test MSE")
lines(c(1:100), mse_list, col='red')


