# #
# install.packages("class",dependencies = TRUE)
# install.packages("adabag",dependencies = TRUE)
# install.packages("randomForest",dependencies = TRUE)
oldw <- getOption("warn")
options(warn = -1)
library(corrplot)
library(stats)
library(class)
library(adabag)
library(randomForest)
library(ROCR)
library(rpart)


checkNA <- function(myData){
  testNA <- apply(myData, MARGIN = 2, FUN = function(x) sum(is.na(x)))

  for(i in 1:(ncol(myData)))
  {
    col <- myData[[i]]
    col_na <- which(is.na(col))
    med = median(col,na.rm=TRUE)
    for (j in col_na) {
      myData[[i]][j] <- med
    }
  }

  return(myData)

}

preprocess <- function(data){

  b <- data

  for(i in 1:(ncol(b)))
  {
    # print (bank[[i]])
    if(is.numeric(b[[i]])==FALSE)
    {
      b[[i]]<-as.numeric(b[[i]])-1;
      b[[i]]<-as.numeric(b[[i]]);
    }
  }

  # for(i in 1:(ncol(b))){hist(b[[i]]) break; }
  b <- subset(b, select=-c(LotShape, LotConfig, BsmtFullBath, X3SsnPorch, MiscFeature))
  return(b)

}


rforest_function <- function(train, test) {
  # train$SalePrice <- as.factor(train$SalePrice)
  n <- floor(sqrt(ncol(train)))
  print(n)
	model <- randomForest(SalePrice~., data=train, ntree = 50, mtry = n, importance = TRUE)
  # summary(model)

  SalePrice <- predict(model,test)
  Id <- test$Id
  result <- data.frame(Id, SalePrice)
  write.csv(result, "output.csv", row.names = F)

  ActualSalePrice <- test$SalePrice
  evaluation <- data.frame(result,ActualSalePrice, abs(SalePrice-ActualSalePrice))
  write.csv(evaluation, "evaluation.csv", row.names = F)


	# pred <- predict(model,newdata=test,type="response");
	# print(pred)
	# out<- table(pred, test$SalePrice);
	# res <- (out[2,2]+out[1,1])*100/(out[1,1]+out[1,2]+out[2,1]+out[2,2])
	# pred <- prediction(as.numeric(pred), test[,ncol(test)]);
	# perf <- performance(pred, measure = "tpr", x.measure = "fpr")
	# plot(perf)
	# auc <- performance(pred, measure = "auc")
	# auc <- auc@y.values[[1]]
	# return (res); # return(auc)
}


correlation_matrix <- function(data)
{
  # b<-preprocess(data)
  vect<-colnames(data)
  #library(corrplot)
  print(ncol(data))
  for(j in 1:ncol(data))
  {
    for(i in j:ncol(data))
    {

      s<-cor(data[[j]],data[[i]]);
      # print(vect[i])
      #print(s)
      if(abs(s) <= 0.0002){
        cat ("the correlation b/w attributes",vect[j],vect[i],s,"\n")
      }

    }
  }
  M<-cor(data)
  corrplot(M,method = "number")

}



# #Read the full data into full_data variable
# full_data <- read.csv("train.csv", header = TRUE, sep=",")

train_data <- read.csv("train.csv", header = TRUE, sep=",")
test_data <- read.csv("test2.csv", header = TRUE, sep=",")

#pre-process train data
cat("No of cols before: ", ncol(train_data),"\n")
train_data <- preprocess(train_data)
train_data <- checkNA(train_data)
cat("No of cols after: ", ncol(train_data),"\n")

# correlation_matrix(train_data)

#pre-process train data
cat("No of cols before: ", ncol(test_data),"\n")
test_data <- preprocess(test_data)
test_data <- checkNA(test_data)
cat("No of cols after: ", ncol(test_data),"\n")




# #pre-processing
# full_data <- preprocess(full_data)
# full_data <- checkNA(full_data)

# # na.omit(full_data)
# samp_train_data <- sample(1:nrow(full_data), 0.8*nrow(full_data))
# train_data <- full_data[samp_train_data,]
# test_data <- full_data[-samp_train_data,]

rforest_function(train_data,test_data)












# full_data_sam <- sample(1:nrow(full_data), nrow(full_data))
# sz_part <-0;
# k <- 10;
# t_rows <- nrow(full_data)
# sz_part <-floor(t_rows/k)


#  # classifiers<- c("lreg", "knn", "rforest", "boosting", "bagging")
# classifiers <- c("rforest")

# total_accuracy <-0;
# for (c in classifiers) {
# j<-1;

# steps <- 0;
# # k-fold cross validation
# while((j+sz_part)<= t_rows)
# {
#   #if((j+sz_part)>t_rows)
#    # break;
#   #full_data <- preprocess(full_data)

#   test_samples <- full_data_sam[(j):(j+sz_part)]

#   train_data <- full_data[-test_samples,]

#   test_data <- full_data[test_samples,]

#   if(c=="lreg"){
#     #Do Preprocessing
#     #......
#     #cat("Classifier is :",c,"\n")
#   # total_accuracy<- total_accuracy+logistic_regression(train_data, test_data)

#   }
#   else if(c == "knn"){
#   	#cat("Classifier is :",c,"\n")
#     # temp<- k_nearest_neighbours(train_data, test_data)
#     # total_accuracy <- temp+total_accuracy;

#   }
#   else if(c=="rforest")
#   {
#     #cat("Classifier is :",c,"\n")
#     temp <- rforest_function(train_data,test_data)
#     total_accuracy <- temp+total_accuracy;
#   }

#   j<-j+sz_part
#   steps <-steps+1;
# }

# cat("Classifier is :",c,"\n")
# cat("k-Fold_cross_validation :",k,"\n")
# cat("Avg Accuracy:  ",(total_accuracy/steps),"\n")
# total_accuracy <-0;
# cat("\n")
# }

options(warn = oldw)
