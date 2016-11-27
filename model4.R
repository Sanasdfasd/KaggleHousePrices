# #
# install.packages("class",dependencies = TRUE)
# install.packages("adabag",dependencies = TRUE)
# install.packages("gbm",dependencies = TRUE)
# install.packages("chron",dependencies = TRUE)
# install.packages("xgboost",dependencies = TRUE)
# install.packages("randomForest",dependencies = TRUE)
oldw <- getOption("warn")
options(warn = -1)
library(corrplot)
library(stats)
library(ggplot2)
library(class)
library(adabag)
library(randomForest)
library(ROCR)
library(gbm)
library(chron)
library(xgboost)
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
  Id <- test[[1]]
  actual_sale_price <- test$SalePrice;
 # cat("\n sale_price",actual_sale_price);
  result <- data.frame(Id, SalePrice,actual_sale_price)

  val <- sqrt(mean((actual_sale_price-SalePrice)^2));
  cat("the mean square error found is ",val);
  
 return (list(first=val,second=model));
}


bagging_function <- function(train, test){
  
print('In bagging');
 train$SalePrice <- as.factor(train$SalePrice);
 # control = rpart.control(maxdepth = 1)
 train_bagging <- bagging(SalePrice~., data=train, mfinal = 1, control = rpart.control(maxdepth = 1));
 SalePrice <- predict.bagging(train_bagging, test);
 Id <- test[[1]]
 print(SalePrice)
 # result <- data.frame(Id, SalePrice)
 # write.csv(result,"bagging_output.csv",row.names = F)
 # out <- predict_bagging$confusion;
 # pred_class <- predict_bagging$class;
 # pred_class <- as.numeric(pred_class);
 #  cat("\n")
 # print(out);
 # cat("Accuracy: ","\n")
 # res <- (out[2,2]+out[1,1])*100/(out[1,1]+out[1,2]+out[2,1]+out[2,2])
}


gradient_boosting_function <- function(train, test){
  print("In gbm")
  model <- gbm(SalePrice~. , data = train, distribution="gaussian", n.trees = 100)
  Id <- test[[1]]
  SalePrice <- predict(model, test, n.trees = 500)
  result <- data.frame(Id, SalePrice)
  write.csv(result, "gbm.csv", row.names = F)
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
test_data <- read.csv("test.csv", header = TRUE, sep=",")
# plot_diag <- ggplot(data = train_data, color = 'red')
# pdf("plots.pdf")
# print(plot_diag)
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



gradient_boosting_function(train_data, test_data)

# bagging_function(train_data,test_data)


# #pre-processing
# full_data <- preprocess(full_data)
# full_data <- checkNA(full_data)

# # na.omit(full_data)
# samp_train_data <- sample(1:nrow(full_data), 0.8*nrow(full_data))
# train_data <- full_data[samp_train_data,]
# test_data <- full_data[-samp_train_data,]

tmp <- FALSE;

if(tmp == TRUE)
{

classifier <- c('rforest')

sz_part <-0;
k <- 10;
t_rows <- nrow(train_data)
sz_part <-floor(t_rows/k)
j<-1;
cat("the nof rows of train_data",nrow(train_data));
train_data_sam <- sample(1:nrow(train_data), nrow(train_data))
cat("the value s",train_data[train_data_sam,1])
cat("no of rows",t_rows,"s_part is",sz_part)
 steps <- 1;
 var_list <- c();
 model_list <- c();
 
## k-fold cross validation
 while((j+sz_part)<= t_rows)
 {
   if((j+sz_part)>t_rows)
     break;
  # full_data <- preprocess(train_data)

   test_samples <- train_data_sam[(j):(j+sz_part-1)]
   test_samples

   train_datas <- train_data[-test_samples,]

   test_datas <- train_data[test_samples,]
 
  model_parm <- rforest_function(train_datas,test_datas)
   var_list <- cbind(var_list,model_parm$first)
   ps <- paste("my_model",steps);
   saveRDS(model_parm$second,ps)
   #cat("calculated one time ");
  j<-j+sz_part;
   steps <-steps+1;
 
   
 }
 var_list
 list_values <- c();
 val_inden <- 7;
 
 list_values <- tail(sort(var_list,decreasing=TRUE),val_inden);
 
 result <- matrix(1460,nrow(test_data),val_inden+1);
i<-1;
  while(i<(val_inden+1))
  {
 min_s <- which(var_list==list_values[i]);
 ps1 <- paste("my_model",min_s);
 mod2 <- readRDS(ps1)
 SalePrice <- predict(mod2,test_data)
 Id <- test_data[[1]]
 #actual_sale_price <- test_data$SalePrice;
 # cat("\n sale_price",actual_sale_price);
 if(i==1)
 {
   result1 <- data.frame(Id, SalePrice)
 result1 <- result1[order(result1$Id),];
        
 result[,1] <- result1$Id;
 result[,2] <- result1$SalePrice;
 #write.csv(result1, "C:/Users/SANDEEP/Desktop/Project/output.csv",append = TRUE ,row.names = F)
 }
 else
 {
   result1 <- data.frame(Id, SalePrice)
   result1 <- result1[order(result1$Id),];
   #result[,1] <- result1$Id;
   result[,i+1] <- result1$SalePrice;
   #result <- merge(result,result1$SalePrice) 
   
 
 }
 i <- i+1;
 
  }
 
 output_matrix <- matrix(1460,nrow(test_data),2);
 output_matrix[,1] <- result[,1];
 for(i in 1:nrow(test_data))
 {
   output_matrix[i,2] <-  median(result[i,2:(val_inden+1)]);
 }
 output_df <- as.data.frame((output_matrix));
 names(output_df) <-  c("Id", "SalePrice")
 #output_df
 write.csv(output_df, "output.csv",append = TRUE, row.names = F)



}








# full_data_sam <- sample(1:nrow(full_data), nrow(full_data))



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
