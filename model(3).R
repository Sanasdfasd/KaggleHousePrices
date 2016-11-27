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

  return(b)
}




correlation_matrix <- function(data)
{
  # names(cor_results) <- c("Attr_1", "Attr_2", "Correlation")
  vect<-colnames(data)
  print(ncol(data))
  for(j in 1:ncol(data))
  {
    for(i in j:ncol(data))
    {

      s<-cor(data[[j]],data[[i]]);
      cor_temp <- c(vect[j],vect[i],s)
      cor_results <- rbind(cor_results, cor_temp)
      if(abs(s) <= 0.0002){
        cat ("The correlation b/w attributes",vect[j],vect[i],s,"\n")
      }

    }
  }
  cor_results <- as.data.frame(cor_results);
  names(cor_results) <- c("Attr_1", "Attr_2", "Correlation")
  write.csv(cor_results,"correlation_details.csv", row.names=F)
  M<-cor(data)
  # corrplot(M,method = "number")

  # drop_cols = c()
  # for(i in 1:(nrow(cor_results))){
  #   if(cor_results[[3]] <= 0.005){
  #     drop_cols <- c(drop_cols, cor_results[i,2])
  #   }
  # }

}


rforest_function <- function(train, test) {
  n <- floor(ncol(train)/3)
  print(n)
  model <- randomForest(SalePrice~., data=train, ntree = 50, mtry = n, importance = TRUE)
  SalePrice <- predict(model,test)
  Id <- test[[1]]
  actual_sale_price <- test$SalePrice;
  result <- data.frame(Id, SalePrice,actual_sale_price)
  val <- sqrt(mean((actual_sale_price-SalePrice)^2));
  cat("the mean square error found is ",val);
  return (list(first=val,second=model));
}



train_data <- read.csv("train.csv", header = TRUE, sep=",")
test_data <- read.csv("test.csv", header = TRUE, sep=",")

#pre-process train data
cat("No of cols before: ", ncol(train_data),"\n")
train_data <- preprocess(train_data)
train_data <- checkNA(train_data)
cat("No of cols after: ", ncol(train_data),"\n")

correlation_matrix(train_data)

#pre-process train data
cat("No of cols before: ", ncol(test_data),"\n")
test_data <- preprocess(test_data)
test_data <- checkNA(test_data)
cat("No of cols after: ", ncol(test_data),"\n")






sz_part <-0;
k <- 10;
t_rows <- nrow(train_data)
sz_part <-floor(t_rows/k)
j<-1;
# cat("the nof rows of train_data",nrow(train_data));
train_data_sam <- sample(1:nrow(train_data), nrow(train_data))
# cat("the value s",train_data[train_data_sam,1])
# cat("no of rows",t_rows,"s_part is",sz_part)
steps <- 1;
var_list <- c();
model_list <- c();

## k-fold cross validation
 while((j+sz_part)<= t_rows)
 {
   if((j+sz_part)>t_rows)
     break;

   test_samples <- train_data_sam[(j):(j+sz_part-1)]
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
     }
     else
     {
       result1 <- data.frame(Id, SalePrice)
       result1 <- result1[order(result1$Id),];
       result[,i+1] <- result1$SalePrice;
     }
     i <- i+1;
}

output_matrix <- matrix(1460,nrow(test_data),2);
output_matrix[,1] <- result[,1];
for(i in 1:nrow(test_data))
{
   output_matrix[i,2] <-  mean(result[i,2:(val_inden+1)]);
}
output_df <- as.data.frame((output_matrix));
names(output_df) <-  c("Id", "SalePrice")

 #output_df
write.csv(output_df, "output.csv",append = TRUE, row.names = F)









options(warn = oldw)
