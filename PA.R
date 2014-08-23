setwd("./R/machinelearning")

install.packages("caret")
install.packages("randomForest")
install.packages("rpart")
install.packages("e1071")
e1071
accuracyrpart; outofsamplerpart #42.5 - 57.4


if (!file.exists("pml-training.csv")) {
        fileURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
        download.file(fileURL, method="curl")
}
if (!file.exists("pml-testing.csv")) {
        fileURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
        download.file(fileURL, method="curl")
}

library(caret)

training <- read.csv("pml-training.csv")
testing <- read.csv("pml-testing.csv")

training$X <- NULL
training$user_name <- NULL
training$raw_timestamp_part_1 <- NULL
training$raw_timestamp_part_2 <- NULL

#remove first seven columns
training <- training[,-(1:7)]

# remove if na's appear on more than 90% of total cases
over70NAs <- function(vector){
        if(sum(is.na(vector))/length(vector) > 0.7){ # if vector is made of more than 90% NAs
                TRUE;                             # return true
        }else{                                       # if it doesn't
                FALSE;                            # return false
        }
}

# verify which columns are made most of NAs
vars70NAs <- sapply(training, over70NAs);
# remove these columns
training <- training[,!vars70NAs];

end <- ncol(training)

# convert everything but the class into numeric
training[,-end] <- data.frame(sapply(training[,-end],as.numeric))

# detect and remove variables who don't contribute for the classification
nzv <- nearZeroVar(training[,-end],saveMetrics = TRUE)
training <- training[,!as.logical(nzv$nzv)]

inTrain = createDataPartition(training$classe, p=0.7)[[1]]
training = training[ inTrain,]
crossval = training[-inTrain,]

training_backup <- training
crossval_backup <- crossval

colnames(training)

library(rpart)
modFitrpart <- train(classe~.,method="rpart",data=training)
predrpart <- predict(modFitrpart,newdata=crossval)
correctrpart <- sum(predrpart == crossval$classe, na.rm=TRUE);
falserpart <- sum(predrpart != crossval$classe, na.rm=TRUE)


pml_write_files = function(x){
        n = length(x)
        for(i in 1:n){
                filename = paste0("problem_id_",i,".txt")
                write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
        }
}



### Model-Based

``` {r train lda}
modFitlda <- train(training$classe~.,method="lda",data=train)
predlda <- predict(modFitlda,newdata=val)

accurlda <- postResample(crossval$classe, predlda)
accuracylda <- accurlda[[1]]
outofsamplelda <- 1 - accuracylda
accuracylda; outofsamplelda
```

The Model-based method has an accuracy of `r accuracylda` and an out of sample error of `r outofsamplelda`.