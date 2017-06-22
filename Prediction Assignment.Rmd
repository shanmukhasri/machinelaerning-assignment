#Peer-graded Assignment: Prediction Assignment Writeup

##June 22, 2017

##K.S.S SIVA TEJA

==============================================================================

#Background
Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset).

#Data

##The training data for this project is here:

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

##The test data are available here:

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

```{r cache=TRUE}
library(caret); library(rpart);library(ggplot2);library(randomForest)
```


#Loading Data into R

Check the training and testing data, identifying the missing data, "NA" and "#DIV/0!" as "NA" everywhere.
```{r cache=TRUE}
url.training <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
url.testing <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
training <- read.csv(url(url.training), na.strings = c("NA", "", "#DIV0!"))
testing <- read.csv(url(url.testing), na.strings = c("NA", "", "#DIV0!"))
```
##We need to define the same columns
```{r cache=TRUE}
sameColumsName <- colnames(training) == colnames(testing)
colnames(training)[sameColumsName==FALSE]
```

The "classe" is not included in the testing data.

#Cleaning data
```{r cache=TRUE}
training<-training[,colSums(is.na(training)) == 0]
testing <-testing[,colSums(is.na(testing)) == 0]
```


#Checking the column names
```{R cache=TRUE}
head(colnames(training))
```


The first 7 variables of the training data were deleted, because they are irrelevant to the prediction.
```{r cache=TRUE}
training <- training[,8:dim(training)[2]]
testing <- testing[,8:dim(testing)[2]]
```

#Training, testing & validation data

The training dataset was separated into three parts: tranining part (60%), testing part (20%), and validation part (20%)
```{r cache=TRUE}
set.seed(123)
Seeddata1 <- createDataPartition(y = training$classe, p = 0.8, list = F)
Seeddata2 <- training[Seeddata1,]
validation <- training[-Seeddata1,]
Training_data1 <- createDataPartition(y = Seeddata2$classe, p = 0.75, list = F)
training_data2 <- Seeddata2[Training_data1,]
testing_data <- Seeddata2[-Training_data1,]
```

#Data exploration

```{r cache=TRUE}
qplot(classe,fill = "4",data=training_data2,main="Distribution of Classes")
```

#Findout the predictors
```{r cache=TRUE}
names(training_data2[,-53])
```

#Prediction model (Classification Tree model)
```{r cache=TRUE}
model_tree <- rpart(classe ~ ., data=training_data2, method="class")
prediction_tree <- predict(model_tree, testing_data, type="class")
class_tree <- confusionMatrix(prediction_tree, testing_data$classe)
class_tree
```

#Checking the model_tree
```{r cache=TRUE}
library(rpart.plot)
rpart.plot(model_tree)
```


#Random forest model
```{r cache=TRUE}
forest_model <- randomForest(classe ~ ., data=training_data2, method="class")
prediction_forest <- predict(forest_model, testing_data, type="class")
random_forest <- confusionMatrix(prediction_forest, testing_data$classe)
random_forest
```

#Final prediction
```{r cache=TRUE}
prediction1 <- predict(forest_model, newdata=testing_data)
confusionMatrix(prediction1, testing_data$classe)
```

##The Random Forest is a better predictive model than the Decision Tree, which has accuracy of(99.91%).

#Conclusions
In this study, the characteristics of predictors for both traning and testing datasets are reduced. The training dataset is splitted into subtraining and validation parts to construct a predictive model and evaluate its accuracy. Decision Tree and Random Forest are applied.The Random Forest is a much better predictive model than the Decision Tree, which has a larger accuracy (99.91%).