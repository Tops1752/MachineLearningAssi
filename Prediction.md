# Exercise Manner Prediction
Bai  
Tuesday, August 18, 2015  

##Synopsis
The goal of this project is to predict the manner in which they did the exercise. This is the "classe" variable in the training set.

##Data Processing
Download training data and testing data. 

```r
# read file into data frame
trainingDF <- read.csv("./data/pml-training.csv",na.strings = c('NA','#DIV/0!',''))
testingDF  <- read.csv("./data/pml-testing.csv",na.strings = c('NA','#DIV/0!',''))
```
Split the training data into training set to build the prediction model (70%) and testing set for model validation (30%).

```r
library(caret)
```

```
## Loading required package: lattice
## Loading required package: ggplot2
```

```r
set.seed(32323)
inTrain <- createDataPartition(y=trainingDF$classe,p=.7,list=FALSE)
training <- trainingDF[inTrain,]
testing  <- trainingDF[-inTrain,]
dim(training);dim(testing)
```

```
## [1] 13737   160
```

```
## [1] 5885  160
```

##Data Exploratory and Cleaning
All the data exploratory is applied on the training set.Note there are 159 variables in the dataset. Let's check the NAs in each column and clean the variables with NAs. 


```r
feature_index <- colnames(training)
feature_index <- colnames(training[colSums(is.na(training)) == 0])
length(feature_index)
```

```
## [1] 60
```
There are 60 columns without NAs. By observating those columns, remove column 1 to 7 which are discriptions of the record.


```r
feature_index
```

```
##  [1] "X"                    "user_name"            "raw_timestamp_part_1"
##  [4] "raw_timestamp_part_2" "cvtd_timestamp"       "new_window"          
##  [7] "num_window"           "roll_belt"            "pitch_belt"          
## [10] "yaw_belt"             "total_accel_belt"     "gyros_belt_x"        
## [13] "gyros_belt_y"         "gyros_belt_z"         "accel_belt_x"        
## [16] "accel_belt_y"         "accel_belt_z"         "magnet_belt_x"       
## [19] "magnet_belt_y"        "magnet_belt_z"        "roll_arm"            
## [22] "pitch_arm"            "yaw_arm"              "total_accel_arm"     
## [25] "gyros_arm_x"          "gyros_arm_y"          "gyros_arm_z"         
## [28] "accel_arm_x"          "accel_arm_y"          "accel_arm_z"         
## [31] "magnet_arm_x"         "magnet_arm_y"         "magnet_arm_z"        
## [34] "roll_dumbbell"        "pitch_dumbbell"       "yaw_dumbbell"        
## [37] "total_accel_dumbbell" "gyros_dumbbell_x"     "gyros_dumbbell_y"    
## [40] "gyros_dumbbell_z"     "accel_dumbbell_x"     "accel_dumbbell_y"    
## [43] "accel_dumbbell_z"     "magnet_dumbbell_x"    "magnet_dumbbell_y"   
## [46] "magnet_dumbbell_z"    "roll_forearm"         "pitch_forearm"       
## [49] "yaw_forearm"          "total_accel_forearm"  "gyros_forearm_x"     
## [52] "gyros_forearm_y"      "gyros_forearm_z"      "accel_forearm_x"     
## [55] "accel_forearm_y"      "accel_forearm_z"      "magnet_forearm_x"    
## [58] "magnet_forearm_y"     "magnet_forearm_z"     "classe"
```

```r
feature_index <- feature_index[-c(1:7)]
training <- training[,feature_index]
dim(training)
```

```
## [1] 13737    53
```

##Preprocess and Model 
Use PCA method in preprocess and rf as prediction model.


```r
modelFit <- train(training$classe ~ .,method="rf",preProcess="pca",data=training[,-53],na.action=na.pass)
modelFit
```

```
## Random Forest 
## 
## 13737 samples
##    51 predictor
##     5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## Pre-processing: principal component signal extraction, scaled, centered 
## Resampling: Bootstrapped (25 reps) 
## Summary of sample sizes: 13737, 13737, 13737, 13737, 13737, 13737, ... 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy   Kappa      Accuracy SD  Kappa SD   
##    2    0.9624171  0.9524411  0.003631982  0.004584029
##   27    0.9438287  0.9289230  0.006032378  0.007652144
##   52    0.9436319  0.9286754  0.005952228  0.007555538
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was mtry = 2.
```

##Cross Validation
Apply the model on testing set and compare with the real result. The Accuracy is 97.16%. The out of sample error is 1-accuracy which is 0.0284.


```r
confusionMatrix(testing$cla,predict(modelFit,testing[,-53]))
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1656    0    7    9    2
##          B   20 1105   14    0    0
##          C    3   17  991   14    1
##          D    2    1   44  915    2
##          E    0    7    7    6 1062
## 
## Overall Statistics
##                                           
##                Accuracy : 0.9735          
##                  95% CI : (0.9691, 0.9774)
##     No Information Rate : 0.2856          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.9665          
##  Mcnemar's Test P-Value : 7.388e-09       
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9851   0.9779   0.9323   0.9693   0.9953
## Specificity            0.9957   0.9928   0.9927   0.9901   0.9958
## Pos Pred Value         0.9892   0.9701   0.9659   0.9492   0.9815
## Neg Pred Value         0.9941   0.9947   0.9852   0.9941   0.9990
## Prevalence             0.2856   0.1920   0.1806   0.1604   0.1813
## Detection Rate         0.2814   0.1878   0.1684   0.1555   0.1805
## Detection Prevalence   0.2845   0.1935   0.1743   0.1638   0.1839
## Balanced Accuracy      0.9904   0.9854   0.9625   0.9797   0.9956
```

##Prediction
Apply the model on the test file.

```r
predict(modelFit,testingDF)
```

```
##  [1] B A C A A E D B A A A C B A E E A B B B
## Levels: A B C D E
```


