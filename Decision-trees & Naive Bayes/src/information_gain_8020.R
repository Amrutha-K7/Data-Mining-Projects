install.packages("Hmisc")
install.packages("caret", dependencies = TRUE)
install.packages("rpart")
install.packages("rpart.plot")
install.packages('e1071', dependencies=TRUE)


library("Hmisc")
library("ggplot2")
library("caret")
library("rpart")
library("rpart.plot")
library("dplyr")

#Pre-processing
dframe <- read.csv("dataset.csv", header=TRUE, sep= ";", quote="\"") #Reads the CSV file
nrow(dframe)                    #total 41188 rows were read
dframe                          #Prints the data frame

dframe <- read.table("dataset.csv",sep = ";", header = TRUE, na.strings = "unknown") #replaces the unknown values with <NA> using na.string while reading the CSV file
sum(is.na(dframe))              #count NA values in data frame = 12718(unknown)
dframe <- dframe[-c(3,5,6,7,8)] #Dropping attributes specified: [marital, default, housing, loan, contact]
dframe <- na.omit(dframe)       #omit the rows with <NA> values
nrow(dframe)                    #Number of rows after omitting NA values = 39258
                                #total number of rows omitted = 41188-39258 = 1930


#-----------------------------------------------------------------------------------------------
#Random sampling using seed:60 and splitting train:test = 80:20 

set.seed(60)                                           #Used for random sampling, seed value makes sure we get the same random rows every time
random_sample <- dframe[sample(nrow(dframe), 10000), ] #Gives 10k random rows from the pre-processed dataframe (dframe)
random_sample                                          #print the 10k random rows selected
train_size = floor(0.8*nrow(random_sample))            #compute the training set size as per 80:20 split
train_size
set.seed(60)                                           #set seed again to make sure 80:20 split is made w.r.t same seed value: 60
train_rows = random_sample[sample(nrow(random_sample),size = train_size),] #Gives the random 8K records from the random_sample(10k records)
train_rows
test_rows = random_sample[-sample(nrow(random_sample),size = train_size),] #Gives 2K records remaining from the random_sample(10k records)
nrow(test_rows)                                        #2K test records are selected
test_rows                                              #print the 2k random rows selected


# Decision tree: including all the attributes and builds the tree till maximum depth
dtree_full <- rpart(
  y ~ ., 
  data = train_rows, 
  method = "class",
  parms = list(split = 'information'),
  cp = 0)

print(dtree_full)  #prints the tree (complex tree)

#Prediction: Now lets predict the test set using the model dtree_full and obtain a confusion matrix
predict_test_full<-predict(dtree_full, test_rows , type = 'class') 
prows_full<-test_rows[predict_test_full,]
prows_full

#confusion matrix:  predicted will be on xaxis actual values will be on y axis in the 
confusion_matrix_full <-  table(predict_test_full,test_rows$y)
print(confusion_matrix_full)
#predict_test_full    no   yes
#               no  1744   77
#               yes   52   127

confusionMatrix(confusion_matrix_full) #another way for building the confusion matrix

#Function which computes accuracy_precision_recall_fscore using confusion matrix
accuracy_precision_recall_fscore <- function(c_matrix) {
  tn <- c_matrix[1,1]
  tp <- c_matrix[2,2]
  fn <- c_matrix[1,2]
  fp <- c_matrix[2,1]
  accuracy <- (tp+tn)/(tp+tn+fp+fn)
  precision <- tp/(tp+fp)
  recall  <- tp/(tp+fn)
  f_score <- 2*(precision*recall)/(precision+recall)
  vector <- c(accuracy,precision,recall,f_score)
  return(vector)
}
print(accuracy_precision_recall_fscore(confusion_matrix_full))
#[1] 0.9355000 0.7094972 0.6225490 0.6631854

printcp(dtree_full) # prints the cp values for the tree
minimum_cp <-dtree_full$cptable[which.min(dtree_full$cptable[,"xerror"]),"CP"] #find the minimum cp value where error rate is minimum
minimum_cp          # 0.008

# Optimized decision tree with cp = 0.008
dtree_mincp <- rpart(
  y ~ ., 
  data = train_rows, 
  method = "class",
  parms = list(split = 'information'),
  cp = 0.008)

print(dtree_mincp)
#The attributes duration,euribor3m,nr.employed,pdays,poutcome are being used in the construction of optimal tree.

#plots
rpart.plot(dtree_mincp) #rpart.plot gives the representation of the trained model
plotcp(dtree_mincp) # cp plot gives the cp value variation w.r.t x-error

#Prediction: Now lets predict the test set using the model dtree_mincp and obtain a confusion matrix
predict_test_mincp <-predict(dtree_mincp, test_rows , type = 'class') 
prows_mincp<-test_rows[predict_test_mincp,]
prows_mincp

#confusion matrix:  predicted will be on xaxis actual values will be on y axis in the 
confusion_matrix_mincp <-  table(predict_test_mincp,test_rows$y)
print(confusion_matrix_mincp)
#predict_test_mincp   no    yes
#                 no  1747  113
#                 yes   49   91

print(accuracy_precision_recall_fscore(confusion_matrix_mincp))
#[1] 0.9190000 0.6500000 0.4460784 0.5290698

#Variable Importance: computes the importance of variables in descending order using the modeldtree_mincp
attribute_importance <- data.frame(dtree_mincp$variable.importance)
attribute_importance

# dtree_mincp.variable.importance
# duration                          586.73988286
# nr.employed                       483.73814731
# euribor3m                         423.00879551
# emp.var.rate                      317.35394334
# cons.conf.idx                     270.86691281
# cons.price.idx                    241.58985784
# month                             152.68055496
# pdays                              38.30822568
# poutcome                           37.27829282
# previous                            9.80134498
# age                                 0.84119501
# education                           0.79812653
# campaign                            0.08931157

#withhold the parameter:duration which has the highest importance and checking if the accuracy changes
dtree_withhold_duration <- rpart(
  y ~ (age+job+education+month+day_of_week+campaign+pdays+previous+poutcome+emp.var.rate+cons.price.idx+cons.conf.idx+euribor3m+nr.employed),
  data = train_rows, 
  method = "class",
  parms = list(split = 'information'),
  cp = 0.008)

print(dtree_withhold_duration)
predict_test_duration <-predict(dtree_withhold_duration, test_rows , type = 'class') 
prows_duration<-test_rows[predict_test_duration,]
prows_duration

confusion_matrix_duration <-  table(predict_test_duration,test_rows$y)
print(confusion_matrix_duration)
#predict_test_duration   no    yes
#                    no  1785  168
#                   yes   11    36

print(accuracy_precision_recall_fscore(confusion_matrix_duration))
#[1] 0.9105000 0.7659574 0.1764706 0.2868526

#withhold the parameter:euribor3m which has the high importanc eand check if the accuracy changes as we had identified this parameter as non relevant using correlation matrix
dtree_withhold_euribor3m <- rpart(
  y ~ (age+job+education+month+day_of_week+campaign+pdays+previous+poutcome+emp.var.rate+cons.price.idx+cons.conf.idx+duration+nr.employed),
  data = train_rows, 
  method = "class",
  parms = list(split = 'information'),
  cp = 0.008)

print(dtree_withhold_euribor3m)
predict_test_euribor3m <-predict(dtree_withhold_euribor3m, test_rows , type = 'class') 
prows_euribor3m<-test_rows[predict_test_euribor3m,]
prows_euribor3m

confusion_matrix_euribor3m <-  table(predict_test_euribor3m,test_rows$y)
print(confusion_matrix_euribor3m)
#predict_test_euribor3m   no     yes
#                     no  1763   125
#                     yes   33   79

print(accuracy_precision_recall_fscore(confusion_matrix_euribor3m))
#[1] 0.9210000 0.7053571 0.3872549 0.5000000

#withhold the parameter:pdays  which has the medium/less importance and check if the accuracy changes
dtree_withhold_pdays <- rpart(
  y ~ (age+job+education+month+day_of_week+campaign+euribor3m+previous+poutcome+emp.var.rate+cons.price.idx+cons.conf.idx+duration+nr.employed),
  data = train_rows, 
  method = "class",
  parms = list(split = 'information'),
  cp = 0.008)

print(dtree_withhold_pdays)
predict_test_pdays<-predict(dtree_withhold_pdays, test_rows , type = 'class') 
prows_pdays<-test_rows[predict_test_pdays,]
prows_pdays

confusion_matrix_pdays <-  table(predict_test_pdays,test_rows$y)
print(confusion_matrix_pdays)
#predict_test_pdays   no     yes
#                no   1772   134
#                yes   24    70

print(accuracy_precision_recall_fscore(confusion_matrix_pdays))
#[1] 0.9210000 0.7446809 0.3431373 0.4697987

#withhold the parameter:campeign  which has the least importance and is not used in the tree construction and check if the accuracy changes
dtree_withhold_campaign <- rpart(
  y ~ (age+job+education+month+day_of_week+pdays+euribor3m+previous+poutcome+emp.var.rate+cons.price.idx+cons.conf.idx+duration+nr.employed),
  data = train_rows, 
  method = "class",
  parms = list(split = 'information'),
  cp = 0.008)

print(dtree_withhold_campaign)
predict_test_campeign<-predict(dtree_withhold_campaign, test_rows , type = 'class') 
prows_campeign<-test_rows[predict_test_campeign,]
prows_campeign

confusion_matrix_campeign <-  table(predict_test_campeign,test_rows$y)
print(confusion_matrix_campeign)
#predict_test_campeign   no   yes
#                   no  1747  113
#                  yes   49   91

print(accuracy_precision_recall_fscore(confusion_matrix_campeign))
#[1] 0.9190000 0.6500000 0.4460784 0.5290698


#Dropping the parameters which are not used in the construction of optimal decision tree and checking if it makes any difference
#The attributes age,job,education,month,day_of_week,previous,emp.var.rate,cons.price.idx,cons.conf.idx,campaign are not used in the construction of the tree
train_drop_unused<-train_rows[-c(1,2,3,4,5,7,9,11,12,13)]
train_drop_unused

dtree_drop_unused <- rpart(
  y ~ .,
  data = train_drop_unused, 
  method = "class",
  parms = list(split = 'information'),
  cp = 0.008)

print(dtree_drop_unused)

predict_drop_unused <-predict(dtree_drop_unused, test_rows , type = 'class') 
prows_drop_unused<-test_rows[predict_drop_unused,]
prows_drop_unused

confusion_matrix_unused <-  table(predict_drop_unused ,test_rows$y)   
print(confusion_matrix_unused)
#predict_drop_unused   no   yes
#                 no  1747  113
#                yes   49   91

print(accuracy_precision_recall_fscore(confusion_matrix_unused))
#[1] 0.9190000 0.6500000 0.4460784 0.5290698

