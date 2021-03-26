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
#Random sampling using seed:60 and splitting train:test = 50:50 (using seed = 70)

set.seed(60)                                           #Used for random sampling, seed value makes sure we get the same random rows every time
random_sample <- dframe[sample(nrow(dframe), 10000), ] #Gives 10k random rows from the pre-processed dataframe (dframe)
random_sample                                          #print the 10k random rows selected
train_size = floor(0.5*nrow(random_sample))            #compute the training set size as per 50:50 split
train_size
set.seed(70)                                           #set seed again to make sure 50:50 split is made w.r.t same seed value: 70 (This is done for the error handling, mentioned later in this file)
train_rows = random_sample[sample(nrow(random_sample),size = train_size),] #Gives the random 5K records from the random_sample(10k records)
train_rows
test_rows = random_sample[-sample(nrow(random_sample),size = train_size),] #Gives 5K records remaining from the random_sample(10k records)
nrow(test_rows)                                        #5K test records are selected
test_rows                                              #print the 5k random rows selected


# Decision tree: including all the attributes and builds the tree till maximum depth
dtree_full <- rpart(
  y ~ ., 
  data = train_rows, 
  method = "class",
  parms = list(split = 'information'),
  cp = 0)


print(dtree_full)   #prints the tree (complex tree)

#Prediction: Now lets predict the test set using the model dtree_mincp and obtain a confusion matrix
predict_test_full <-predict(dtree_full, test_rows , type = 'class')
prows_full<-test_rows[predict_test_full,]
prows_full

#confusion matrix:  predicted will be on xaxis actual values will be on y axis in the 
confusion_matrix_full <-  table(predict_test_full,test_rows$y)
print(confusion_matrix_full)
#predict_test_full   no  yes
#               no  4291  229
#               yes  166  314

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
#[1] 0.9210000 0.6541667 0.5782689 0.6138807


printcp(dtree_full) # prints the cp values for the tree
minimum_cp <-dtree_full$cptable[which.min(dtree_full$cptable[,"xerror"]),"CP"] #find the minimum cp value where error rate is minimum
minimum_cp #0.009

# Optimized decision tree with cp = 0.009
dtree_mincp <- rpart(
  y ~ ., 
  data = train_rows, 
  method = "class",
  parms = list(split = 'information'),
  cp = 0.009)

print(dtree_mincp)
#The attributes duration,euribor3m,nr.employed,pdays,poutcome,month,job,age,education,cons.price.idx,campaign are being used in the construction of optimal tree.

#plots
rpart.plot(dtree_mincp); #rpart.plot gives the representation of the trained model
plotcp(dtree_mincp) # cp plot gives the cp value variation w.r.t x-error

#------------------------------------------------------------------------------------------------

#Error handling: For the seed 60: predict methods was giving error: factor education has new levels illiterate 
#as there were no records with education = illiterate in training set but there were 3 records with value illiterate in test set

nrow(subset(train_rows, education == "illiterate"))#has 0 observations with "illiterate" level for the factor education
nrow(subset(test_rows, education == "illiterate")) #has 3 observations with "illiterate" level for the factor education

table(train_rows$education)
#basic.4y     basic.6y    basic.9y   high.school  professional.course  university.degree 
#524          298         744         208                 728         1498

table(test_rows$education)
#basic.4y basic.6y  basic.9y  high.school illiterate professional.course   university.degree 
#528        304         765       1193            3        667                 1540 

#we have changed the seed while splitting random sample (10k) records into 50:50 (new seed value = 70) 
#so that the records with "illiterate" value gets distributed into both training and test sets  
#-------------------------------------------------------------------------------------------------


#Prediction: Now lets predict the test set using the model dtree_mincp and obtain a confusion matrix
predict_test_mincp <-predict(dtree_mincp, test_rows , type = 'class')
prows_mincp<-test_rows[predict_test_mincp,]
prows_mincp

#confusion matrix:  predicted will be on xaxis actual values will be on y axis in the 
confusion_matrix_mincp <-  table(predict_test_mincp,test_rows$y)
print(confusion_matrix_mincp)
# predict_test_mincp   no    yes
#                 no  4329   297
#                yes  128    246


print(accuracy_precision_recall_fscore(confusion_matrix_mincp))
#0.9150000 0.6577540 0.4530387 0.5365322

#Variable Importance: computes the importance of variables in descending order using the modeldtree_mincp
attribute_importance <- data.frame(dtree_mincp$variable.importance)
attribute_importance

# dtree_mincp.variable.importance
# duration                            351.592296
# nr.employed                         276.227394
# euribor3m                           263.068265
# emp.var.rate                        177.489178
# cons.conf.idx                       150.322827
# cons.price.idx                      140.858954
# month                                77.175665
# poutcome                             33.509015
# pdays                                29.786555
# previous                             14.962658
# campaign                              7.584065
# job                                   6.340694
# education                             5.549617
# age                                   5.519878
# day_of_week                           1.227587


#withhold the parameter:duration which has the highest importance and checking if the accuracy changes
dtree_withhold_duration <- rpart(
  y ~ (age+job+education+month+day_of_week+campaign+pdays+previous+poutcome+emp.var.rate+cons.price.idx+cons.conf.idx+euribor3m+nr.employed),
  data = train_rows, 
  method = "class",
  parms = list(split = 'information'),
  cp = 0.009)

print(dtree_withhold_duration)
predict_test_duration <-predict(dtree_withhold_duration, test_rows , type = 'class') 
prows_duration<-test_rows[predict_test_duration,]
prows_duration

confusion_matrix_duration <-  table(predict_test_duration,test_rows$y)
print(confusion_matrix_duration)
#predict_test_duration   no     yes
#                   no   4422   445
#                  yes   35      98

print(accuracy_precision_recall_fscore(confusion_matrix_duration))
#[1] 0.9040000 0.7368421 0.1804788 0.2899408

#withhold the parameter:euribor3m which has the high importanc eand check if the accuracy changes as we had identified this parameter as non relevant using correlation matrix
dtree_withhold_euribor3m <- rpart(
  y ~ (age+job+education+month+day_of_week+campaign+pdays+previous+poutcome+emp.var.rate+cons.price.idx+cons.conf.idx+duration+nr.employed),
  data = train_rows, 
  method = "class",
  parms = list(split = 'information'),
  cp = 0.009)

print(dtree_withhold_euribor3m)
predict_test_euribor3m <-predict(dtree_withhold_euribor3m, test_rows , type = 'class') 
prows_euribor3m<-test_rows[predict_test_euribor3m,]
prows_euribor3m

confusion_matrix_euribor3m <-  table(predict_test_euribor3m,test_rows$y)
print(confusion_matrix_euribor3m)
#predict_test_euribor3m   no    yes
#                    no   4298  277
#                    yes  159   266

print(accuracy_precision_recall_fscore(confusion_matrix_euribor3m))
#[1] 0.9128000 0.6258824 0.4898711 0.5495868

#withhold the parameter:pdays  which has the medium/less importance and check if the accuracy changes
dtree_withhold_pdays <- rpart(
  y ~ (age+job+education+month+day_of_week+campaign+euribor3m+previous+poutcome+emp.var.rate+cons.price.idx+cons.conf.idx+duration+nr.employed),
  data = train_rows, 
  method = "class",
  parms = list(split = 'information'),
  cp = 0.009)

print(dtree_withhold_pdays)
predict_test_pdays<-predict(dtree_withhold_pdays, test_rows , type = 'class') 
prows_pdays<-test_rows[predict_test_pdays,]
prows_pdays

confusion_matrix_pdays <-  table(predict_test_pdays,test_rows$y)
print(confusion_matrix_pdays)
#predict_test_pdays   no    yes
#                no   4329  297
#                yes  128   246

print(accuracy_precision_recall_fscore(confusion_matrix_pdays))
#[1] 0.9150000 0.6577540 0.4530387 0.5365322

#withhold the parameter:day_of_week which has the least importance and is not used in the tree construction and check if the accuracy changes
dtree_withhold_day <- rpart(
  y ~ (age+job+education+month+campaign+pdays+euribor3m+previous+poutcome+emp.var.rate+cons.price.idx+cons.conf.idx+duration+nr.employed),
  data = train_rows, 
  method = "class",
  parms = list(split = 'information'),
  cp = 0.009)

print(dtree_withhold_day)
predict_test_day<-predict(dtree_withhold_day, test_rows , type = 'class') 
prows_day<-test_rows[predict_test_day,]
prows_day

confusion_matrix_day <-  table(predict_test_day,test_rows$y)
print(confusion_matrix_day)
#predict_test_day     no    yes
#               no    4329  297
#               yes   128   246
print(accuracy_precision_recall_fscore(confusion_matrix_day))
#[1] 0.9150000 0.6577540 0.4530387 0.5365322

#Dropping the parameters which are not used in the construction of optimal decision tree and checking if it makes any difference
#The attributes day_of_week,previous,emp.var.rate,cons.conf.idx are not used in the construction of the tree
train_drop_unused<-train_rows[-c(5,9,11,13)]
train_drop_unused

dtree_drop_unused <- rpart(
  y ~ .,
  data = train_drop_unused, 
  method = "class",
  parms = list(split = 'information'),
  cp = 0.009)

print(dtree_drop_unused)

predict_drop_unused <-predict(dtree_drop_unused, test_rows , type = 'class') 
prows_drop_unused<-test_rows[predict_drop_unused,]
prows_drop_unused

confusion_matrix_unused <-  table(predict_drop_unused ,test_rows$y)   
print(confusion_matrix_unused)
#predict_drop_unused   no    yes
#                  no  4323  296
#                  yes 134   247

print(accuracy_precision_recall_fscore(confusion_matrix_unused))
#[1] 0.9140000 0.6482940 0.4548803 0.5346320

