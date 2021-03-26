install.packages("e1071")
library("caret")
library("e1071")

dframe <- read.csv("dataset.csv", header=TRUE, sep= ";", quote="\"") #Reads the CSV file

nrow(dframe)                    #total 41188 rows were read
dframe                          #Prints the data frame

dframe <- read.table("dataset.csv",sep = ";", header = TRUE, na.strings = "unknown") #replaces the unknown values with <NA> using na.string while reading the CSV file

sum(is.na(dframe))              #count NA values in data frame = 12718(unknown)

dframe <- dframe[-c(3,5,6,7,8)] #Dropping attributes specified: [marital, default, housing, loan, contact]

dframe <- na.omit(dframe)       #omit the rows with <NA> values

nrow(dframe)

set.seed(60)                                           #Used for random sampling, seed value makes sure we get the same random rows every time
random_sample <- dframe[sample(nrow(dframe), 10000), ]
random_sample                                          #print the 10k random rows selected
train_size = floor(0.8*nrow(random_sample))            #compute the training set size as per 80:20 split
train_size
set.seed(60)
train_rows = random_sample[sample(nrow(random_sample),size = train_size),] #Gives the random 8K records from the random_sample(10k records)
train_rows
test_rows = random_sample[-sample(nrow(random_sample),size = train_size),] #Gives 2K records remaining from the random_sample(10k records)
nrow(test_rows)                                        #2K test records are selected
test_rows  

naivebaye_data=naiveBayes(as.factor(y)~., data=train_rows) #training the naivebayes model for 80:20 splits
naivebaye_data                                            #naivebayes model

naivebaye_prediction=predict(naivebaye_data,test_rows) #predicting the test set using the naivebayes model
predicted_rows<-test_rows[naivebaye_prediction,]       #predicted rows
predicted_rows


#confusion matrix:  predicted will be on xaxis actual values will be on y axis in the 
confusion_matrix <-  table(naivebaye_prediction,test_rows$y)
print(confusion_matrix)
#80:20
#naivebaye_prediction   no   yes
#                  no  1626   84
#                  yes  170  120


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
print(accuracy_precision_recall_fscore(confusion_matrix))
#[1] 0.8730000 0.4137931 0.5882353 0.4858300

#============================================================
#naivebayes for 50:50 splits

set.seed(60)                                           #Used for random sampling, seed value makes sure we get the same random rows every time
random_sample_s <- dframe[sample(nrow(dframe), 10000), ]
random_sample_s                                          #print the 10k random rows selected
train_size_s = floor(0.5*nrow(random_sample_s))            #compute the training set size as per 50:50 split
train_size_s
set.seed(60)
train_rows_s = random_sample_s[sample(nrow(random_sample_s),size = train_size_s),] #Gives the random 5K records from the random_sample(10k records)
train_rows_s
test_rows_s = random_sample_s[-sample(nrow(random_sample_s),size = train_size_s),] #Gives 5K records remaining from the random_sample(10k records)
nrow(test_rows_s)                                        #5K test records are selected
test_rows_s  

naivebaye_data_s=naiveBayes(as.factor(y)~., data=train_rows_s) #training the naivebayes model for 50:50 splits
naivebaye_data_s                                             #naivebayes model

naivebaye_prediction_s=predict(naivebaye_data_s,test_rows_s) #predicting the test set using the naivebayes model
predicted_rows_s<-test_rows_s[naivebaye_prediction_s,]       #predicted rows
predicted_rows_s


#confusion matrix:  predicted will be on xaxis actual values will be on y axis in the 
confusion_matrix_s <-  table(naivebaye_prediction_s,test_rows_s$y)
print(confusion_matrix_s)
#50:50
#naivebaye_prediction_s   no      yes
#                    no   4029    226
#                   yes   444     301

print(accuracy_precision_recall_fscore(confusion_matrix_s))
#[1] 0.8660000 0.4040268 0.5711575 0.4732704
