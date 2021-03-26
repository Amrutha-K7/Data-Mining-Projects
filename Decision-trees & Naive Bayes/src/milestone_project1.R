
install.packages("Hmisc")
library(Hmisc)
library(ggplot2)

dframe <- read.csv("dataset.csv", header=TRUE, sep= ";", quote="\"") #Reads the CSV file

nrow(dframe)                    #total 41188 rows were read
dframe                          #Prints the data frame

dframe <- read.table("dataset.csv",sep = ";", header = TRUE, na.strings = "unknown") #replaces the unknown values with <NA> using na.string while reading the CSV file

sum(is.na(dframe))              #count NA values in data frame = 12718(unknown)

dframe <- dframe[-c(3,5,6,7,8)] #Dropping attributes specified: [marital, default, housing, loan, contact]

dframe <- na.omit(dframe)       #omit the rows with <NA> values

nrow(dframe)                    #Number of rows after omitting NA values = 39258
                                #total number of rows omitted = 41188-39258 = 1930

#Identifying non relevant attributes using histogram analysis

numeric_cols <-dframe[, sapply(dframe, is.numeric)] # select the numeric attributes from the data frame
hist.data.frame(numeric_cols)                       # plots histogram for numeric attributes on one plot window


categorical_cols <-dframe[, !sapply(dframe, is.numeric)]  # selects the non numeric attributes from the data frame
hist.data.frame(categorical_cols)                         # plots the histogram for categorical attributes on one plot window

ggplot(dframe, aes(pdays))+geom_histogram(binwidth = 2)            # plots for both type of variables using ggplot for better view
ggplot(dframe, aes(education))+geom_bar()

#Identifying non relevant attributes using correlation matrix
result<-cor(numeric_cols,  method = "pearson")     #correlation matrix for numeric columns
round(result,2)                                    #Rounding up the results till 2 decimal points

#random sampling
set.seed(60)
sample_data <- dframe[sample(nrow(dframe), 10000), ]
sample_data
