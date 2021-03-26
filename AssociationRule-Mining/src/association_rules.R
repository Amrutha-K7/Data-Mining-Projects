install.packages("tidyverse")
install.packages("arules")
install.packages("arulesViz")
install.packages("readxml")
install.packages("knitr")
install.packages("lubridate")
install.packages("plyr")
install.packages("RColorBrewer")

library(readxl)
library(dplyr)
library(arules)
library(arulesViz)
library(tidyverse)
library(knitr)
library(ggplot2)
library(lubridate)
library(plyr)
library(dplyr)
library(RColorBrewer)

dframe<- read_excel("online-retail.xlsx", sheet = "Online Retail")
dframe
nrow(dframe)                                      # 541909 rows read

#-------------------------------------------------Pre-processing----------------------------------------------------

table(startsWith(dframe$InvoiceNo, "C"))          # 9288 Invoice Numbers are starting with C
dframe <-dframe[!grepl("C", dframe$InvoiceNo),]   # Discarding the transactions whose Invoice Numbers are starting with C
nrow(dframe)                                      # 532621 records are remaining


dframe<-dframe[!grepl("WRONG|LOST|CRUSHED|SMASHED|DAMAGED|FOUND|THROWN|AWAY|MISSING|POSTAGE", dframe$Description, ignore.case = TRUE),]

dframe<-dframe[!grepl("^CHECK",dframe$Description),]        
dframe<-dframe[!grepl("^check",dframe$Description),]       
dframe<-dframe[!grepl("^stock check",dframe$Description),]

dframe<-dframe[!grepl("MANUAL|CHARGES|FAULT|SALES|ADJUST|AMAZON|COUNTED|label mix up|INCORRECT|BROKEN|BARCODE|RETURNED|MAILOUT|DELIVERY|MIX UP|MOULDY|PUT ASIDE|ERROR|DESTROYED|RUSTY|damages",dframe$Description, ignore.case = TRUE),] 

#df<-dframe[grepl("FEE",dframe$Description, ignore.case = TRUE),] 
#table(df$Description)

dframe<-dframe[!grepl("^cracked",dframe$Description),] # Matches only cracked in "small case"=1
dframe<-dframe[!grepl("sold",dframe$Description),]
dframe<-dframe[!grepl("Sold",dframe$Description),]
dframe<-dframe[!grepl("\\?",dframe$Description),]
nrow(dframe)

dframe <- dframe[complete.cases(dframe$Description), ]# removes the records with missing values for Description
nrow(dframe)                                      # 1400 rows are getting eliminated
dframe <- dframe[complete.cases(dframe$InvoiceNo), ]


nrow(dframe) # 528400 records were selected after preprocessing

itemssold<-unique(dframe$Description)
count(itemssold)
itemssold

#------------------------------------------Generating Transactions----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#The function paste() groups the items bought in each transaction and writes them as comma separated values
transactionData <- ddply(dframe,c("InvoiceNo"),function(df1)paste(df1$Description, collapse = ",")) # csv file transactions
head(transactionData,2)

transactionData$InvoiceNo <- NULL #set column InvoiceNo of transactionData to null
colnames(transactionData) <- c("items") #renaming
head(transactionData,2)

write.csv(transactionData,"mb_transactions.csv", quote = FALSE, row.names = FALSE) #writing basket data to csv file

txnlist = read.transactions("mb_transactions.csv",format="basket",header=TRUE,rm.duplicates=FALSE,sep=",")#reading basket data from csv file
summary(txnlist)
inspect(head(txnlist, 2))

#Create an item frequency plot(absolute) for the top 20 items
itemFrequencyPlot(txnlist,topN=20,type="absolute",col=brewer.pal(8,'Pastel1'), main="Absolute Item Frequency Plot",cex.names = 0.7)
#Create an item frequency plot((relative) for the top 20 items
itemFrequencyPlot(txnlist,topN=20,type="relative",col=brewer.pal(8,'Pastel2'), main="Relative Item Frequency Plot",cex.names = 0.7)

#-------------------------------------------------------------------------------------------------------------------
#min_sup:  0.006, 0.009, 0.014
#min_conf: 0.5,   0.7,   0.8

#Candidate itemsets for each iteration: support = 0.0006 and conf = 0.0005

citemsets111 <- apriori(txnlist, parameter = list(supp = 0.0006, conf = 0.0005, target="frequent itemsets", minlen=1, maxlen=1))
citemsets111len <- length(citemsets111)
citemsets122 <- apriori(txnlist, parameter = list(supp = 0.0006, conf = 0.0005, target="frequent itemsets", minlen=2, maxlen=2))
citemsets122len <- length(citemsets122)
citemsets133 <- apriori(txnlist, parameter = list(supp = 0.0006, conf = 0.0005, target="frequent itemsets", minlen=3, maxlen=3))
citemsets133len <- length(citemsets133)
citemsets144 <- apriori(txnlist, parameter = list(supp = 0.0006, conf = 0.0005, target="frequent itemsets", minlen=4, maxlen=4))
citemsets144len <- length(citemsets144)
citemsets155 <- apriori(txnlist, parameter = list(supp = 0.0006, conf = 0.0005, target="frequent itemsets", minlen=5, maxlen=5))
citemsets155len <- length(citemsets155)
#citemsets156 <- apriori(txnlist, parameter = list(supp = 0.00006, conf = 0.00005, target="frequent itemsets", minlen=6, maxlen=6))
#citemsets156len <- length(citemsets156)
plotcand <- c(citemsets111len,citemsets122len,citemsets133len,citemsets144len,citemsets155len)
plotcand
barplot(plotcand, names.arg = c(1, 2, 3, 4, 5), main = "Candidate Itemsets per Iteration",xlab = "No of Iterations",ylab = "Candidate Itemssets", col = "black")


#Frequent itemsets for each iteration: support = 0.006 and conf = 0.5
fitemsets111 <- apriori(txnlist, parameter = list(supp = 0.006, conf = 0.5, target="frequent itemsets", minlen=1, maxlen=1))
fitemsets111len <- length(fitemsets111)
fitemsets122 <- apriori(txnlist, parameter = list(supp = 0.006, conf = 0.5, target="frequent itemsets", minlen=2, maxlen=2))
fitemsets122len <- length(fitemsets122)
fitemsets133 <- apriori(txnlist, parameter = list(supp = 0.006, conf = 0.5, target="frequent itemsets", minlen=3, maxlen=3))
fitemsets133len <- length(fitemsets133)
fitemsets144 <- apriori(txnlist, parameter = list(supp = 0.006, conf = 0.5, target="frequent itemsets", minlen=4, maxlen=4))
fitemsets144len <- length(fitemsets144)
fitemsets155 <- apriori(txnlist, parameter = list(supp = 0.006, conf = 0.5, target="frequent itemsets", minlen=5, maxlen=5))
fitemsets155len <- length(fitemsets155)
plotfreq <- c(fitemsets111len,fitemsets122len,fitemsets133len,fitemsets144len,fitemsets155len)
plotfreq # 1040 1142  403   76    8
barplot(plotfreq, names.arg = c(1, 2, 3, 4, 5), main = "Frequent Itemssets per Iteration: sup=0.006 and conf=0.5",xlab = "No of Iterations",ylab = "Frequent Itemssets", col = "darkred")


#Candidate itemsets for each iteration: support = 0.0009 and conf = 0.0007
citemsets211 <- apriori(txnlist, parameter = list(supp = 0.0009, conf = 0.0007, target="frequent itemsets", minlen=1, maxlen=1))
citemsets211len <- length(citemsets211)
citemsets222 <- apriori(txnlist, parameter = list(supp = 0.0009, conf = 0.0007, target="frequent itemsets", minlen=2, maxlen=2))
citemsets222len <- length(citemsets222)
citemsets233 <- apriori(txnlist, parameter = list(supp = 0.0009, conf = 0.0007, target="frequent itemsets", minlen=3, maxlen=3))
citemsets233len <- length(citemsets233)
citemsets244 <- apriori(txnlist, parameter = list(supp = 0.0009, conf = 0.0007, target="frequent itemsets", minlen=4, maxlen=4))
citemsets244len <- length(citemsets244)
citemsets255 <- apriori(txnlist, parameter = list(supp = 0.0009, conf = 0.0007, target="frequent itemsets", minlen=5, maxlen=5))
citemsets255len <- length(citemsets255)
plotcand2<- c(citemsets211len,citemsets222len,citemsets233len,citemsets244len,citemsets255len)
plotcand2
barplot(plotcand2)

#Frequent itemsets for each iteration: support = 0.009 and conf = 0.7
fitemsets211 <- apriori(txnlist, parameter = list(supp = 0.009, conf = 0.7, target="frequent itemsets", minlen=1, maxlen=1))
fitemsets211len <- length(fitemsets211)
fitemsets222 <- apriori(txnlist, parameter = list(supp = 0.009, conf = 0.7, target="frequent itemsets", minlen=2, maxlen=2))
fitemsets222len <- length(fitemsets222)
fitemsets233 <- apriori(txnlist, parameter = list(supp = 0.009, conf = 0.7, target="frequent itemsets", minlen=3, maxlen=3))
fitemsets233len <- length(fitemsets233)
fitemsets244 <- apriori(txnlist, parameter = list(supp = 0.009, conf = 0.7, target="frequent itemsets", minlen=4, maxlen=4))
fitemsets244len <- length(fitemsets244)
fitemsets255 <- apriori(txnlist, parameter = list(supp = 0.009, conf = 0.7, target="frequent itemsets", minlen=5, maxlen=5))
fitemsets255len <- length(fitemsets255)
plotfreq2 <- c(fitemsets211len,fitemsets222len,fitemsets233len,fitemsets244len,fitemsets255len)
plotfreq2 # 671 376  75   2   0
barplot(plotfreq2, names.arg = c(1, 2, 3, 4, 5), main = "Frequent Itemssets per Iteration: sup=0.009 and conf=0.7",xlab = "No of Iterations",ylab = "Frequent Itemssets", col = "darkred")

#Candidate itemsets for each iteration: support = 0.00014 and conf = 0.0008
citemsets311 <- apriori(txnlist, parameter = list(supp = 0.00014, conf = 0.00008, target="frequent itemsets", minlen=1, maxlen=1))
citemsets311len <- length(citemsets311)
citemsets322 <- apriori(txnlist, parameter = list(supp = 0.00014, conf = 0.00008, target="frequent itemsets", minlen=2, maxlen=2))
citemsets322len <- length(citemsets322)
citemsets333 <- apriori(txnlist, parameter = list(supp = 0.00014, conf = 0.00008, target="frequent itemsets", minlen=3, maxlen=3))
citemsets333len <- length(citemsets333)
citemsets344 <- apriori(txnlist, parameter = list(supp = 0.00014, conf = 0.00008, target="frequent itemsets", minlen=4, maxlen=4))
citemsets344len <- length(citemsets344)
citemsets355 <- apriori(txnlist, parameter = list(supp = 0.00014, conf = 0.00008, target="frequent itemsets", minlen=5, maxlen=5))
citemsets355len <- length(citemsets355)
plotcand3<- c(citemsets311len,citemsets322len,citemsets333len,citemsets344len,citemsets355len)
plotcand3
barplot(plotcand3)

#Frequent itemsets for each iteration: support = 0.014 and conf = 0.8
fitemsets311 <- apriori(txnlist, parameter = list(supp = 0.014, conf = 0.8, target="frequent itemsets", minlen=1, maxlen=1))
fitemsets311len <- length(fitemsets311)
fitemsets322 <- apriori(txnlist, parameter = list(supp = 0.014, conf = 0.8, target="frequent itemsets", minlen=2, maxlen=2))
fitemsets322len <- length(fitemsets322)
fitemsets333 <- apriori(txnlist, parameter = list(supp = 0.014, conf = 0.8, target="frequent itemsets", minlen=3, maxlen=3))
fitemsets333len <- length(fitemsets333)
fitemsets344 <- apriori(txnlist, parameter = list(supp = 0.014, conf = 0.8, target="frequent itemsets", minlen=4, maxlen=4))
fitemsets344len <- length(fitemsets344)
fitemsets355 <- apriori(txnlist, parameter = list(supp = 0.014, conf = 0.8, target="frequent itemsets", minlen=5, maxlen=5))
fitemsets355len <- length(fitemsets355)
plotfreq3 <- c(fitemsets311len,fitemsets322len,fitemsets333len,fitemsets344len,fitemsets355len)
plotfreq3 #379 108   6   0   0
barplot(plotfreq3, names.arg = c(1, 2, 3, 4, 5), main = "Frequent Itemssets per Iteration: sup=0.014 and conf=0.8",xlab = "No of Iterations",ylab = "Frequent Itemssets", col = "darkred")


#-----------------------------------------------------------------------------------------------------------------------------
#Rule generation:
rules106 <- apriori(txnlist, parameter = list(supp = 0.006, conf = 0.5, target="rules", minlen=2))
length(rules106)#1389
inspect(rules106[1200:1203])
plot(rules106)

#Interactive visualization of some top 5 rules (Order 2)
rules106headtop5<-head(rules106, n = 5, by = "confidence")
plot(rules106headtop5, method = "graph",engine = "htmlwidget")

#Interactive visualization of some Order 4 rules
rules106head<-head(rules106[1200:1205], n = 2, by = "confidence")
plot(rules106head, method = "graph",engine = "htmlwidget")


rules206 <- apriori(txnlist, parameter = list(supp = 0.006, conf = 0.7, target="rules", minlen=2))
length(rules206)#513
inspect(rules206[1:3])
plot(rules206)


rules306 <- apriori(txnlist, parameter = list(supp = 0.006, conf = 0.8, target="rules", minlen=2))
length(rules306)#292
inspect(rules306[1:3])
plot(rules306)


rules109 <- apriori(txnlist, parameter = list(supp = 0.009, conf = 0.5, target="rules", minlen=2))
plot(rules109)
length(rules109)#326

rules209 <- apriori(txnlist, parameter = list(supp = 0.009, conf = 0.7, target="rules", minlen=2))
plot(rules209)
length(rules209)#86

rules309 <- apriori(txnlist, parameter = list(supp = 0.009, conf = 0.8, target="rules", minlen=2))
plot(rules309)
length(rules309)#41

rules114 <- apriori(txnlist, parameter = list(supp = 0.014, conf = 0.5, target="rules", minlen=2))
plot(rules114)
length(rules114)#77

rules214 <- apriori(txnlist, parameter = list(supp = 0.014, conf = 0.7, target="rules", minlen=2))
plot(rules214)
length(rules214)#24

rules314 <- apriori(txnlist, parameter = list(supp = 0.014, conf = 0.8, target="rules", minlen=2))
plot(rules314)
length(rules314)#15

#-----------------------------------------------------------------------------------------------------------------------------
#Filter 10 rules each for lift > 10, Lift < 10

#for lift > 10
allrules<-c(rules106,rules206,rules306,rules109,rules209,rules309,rules114,rules214,rules314)# merging rules from all 9 combinations
allrules<-unique(allrules) #selecting unique rules
rulesgreaterthan10 <- sort(subset(allrules, subset = lift > 10),by="lift")#sorting rules by lift
inspect(rulesgreaterthan10[1:10])
rulesgreaterthan10head <- head(rulesgreaterthan10, n = 10, by ="lift")
inspect(rulesgreaterthan10head)
plot(rulesgreaterthan10head,engine = "htmlwidget")
plot(rulesgreaterthan10head, method="paracoord") #parallel rules

#for Lift < 10
ruleslessthan10 <- sort(subset(allrules, subset = lift < 10),by="lift")
inspect(ruleslessthan10[1:3])
ruleslessthan10head <- head(ruleslessthan10, n = 10, by ="lift")
inspect(ruleslessthan10head)
plot(ruleslessthan10head,engine = "htmlwidget")
plot(ruleslessthan10head, method="paracoord")#parallel rules

#----------------------------------------------------------------------------------------------------------------------
#Visualization of Top 100 rules:
allrules_conf<-sort(allrules, by ="lift") #sort allrules by confidence default = descending
inspect(allrules_conf[1:3])
allrules_conf100 <- head(allrules_conf, n=100, by ="confidence",decreasing = TRUE)#rules are sorted based on the decreasing order of confidence
inspect(allrules_conf100[1:3])
#interactive vizualizing top 3 rules with high confidence
plot(allrules_conf100[1:3], method = "graph",engine = "htmlwidget")

#interactive vizualizing bottom 3 rules with low confidence
plot(allrules_conf100[99:100], method = "graph",engine = "htmlwidget")

plot(allrules_conf100, method="two-key plot")# scatter plot with rule orders in different colour
plot(allrules_conf100,engine = "htmlwidget")
plot(allrules_conf100, method = "graph",engine = "htmlwidget")

#plot(allrules, method="two-key plot")# scatter plot with rule orders in different colour all rules not sorted

plot(allrules_conf,method="two-key plot") # all rules sorted by confidence
plot(allrules_conf,engine = "htmlwidget") # all rules sorted by confidence


allrules_sortconf1<-sort(allrules, by ="confidence")
allrules_sortlift<-sort(allrules_sortconf1, by ="lift")
inspect(allrules_sortlift[1:10])
plot(allrules_sortlift,engine = "htmlwidget") # All rules sorted by lift and confidence
plot(allrules_sortlift[1:10],engine = "htmlwidget") # All rules sorted by lift and confidence




sup<-c(0.006,0.006,0.006,0.009,0.009,0.009,0.014,0.014,0.014)
con<-c(0.5,0.7,0.8,0.5,0.7,0.8,0.5,0.7,0.8)
col1<-c(1,2,3,1,2,3,1,2,3)
rul<-c(length(rules106),length(rules206),length(rules306),length(rules109),length(rules209),length(rules309),length(rules114),length(rules214),length(rules314))
rul
y<-data.frame(sup=sup,rul=rul,col=col1)
y
barplot(rul,xlab="sup",ylab="Number of Rules")


