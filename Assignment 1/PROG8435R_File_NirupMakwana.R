##################################################
### PROG8435                                    ##
##################################################
#                                               ##
# PROG8435 Assignment 01                        ##
##################################################
# Written by Nirup Makwana                      ##
# ID: 8931418                                   #
#
##################################################
### Assignment 1                                ##
##################################################

#Reading and Verifying the Data and Column names
Data_NM <- read.csv('E:\\Big Data\\Sem 2\\R\\PROG8435_Assign_Explore_24W.csv')
print(head(Data_NM))
print(colnames(Data_NM))
View(Data_NM)

#Question 1.1
#Creating a Summary table for income and marital status 
TIncome_NM <- aggregate(income ~ m.status, data = Data_NM, FUN = sum)
print(TIncome_NM) #printing the results
  
#Question 1.2.a
#Calculating round mean
RMean_NM <- round(mean(Data_NM$age[Data_NM$nation== "Asia"]), digits = 2)
print(paste("The Mean age of respondants from Asia is:", RMean_NM))

#Question 1.2.b
#calculating weighted mean 
WMean_NM <- round(weighted.mean(x = Data_NM$age[Data_NM$nation == "Asia"], w = Data_NM$n.child[Data_NM$nation == "Asia"]), digits = 2)
print(paste("The mean age of respondants from Asia weighted by the number of childrens they have is:", WMean_NM))

#Question 1.3
#Table Comparison of score and gender
TC_NM <- aggregate(score ~ gender, data = Data_NM, FUN = mean)
print(TC_NM)

#Question 1.4 
#Quartiles
quantile(Data_NM$time1, probs = c(0.34, 0.63))

#Question 2.1
#Pie Chart
PC_NM <- table(Data_NM$political)
pie(PC_NM)

#Question 2.2
#Calculating Summary table for the percentage of respondents from each region belonging to the treat group
TableData_NM <- table(Data_NM$nation, Data_NM$group) #Making new dataframe only containing nation and group columns
TableCount_NM <- TableData_NM[, "treat"] #only taking the rows with the word "treat"
TotalCount_NM <- rowSums(TableData_NM) #now taking out the total count for nation
PerT_NM <- TableCount_NM/TotalCount_NM * 100
print(PerT_NM)

#Question 2.3
#Bar Chart
meand_NM <- aggregate(scr ~ nation, data = Data_NM, FUN = mean)
barplot(height = meand_NM$scr, names.arg = meand_NM$nation, xlab = "Nation", ylab = "Scores", main = "Bar chart Mean Standardized Score vs Nation")

#Question 2.4
#Histogram
hist(Data_NM$food, breaks = 5, xlab = "Percentage of Household Income for Food", ylab = "Frequency", main = "Distribution of Food Percentage")

#Question 2.5
#Box Plot
boxplot(income ~ m.status, data = Data_NM, main = "Boxplot of Income by Marital Status", xlab = "Marital Status", ylab = "Income")

#Question 2.6
#Histogram for Income
hist(Data_NM$income, breaks = 5, xlab = "Income", ylab = "Frequency", main = "Distribution of Income")
#Histogram for Standardized score
hist(Data_NM$scr, breaks = 5, xlab = "Standardized Score", ylab = "Frequency", main = "Distribution of Standardized Score")
#Scatter plot between income and standardized score
plot(scr ~ income, data = Data_NM, xlab = "Income", ylab = "Standardized Score", main = "Scatter Plot Income vs Standardized Score")
#Correlation coefficient
cor(Data_NM$income, Data_NM$scr)