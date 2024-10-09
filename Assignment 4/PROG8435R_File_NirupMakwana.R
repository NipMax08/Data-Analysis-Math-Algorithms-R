##################################################
### PROG8435                                    ##
##################################################
#                                               ##
# PROG8435 Assignment 04                        ##
##################################################
# Written by Nirup Makwana                      ##
# ID: 8931418                                   #
#
##################################################
### Assignment 4 Regression                     ##
##################################################

#Reading the data and checking the statistics to get a feel of the data 
RData_NM <- read.csv('E:\\Big Data\\Sem 2\\R\\PROG8435_Assign_MLR_24W.csv')
summary(RData_NM)
head(RData_NM)
library("pastecs")
library("corrgram")
colnames(RData_NM) <- paste0(colnames(RData_NM), "_NM")
print(colnames(RData_NM))
str(RData_NM)
stat.desc(RData_NM)


#Dimensionality Reduction
#Missing Values
Miss_Col_NM <- names(RData_NM)[colSums(is.na(RData_NM)) > 0]
  print(Miss_Col_NM)

#Removal of Columns
RData_NM <- RData_NM[,-17]
RData_NM <- RData_NM[,-1]
RData_NM <- RData_NM[,-16]

#Low Variance Filter
stat.desc(RData_NM)
table(RData_NM$housing_NM)

#High Correlation Filter
corrgram(RData_NM, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Correlations")
cor(RData_NM[, sapply(RData_NM, is.numeric)], method = "spearman")
#other hypothesized to be removed columns food, housing, other, time1 but decided against for now due to some reasons they might be important 
#might think about removing them after significance level is checked by the model


#Standardization and other stuff
NumCols_NM <- sapply(RData_NM, is.numeric)
RData_NM[, NumCols_NM] <- scale(RData_NM[, NumCols_NM])
NonNumCols_NM <- !NumCols_NM
RData_NM[, NonNumCols_NM] <- lapply(RData_NM[, NonNumCols_NM], as.factor)
summary(RData_NM)

#Categorical to Dummy Section
#Making use of the code from the Sample File to convert categorical to dummy
#I don't know which one is better to use Factors or Dummy so including the code for both.
#I will be keeping all into factors as of now but the code is provided to convert all the categorical values into dummy
# Identify categorical columns
Categorical_NM <- c("group_NM", "hs.grad_NM", "nation_NM", "gender_NM", "m.status_NM", "political_NM")
# Create dummy variables for categorical columns
dummy_vars_NM <- model.matrix(~ . - 1, data = RData_NM[, Categorical_NM])
#ONLY RUN IF YOU ARE CONVERTING TO DUMMY 
RData_NM <- cbind(RData_NM, dummy_vars)
# Remove the original categorical columns if needed
RData_NM <- RData_NM[, !names(RData_NM) %in% Categorical_NM]



#Box plots
par(mfrow=c(3,2))

for (i in 1:ncol(RData_NM)) {
  if (is.numeric(RData_NM[,i])) {
    boxplot(RData_NM[,i], main=names(RData_NM)[i], xlab="", horizontal=TRUE)
  }
}

#Correlation
pairs(RData_NM[sapply(RData_NM, is.numeric)], pch=46)

Cor_Matrix_NM <- cor(RData_NM[sapply(RData_NM, is.numeric)], method = "pearson")
round(Cor_Matrix_NM, 2) # Round the correlation matrix to two decimal places for readability.

#Simple Linear Regression
#Political Awareness and Test Score
par(mfrow=c(1,1))
SLR1_NM <- lm(Pol_NM ~ score_NM, data = RData_NM)
SLR1_NM
plot(Pol_NM ~ score_NM, data=RData_NM, main="Political Awareness by Test Score (with Regression Line)")
abline(SLR1_NM)
summary(SLR1_NM)

#Political Awareness and Standardized Test Score
SLR2_NM <- lm(Pol_NM ~ scr_NM, data = RData_NM)
SLR2_NM
plot(Pol_NM ~ scr_NM, data=RData_NM, main="Political Awareness by Standardized Test Score (with Regression Line)")
abline(SLR2_NM)
summary(SLR2_NM)


#Multivariate Regression
#Full Model
FullM_NM <- lm(Pol_NM ~ ., data = RData_NM, na.action = na.omit)
summary(FullM_NM)
Pred1_NM <- predict(FullM_NM, newdata=RData_NM)
RMSE_Full_NM <- sqrt(mean((RData_NM$Pol_NM - Pred1_NM)^2))
# Calculate and display percentage error for better context of the RMSE value.
PerctErr_Full_NM <- (RMSE_Full_NM / mean(RData_NM$Pol_NM)) * 100
round(RMSE_Full_NM,2)
round(PerctErr_Full_NM,3)

#Backwards Selection Model
BackM_NM <- step(FullM_NM, direction="backward", details=TRUE)
summary(BackM_NM)
# Predicts and calculates RMSE for the backward model.
Pred2_NM <- predict(BackM_NM, newdata=RData_NM)
RMSE_Back_NM <- sqrt(mean((RData_NM$Pol_NM - Pred2_NM)^2))
# Calculate and display percentage error for the backward model.
PerctErr_Back_NM <- (RMSE_Back_NM / mean(RData_NM$Pol_NM)) * 100
round(RMSE_Back_NM,2)
round(PerctErr_Back_NM,2)

#Residuals for the Full Model
FullResi_NM <- residuals(FullM_NM)
#Mean of Zero (Mean of Residuals)(Error Terms Means of Zero)
MeanResiFull_NM <- mean(FullResi_NM)
print(MeanResiFull_NM)

#Residuals for the Back Model
BackResi_NM <- residuals(BackM_NM)
#Mean of Zero (Mean of Residuals)(Error Terms Means of Zero)
MeanResiBack_NM <- mean(BackResi_NM)
print(MeanResiBack_NM)

#Constant Variance Full Model
VarFull_NM <- var(FullResi_NM)
print(VarFull_NM)

#Constant Variance Back Model
VarBack_NM <- var(BackResi_NM)
print(VarBack_NM)

#Shapiro Test Full Model
ShapiroTestFull_NM <- shapiro.test(FullResi_NM)
print(ShapiroTestFull_NM)

#Shapiro Test Back Model
ShapiroTestBack_NM <- shapiro.test(BackResi_NM)
print(ShapiroTestBack_NM)

#Not in the Question but the Plots for the Models MLR
#Plots for both the MLR Models 
#Full Model
par(mfrow = c(2, 2))  
plot(FullM_NM)  
par(mfrow = c(1, 1))  

#Back Model
plot(BackM_NM)  
par(mfrow = c(1, 1))  

#Recommendation for 1?
#Backwards Elimination Model
#More Explanation in Word File 