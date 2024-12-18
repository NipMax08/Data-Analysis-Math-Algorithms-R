##################################################
### PROG8435                                    ##
##################################################
#                                               ##
# PROG8435 Assignment 05                        ##
##################################################
# Written by Nirup Makwana                      ##
# ID: 8931418                                   #
#
##################################################
### Assignment 5 Classification                 ##
##################################################

# Check if any graphics devices are open and close them. This is useful for resetting the plotting window.
if(!is.null(dev.list())) dev.off()

# Clear the console to ensure a clean slate for output. This helps improve readability of the output.
cat("\014") # The "\014" character is a form control character that clears the console.

# Remove all objects from the R environment to prevent any data or variable conflicts.
rm(list=ls())

#Setting Working Directory
setwd('E:\\Big Data\\Sem 2\\R')

#Standardize and Factor before the split and outliers or after the split and outliers?
#Wildly different accuracies 
#Un-standardized and Factored similar to fully standardized and factor before the split


#Proceeded with Unstandardized but factored data, did this before the split of train and test data 
#Reading the data and checking its structure and summary
CData_NM <- read.table('E:\\Big Data\\Sem 2\\R\\PROG8435-24W-Assign04.txt', header = TRUE, sep = ",")
head(CData_NM)
print(colnames(CData_NM))

#Libraries
library("pastecs")
library("corrgram")
library("polycor")
library("klaR")
library("partykit")
library("caret")

# Removing data with Temp > 155
CData_NM <- CData_NM[CData_NM$Temp < 155,]
#Creating Variable HB_NM and deleting variable 
CData_NM$HB <- as.factor(ifelse(CData_NM$RBC <= 1700,1,0))
CData_NM$RBC <- NULL

#Renaming the Columns
names(CData_NM) <- c("Hour_of_the_Day", "Solar_Radiation", "Wind_Speed", "Snow_Fall", "National_Holiday", "Humidity", "Rain_Fall", "Temperature_(C)", "Season_of_the_Year", "HB")
colnames(CData_NM) <- paste0(colnames(CData_NM), "_NM")
print(colnames(CData))

#Standardization
#NumCols_NM <- sapply(CData_NM, is.numeric)
#CData_NM[, NumCols_NM] <- scale(CData_NM[, NumCols_NM])
#summary(CData_NM)

#Transforming into Factors
print(colnames(CData_NM))
CData_NM <- as.data.frame(unclass(CData_NM), stringsAsFactors = TRUE)


set.seed(123)  
Split_NM <- createDataPartition(CData_NM$HB_NM, p = 0.75, list = FALSE)
Train_NM <- CData_NM[Split_NM, ]
Test_NM <- CData_NM[-Split_NM, ]


#Checking the data
str(Train_NM)
head(Train_NM)
summary(Train_NM)
head(Train_NM$HB_NM)
#Writing the data into the session
#write.table(Train_NM, file = "PROG8435-24W-Assign04Train_NM", sep = ",", row.names = FALSE)
#write.table(Test_NM, file = "PROG8435-24W-Assign04Test_NM", sep = ",", row.names = FALSE)



#Missing Value Filter
Miss_Col_NM <- names(Train_NM)[colSums(is.na(Train_NM)) > 0]
print(Miss_Col_NM)

#Low Variance Filter
stat.desc(Train_NM)

#Correlation Filter
Cor_NM <- cor(Train_NM[, sapply(Train_NM, is.numeric)], method = "spearman")
round(Cor_NM, 2)

#Pictorial Representation
corrgram(Train_NM, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Correlations")
#pairs(Train_NM[sapply(Train_NM, is.numeric)], pch=46)



#Boxplots
par(mfrow=c(3,2))
for (i in 1:ncol(CData_NM)) {
  if (is.numeric(CData_NM[,i])) {
    boxplot(CData_NM[,i], main=names(CData_NM)[i], xlab="", horizontal=TRUE, col = "red")
  }
}

# Histograms
par(mfrow = c(3, 2))
for (i in 1:ncol(Train_NM)) {
  if (is.numeric(Train_NM[, i])) {
    hist(Train_NM[, i], main = names(Train_NM)[i], xlab = "", col = "red")
  }
}
#par(mfrow=c(1,1))
#hist(Train_NM$`Temperature_(C)_NM`)

#Here I will be studying the outliers for the dimensions where outliers are prevalent and then I checked it with the real life data for Seoul 
#Outliers and Study of Data for the Temperature Variable
summary(CData_NM$Temperature..C._NM)
IQRT_NM <- quantile(CData_NM$Temperature..C._NM, 0.75) - quantile(CData_NM$Temperature..C._NM, 0.25)
Up1_NM <- quantile(CData_NM$Temperature..C._NM, 0.75) + 1.5 * IQRT_NM
Low1_NM <- quantile(CData_NM$Temperature..C._NM, 0.25) - 1.5 * IQRT_NM
Up1_NM
Low1_NM
OUT_NM <- CData_NM$Temperature..C._NM[CData_NM$Temperature..C._NM > Up1_NM]
OLT_NM <- CData_NM$Temperature..C._NM[CData_NM$Temperature..C._NM < Low1_NM]
OUT_NM
OLT_NM

#Outliers and Study of Data for the Solar Radiation Variable
summary(Train_NM$Solar.Radiation_NM)
IQRS_NM <- quantile(Train_NM$Solar.Radiation_NM, 0.75) - quantile(Train_NM$Solar.Radiation_NM, 0.25)
Up2_NM <- quantile(Train_NM$Solar.Radiation_NM, 0.75) + 1.5 * IQRS_NM
Low2_NM <- quantile(Train_NM$Solar.Radiation_NM, 0.25) - 1.5 * IQRS_NM
Up2_NM
Low2_NM
OUS_NM <- Train_NM$Solar.Radiation_NM[Train_NM$Solar.Radiation_NM > Up2_NM]
OLS_NM <- Train_NM$Solar.Radiation_NM[Train_NM$Solar.Radiation_NM < Low2_NM]
OUS_NM
OLS_NM

#Outliers and Study of Data for the Wind Speed Variable
summary(Train_NM$Wind.Speed_NM)
IQRW_NM <- quantile(Train_NM$Wind.Speed_NM, 0.75) - quantile(Train_NM$Wind.Speed_NM, 0.25)
Up3_NM <- quantile(Train_NM$Wind.Speed_NM, 0.75) + 1.5 * IQRW_NM
Low3_NM <- quantile(Train_NM$Wind.Speed_NM, 0.25) - 1.5 * IQRW_NM
Up3_NM
Low3_NM
OUW_NM <- Train_NM$Wind.Speed_NM[Train_NM$Wind.Speed_NM > Up3_NM]
OLW_NM <- Train_NM$Wind.Speed_NM[Train_NM$Wind.Speed_NM < Low3_NM]
OUW_NM
OLW_NM

#Outliers and Study of Data for the Snow Fall Variable
summary(Train_NM$Snow.Fall_NM)
IQRSn_NM <- quantile(Train_NM$Snow.Fall_NM, 0.75) - quantile(Train_NM$Snow.Fall_NM, 0.25)
Up4_NM <- quantile(Train_NM$Snow.Fall_NM, 0.75) + 1.5 * IQRSn_NM
Low4_NM <- quantile(Train_NM$Snow.Fall_NM, 0.25) - 1.5 * IQRSn_NM
Up4_NM
Low4_NM
OUSn_NM <- Train_NM$Snow.Fall_NM[Train_NM$Snow.Fall_NM > Up4_NM]
OLSn_NM <- Train_NM$Snow.Fall_NM[Train_NM$Snow.Fall_NM < Low4_NM]
OUSn_NM
OLSn_NM

#Outliers and Study of Data for the Rain Fall Variable
summary(Train_NM$Rain.Fall_NM)
IQRR_NM <- quantile(Train_NM$Rain_Fall_NM, 0.75) - quantile(Train_NM$Rain_Fall_NM, 0.25)
Up5_NM <- quantile(Train_NM$Rain_Fall_NM, 0.75) + 1.5 * IQRR_NM
Low5_NM <- quantile(Train_NM$Rain_Fall_NM, 0.25) - 1.5 * IQRR_NM
Up5_NM
Low5_NM
OUR_NM <- Train_NM$Rain_Fall_NM[Train_NM$Rain_Fall_NM > Up5_NM]
OLR_NM <- Train_NM$Rain_Fall_NM[Train_NM$Rain_Fall_NM < Low5_NM]
OUR_NM
OLR_NM



#Standardization(Won't be doing)
#NumCols_NM <- sapply(Train_NM, is.numeric)
#Train_NM[, NumCols_NM] <- scale(Train_NM[, NumCols_NM])
#summary(Train_NM)


#Boxplots
par(mfrow=c(3,2))
for (i in 1:ncol(Train_NM)) {
  if (is.numeric(Train_NM[,i])) {
    boxplot(Train_NM[,i], main=names(Train_NM)[i], xlab="", horizontal=TRUE)
  }
}

#Histograms
par(mfrow=c(3, 2))
for (i in 1:ncol(Train_NM)) {
  if (is.numeric(Train_NM[,i])) {
    hist(Train_NM[,i], main=names(Train_NM)[i], xlab="")
  }
}

print(colnames(Train_NM))

#Full Model
FullML_NM <- glm(HB_NM ~ ., family = "binomial", data = Train_NM, na.action = na.omit)
summary(FullML_NM)
#Residuals
RFM_NM <- residuals(FullML_NM)
head(RFM_NM)
plot(RFM_NM)
#Cook's Distance 
par(mfrow = c(1,1))
plot(FullML_NM,which=4, id.n=6)
#Plots
#par(mfrow = c(2, 2))  
#plot(FullML_NM)
#par(mfrow = c(1,1))

# Full Model - Confusion Matrix using Train Data
PredF_NM <- factor(ifelse(predict(FullML_NM, newdata = Train_NM, type = "response") > 0.5, "1", "0"))
ConfMF_NM <- table(Train_NM$HB_NM, PredF_NM)
ConfMF_NM
AccFTr_NM <- sum(diag(ConfMF_NM)) / sum(ConfMF_NM)
print(paste("Accuracy: ", accuracy))


#Stepwise Model
StepML_NM <- step(FullML_NM)
summary(StepML_NM)
#Residuals
RSM_NM <- residuals(StepML_NM)
head(RSM_NM)
plot(RSM_NM)
#Cook's Distance 
par(mfrow = c(1,1))
plot(StepML_NM,which=4, id.n=6)
#Plots
#par(mfrow = c(2, 2))  
#plot(StepML_NM)
#par(mfrow = c(1,1))

#Stepwise Model Part B
StartTimeS_NM <- Sys.time()
StepML_NM <- step(FullML_NM)
EndTimeS_NM <- Sys.time()
STime_NM <- EndTimeS_NM - StartTimeS_NM
summary(StepML_NM)
#Stepwise Model - Confusion Matrix using Train Data
PredS_NM <- factor(ifelse(predict(StepML_NM, newdata = Train_NM, type = "response") > 0.5, "1", "0"))
ConfMS_NM <- table(Train_NM$HB_NM, PredS_NM)
ConfMS_NM
AccSTr_NM <- sum(diag(ConfMS_NM)) / sum(ConfMS_NM)
print(paste("Accuracy: ", AccSTr_NM))

#Naïve-Bayes Classification
StartTimeNB_NM <- Sys.time()
NB_NM <- NaiveBayes(HB_NM ~ ., data = Train_NM, na.action = na.omit)
EndTimeNB_NM <- Sys.time()
NBTime_NM <- EndTimeNB_NM - StartTimeNB_NM
PredNB_NM <- predict(NB_NM, newdata=Train_NM)
ConfNB_NM <- table(Actual=Train_NM$HB_NM, Predicted=PredNB_NM$class)
ConfNB_NM
NBTime_NM

#Recursive Partitioning Analysis
StartTimeRP_NM <- Sys.time()
RP_NM <- ctree(HB_NM ~ ., data = Train_NM)
EndTimeRP_NM <- Sys.time()
RPTime_NM <- EndTimeRP_NM - StartTimeRP_NM
#plot(RP_NM, gp = gpar(fontsize = 10), node_terminal_args = list(fontsize = 8), inner_panel = node_bivplot)
PredRP_NM <- predict(RP_NM, newdata=Train_NM)
ConfRP_NM <- table(Actual=Train_NM$HB_NM, Predicted=PredRP_NM)
ConfRP_NM
RPTime_NM

#Testing Phase
head(Test_NM)
Test_NM <- as.data.frame(unclass(Test_NM), stringsAsFactors = TRUE)
summary(Test_NM)

#Full Model
FullTest_NM <- factor(ifelse(predict(FullML_NM, newdata = Test_NM, type = "response") > 0.5, "1", "0"))
ConfMFTest_NM <- table(Test_NM$HB_NM, FullTest_NM)
ConfMFTest_NM

#For Stepwise
RStep_NM <- predict(StepML_NM, newdata = Test_NM, type = "response")
ClassStep_NM <- ifelse(RStep_NM > 0.5, "1", "0")
ConfStepTest_NM <- table(Test_NM$HB_NM, ClassStep_NM, dnn = list("Actual", "Predicted"))
ConfStepTest_NM
AccSt_NM <- sum(diag(ConfStepTest_NM)) / sum(ConfStepTest_NM)
print(paste("Accuracy: ", AccSt_NM))

#Naive Bayes
PredNBTest_NM <- predict(NB_NM, newdata = Test_NM)
ConfNBTest_NM <- table(Actual=Test_NM$HB_NM, Predicted=PredNBTest_NM$class)
ConfNBTest_NM
AccNB_NM <- sum(diag(ConfNBTest_NM)) / sum(ConfNBTest_NM)
print(paste("Accuracy: ", AccNB_NM))

#Recursive Partitioning 
PredRPTest_NM <- predict(RP_NM, newdata = Test_NM)
ConfRPTest_NM <- table(Actual=Test_NM$HB_NM, Predicted=PredRPTest_NM)
ConfRPTest_NM
AccRP_NM <- sum(diag(ConfRPTest_NM)) / sum(ConfRPTest_NM)
print(paste("Accuracy: ", AccRP_NM))