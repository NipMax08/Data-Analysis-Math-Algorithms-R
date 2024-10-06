##################################################
### PROG8435                                    ##
##################################################
#                                               ##
# PROG8435 Assignment 02                        ##
##################################################
# Written by Nirup Makwana                      ##
# ID: 8931418                                   #
#
##################################################
### Assignment 2                                ##
##################################################

#Reading the Data, Verifying the Data and Using the needed libraries
Data_NM <- read.csv('E:\\Big Data\\Sem 2\\R\\PROG8435-24W-Assign02.txt')
summary(Data_NM)
head(Data_NM)
library(ggplot2)
library("pastecs")
print(colnames(Data_NM))

#Question 1.1
colnames(Data_NM) <- paste0(colnames(Data_NM), "NM")
print(colnames(Data_NM))

#Question 1.2 Converting all the Character variables to Factor Variables
Data_NM[, sapply(Data_NM, is.character)] <- lapply(Data_NM[, sapply(Data_NM, is.character)], as.factor)

#Conducting analysis and different forms of Dimensionality Reduction methods to reduce the dimensions of the dataset 
#Question 1.2.1 & 1.2.2
colmiss_NM <- names(Data_NM)[colSums(is.na(Data_NM)) > 0]
print(colmiss_NM)
#Here CAEC has alot of NA's so we can remove that
Data_NM <- Data_NM[-c(11)]

#Question 1.2.3 Low Variance Filter
stat.desc(Data_NM)
table(Data_NM$WBCNM)
#here wbc has the low variance so we can remove that, furthermore rbc and wbc have high cor factor as well so we can remove 
Data_NM <- Data_NM[-c(17)]

#hgt is height in feet so we can remove
Data_NM <- Data_NM[-c(5)]

#index column removed
Data_NM <- Data_NM[-c(1)]

#Question 1.2.4 Correlation Filter
A_NM <- Data_NM[sapply(Data_NM, is.numeric)]
cor_NM <- cor(A_NM)
print(cor_NM)


print(colnames(Data_NM))

#Question 1.3.1 and 1.3.2 Outliers
plot(Data_NM$AgeNM, outline=TRUE, main = "Scatter Plot of Age", ylab = "Age")
boxplot(Data_NM$WeightNM, outline=TRUE, main = "Boxplot of Weight", ylab = "Weight")


iqr_NM <- IQR(Data_NM$AgeNM)
q1_NM <- quantile(Data_NM$AgeNM, 0.25)
q3_NM <- quantile(Data_NM$AgeNM, 0.75)
outliers_NM <- Data_NM$AgeNM < (q1_NM - 1.5 * iqr_NM) | Data_NM$AgeNM > (q3_NM + 1.5 * iqr_NM)
print(Data_NM$AgeNM[outliers_NM])
#here age of 173 is our outlier
#removing that outlier


iqr2_NM <- IQR(Data_NM$HeightNM)
q12_NM <- quantile(Data_NM$HeightNM, 0.25)
q32_NM <- quantile(Data_NM$HeightNM, 0.75)
outliers2_NM <- Data_NM$HeightNM < (q12_NM - 1.5 * iqr2_NM) | Data_NM$HeightNM > (q32_NM + 1.5 * iqr2_NM)
print(Data_NM$HeightNM[outliers2_NM])

iqr3_NM <- IQR(Data_NM$WeightNM)
q13_NM <- quantile(Data_NM$WeightNM, 0.25)
q33_NM <- quantile(Data_NM$WeightNM, 0.75)
outliers3_NM <- Data_NM$WeightNM < (q13_NM - 1.5 * iqr3_NM) | Data_NM$HeightNM > (q33_NM + 1.5 * iqr3_NM)
print(Data_NM$WeightNM[outliers3_NM])
#here there is someone with -20 weight so removing that 

iqr4_NM <- IQR(Data_NM$NObeyesdadNM)
q14_NM <- quantile(Data_NM$NObeyesdadNM, 0.25)
q34_NM <- quantile(Data_NM$NObeyesdadNM, 0.75)
outliers4_NM <- Data_NM$NObeyesdadNM < (q14_NM - 1.5 * iqr4_NM) | Data_NM$HeightNM > (q34_NM + 1.5 * iqr4_NM)
print(Data_NM$NObeyesdadNM[outliers4_NM])

#Removing Outliers
o_NM <- Data_NM$AgeNM == 173
Data_NM <- Data_NM[!o_NM, , drop = FALSE]

#Removing Outliers
o2_NM <- Data_NM$WeightNM == -20.6
Data_NM <- Data_NM[!o2_NM, , drop= FALSE]

#Question 2.1 Histogram for height
# Create a histogram with more breaks
hist(Data_NM$HeightNM, breaks = 30, main = "Histogram of Height", xlab = "Height", ylab = "Frequency")


#Question 2.2 histogram for weight
hist(Data_NM$WeightNM, breaks = 30, main = "Histogram of Weight", xlab = "Weight", ylab = "Frequency")


#Question 2.3 Scatter Plot between Weight and Age
ggplot(Data_NM, aes(x = WeightNM, y = AgeNM)) + 
  geom_point() + theme_minimal() +
  labs(title = " Scatter Plot of Weight and Age", x = "Weight", y = "Age")

#Question 2.5
cor(Data_NM$WeightNM, Data_NM$AgeNM)

#Question 3.1.1
qqplot(qnorm(ppoints(length(Data_NM$RBCNM))), Data_NM$RBCNM, main = "QQ Plot for RBC", xlab = "Theoretical", ylab = "Sample")

#Question 3.1.2
shapiro.test(Data_NM$RBCNM)

#Question 3.2
#here since the RBC count is normally distributed we shall use the T-Test 
t.test(x = Data_NM$RBCNM[Data_NM$GenderNM == "Female"], y = Data_NM$RBCNM[Data_NM$GenderNM == "Male"])

#Question 3.3.1
Anova_NM <- summary(aov(WeightNM ~ MTRANSNM, data = Data_NM))
print(Anova_NM)
ggplot(Data_NM, aes(x = MTRANSNM, y = WeightNM)) +
  geom_boxplot() + theme_minimal() +
  labs(title = "Boxplot of Weight by Transportation Method", x = "Transportation Method", y = "Weight")

#Question 3.3.2
Anova2_NM <- summary(aov(RBCNM ~ MTRANSNM, data = Data_NM))
print(Anova2_NM)

ggplot(Data_NM, aes(x = MTRANSNM, y = RBCNM)) +
  geom_boxplot() + theme_minimal() + 
  labs(title = "Boxplot of Red Blood Cell Count by Transportation Method", x = "Transportation Method", y = "RBC")
