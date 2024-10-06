##################################################
### PROG8435                                    ##
##################################################
#                                               ##
# PROG8435 Assignment 03                        ##
##################################################
# Written by Nirup Makwana                      ##
# ID: 8931418                                   #
#
##################################################
### Assignment 3 Clustering                     ##
##################################################

#Reading the Data and Descriptive Analysis and loading the libraries 
IData_NM <- read.csv('E:\\Big Data\\Sem 2\\R\\PROG8435-24W-Assign03.txt')
summary(IData_NM)
head(IData_NM)
library(ggplot2)
library(dplyr)    
library(cluster)

for(col in names(IData_NM)) {
  print(
    ggplot(IData_NM, aes(x = "", y = .data[[col]])) +
      geom_boxplot() +
      labs(y = col, title = paste("Boxplot of", col)) + theme_minimal() +
      theme_light()  # Using a light theme for a clean look
  )
}
#Histograms
hist(IData_NM$Milk, breaks = 30, col = "skyblue", border = "black",
     xlab = "Milk", ylab = "Frequency", main = "Histogram of Milk")

hist(IData_NM$Groc, breaks = 30, col = "skyblue", border = "black",
     xlab = "Groc", ylab = "Frequency", main = "Histogram of Groc")

hist(IData_NM$Froz, breaks = 30, col = "skyblue", border = "black",
     xlab = "Froz", ylab = "Frequency", main = "Histogram of Froz")

hist(IData_NM$Deli, breaks = 30, col = "skyblue", border = "black",
     xlab = "Deli", ylab = "Frequency", main = "Histogram of Deli")

hist(IData_NM$Frsh, breaks = 30, col = "skyblue", border = "black",
     xlab = "Frsh", ylab = "Frequency", main = "Histogram of Frsh")

hist(IData_NM$Clen, breaks = 30, col = "skyblue", border = "black",
     xlab = "Clen", ylab = "Frequency", main = "Histogram of Clen")

#Standardizing the data
IDataSta_NM <- as.data.frame(scale(IData_NM))
summary(IDataSta_NM)

IDSC_NM <- select(IDataSta_NM, Milk, Froz)

WSS_NM <- sapply(2:7, function(k){
  kmeans(IDSC_NM, centers = k, nstart = 20)$tot.withinss
})

plot(2:7, WSS_NM, type = "b", pch = 19, xlab = "Number of clusters", ylab = "Total within-cluster sum of squares")

K_NM <- 2:7
KMR_NM <- lapply(K_NM, function(k) kmeans(IDSC_NM, centers = k, nstart = 20))

#Clusters for k =2,3,4,5,6,7

for (k in 2:7) {
  KMeansR_NM <- kmeans(IDSC_NM, centers = k, nstart = 100)
  
  NC_NM <- length(unique(KMeansR_NM$cluster))
  print(
    ggplot(IDSC_NM, aes(x = Milk, y = Froz, color = as.factor(KMeansR_NM$cluster))) +
      geom_point() +
      scale_color_discrete() +
      labs(title = paste("Cluster visualization with k =", k), x = "Milk", y = "Frozen") +
      theme_minimal()
  )
}
set.seed(123)
KOP_NM <- 4
KOP0_NM <- 3
KOP2_NM <- 5
KMeansR1_NM <- kmeans(IDSC_NM, centers = KOP_NM, nstart = 20)
KMeansR0_NM <- kmeans(IDSC_NM, centers = KOP0_NM, nstart = 20)
KMeansR2_NM <- kmeans(IDSC_NM, centers = KOP2_NM, nstart = 20)


ggplot(IDSC_NM, aes(x = Milk, y = Froz)) +
  geom_point(aes(color = as.factor(KMeansR1_NM$cluster))) +  # Coloring points by cluster
  scale_color_manual(values = rainbow(KOP_NM)) +  # Using different colors for each cluster
  labs(title = "Cluster visualization with k=4", x = "Milk", y = "Frozen")

ggplot(IDSC_NM, aes(x = Milk, y = Froz)) +
  geom_point(aes(color = as.factor(KMeansR0_NM$cluster))) +  # Coloring points by cluster
  scale_color_manual(values = rainbow(KOP0_NM)) +  # Using different colors for each cluster
  labs(title = "Cluster visualization with k=3", x = "Milk", y = "Frozen")

ggplot(IDSC_NM, aes(x = Milk, y = Froz)) + 
  geom_point(aes(color = as.factor(KMeansR2_NM$cluster))) + #Coloring points by cluster 
  scale_color_manual(values = rainbow(KOP2_NM)) + #Using different colors for different clusters
  labs(title = "Cluster visualization with k=5", x = "Milk", y = "Frozen")

#Summary Table
ClsA_NM <- data.frame(Cluster = KMeansR1_NM$cluster)
CD_NM <- cbind(ClsA_NM, IDSC_NM)
summarytable_NM <- aggregate(. ~ Cluster, data = CD_NM, FUN = function(x) c(mean = mean(x), median = median(x), sd = sd(x), count = length(x)))
print("Summary table for k=4")
print(summarytable_NM)
