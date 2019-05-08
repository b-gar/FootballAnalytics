# Source: http://stats.ncaa.org/rankings/change_sport_year_div #
# *Note* Copy and Pasted From Header Down (ommiting the Reclassifying Teams at Bottom) into Notepad ++ 
# Already Sorted by Team Name From Website

library(tidyr)
library(dplyr)
library(corrplot)
library(factoextra)
library(NbClust)
library(ROCR)

# Import Punting Data, Seperate Record into Wins/Losses, Calculate Win %
Punt2017 <- read.csv("2017 - Punt.txt", sep = "\t", stringsAsFactors = FALSE)
Punt2017 <- separate(Punt2017, W.L, c("Wins", "Losses"), sep = "-")
Punt2017$Wins <- as.integer(Punt2017$Wins)
Punt2017$Losses <- as.integer(Punt2017$Losses)
Punt2017$Win_Percentage <- Punt2017$Wins/(Punt2017$Wins+Punt2017$Losses)
Punt2017 <- Punt2017[,c(2,10,11)]

# Import Kick Return Data
KR2017 <- read.csv("2017 - KR.txt", sep = "\t", stringsAsFactors = FALSE)
KR2017 <- KR2017[,c(2,8)]

# Import Penalty Data
Pen2017 <- read.csv("2017 - Penalties.txt", sep = "\t", stringsAsFactors = FALSE)
Pen2017 <- Pen2017[,c(2,7)]

# Import 3rd Down Data
Third2017 <- read.csv("2017 - 3D.txt", sep = "\t", stringsAsFactors = FALSE)
Third2017 <- Third2017[,c(2,7)]

# Import Time of Possession Data, Split TOP into Minute/Second, Combine into Seconds
TOP2017 <- read.csv("2017 - TOP.txt", sep = "\t", stringsAsFactors = FALSE)
TOP2017 <- TOP2017[,c(2,6)]
TOP2017 <- separate(TOP2017, AvgTOP, c("Minute", "Second"), sep = ":")
TOP2017$Minute <- as.numeric(TOP2017$Minute)
TOP2017$Second <- as.numeric(TOP2017$Second)
TOP2017$AvgTOP <- floor((TOP2017$Minute * 60) + TOP2017$Second)
TOP2017 <- TOP2017[,c(1,4)]

# Import Turnover Margin Data
TOM2017 <- read.csv("2017 - TOM.txt", sep = "\t", stringsAsFactors = FALSE)
TOM2017 <- TOM2017[,c(2,11)]

# Import Total Offense Data
TO2017 <- read.csv("2017 - TO.txt", sep = "\t", stringsAsFactors = FALSE)
TO2017 <- TO2017[,c(2,7)]

# Import Game Attendance Data (Came From Reports Tab)
Att2017 <- read.csv("2017 - Attendance.txt", sep = "\t")

# Check for Name/Row Matching (2018 Data Had Issues)
All <- data.frame("Punt2017" = Punt2017$Team,"KR2017" = KR2017$Team, "Pen2017" = Pen2017$Team, 
                  "Third2017" = Third2017$Team, "TO2017" = TO2017$Team,"TOM2017" = TOM2017$Team, 
                  "TOP2017" = TOP2017$Team)   # All Match

# Attendance - Check for Team Name Discrepancies
All <- separate(All, Punt2017, c("Team"), sep = "\\),\\(|\\)|\\(", remove = FALSE)
test <- data.frame("Team" = All$Team, "Attendance_Team" = Att2017$Institution[1:242])
# Belhaven (Row 18),Brevard (Row 27),Finlandia (Row 71),McMurry (Row 134)
Att2017 <- Att2017[-c(18,27,71,134),c(1,4)]
rownames(Att2017) <- seq(length = nrow(Att2017))
Att2017$Avg.Attendance <- as.numeric(gsub(",","",Att2017$Avg.Attendance))

# Clean up Environment
rm(All)
rm(test)

# Make Data Frame with All Data
Data <- data.frame("Team" = KR2017$Team,"Win_Percentage" = Punt2017$Win_Percentage,
                   "Average_Attendance" = Att2017$Avg.Attendance,"KR_Avg_Yards" = KR2017$Avg,
                   "Avg_Penalty_Yards_Per_Game" = Pen2017$YPG, "Net_Punting_Yards" = Punt2017$Net.Yds, 
                   "Third_Down_Percentage" = Third2017$Pct,"Average_Yards_Per_Play" = TO2017$Yds.Play, 
                   "Turnover_Margin" = TOM2017$Margin,"Average_Time_of_Possesion" = TOP2017$AvgTOP)

Data$Team <- as.character(Data$Team)

# Data for Tests w/o Team Name
data.simple <- Data[,-1]
data.simpler <- Data[,-c(1,8)]

# Correlation/Regression
r <- cor(data.simple)
corrplot(r)
fit <- lm(Win_Percentage ~ ., data = data.simple)
summary(fit)
varImp(fit)

# Correlation/Regression w/o Yards/Play (Average Yards/Play & 3rd Down % Had High Correlation)
r2 <- cor(data.simpler)
corrplot(r2)
fit2 <- lm(Win_Percentage ~ ., data = data.simpler)
summary(fit2)
varImp(fit2)

# Clean Environment
rm(r)
rm(fit)

## Cluster via K-Means ##

# Scale and Make New DF
Data.Scaled <- data.frame(scale(Data[,c(3:10)]))
Data.Scaled$Team <- Data$Team
Data.Scaled <- Data.Scaled[,c(9,1:8)]

# Find Optimal K
fviz_nbclust(Data.Scaled[,-1],kmeans,method = "wss")
nc <- NbClust(Data.Scaled[,-1],min.nc = 2,max.nc = 10,method = "kmeans") # 3 is Best Amount of Clusters

# Run K-Means
set.seed(123)
k3 <- kmeans(Data.Scaled[,-1], centers = 3)
cluster <- k3$cluster

# Create New DF with Cluster Assignment
Data.Clusters <- mutate(Data.Scaled, Cluster = cluster)

# Plot Clusters
ggplot(Data.Clusters, aes(x = Data$Win_Percentage, y = Data$Turnover_Margin, color = factor(Cluster))) +
  geom_point() + ggtitle("2017 D3 Football K-Means Clustering") + xlab("Win Percentage") + 
  ylab("Turnover Margin") + labs(color = "Cluster")

# Principle Component Analysis/MDS
pca <- data.frame(cmdscale(dist(Data.Clusters[,2:9])))
pca$cluster <- Data.Clusters$Cluster

# Plot using PCA Dimensions Instead
ggplot(pca, aes(x = X1, y = X2, color = factor(cluster))) + geom_point() + 
  ggtitle("2017 D3 Football PCA")+ xlab("Dimension 1") + ylab("Dimension 2") + labs(color = "Cluster")

## Prediction via Logistic Regression ##

# Create New Binary Predictor Column
Data.Clusters$Winning_Team <- Data$Win_Percentage > 0.5
Data.Clusters$Winning_Team[Data.Clusters$Winning_Team == "False"] <- 0
Data.Clusters$Winning_Team[Data.Clusters$Winning_Team == "True"] <- 1
Data$Winning_Team <- Data.Clusters$Winning_Team

# Create New DF
Data.Log <- Data.Clusters[,-c(1,7,10)]

# Run Logistic Regression
data.glm <- glm(Winning_Team ~ ., family = "binomial", data = Data.Log)

# Get Predictions
predict <- predict(data.glm, Data.Log, type = "response")

# ROCR Plot
rocr <- prediction(predict, Data.Log$Winning_Team)
rocrp <- performance(rocr, 'tpr','fpr')
rocr.cost <- performance(rocr, "cost")
plot(rocrp)

# Function for Optimal Cut Point
# Source: https://hopstat.wordpress.com/2014/12/19/a-small-introduction-to-the-rocr-package/
opt.cut = function(perf, pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
      cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)
}
print(opt.cut(rocrp, rocr))

# Put Predictions in DF Based on Optimal Cut Point
Data.Clusters$Predictions <- predict
Data.Clusters$Predictions[Data.Clusters$Predictions > 0.4463935] <- 1
Data.Clusters$Predictions[Data.Clusters$Predictions <= 0.4463935] <- 0

# Confusion Matrix
table(Data.Log$Winning_Team, Data.Clusters$Predictions) # 81% Accuracy

# Check Teams for Final Thoughts
final <- Data.Clusters[,c(1,10:12)]
final$Win_Percentage <- Data$Win_Percentage
final$Cluster[final$Cluster == 1 ] <- "Average"
final$Cluster[final$Cluster == 2 ] <- "Good"
final$Cluster[final$Cluster == 3 ] <- "Bad"
