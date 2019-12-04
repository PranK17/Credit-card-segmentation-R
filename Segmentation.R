#clear all data
rm(list=ls())
#Set Working directory
setwd("../Project 3/")

####################Loading and Installing Libraries that are required for project######################
#install.packages("psych")
#install.packages("scales")
#install.packages("gplots")
#install.packages("dummies")
#install.packages("ggplot2")
#install.packages("corrgram")
#install.packages("DMwR")
#install.packages("caret")
#install.packages("randomForest")
#install.packages("unbalanced")
#install.packages("C50")
#install.packages("e10")
#install.packages("MASS")
#install.packages("rpart")
#install.packages("gbm")
#install.packages("sampling")
#install.packages("ROSE")
#install.packages("DataCombine")
#install.packages("ggplot2")
#install.packages("rpart")
#install.packages("e1071")
#install.packages("unbalanced")
#install.packages("GPArotation")
#install.packages("dplyr")
#install.packages("NbClust")
#install.packages(NbClust)
#install.packages(purrr)
#install.packages("LICORS")
x=c("tables","LICORS","purrr","factoextra","gridExtra","tables","NbClust","dplyr","GPArotation","e1071","rpart","scales","psych","gplots","ggplot2","corrgram","DMwR","caret","randomForest","unbalanced","dummies","MASS","rpart","gbm","sampling","C50")
lapply(x,require,character.only = TRUE)


#Loading train and test data
san_train_=read.csv("credit-card-data.csv", header=T,na.strings = c("","","NA"))

#Data Exploration
str(san_train_)
dim(san_train_)
names(san_train_)
summary(san_train_)
#remove factor variable
san_train<-san_train_[,-1]

#--------------------------------------- Missing Value Analysis --------------------------------------------------
# Total number of missing values present in whole datset 
sum(is.na(san_train))
# Missing values present in each column in the dataset
apply(san_train,2,function(x){sum(is.na(x))}) 
# Store these missing values present in each variable in to data frame 
missing_val = data.frame(apply(san_train,2,function(x){sum(is.na(x))}))
# Creating new variable Column with values as rownames 
missing_val$Columns = row.names(missing_val)
# Rename column 1 in missing_val dataframe
names(missing_val)[1] =  "Missing_percentage"
# Lets calculate  percentage of missing values
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(san_train))*100
# sort the missing percentage in descending order
missing_val = missing_val[order(-missing_val$Missing_percentage),]
# Delete rownames 
row.names(missing_val) <- NULL
# Re order the columns for clear understanding
missing_val <- missing_val[,c(2,1)]


#Mean Method
san_train$MINIMUM_PAYMENTS[is.na(san_train$MINIMUM_PAYMENTS)] = mean(san_train$MINIMUM_PAYMENTS, na.rm = T)
#check for Missing value
apply(san_train,2,function(x){sum(is.na(x))}) 
#We will remove single missing value
san_train = na.omit(san_train)
#lets check data again
dim(san_train)


#-------------------------------------EXPORATORY DATA ANALYSIS---------------------------------------------
hist(san_train)


#--------------------------------------Frequency Data cleaning--------------------------------------------
range(san_train$BALANCE_FREQUENCY)#0-1
range(san_train$PURCHASES_FREQUENCY)#0-1
range(san_train$ONEOFF_PURCHASES_FREQUENCY)#0-1
range(san_train$PURCHASES_INSTALLMENTS_FREQUENCY)#0-1
range(san_train$CASH_ADVANCE_FREQUENCY)

san_train$CASH_ADVANCE_FREQUENCY[san_train$CASH_ADVANCE_FREQUENCY>1]
san_train = na.omit(san_train)
dim(san_train)

#--------------------------------------Outlier Analysis----------------------------------------------------
# Boxplots-Distribution and outlier check
cnames<-c("BALANCE", "BALANCE_FREQUENCY","PURCHASES", "ONEOFF_PURCHASES", "INSTALLMENTS_PURCHASES","CASH_ADVANCE","PURCHASES_FREQUENCY","ONEOFF_PURCHASES_FREQUENCY","PURCHASES_INSTALLMENTS_FREQUENCY","CASH_ADVANCE_FREQUENCY","CASH_ADVANCE_TRX","PURCHASES_TRX","CREDIT_LIMIT","PAYMENTS","MINIMUM_PAYMENTS","PRC_FULL_PAYMENT")
for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = cnames[i]), data = san_train)+
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i])+
           ggtitle(paste("Box plot for",cnames[i])))
}



gridExtra::grid.arrange(gn1,gn2,gn3,gn4,gn5,ncol=2)
gridExtra::grid.arrange(gn6,gn7,gn8,gn9,gn10,ncol=2)
gridExtra::grid.arrange(gn11,gn12,gn13,gn14,gn15,gn16,ncol=2)

summary(san_train)
#Data Preparation Outlier Treatment
for (i in cnames) {
  Q1<-quantile(san_train[,i],0.25,names = FALSE)
  Q3<-quantile(san_train[,i],0.75,names = FALSE)
  UL <- Q3 + (1.5*IQR(san_train[,i]))
  LL <- Q1 - (1.5*IQR(san_train[,i]))
  min1<-which(san_train[,i]<LL)
  max1<-which(san_train[,i]>UL)
  san_train<-as.matrix(san_train)
  san_train[min1] <- LL
  san_train[max1] <- UL
  san_train<-as.data.frame(san_train)
}
summary(san_train)

#------------------------------------------------KIT--------------------------------------------
#Creating New Variables & Advanced Data Preparation
#monthly average purchase
san_train$Monthly_Avg_expns <- san_train$PURCHASES/(san_train$PURCHASES_FREQUENCY*san_train$TENURE)
san_train$Monthly_Avg_expns[is.nan(san_train$Monthly_Avg_expns)] <- 0
san_train$Monthly_Avg_expns[san_train$Monthly_Avg_expns==Inf] <- 0

#monthly cash advance amount
san_train$Monthly_CASH_ADVANCE <- san_train$CASH_ADVANCE/(san_train$CASH_ADVANCE_FREQUENCY*san_train$TENURE)
san_train$Monthly_CASH_ADVANCE[is.nan(san_train$Monthly_CASH_ADVANCE)] <- 0
san_train$Monthly_CASH_ADVANCE[san_train$Monthly_CASH_ADVANCE==Inf] <- 0


#purchases by type
san_train$Monthly_INSTALLMENTS_PURCHASES  <- san_train$INSTALLMENTS_PURCHASES/(san_train$PURCHASES_INSTALLMENTS_FREQUENCY*san_train$TENURE)
san_train$Monthly_INSTALLMENTS_PURCHASES[is.nan(san_train$Monthly_INSTALLMENTS_PURCHASES)] <- 0
san_train$Monthly_INSTALLMENTS_PURCHASES[san_train$Monthly_INSTALLMENTS_PURCHASES==Inf] <- 0

san_train$Monthly_ONEOFF_PURCHASES  <- san_train$ONEOFF_PURCHASES/(san_train$ONEOFF_PURCHASES_FREQUENCY*san_train$TENURE)
san_train$Monthly_ONEOFF_PURCHASES[is.nan(san_train$Monthly_ONEOFF_PURCHASES)] <- 0
san_train$Monthly_ONEOFF_PURCHASES[san_train$Monthly_ONEOFF_PURCHASES==Inf] <- 0

#average amount per purchase
san_train$AVG_AMT_PRC=san_train$PURCHASES/san_train$PURCHASES_TRX
san_train$AVG_AMT_PRC[is.nan(san_train$AVG_AMT_PRC)] <- 0
san_train$AVG_AMT_PRC[san_train$AVG_AMT_PRC==Inf] <- 0

#limit usage(balance to credit limit ratio)
san_train$LIMIT_USAGE=san_train$BALANCE/san_train$CREDIT_LIMIT
san_train$LIMIT_USAGE[is.nan(san_train$LIMIT_USAGE)] <- 0
san_train$LIMIT_USAGE[san_train$LIMIT_USAGE==Inf] <- 0

#payments to minimum payments ratio
san_train$PAY_MIN_PAY=san_train$PAYMENTS/san_train$MINIMUM_PAYMENTS
san_train$PAY_MIN_PAY[is.nan(san_train$PAY_MIN_PAY)] <- 0
san_train$PAY_MIN_PAY[san_train$PAY_MIN_PAY==Inf] <- 0


#---------------------------------------------Feature selection--------------------------------
corrm <- cor(san_train)
corrgram(san_train, order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")


#--------------------------------------------standardizing the data----------------------------------
inputdata_final=scale(san_train)
head(inputdata_final)
#--------------------------------------------Kmeans------------------------------------------------
#Elbow Method for finding the optimal number of clusters
data=inputdata_final

set.seed(123)
# function to calculate total intra-cluster sum of square 
iss <- function(k) {
  kmeans(data,k,iter.max=100,nstart=100,algorithm="Lloyd" )$tot.withinss
}

k.values <- 1:20
iss_values <- map_dbl(k.values, iss)

plot(k.values, iss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total intra-clusters sum of squares")


#We will choose K =8
set.seed(124)
k8<- kmeans(data, centers = 8, nstart = 25)
str(k8)
k8$centers
k8$totss
k8$withinss
k8$tot.withinss
#library("fviz_nbclust")
fviz_cluster(k8, data = data)
k8

#--------------------------------------------factor analysis-----------------------------------
scree(corrm, factors=T, pc=T, main="scree plot", hline=NULL, add=FALSE) ### SCREE PLOT
data.frame(eigen(corrm)$values)
eigen_values <- mutate(data.frame(eigen(corrm)$values)
                       ,cum_sum_eigen=cumsum(eigen.corrm..values)
                       , pct_var=eigen.corrm..values/sum(eigen.corrm..values)
                       , cum_pct_var=cum_sum_eigen/sum(eigen.corrm..values))  # CALCULATING VARIANCE, CUMULATIVE VARIANCE etc... 
FA<-fa(r=corrm, 10, rotate="varimax", fm="ml")               ### CONDUCTING FACTOR ANALYSIS
FA_SORT<-fa.sort(FA)                                         ### SORTING THE LOADINGS
ls(FA_SORT)                                                  ### LISTING OUT THE OBJECTS
FA_SORT$loadings
Loadings<-data.frame(FA_SORT$loadings[1:ncol(san_train),]) ### CAPTURING ONLY LOADINGS INTO DATA FRAME
write.csv(Loadings, "loading.csv") ### SAVING THE FILE

vars <- c("ONEOFF_PURCHASES","PURCHASES","PAYMENTS","PURCHASES_TRX","AVG_AMT_PRC","Monthly_Avg_expns","Monthly_ONEOFF_PURCHASES","PURCHASES_FREQUENCY","PURCHASES_INSTALLMENTS_FREQUENCY","INSTALLMENTS_PURCHASES")
data<-san_train[vars]

data<-scale(data)

k8plus<-kmeanspp(data,k=8)
str(k8plus)
k8plus$cluster
file_k8plus<-cbind(san_train[vars],k8plus$cluster)
file_k8plus
write.csv(file_k8plus,"kmeansplusr.csv") 
fviz_cluster(k8plus, data = data)
