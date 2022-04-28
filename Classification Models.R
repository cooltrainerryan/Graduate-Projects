####################################
####################################

setwd("C:\\Users\\rocho\\OneDrive\\Documents\\R\\Quantitative Methods\\Project")
library(class)
library(caret)
library(e1071)
library(dplyr)
library(e1071)
library(ggplot2)
library(lattice)
library(caret)
library(rpart)
library(rpart.plot)
##################################
##################################

marketing <- read.csv("marketingdata.csv", header=TRUE)

marketing$Year_Birth <- (2022-marketing$Year_Birth)
names(marketing)[names(marketing)=="Year_Birth"] <- "Age"

marketing$Income <- as.numeric(marketing$Income)
#marketing$Income <- log(marketing$Income)
#names(marketing)[names(marketing)=="Income"] <- "ln_Income"

marketing$Education <- as.factor(ifelse(marketing$Education=="2n Cycle" & marketing$Education=="Master", "Master",
                            ifelse(marketing$Education=="Graduation", "Bachelor", 
                                   ifelse(marketing$Education=="Basic", "Basic", 
                                          ifelse(marketing$Education=="PhD", "PhD", "Master")))))

marketing$Marital_Status <- as.factor(ifelse(marketing$Marital_Status==c("Single", "Alone", "Absurd", "YOLO"), "Single",
                                        ifelse(marketing$Marital_Status=="Married", "Married", 
                                               ifelse(marketing$Marital_Status=="Together", "Together",
                                                      ifelse(marketing$Marital_Status=="Divorced", "Divorced",
                                                             ifelse(marketing$Marital_Status=="Widow", "Widow", "Single"))))))


marketing$Complain <- as.factor(marketing$Complain)
marketing$AcceptedCmp1 <- as.factor(marketing$AcceptedCmp1)
marketing$AcceptedCmp2 <- as.factor(marketing$AcceptedCmp2)
marketing$AcceptedCmp3 <- as.factor(marketing$AcceptedCmp3)
marketing$AcceptedCmp4 <- as.factor(marketing$AcceptedCmp4)
marketing$AcceptedCmp5 <- as.factor(marketing$AcceptedCmp5)
marketing$Response <- as.factor(marketing$Response)

marketing$drunkie <- as.factor(ifelse(marketing$MntWines>303.94,1,0))


#####################################
### Logistic Regression

marketing.df <- marketing[ , -c(1,27, 28)]  # Drop ID column

# treat Education as categorical (R will create dummy variables)
#marketing.df$Education <- factor(marketing.df$Education, levels = c(1, 2, 3, 4), labels = c("Bachelor", "Basic", "Master", "PhD")) 

# partition data
set.seed(666)
train.index <- sample(c(1:dim(marketing.df)[1]), dim(marketing.df)[1]*0.6)  
train.df <- marketing.df[train.index, -9]
valid.df <- marketing.df[-train.index, -9]

# run logistic regression
# use glm() (general linear model) with family = "binomial" to fit a logistic 
# regression.
logit.reg <- glm(drunkie ~ ., data = train.df, family = "binomial") 
options(scipen=999)
summary(logit.reg)

# use predict() with type = "response" to compute predicted probabilities. 
logit.reg.pred <- predict(logit.reg, valid.df[,-26], type = "response")
head(logit.reg.pred,10)
# first 10 actual and predicted records
data.frame(actual = valid.df$drunkie[1:10], predicted = logit.reg.pred[1:10])

pred.class <- ifelse(logit.reg.pred > 0.5, 1,0 )

data.frame(actual = valid.df$drunkie[1:10], pred.class=pred.class[1:10], predicted = logit.reg.pred[1:10])

table(pred.class, valid.df$drunkie)


pred <- predict(logit.reg, valid.df)
confusionMatrix(as.factor(ifelse(pred > 0.5, 1, 0)), as.factor(valid.df$drunkie), positive="1")

stepwise <- stepAIC(logit.reg, direction = "both")
summary(stepwise)

pred.step <- predict(stepwise, valid.df)
confusionMatrix(as.factor(ifelse(pred.step > 0.5, 1, 0)), as.factor(valid.df$drunkie), positive="1")

###############################################################################################
###############################################################################################
### KNN

marketing <- read.csv("marketingdata.csv", header=TRUE)

marketing$Year_Birth <- (2022-marketing$Year_Birth)
names(marketing)[names(marketing)=="Year_Birth"] <- "Age"

marketing$Income <- as.numeric(marketing$Income)
#marketing$Income <- log(marketing$Income)
#names(marketing)[names(marketing)=="Income"] <- "ln_Income"

marketing$Education <- as.factor(ifelse(marketing$Education=="2n Cycle" & marketing$Education=="Master", "Master",
                                        ifelse(marketing$Education=="Graduation", "Bachelor", 
                                               ifelse(marketing$Education=="Basic", "Basic", 
                                                      ifelse(marketing$Education=="PhD", "PhD", "Master")))))

marketing$Marital_Status <- as.factor(ifelse(marketing$Marital_Status==c("Single", "Alone", "Absurd", "YOLO"), "Single",
                                             ifelse(marketing$Marital_Status=="Married", "Married", 
                                                    ifelse(marketing$Marital_Status=="Together", "Together",
                                                           ifelse(marketing$Marital_Status=="Divorced", "Divorced",
                                                                  ifelse(marketing$Marital_Status=="Widow", "Widow", "Single"))))))


marketing$Complain <- as.factor(marketing$Complain)
marketing$AcceptedCmp1 <- as.factor(marketing$AcceptedCmp1)
marketing$AcceptedCmp2 <- as.factor(marketing$AcceptedCmp2)
marketing$AcceptedCmp3 <- as.factor(marketing$AcceptedCmp3)
marketing$AcceptedCmp4 <- as.factor(marketing$AcceptedCmp4)
marketing$AcceptedCmp5 <- as.factor(marketing$AcceptedCmp5)
marketing$Response <- as.factor(marketing$Response)

marketing$drunkie <- as.factor(ifelse(marketing$MntWines>303.94,1,0))

marketing.df <- marketing

### Create new dummies
marketing.df$Basic <- as.factor(ifelse(marketing.df$Education=="Basic",1,0))
marketing.df$Bachelor <- as.factor(ifelse(marketing.df$Education=="Bachelor",1,0))
marketing.df$Master <- as.factor(ifelse(marketing.df$Education=="Master",1,0))
marketing.df$PhD <- as.factor(ifelse(marketing.df$Education=="PhD",1,0))

marketing.df$Divorced <- as.factor(ifelse(marketing.df$Marital_Status=="Divorced",1,0))
marketing.df$Married <- as.factor(ifelse(marketing.df$Marital_Status=="Married",1,0))
marketing.df$Together <- as.factor(ifelse(marketing.df$Marital_Status=="Together",1,0))
marketing.df$Single <- as.factor(ifelse(marketing.df$Marital_Status=="Single",1,0))
marketing.df$Widow <- as.factor(ifelse(marketing.df$Marital_Status=="Widow",1,0))

marketing.df <- marketing[ , -c(1,3,4,10,27,28)]  # Drop ID column

#############################################################################

set.seed(420)  
train.index <- sample(row.names(marketing.df), 0.6*dim(marketing.df)[1])
valid.index <- sample(setdiff(row.names(marketing.df), train.index))
test.index <-  setdiff(row.names(marketing.df), c(train.index, valid.index)) 
train.df <- marketing.df[train.index, ]
valid.df <- marketing.df[valid.index, ]
test.df <- marketing.df[test.index, ]

# normalization
train.norm.df <- train.df[,-24]
valid.norm.df <- valid.df[,-24]
test.norm.df <- test.df[,-24]
norm.values <- preProcess(train.df[, -24], method=c("center", "scale"))
train.norm.df <- predict(norm.values, train.df[, -24])
valid.norm.df <- predict(norm.values, valid.df[, -24])
test.norm.df <- predict(norm.values, test.df[, -24])

################################################


# optimal k
prediction <- numeric(15)

accuracy.df <- data.frame(k = seq(1, 15), overallaccuracy = rep(0, 15))
for(i in 1:15) {
  knn.pred <- class::knn(train = train.norm.df, 
                         test = valid.norm.df, 
                         cl = train.df$drunkie, k = i)
  accuracy.df[i, 2] <- confusionMatrix(knn.pred, 
                                       as.factor(valid.df$drunkie))$overall[1]
}

which(accuracy.df[,2] == max(accuracy.df[,2])) 


accuracy.df

#highest accuracy, k=3

### training
knn.predt <- class::knn(train = train.norm.df, 
                        test = train.norm.df, 
                        cl = train.df$drunkie, k = 11)

confusionMatrix(knn.predt, as.factor(train.df$drunkie), positive = "1")


### valid
knn.predv <- class::knn(train = train.norm.df, 
                        test = valid.norm.df, 
                        cl = train.df$drunkie, k = 11)

confusionMatrix(knn.predv, as.factor(valid.df$drunkie), positive = "1")


### test
knn.predtt <- class::knn(train = train.norm.df, 
                         test = test.norm.df, 
                         cl = train.df$drunkie, k = 3)
confusionMatrix(knn.predtt, as.factor(test.df$drunkie), positive = "1")




####################################
### Neural Net

library(class)
library(caret)
library(e1071)
library(neuralnet)
####################################
### Reset
marketing <- read.csv("marketingdata.csv", header=TRUE)

marketing$Year_Birth <- (2022-marketing$Year_Birth)
names(marketing)[names(marketing)=="Year_Birth"] <- "Age"

marketing$Income <- as.numeric(marketing$Income)
#marketing$Income <- log(marketing$Income)
#names(marketing)[names(marketing)=="Income"] <- "ln_Income"

marketing$Education <- as.factor(ifelse(marketing$Education=="2n Cycle" & marketing$Education=="Master", "Master",
                                        ifelse(marketing$Education=="Graduation", "Bachelor", 
                                               ifelse(marketing$Education=="Basic", "Basic", 
                                                      ifelse(marketing$Education=="PhD", "PhD", "Master")))))

marketing$Marital_Status <- as.factor(ifelse(marketing$Marital_Status==c("Single", "Alone", "Absurd", "YOLO"), "Single",
                                             ifelse(marketing$Marital_Status=="Married", "Married", 
                                                    ifelse(marketing$Marital_Status=="Together", "Together",
                                                           ifelse(marketing$Marital_Status=="Divorced", "Divorced",
                                                                  ifelse(marketing$Marital_Status=="Widow", "Widow", "Single"))))))


marketing$Complain <- as.factor(marketing$Complain)
marketing$AcceptedCmp1 <- as.factor(marketing$AcceptedCmp1)
marketing$AcceptedCmp2 <- as.factor(marketing$AcceptedCmp2)
marketing$AcceptedCmp3 <- as.factor(marketing$AcceptedCmp3)
marketing$AcceptedCmp4 <- as.factor(marketing$AcceptedCmp4)
marketing$AcceptedCmp5 <- as.factor(marketing$AcceptedCmp5)
marketing$Response <- as.factor(marketing$Response)

marketing$drunkie <- as.factor(ifelse(marketing$MntWines>303.94,1,0))

marketing.df <- marketing

####################################################################################################

marketing.df <- marketing[ , -c(1,3,4,21:29)]  # Drop ID column


#################################################################################
#################################################################################
marketing.df$Kidhome <- as.numeric(marketing.df$Kidhome)
marketing.df$Teenhome <- as.numeric(marketing.df$Teenhome)
marketing.df$Years_a_Customer <- as.numeric(marketing.df$Years_a_Customer)
marketing.df$Recency <- as.numeric(marketing.df$Recency)
marketing.df$MntWines <- as.numeric(marketing.df$MntWines)
marketing.df$MntFruits <- as.numeric(marketing.df$MntFruits)
marketing.df$MntMeatProducts <- as.numeric(marketing.df$MntMeatProducts)
marketing.df$MntFishProducts <- as.numeric(marketing.df$MntFishProducts)
marketing.df$MntSweetProducts <- as.numeric(marketing.df$MntSweetProducts)
marketing.df$MntGoldProds <- as.numeric(marketing.df$MntGoldProds)
marketing.df$NumDealsPurchases <- as.numeric(marketing.df$NumDealsPurchases)
marketing.df$NumWebPurchases <- as.numeric(marketing.df$NumWebPurchases)
marketing.df$NumCatalogPurchases <- as.numeric(marketing.df$NumCatalogPurchases)
marketing.df$NumStorePurchases <- as.numeric(marketing.df$NumStorePurchases)
marketing.df$NumWebVisitsMonth <- as.numeric(marketing.df$NumWebVisitsMonth)
#################################################################################
#################################################################################




summary(marketing.df)
n=dim(marketing.df)[1]


index <- sample(seq(1:n), round(.70*n))

train.df <- marketing.df[index,]
# valid.df <- setdiff(seq(1:n), index)
valid.df <- marketing.df[-index,]

lm.fit <- lm(MntWines ~ ., data=train.df)
summary(lm.fit)

pr.lm.t <- predict(lm.fit, train.df[,-7])
pr.lm.v <- predict(lm.fit, valid.df[,-7])

RMSE.t.lm <- sqrt(sum((train.df$MntWines-pr.lm.t)^2)/nrow(train.df))
RMSE.v.lm <- sqrt(sum((valid.df$MntWines-pr.lm.v)^2)/nrow(valid.df))



ncol <- dim(marketing.df)[2]

for (i in 1:ncol){
  train.df[,i] <- (train.df[,i]-min(train.df[,i]))/(max(train.df[,i])-min(train.df[,i]))
  valid.df[,i] <- (valid.df[,i]-min(valid.df[,i]))/(max(valid.df[,i])-min(valid.df[,i]))
}



n <- names(train.df)
f <- as.formula(paste("MntWines~", paste(n[!n %in% "MntWines"], collapse = "+")))

nn <- neuralnet(f, data=train.df, hidden=c(5,3), linear.output=T)
plot(nn)

pr.nn.t <- compute(nn, train.df[,-7])$net.result
pr.nn.v <- compute(nn, valid.df[,-7])$net.result


pr.nn.t.usd <- pr.nn.t*(max(marketing.df$MntWines[index])-min(marketing.df$MntWines[index]))+min(marketing.df$MntWines[index])
pr.nn.v.usd <- pr.nn.v*(max(marketing.df$MntWines[-index])-min(marketing.df$MntWines[-index]))+min(marketing.df$MntWines[-index])


RMSE.t <- sqrt(sum((marketing.df$MntWines[index]-pr.nn.t.usd)^2)/nrow(train.df))
RMSE.v <- sqrt(sum((marketing.df$MntWines[-index]-pr.nn.v.usd)^2)/nrow(valid.df))


RMSE.lm <- c(RMSE.t.lm, RMSE.v.lm)
RMSE.nn <- c(RMSE.t, RMSE.v)

RMSE.lm
RMSE.nn

par(mfrow=c(1,2))
plot(marketing.df$MntWines[index], pr.lm.t, ylab="predicted", xlab="actual", main="OLS")
abline(0,1, col="green")

plot(marketing.df$MntWines[index], pr.nn.t.usd, ylab="predicted", xlab="actual", main="NN")
abline(0,1, col="hot pink")




#############################################################
####### ############## ############## ############## ########
######   ############   ############   ############   #######
#####     ##########     ##########     ##########     ######
####       ########       ########       ########       #####
###         ######         ######         ######         ####
######  #############  ##############  #############  #######
######  #############  ##############  #############  #######
#############################################################
### Trees

### Reset
marketing <- read.csv("marketingdata.csv", header=TRUE)

marketing$Year_Birth <- (2022-marketing$Year_Birth)
names(marketing)[names(marketing)=="Year_Birth"] <- "Age"

marketing$Income <- as.numeric(marketing$Income)
#marketing$Income <- log(marketing$Income)
#names(marketing)[names(marketing)=="Income"] <- "ln_Income"

marketing$Education <- as.factor(ifelse(marketing$Education=="2n Cycle" & marketing$Education=="Master", "Master",
                                        ifelse(marketing$Education=="Graduation", "Bachelor", 
                                               ifelse(marketing$Education=="Basic", "Basic", 
                                                      ifelse(marketing$Education=="PhD", "PhD", "Master")))))

marketing$Marital_Status <- as.factor(ifelse(marketing$Marital_Status==c("Single", "Alone", "Absurd", "YOLO"), "Single",
                                             ifelse(marketing$Marital_Status=="Married", "Married", 
                                                    ifelse(marketing$Marital_Status=="Together", "Together",
                                                           ifelse(marketing$Marital_Status=="Divorced", "Divorced",
                                                                  ifelse(marketing$Marital_Status=="Widow", "Widow", "Single"))))))


marketing$Complain <- as.factor(marketing$Complain)
marketing$AcceptedCmp1 <- as.factor(marketing$AcceptedCmp1)
marketing$AcceptedCmp2 <- as.factor(marketing$AcceptedCmp2)
marketing$AcceptedCmp3 <- as.factor(marketing$AcceptedCmp3)
marketing$AcceptedCmp4 <- as.factor(marketing$AcceptedCmp4)
marketing$AcceptedCmp5 <- as.factor(marketing$AcceptedCmp5)
marketing$Response <- as.factor(marketing$Response)

marketing$drunkie <- as.factor(ifelse(marketing$MntWines>303.94,1,0))

################################################################################

marketing.df <- marketing[ , -c(1,10,27,28)]

set.seed(666)  
train.index <- sample(c(1:dim(marketing.df)[1]), dim(marketing.df)[1]*0.6)  
train.df <- marketing.df[train.index, ]
valid.df <- marketing.df[-train.index, ]

#######################################################################
### Train - Default

default.ct <- rpart(drunkie ~ ., data = train.df, control = rpart.control(maxdepth = 2), method = "class")

prp(default.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10)

default.ct.point.pred.train <- predict(default.ct,train.df,type = "class")

confusionMatrix(default.ct.point.pred.train, as.factor(train.df$drunkie))

### Valid - Default
default.ct.point.pred.valid <- predict(default.ct,valid.df,type = "class")

confusionMatrix(default.ct.point.pred.valid, as.factor(valid.df$drunkie))

#####################################################################
### Train - Deeper

deeper.ct <- rpart(drunkie ~ ., data = train.df, method = "class", cp = 0.05, minsplit = 1)

# count number of leaves
length(deeper.ct$frame$var[deeper.ct$frame$var == "<leaf>"])

prp(deeper.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
    box.col=ifelse(deeper.ct$frame$var == "<leaf>", 'blue', 'red'))  

deeper.ct.point.pred.train <- predict(deeper.ct,train.df,type = "class")

confusionMatrix(deeper.ct.point.pred.train, as.factor(train.df$drunkie))

### Valid - Deeper
deeper.ct.point.pred.valid <- predict(deeper.ct,valid.df,type = "class")

confusionMatrix(deeper.ct.point.pred.valid, as.factor(valid.df$drunkie))
#######################################################################



### Cross-Validation
cv.ct <- rpart(drunkie ~ ., data = train.df, method = "class", 
               cp = 0.001, minsplit = 5, xval = 5)
# use printcp() to print the table. 
printcp(cv.ct)

prp(cv.ct, type = 1, extra = 1, split.font = 1, varlen = -10,
    box.col=ifelse(deeper.ct$frame$var == "<leaf>", 'pink', 'sky blue'))

### Train - CV
cv.ct.point.pred.train <- predict(cv.ct,train.df,type = "class")

confusionMatrix(cv.ct.point.pred.train, as.factor(train.df$drunkie), positive = "1")

### Valid - CV
cv.ct.point.pred.valid <- predict(cv.ct,valid.df,type = "class")

confusionMatrix(cv.ct.point.pred.valid, as.factor(valid.df$drunkie), positive = "1")

#### cp=0034682
###############################################################################
#### Pruney-prune

pruned.ct <- prune(cv.ct, 
                   cp = cv.ct$cptable[which.min(cv.ct$cptable[,"xerror"]),"CP"])
length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"])
prp(pruned.ct, type = 1, extra = 1, split.font = 1, varlen = -10,
    box.col=ifelse(deeper.ct$frame$var == "<leaf>", 'pink', 'sky blue'))

### Train - Pruned
pruned.ct.point.pred.train <- predict(pruned.ct,train.df,type = "class")

confusionMatrix(pruned.ct.point.pred.train, as.factor(train.df$drunkie), positive = "1")

### Valid - Pruned
pruned.ct.point.pred.valid <- predict(pruned.ct,valid.df,type = "class")

confusionMatrix(pruned.ct.point.pred.valid, as.factor(valid.df$drunkie), positive = "1")





library(randomForest)
## random forest, "ntree = number of trees!"
rf <- randomForest(as.factor(drunkie) ~ ., data = train.df, ntree = 500, 
                   mtry = 4, nodesize = 5, importance = TRUE)  

## variable importance plot
varImpPlot(rf, type = 1)

## confusion matrix
rf.pred <- predict(rf, valid.df)
confusionMatrix(rf.pred, as.factor(valid.df$drunkie), positive = "1")




#### Table 9.5

library(adabag)
library(rpart) 
library(caret)

train.df$drunkie <- as.factor(train.df$drunkie)

set.seed(666)
boost <- boosting(drunkie ~ ., data = train.df)


pred <- predict(boost, valid.df)
confusionMatrix(as.factor(pred$class), as.factor(valid.df$drunkie), positive = "1")

