rm(list=ls())

installIfAbsentAndLoad <- function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = T) )
    { install.packages(thispackage)}
    require(thispackage, character.only = T)
  }
}
needed  <-  c("ISLR", "MASS", "class", "boot")      
installIfAbsentAndLoad(needed)

#########################
####  Question 1  #######
#########################

data <- Weekly

set.seed(5072)

###  Logistic Regression
glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data=data, family=binomial)
summary(glm.fit)
glm.probs <- predict(glm.fit, type='response')
glm.pred <- rep("Down", 1089)
glm.pred[glm.probs > .5] <- 'Up'
###   The only significant predictor was the Lag2 predictor with a p-value of 0.0297

table1 <- table(data$Direction, glm.pred)

print(paste("Success Rate: ", (table1['Down','Down']+table1['Up','Up'])/sum(table1)))
print(paste("Overall Error Rate: ", (table1['Down', 'Up']+table1['Up', 'Down'])/ sum(table1)))
print(paste("Type 1 Error Rate: ", table1['Down', 'Up']/sum(table1['Down',])))
print(paste("Type 2 Error Rate: ", table1['Up', 'Down']/sum(table1['Up',])))
print(paste("Power (Sensitivity): ", 1- (table1['Up', 'Down']/sum(table1['Up',]))))
print(paste("Precision: ", table1['Up','Up']/sum(table1[,'Up'])))

###   Logistic Regression
train <- data$Year<2009
trainset <- data[train,]
testset <- data[!train, ]          
testset.dir <- data$Direction[!train]

glm.fit <- glm(Direction ~ Lag2, data=data, family=binomial, subset = train)
summary(glm.fit)
glm.probs <- predict(glm.fit, testset, type='response')
glm.pred <- rep("Down", 104)
glm.pred[glm.probs > .5] <- 'Up'

table2 <- table(testset.dir, glm.pred)

print(paste("Success Rate: ", (table2['Down','Down']+table2['Up','Up'])/sum(table2)))
print(paste("Overall Error Rate: ", (table2['Down', 'Up']+table2['Up', 'Down'])/ sum(table2)))
print(paste("Type 1 Error Rate: ", table2['Down', 'Up']/sum(table2['Down',])))
print(paste("Type 2 Error Rate: ", table2['Up', 'Down']/sum(table2['Up',])))
print(paste("Power (Sensitivity): ", 1- (table2['Up', 'Down']/sum(table2['Up',]))))
print(paste("Precision: ", table2['Up','Up']/sum(table2[,'Up'])))

###   LDA
lda.fit <- lda(Direction~Lag2, data=data, subset=train)
lda.pred <- predict(lda.fit, testset)
ldatable <- table(testset.dir, lda.pred$class)

print(paste("LDA Success Rate: ", (ldatable['Down','Down']+ldatable['Up','Up'])/sum(ldatable)))
print(paste("LDA Overall Error Rate: ", (ldatable['Down', 'Up']+ldatable['Up', 'Down'])/ sum(ldatable))) 
print(paste("LDA Type 1 Error Rate: ", ldatable['Down', 'Up']/sum(ldatable['Down',]))) 
print(paste("LDA Type 2 Error Rate: ", ldatable['Up', 'Down']/sum(ldatable['Up',])))
print(paste("LDA Power (Sensitivity): ", 1- (ldatable['Up', 'Down']/sum(ldatable['Up',]))))
print(paste("LDA Precision: ", ldatable['Up','Up']/sum(ldatable[,'Up'])))

###   QDA
qda.fit <- qda(Direction~Lag2, data=data, subset=train)
qda.pred <- predict(qda.fit, testset)
qdatable <- table(testset.dir, qda.pred$class)

print(paste("QDA Success Rate: ", (qdatable['Down','Down']+qdatable['Up','Up'])/sum(qdatable)))
print(paste("QDA Overall Error Rate: ", (qdatable['Down', 'Up']+qdatable['Up', 'Down'])/ sum(qdatable)))
print(paste("QDA Type 1 Error Rate: ", qdatable['Down', 'Up']/sum(qdatable['Down',])))
print(paste("QDA Type 2 Error Rate: ", qdatable['Up', 'Down']/sum(qdatable['Up',])))
print(paste("QDA Power (Sensitivity): ", 1- (qdatable['Up', 'Down']/sum(qdatable['Up',]))))
print(paste("QDA Precision: ", qdatable['Up','Up']/sum(qdatable[,'Up'])))

###   KNN = 1
train.x <- matrix(data['Lag2'][train,])
test.x <- testset['Lag2']
train.dir <- matrix(data$Direction[train])

knn.pred <- knn(train.x, test.x, train.dir, k=1)
knn1table <- table(testset.dir, knn.pred)

print(paste("KNN=1 Success Rate: ", (knn1table['Down','Down']+knn1table['Up','Up'])/sum(knn1table)))
print(paste("KNN=1 Overall Error Rate: ", (knn1table['Down', 'Up']+knn1table['Up', 'Down'])/ sum(knn1table)))
print(paste("KNN=1 Type 1 Error Rate: ", knn1table['Down', 'Up']/sum(knn1table['Down',])))
print(paste("KNN=1 Type 2 Error Rate: ", knn1table['Up', 'Down']/sum(knn1table['Up',])))
print(paste("KNN=1 Power (Sensitivity): ", 1- (knn1table['Up', 'Down']/sum(knn1table['Up',]))))
print(paste("KNN=1 Precision: ", knn1table['Up','Up']/sum(knn1table[,'Up'])))

###   KNN = 5
knn.pred <- knn(train.x, test.x, train.dir, k=5)
knn5table <- table(testset.dir, knn.pred)

print(paste("KNN=5 Success Rate: ", (knn5table['Down','Down']+knn5table['Up','Up'])/sum(knn5table)))
print(paste("KNN=5 Overall Error Rate: ", (knn5table['Down', 'Up']+knn5table['Up', 'Down'])/ sum(knn5table)))
print(paste("KNN=5 Type 1 Error Rate: ", knn5table['Down', 'Up']/sum(knn5table['Down',])))
print(paste("KNN=5 Type 2 Error Rate: ", knn5table['Up', 'Down']/sum(knn5table['Up',])))
print(paste("KNN=5 Power (Sensitivity): ", 1- (knn5table['Up', 'Down']/sum(knn5table['Up',]))))
print(paste("KNN=5 Precision: ", knn5table['Up','Up']/sum(knn5table[,'Up']))) #.6393

# Based on the confusion matrices, the best model was either the logistic regression or LDA because there was a larger success rate,
    #as well as the substantially lower type 2 error rate. 

##################################
#########  Question 2  ###########
##################################

data <- Auto

set.seed(5072)
data$mpg01 <- 0
for(i in 1:length(data$mpg)){
  if(data$mpg[i]>median(data$mpg)){
    #print(data$mpg[i])
    data$mpg01[i] <- 1
  }
}

data1 <- data[-1]

n <- nrow(data1)
trainprop <- .8
train <- sample(n, trainprop*n)
test <- setdiff(1:n, train)

trainset <- data1[train,]
testset <- data1[test,]

glm.fit <- glm(mpg01 ~ cylinders + displacement + weight, data=data1, family=binomial, subset = train)
summary(glm.fit)
glm.probs <- predict(glm.fit, testset, type='response')
glm.pred <- rep(0, 79)
glm.pred[glm.probs > .5] <- 1

table1 <- table(testset$mpg01, glm.pred)

print(paste("LR Success Rate: ", (table1['0','0']+table1['1','1'])/sum(table1))) 
print(paste("LR Overall Error Rate: ", (table1['0', '1']+table1['1','0'])/ sum(table1)))
print(paste("LR Type 1 Error Rate: ", table1['0', '1']/sum(table1['0',]))) 
print(paste("LR Type 2 Error Rate: ", table1['1', '0']/sum(table1['1',])))
print(paste("LR Power (Sensitivity): ", 1- (table1['1', '0']/sum(table1['1',]))))
print(paste("LR Precision: ", table1['1','1']/sum(table1[,'1']))) 

###   LDA
lda.fit <- lda(mpg01~cylinders + displacement + weight, data=data, subset=train)
lda.pred <- predict(lda.fit, testset)
ldatable <- table(testset$mpg01, lda.pred$class)

print(paste("LDA Success Rate: ", (ldatable['0','0']+ldatable['1','1'])/sum(ldatable)))
print(paste("LDA Overall Error Rate: ", (ldatable['0', '1']+ldatable['1', '0'])/ sum(ldatable)))
print(paste("LDA Type 1 Error Rate: ", ldatable['0', '1']/sum(ldatable['0',])))
print(paste("LDA Type 2 Error Rate: ", ldatable['1', '0']/sum(ldatable['1',])))
print(paste("LDA Power (Sensitivity): ", 1- (ldatable['1', '0']/sum(ldatable['1',]))))
print(paste("LDA Precision: ", ldatable['1','1']/sum(ldatable[,'1'])))

###   QDA
qda.fit <- qda(mpg01~cylinders + weight + displacement, data=data, subset=train)
qda.pred <- predict(qda.fit, testset)
qdatable <- table(testset$mpg01, qda.pred$class)

print(paste("QDA Success Rate: ", (qdatable['0','0']+qdatable['1','1'])/sum(qdatable))) 
print(paste("QDA Overall Error Rate: ", (qdatable['0', '1']+qdatable['1', '0'])/ sum(qdatable)))
print(paste("QDA Type 1 Error Rate: ", qdatable['0', '1']/sum(qdatable['0',])))
print(paste("QDA Type 2 Error Rate: ", qdatable['1', '0']/sum(qdatable['1',])))
print(paste("QDA Power (Sensitivity): ", 1- (qdatable['1', '0']/sum(qdatable['1',]))))
print(paste("QDA Precision: ", qdatable['1','1']/sum(qdatable[,'1']))) 

###   KNN = 1
train.x <- cbind(data1$cylinders, data1$displacement, data1$weight)[train,]
test.x <- cbind(data1$cylinders, data1$displacement, data1$weight)[test,]
train.mpg01 <- data1$mpg01[train]

knn.pred <- knn(train.x, test.x, train.mpg01, k=1)
knn1table <- table(testset$mpg01, knn.pred)

print(paste("KNN=1 Success Rate: ", (knn1table['0','0']+knn1table['1','1'])/sum(knn1table)))
print(paste("KNN=1 Overall Error Rate: ", (knn1table['0', '1']+knn1table['1', '0'])/ sum(knn1table)))
print(paste("KNN=1 Type 1 Error Rate: ", knn1table['0', '1']/sum(knn1table['0',])))
print(paste("KNN=1 Type 2 Error Rate: ", knn1table['1', '0']/sum(knn1table['1',])))
print(paste("KNN=1 Power (Sensitivity): ", 1- (knn1table['1', '0']/sum(knn1table['1',]))))
print(paste("KNN=1 Precision: ", knn1table['1', '1']/sum(knn1table[,'1'])))

###   Best K
numreps <- 100
test.errors <- rep(0, numreps)
train.errors <- rep(0, numreps)
for(k in 1:numreps) {
  knn.pred <- knn(train.x, test.x,  train.mpg01, k = k)
  mytable <- table(testset$mpg01, knn.pred)
  test.errors[k] <- mean(testset$mpg01 != knn.pred)
}
knn.pred <- knn(train.x, test.x, train.mpg01, k=which.min(test.errors))
knn5table <- table(testset$mpg01, knn.pred)

print(paste("KNN=3 Success Rate: ", (knn5table['0','0']+knn5table['1','1'])/sum(knn5table)))
print(paste("KNN=3 Overall Error Rate: ", (knn5table['0', '1']+knn5table['1', '0'])/ sum(knn5table)))
print(paste("KNN=3 Type 1 Error Rate: ", knn5table['0', '1']/sum(knn5table['0',])))
print(paste("KNN=3 Type 2 Error Rate: ", knn5table['1', '0']/sum(knn5table['1',])))
print(paste("KNN=3 Power (Sensitivity): ", 1- (knn5table['1', '0']/sum(knn5table['1',]))))
print(paste("KNN=3 Precision: ", knn5table['1', '1']/sum(knn5table[,'1'])))

# Based on the cofusion matrices, the best model is the QDA model because of the higher success rate,
    #as well as the low type 1 and 2 error rates, with a large precision ratio.

#############################
#######  Question 3  ########
#############################

set.seed(5072)
data <- Boston

data$crim01 <- 0
for(i in 1:length(data$crim)){
  if(data$crim[i]>median(data$crim)){
    #print(data$mpg[i])
    data$crim01[i] <- 1
  }
}

trainprop <- .80
n <- nrow(data)
train <- sample(n, trainprop*n)
test <- setdiff(1:n, train)
trainset <- data[train,]
testset <- data[test,]

###   Logistic Regression
glm.fit <- glm(crim01 ~ nox + rad + dis, data=data, family=binomial, subset = train)
summary(glm.fit)
glm.probs <- predict(glm.fit, testset, type='response')
glm.pred <- rep(0, 102)
glm.pred[glm.probs > .5] <- 1

table1 <- table(testset$crim01, glm.pred)

print(paste("LR Success Rate: ", (table1['0','0']+table1['1','1'])/sum(table1))) 
print(paste("LR Overall Error Rate: ", (table1['0', '1']+table1['1','0'])/ sum(table1)))
print(paste("LR Type 1 Error Rate: ", table1['0', '1']/sum(table1['0',])))
print(paste("LR Type 2 Error Rate: ", table1['1', '0']/sum(table1['1',])))
print(paste("LR Power (Sensitivity): ", 1- (table1['1', '0']/sum(table1['1',]))))
print(paste("LR Precision: ", table1['1','1']/sum(table1[,'1'])))

###   LDA
lda.fit <- lda(crim01~nox + rad + dis, data=data, subset=train)
lda.pred <- predict(lda.fit, testset)
ldatable <- table(testset$crim01, lda.pred$class)

print(paste("LDA Success Rate: ", (ldatable['0','0']+ldatable['1','1'])/sum(ldatable)))
print(paste("LDA Overall Error Rate: ", (ldatable['0', '1']+ldatable['1', '0'])/ sum(ldatable)))
print(paste("LDA Type 1 Error Rate: ", ldatable['0', '1']/sum(ldatable['0',])))
print(paste("LDA Type 2 Error Rate: ", ldatable['1', '0']/sum(ldatable['1',])))
print(paste("LDA Power (Sensitivity): ", 1- (ldatable['1', '0']/sum(ldatable['1',]))))
print(paste("LDA Precision: ", ldatable['1','1']/sum(ldatable[,'1'])))


###   Best K
train.x <- cbind(data$nox,data$rad, data$dis)[train,]
test.x <- cbind(data$nox, data$rad, data$dis)[test,]
train.crim01 <- data$crim01[train]

numreps <- 100
test.errors <- rep(0, numreps)
train.errors <- rep(0, numreps)
for(k in 1:numreps) {
  knn.pred <- knn(train.x, test.x,  train.crim01, k = k)
  mytable <- table(testset$crim01, knn.pred)
  test.errors[k] <- mean(testset$crim01 != knn.pred)
}
knn.pred <- knn(train.x, test.x, train.crim01, k=which.min(test.errors))
knn5table <- table(testset$crim01, knn.pred)

print(paste("KNN=1 Success Rate: ", (knn5table['0','0']+knn5table['1','1'])/sum(knn5table)))
print(paste("KNN=1 Overall Error Rate: ", (knn5table['0', '1']+knn5table['1', '0'])/ sum(knn5table)))
print(paste("KNN=1 Type 1 Error Rate: ", knn5table['0', '1']/sum(knn5table['0',])))
print(paste("KNN=1 Type 2 Error Rate: ", knn5table['1', '0']/sum(knn5table['1',])))
print(paste("KNN=1 Power (Sensitivity): ", 1- (knn5table['1', '0']/sum(knn5table['1',]))))
print(paste("KNN=1 Precision: ", knn5table['1', '1']/sum(knn5table[,'1'])))

# Based on the confusion matrices above, the best model is the KNN model with the best k (k=1). This produced
    #the largest success rate with a large precision rate and low type 1 and 2 error rates.

#############################
########  Question 4  #######
#############################

set.seed(5072)
x <- rnorm(100)
y <- x-2*x**2 + rnorm(100)

data <- data.frame(x,y)

plot(x,y, main='Scatterplot of Y versus X')

set.seed(123)
glm.fit1 <- glm(y~x)
cv.glm(data, glm.fit1)$delta[1]

glm.fit2 <- glm(y~poly(x,2))
cv.glm(data, glm.fit2)$delta[1]

glm.fit3 <- glm(y~poly(x,3))
cv.glm(data, glm.fit3)$delta[1]

glm.fit4 <- glm(y~poly(x,4))
cv.glm(data, glm.fit4)$delta[1]

set.seed(456)
glm.fit1 <- glm(y~x)
cv.glm(data, glm.fit1)$delta[1]

glm.fit2 <- glm(y~poly(x,2))
cv.glm(data, glm.fit2)$delta[1]

glm.fit3 <- glm(y~poly(x,3))
cv.glm(data, glm.fit3)$delta[1]

glm.fit4 <- glm(y~poly(x,4))
cv.glm(data, glm.fit4)$delta[1]

# The results are the same regardless of the seed because LOOCV because
    #there are n observations with n squared errors, given the average as the MSE.

# The smallest LOOCV occured for the quadratic function (glm.fit2), more than likely
    #because there is a quadratic relationship between x and y (plot).

summary(glm.fit4)
# We can see that only the first and second degree-values have a significant p-value.
    #Therefore, we can agree with the conclusions drawn earlier (quadratic is lowest).

