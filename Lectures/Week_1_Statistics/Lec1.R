#Lecture 1
train <- read.csv('train.csv', stringsAsFactors = FALSE)
summary(train)

str(train)
head(train)

contVar <- NULL
corr <- NULL
catVar <- NULL
catCorr <- NULL
for (i in 1:dim(train)[2]) {
  if (is.numeric((train[,i]))){
    contVar <- c(contVar, i)
    corr <- c(corr, cor(train[,i], train$SalePrice, use = 'pairwise.complete.obs'))
  } else {
    catVar <- c(catVar, i)
  }
}

contVar
length(contVar)
catVar
length(catVar)

library(corrplot)
trainCont <- train[,contVar]
trainCont <- trainCont[,-1]
trainCatg <- train[,catVar]
correlations <- cor(trainCont, use = 'pairwise.complete.obs')
corrplot(correlations, method = "square")

trainCont
trainCatg
# first 5 highest correlated factors
# find absolute value for each correlations
absolute = abs(correlations)
highestCorrelation <- NULL
# find the sum of the absolute value for each correlations
for (i in 1:dim(absolute)[1]) {
  highestCorrelation <- c(highestCorrelation, sum(absolute[i,]))
}     
highestCorrelation
orderIndex = order(highestCorrelation)
orderIndex
# find the 5 index with the highest absolute value for each correlations are
# very likely to be the most important correlated factors among the 37 factors 
highestIndex = orderIndex[33:37]
highestCorrelation[highestIndex] # their correlations values
highestCorrelationNames = colnames(trainCont[,highestIndex]) #their names
highestCorrelationNames
# Cut the data of these columns
trimmedData = trainCont[, highestIndex]
trimCorrelations <- cor(trimmedData, use = 'pairwise.complete.obs')
corrplot(trimCorrelations, method = "square") # plot the correlations

#predictive power
#for continuous variables, the predictive power is described as the absolute
#correlation with SalePrice, when the absolute correlation is higher, it means 
#it is positively or negatively correlated to the SalePrice, which can be
#considered as the predictive power
contVar <- contVar[-1] #remove ID
corr <- corr[-1] 
contVar <- contVar[-37] #remove SalePrice
corr <- corr[-37]
abscorr = abs(corr)
abscorr
contPredictIndex = order(abscorr)
colnames(train)[contVar]
# top 5 predictive power
rev(colnames(train)[contVar[contPredictIndex[32:36]]])
rev(abscorr[contPredictIndex[32:36]])

#for categorical variables, the predictive power can be identified as the mean
#difference between leftmost and rightmost categories, the more different the mean is, the 
#higher predictive power the categorical variable has
percentage <- sapply(train, function(x) {length(which(is.na(x)))/nrow(train)})
percentage <- percentage[catVar]
catVar <- catVar[percentage < 0.05]
percentage <- percentage[percentage < 0.05]
library(lattice)
meanDifference <- NULL
for (i in 1:length(catVar)) {
  xtab <- xtabs(train$SalePrice ~ eval(parse(text = paste("train$", colnames(train)[catVar[i]], sep = ""))))
  count <- table(train[catVar[i]])
  tab = xtab / count
  meanDifference <- c(meanDifference, max(tab) - min(tab))
  bwplot(eval(parse(text = colnames(train)[catVar[i]])) ~ SalePrice, data = train)
}
meanOrder <- order(meanDifference)
meanDifference[meanOrder]
meanOrder[(length(meanOrder) - 4):length(meanOrder)]
"The 5 Most Powerful Predicting Categorical Variable is"
colnames(train)[catVar[meanOrder[(length(meanOrder) - 4):length(meanOrder)]]]
meanDifference[meanOrder[(length(meanOrder) - 4):length(meanOrder)]]


#generate potential useful features from existing features
#for this, I believe the absolute correlations between variables can describe
#the potential for the combined features. When the absolute correlations between
# A and B is higher than the absolute correlations between C and D. A and B are 
#more likely to be combined and generate a potential useful features.
contVar <- c(contVar, 81)
absoluteOrder <- order(absolute)
#Since the correlations come within pairs and the last 37 self-self correlations are 1, therefore, take the 10 elements 1323 - 1332
top5Order <- absoluteOrder[1323:1332]
top5Order <- top5Order[-seq(1, 9, 2)]
x = ceiling((top5Order - 1) / 37)
y = (top5Order - 1) %% 37 + 1
# the 1st x and 1st y is the potential most powerful features, the 2nd x and 2nd y and so on
colnames(train)[contVar[x]]
colnames(train)[contVar[y]]
correlations[x, y] # the diagonal line is the correlations between two factors, those factors are the highest
top5Order
absolute[top5Order]
# check to see that they are the same, the answer is correct
