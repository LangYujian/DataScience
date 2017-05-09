# Vector: logical, integer, numeric (double), and character.

dbl_var <- c(1, 2.5, 4.5)
# With the L suffix, you get an integer rather than a double
int_var <- c(1L, 6L, 10L)
# Use TRUE and FALSE (or T and F) to create logical vectors
log_var <- c(TRUE, FALSE, T, F)
chr_var <- c("these are", "some strings")

# Vectors are always flat, even if you nest c()
c(1, c(2, c(3, 4)))
c(1, 2, 3, 4)

# missing value is NA
t = c(1, NA, 2)
is.na(t)

# Given a vector, you can determine its type with typeof(), 
# or check if it’s a specific type with an “is” function: 
# is.character(), is.double(), is.integer(), is.logical(), 
# or, more generally, is.atomic().

typeof(int_var)
is.integer(int_var)
is.atomic(int_var)

# is.numeric() is a general test for the “numberliness” of a vector 
# and returns TRUE for both integer and double vectors. 
is.numeric(int_var)
is.numeric(dbl_var)

# Coercion: 
# All elements of a vector must be the same type, 
# so when you attempt to combine different types,
# they will be coerced to the most flexible type. 
# Types from least to most flexible are: logical, integer, double, and character.

str(c("a", 1))

x <- c(FALSE, FALSE, TRUE)
as.numeric(x)

# List
# Lists are different from vectors,
# because their elements can be of any type, including lists. 

x <- list(1:3, "a", c(TRUE, FALSE, TRUE), c(2.3, 5.9))
str(x)

# The typeof() a list is list. 
# You can test for a list with is.list() and coerce to a list with as.list(). 
typeof(x)
is.list(x)

# You can turn a list into a vector with unlist(). 
# If the elements of a list have different types, unlist() uses the same coercion rules as c().

unlist(x)

# Lists are used to build up many of the more complicated data structures in R
# e.g., linear models objects (as produced by lm()) are lists.

# Matrix
a <- matrix(1:6, ncol = 3, nrow = 2)

# You can also modify an object in place by setting dim()
c <- 1:6
dim(c) <- c(2, 3)

# length() generalises to nrow() and ncol() for matrices.
# names() generalises to rownames() and colnames() for matrices, and dimnames(), a list of character vectors, for arrays.

length(a)
nrow(a)
ncol(a)

rownames(a) <- c("A", "B")
colnames(a) <- c("a", "b", "c")
a

# Data frame: the most common way of storing data in R
df <- data.frame(x = 1:3, y = c("a", "b", "c"))
str(df)
# why we don't like factor, see this post:http://www.win-vector.com/blog/2014/09/factors-are-not-first-class-citizens-in-r/
# x = as.factor(c('a', 'b', 'c'))
# x = c(x, 'd')
df <- data.frame(x = 1:3, y = c("a", "b", "c"), stringsAsFactors = FALSE)
str(df)

is.data.frame(df)

# Coercion
# You can coerce an object to a data frame with as.data.frame():
# A vector will create a one-column data frame.
# A list will create one column for each element; 
# it’s an error if they’re not all the same length. 
x
as.data.frame(x)
# A matrix will create a data frame with the same number of columns and rows as the matrix.
a
as.data.frame(a)

# Material: https://cran.r-project.org/doc/manuals/r-patched/R-intro.html
train <- read.csv("train.csv", stringsAsFactors = FALSE)

# Understand the data
summary(train)
str(train)
dim(train)
head(train)
colnames(train)
head(rownames(train))
length(unique(train$Id))

# check how many NAs in each feature
length(which(is.na(train$LotFrontage)))
sapply(train, function(x) {length(which(is.na(x)))})
# same as:
colSums(sapply(train, is.na))
sort(sapply(train, function(x) { sum(is.na(x)) }), decreasing=TRUE)
# The percentage of data missing in train.
sum(is.na(train)) / (nrow(train) *ncol(train))

# Find out variables with largest number of missing values
# how to treat missing values, (1) actually missing (2) add new level (3) imputation
# (4) remove all rows with NA

# categorial variables
table(train$OverallQual)
table(train$OverallQual) / dim(train)[1]
# barplot(counts of data not data itself)
barplot(table(train$OverallQual))
barplot(table(train$OverallCond))

# continous variables
mean(train$SalePrice)
sd(train$SalePrice)
median(train$SalePrice)
quantile(train$SalePrice, c(0.1, 0.25, 0.5, 0.75, 0.9))

# let's use pdf
plot(density(train$SalePrice)) # Right skewed
plot(density(log(train$SalePrice))) # looks more normal distributed

# boxplot http://www.physics.csbsju.edu/stats/box2.html
boxplot(train$SalePrice) 
# Q1 - 1.5IQR, Q1, median, Q3, Q3 + 1.5IQR, where IQR is  interquartile range: Q3 - Q1

# Get to explore the relationship between features and outcome
# what could be the most useful features from the list? Most times choose features by intuition.
# Talk about example at work.
# Start from OverallQual
boxplot(log(subset(train, OverallQual <= 5)$SalePrice + 1),
        log(subset(train, OverallQual > 5)$SalePrice + 1))
# add more notation https://www.r-bloggers.com/box-plot-with-r-tutorial/
boxplot(log(subset(train, OverallQual <= 5)$SalePrice + 1),
        log(subset(train, OverallQual > 5)$SalePrice + 1),
        xlab = "OverallQuality", ylab = "log(SalesPrice)", 
        names = c("Low Qual", "High Qual"), col = c("red", "green"))
# another version of boxplot, called violin plot
# library(vioplot)
# lowQual <- subset(train, OverallQual <= 5, select="SalePrice")
# highQual <- subset(train, OverallQual > 5, select="SalePrice")
# vioplot(log(lowQual$SalePrice), log(highQual$SalePrice), col ='gold')

# What if we want to have a boxplot for each category of the overallQuality feature
library(lattice)
bwplot(OverallQual ~ SalePrice, data = train)
bwplot(Neighborhood ~ SalePrice, data = train)
bwplot(LotArea ~ SalePrice, data = train) # not really good for continuous feature

# how to explore relationship between continuous feature and response: calculate the correlation
with(train, cor(LotArea, SalePrice))
contVar <- NULL
corr <- NULL
for(i in 1:dim(train)[2]) {
  if(is.numeric(train[, i])) {
    contVar <- c(contVar, i)
    corr <- c(corr, cor(train[, i], train$SalePrice))
    
  }
}

# find out all of the continous columns
contVar <- names(train)[which(sapply(train, is.numeric))]
trainCont <- train[, contVar]
dim(trainCont)

library(corrplot)
correlations <- cor(trainCont[, -1]) # see a lot of NA
correlations <- cor(trainCont[, -1], use = "pairwise.complete.obs")
corrplot(correlations, method = "square")
# some features are not helpful at all, which could be due to the missingness as well
corrplot(correlations[rowInd, rowInd], method = "square")
