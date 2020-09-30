
library(rpart)
library(titanic)

# http://sas.uwaterloo.ca/~rwoldfor/software/R-code/iris-rpart.R

newwindow <- function(x) {quartz()}
#
#  Now we will build the iris data frame just as was done
# in the file web441("iris-multinom.R")
#
#  Of the two methods of building a data frame described there,
#  The first will be used here.
#
# Each species is a slice in a three way array,
# which will be separated into three matrices to
# begin with. 

setosa <- iris3[,,1]
versicolor <- iris3[,,2]
virginica <- iris3[,,3]

# Put these together into a single array

iris <- rbind(setosa,versicolor,virginica)

# Construct some Species labels
#

Species <- c(rep("Setosa",nrow(setosa)),
             rep("Versicolor",nrow(versicolor)),
             rep("Virginica",nrow(virginica))
)

# some meaningful row labels

rownames(iris) <-c(paste("Setosa", 1:nrow(setosa)), 
                   paste("Versicolor", 1:nrow(versicolor)), 
                   paste("Virginica", 1:nrow(virginica)))
#
# And to be perverse, a more meaningful collection
# of variate names
#

irisVars <- c("SepalLength", "SepalWidth", 
              "PetalLength", "PetalWidth")

#
# which can replace the column names we started with
#

colnames(iris) <- irisVars

#
# Now  to construct the data frame
#

iris.df <- data.frame(iris, Species = Species)

# Select a training set (Say half the data)
# Note that unlike other authors, I choose to randomly
# sample from the whole data set rather than stratify by the
# Species.  The thinking here is to produce a training and
# a test set according to how the data might arrive rather than
# to have two sets which most resemble the whole dataset.
#

set.seed(2568)
n <- nrow(iris.df)
train <- sort(sample(1:n, floor(n/2)))

# Training data will be:

iris.train <- iris.df[train,]

# negate the indices to get the test data:
iris.test <- iris.df[-train,]

#
#  Build a tree for the training data
#

iris.rp <-rpart(Species ~ . ,                         # formula: . means all
                data = iris.df,                       # data frame
                subset = train,                       # indices of training set
                method = "class",                     # classification tree
                parms = list(split = "information"),  # use entropy/deviance
                maxsurrogate = 0,                     # since no missing values
                cp = 0.5,                               # no size penalty
                minsplit = 5,                         # Nodes of size 5 (or 
                # more) can be split,
                minbucket = 2,                        # provided each sub-node
                # contains at least 2 obs.
)

#  The above size-related parameters (cp, minsplit, minbucket)
#  have essentially little impact on the iris data.
#
#
#  The summary (can be huge for large trees).
#

summary(iris.rp)

#
#  The classification tree itself - plotted and labelled.
#  See help("plot.rpart") and help("text.rpart")
#  for parameter details.

#newwindow()
plot(iris.rp, 
     uniform=TRUE,
     compress=TRUE,
     margin = .2)
text(iris.rp, 
     use.n=TRUE, 
     all = TRUE,
     fancy = TRUE)

#
#  Note that only the petal variates were used
#  in the splits.
#
#newwindow()
colours <- apply(matrix(iris.df[,"Species"]), 
                 1,
                 function(x){if (x=="Setosa") 
                   1 else 
                     if (x == "Versicolor")
                       2 else 3}
)

colours <-  c("red", "green3", "blue")[colours]

plot(iris.df[train,"PetalWidth"],iris.df[train,"PetalLength"], 
     col = colours[train], main = "Recursive partitoning regions")
lines(x=c(0,2.5), y = c(2.45,2.45), lty = 1)
lines(x=c(1.65,1.65), y = c(2.45,7), lty = 2)
lines(x=c(0,1.65), y = c(4.96,4.95), lty = 3)

#
# Get the predictions
#

pred.rp <- predict(iris.rp,
                   newdata = iris.df[-train,],
                   type = "class")
pred.rp

#
# Other predictive info
#

predict(iris.rp, 
        newdata = iris.df[-train,],
        type = "prob")

predict(iris.rp, 
        newdata = iris.df[-train,],
        type = "vector")

predict(iris.rp, 
        newdata = iris.df[-train,],
        type = "matrix")

#
#  Classification table on the test data
#

table(iris.df$Species[-train], pred.rp)

#
# Some CP (complexity parameter) information
#

printcp(iris.rp)
#newwindow()
plotcp(iris.rp)

#
#  Prune this tree
#

iris.pruned <- prune(iris.rp, cp = 0.1)

#The pruned tree itself.

#newwindow()
plot(iris.pruned, 
     compress=TRUE,
     margin = .2)
text(iris.pruned, 
     use.n=TRUE, 
     all = TRUE,
     fancy = TRUE)


####################################################
#
#  A different tree model
#
####################################################

train_df <- titanic_train

titanic.rp <-rpart(Survived ~ Pclass + Sex + Age + SibSp,                         # formula: . means all
                   data = train_df,                       # data frame
                   method = "class",                     # classification tree
                   parms = list(split = "information"),  # use entropy/deviance
                   maxsurrogate = 0,                     # since no missing values
                   cp = 0.0,                               # no size penalty
                   minsplit = 5,                         # Nodes of size 5 (or 
                   # more) can be split,
                   minbucket = 2,                        # provided each sub-node
                   # contains at least 2 obs.
)

summary(titanic.rp)

plot(titanic.rp, 
     uniform=TRUE,
     compress=TRUE,
     margin = .2)
text(titanic.rp, 
     use.n=TRUE, 
     all = TRUE,
     fancy = FALSE)

####################################################
#
#  A different tree model
#
####################################################

iris.rpint <-rpart(Species ~ PetalWidth + PetalLength +  
                     SepalWidth + SepalLength + 
                     sqrt(PetalWidth*PetalLength) +  
                     sqrt(SepalWidth*SepalLength),   
                   # formula: includes interactions
                   data = iris.df,                       # data frame
                   subset = train,                       # indices of training set
                   method = "class",                     # classification tree
                   parms = list(split = "information"),  # use entropy/deviance
                   maxsurrogate = 0,                     # since no missing values
                   cp = 0,                               # no size penalty
                   minsplit = 2,                         # Nodes of size 5 (or 
                   # more) can be split,
                   minbucket = 1,                        # provided each sub-node
                   # contains at least 2 obs.
)

#  Note that rpart cannot handle interaction terms
#  The above * is actually a multiplication.
#
#
#  The summary (can be huge for large trees).
#

summary(iris.rpint)

#
#  The classification tree itself - plotted and labelled.
#  See help("plot.rpart") and help("text.rpart")
#  for parameter details.

#newwindow()
plot(iris.rpint, 
     uniform=TRUE,
     compress=TRUE,
     margin = .15)
text(iris.rpint, 
     use.n=TRUE, 
     all = TRUE,
     fancy = TRUE
)

#

pred.rpint <- predict(iris.rpint,
                      newdata = iris.df[-train,],
                      type = "class")


#
#  Classification table on the test data
#

table(iris.df$Species[-train], pred.rpint)

#
# Some CP (complexity parameter) information
#

printcp(iris.rpint)
newwindow()
plotcp(iris.rpint)

#
#  Prune this tree
#

iris.prunint <- prune(iris.rpint, cp = 0.1)

#  The pruned tree itself.

#newwindow()
plot(iris.prunint, 
     compress=TRUE,
     margin = .2)
text(iris.prunint, 
     use.n=TRUE, 
     all = TRUE,
     fancy = TRUE)

#
# Again a function only of petal-length and width
#

#newwindow()
plot(iris.df[train,"PetalWidth"],iris.df[train,"PetalLength"], 
     col = colours[train], main = "Recursive partitoning regions (with int)")
lines(x=c(0.8, 0.8), y = c(0,7), lty = 1)
x<- seq(0.8,2.5, .1)
y <- ( 2.725 ^ 2) / x
lines(x= x, y = y, lty = 2)
#
#  Classification table on the test data
#

table(iris.df$Species[-train], 
      predict(iris.prunint,
              newdata = iris.df[-train,],
              type = "class")
)
####################################################
#
#  A different tree model
#
####################################################


iris.rpint1 <-rpart(Species ~ sqrt(PetalWidth*PetalLength) +  
                      sqrt(SepalWidth*SepalLength),   
                    # formula: includes interactions
                    data = iris.df,                       # data frame
                    subset = train,                       # indices of training set
                    method = "class",                     # classification tree
                    parms = list(split = "information"),  # use entropy/deviance
                    maxsurrogate = 0,                     # since no missing values
                    cp = 0,                               # no size penalty
                    minsplit = 5,                         # Nodes of size 5 (or 
                    # more) can be split,
                    minbucket = 2,                        # provided each sub-node
                    # contains at least 2 obs.
)

#  Note that rpart cannot handle interaction terms
#  The above * is actually a multiplication.
#
#
#  The summary (can be huge for large trees).
#

summary(iris.rpint1)

#
#  The classification tree itself - plotted and labelled.
#  See help("plot.rpart") and help("text.rpart")
#  for parameter details.

newwindow()
plot(iris.rpint1, 
     uniform=TRUE,
     compress=TRUE,
     margin = .15)
text(iris.rpint1, 
     use.n=TRUE, 
     all = TRUE,
     fancy = TRUE
)

#

pred.rpint1 <- predict(iris.rpint1,
                       newdata = iris.df[-train,],
                       type = "class")


#
#  Classification table on the test data
#

table(iris.df$Species[-train], pred.rpint1)

#
# Some CP (complexity parameter) information
#

printcp(iris.rpint1)
newwindow()
plotcp(iris.rpint1)

#
#  Prune this tree
#

iris.prunint1 <- prune(iris.rpint1, cp = 0.1)

#  The pruned tree itself.

newwindow()
plot(iris.prunint1, 
     compress=TRUE,
     margin = .2)
text(iris.prunint1, 
     use.n=TRUE, 
     all = TRUE,
     fancy = TRUE)

#
#  Classification table on the test data
#

table(iris.df$Species[-train], 
      predict(iris.prunint1,
              newdata = iris.df[-train,],
              type = "class")
)