# Source: https://www.h2o.ai/blog/finally-you-can-plot-h2o-decision-trees-in-r/

library(data.table)
library(rattle)
library(RColorBrewer)
library(rpart)

##### Load Data
titanicDT = fread("https://s3.amazonaws.com/h2o-public-test-data/smalldata/gbm_test/titanic.csv")

##### Data Engineering

# Titles mapping
TITLES = data.frame(
  from=c("Capt", "Col", "Major", "Jonkheer", 
         "Don", "Sir", "Dr", "Rev", "the Countess", 
         "Mme", "Mlle", "Ms", "Mr", "Mrs", "Miss", "Master", "Lady"),
  to = c("Officer", "Officer", "Officer", "Royalty", "Royalty", 
         "Royalty", "Officer", "Officer", "Royalty",
         "Mrs", "Miss", "Mrs", "Mr", "Mrs", "Miss", "Master", "Royalty"),
  stringsAsFactors = FALSE)

# Create features
titanicDT[, 
          c("sex", "embarked", "survived", "pclass", "cabin_type",
            "family_size", "family_type","title") := 
            list(
              factor(sex, labels = c("Female","Male")),
              factor(embarked, labels = c("", "Cherbourg",
                                          "Queenstown","Southampton")),
              factor(-survived, labels = c('Yes','No')),
              factor(pclass, labels = c("Class 1","Class 2","Class 3")),
              as.factor(substring(cabin, 1, 1)),
              sibsp + parch,
              as.factor(ifelse(sibsp + parch <= 1, "SINGLE", 
                               ifelse(sibsp + parch <= 3, "SMALL", "LARGE"))),
              as.factor(sapply(strsplit(name, "[\\., ]+"), function(x) {
                words = trimws(x)
                words = words[!words=="" ]
                words = words[words %in% TITLES$from]
                if (length(words) > 0) 
                  title_word = words[[1]]
                else
                  return(NA)
                return(TITLES[title_word == TITLES$from, 'to'])
              }))
            )]

# Handle missing values by imputing them with nulls
titanicDT[, c("age","fare") :=
            list(ifelse(is.na(age), mean(age, na.rm=T), age),
                 ifelse(is.na(fare), mean(fare, na.rm=T), fare)),
          by = c("survived","sex","embarked")]

# create dataset for Titanic survived predictive model    
response = "survived"
predictors = setdiff(
  colnames(titanicDT), 
  c(response,"name","ticket","cabin","boat","body","home.dest"))

titanicDT = titanicDT[, c(response, predictors), with=FALSE]

##### Simple rpart plotting
titanic.rp <-rpart(survived ~ . ,                         # formula: . means all
                   data = titanicDT,                       # data frame
                   method = "class",                     # classification tree
                   parms = list(split = "information"),
                   maxsurrogate = 1, 
                   cp = 0,
                   minsplit = 50,
                   minbucket = 20,

)

summary(titanic.rp)

plot(titanic.rp, 
     uniform=TRUE,
     compress=TRUE,
     margin = .1)
text(titanic.rp, 
     use.n=TRUE, 
     all = TRUE,
     fancy = TRUE)

fancyRpartPlot(titanic.rp, main="Decision Tree Graph")

#####

library(h2o)
h2o.init()

titanicHex = as.h2o(titanicDT)

# split into train and validation
splits = h2o.splitFrame(data = titanicHex, ratios = .8, seed = 1234)
trainHex = splits[[1]]
validHex = splits[[2]]

# Build and train the model:
titanic_2tree <- h2o.randomForest(x = predictors,
                             y = response,
                             ntrees = 5,
                             max_depth = 5,
                             min_rows = 20,
                             binomial_double_trees = TRUE,
                             training_frame = trainHex,
                             validation_frame = validHex)

titanicH2oTree2 = h2o.getModelTree(model = titanic_2tree, tree_number = 2)

library(data.tree)

createDataTree <- function(h2oTree) {
  h2oTreeRoot = h2oTree@root_node
  dataTree = Node$new(h2oTreeRoot@split_feature)
  dataTree$type = 'split'
  addChildren(dataTree, h2oTreeRoot)
  return(dataTree)
}

addChildren <- function(dtree, node) {
  
  if(class(node)[1] != 'H2OSplitNode') return(TRUE)
  
  feature = node@split_feature
  id = node@id
  na_direction = node@na_direction
  
  if(is.na(node@threshold)) {
    leftEdgeLabel = printValues(node@left_levels, 
                                na_direction=='LEFT', 4)
    rightEdgeLabel = printValues(node@right_levels, 
                                 na_direction=='RIGHT', 4)
  }else {
    leftEdgeLabel = paste("<", node@threshold, 
                          ifelse(na_direction=='LEFT',',NA',''))
    rightEdgeLabel = paste(">=", node@threshold, 
                           ifelse(na_direction=='RIGHT',',NA',''))
  }
  
  left_node = node@left_child
  right_node = node@right_child
  
  if(class(left_node)[[1]] == 'H2OLeafNode')
    leftLabel = paste("prediction:", left_node@prediction)
  else
    leftLabel = left_node@split_feature
  
  if(class(right_node)[[1]] == 'H2OLeafNode')
    rightLabel = paste("prediction:", right_node@prediction)
  else
    rightLabel = right_node@split_feature
  
  if(leftLabel == rightLabel) {
    leftLabel = paste(leftLabel, "(L)")
    rightLabel = paste(rightLabel, "(R)")
  }
  
  dtreeLeft = dtree$AddChild(leftLabel)
  dtreeLeft$edgeLabel = leftEdgeLabel
  dtreeLeft$type = ifelse(class(left_node)[1] == 'H2OSplitNode', 'split', 'leaf')
  
  dtreeRight = dtree$AddChild(rightLabel)
  dtreeRight$edgeLabel = rightEdgeLabel
  dtreeRight$type = ifelse(class(right_node)[1] == 'H2OSplitNode', 'split', 'leaf')
  
  addChildren(dtreeLeft, left_node)
  addChildren(dtreeRight, right_node)
  
  return(FALSE)
}

printValues <- function(values, is_na_direction, n=4) {
  l = length(values)
  if(l == 0)
    value_string = ifelse(is_na_direction, "NA", "")
  else
    value_string = paste0(paste0(values[1:min(n,l)], collapse = ', '),
                          ifelse(l > n, ",...", ""),
                          ifelse(is_na_direction, ", NA", ""))
  return(value_string)
}

library(DiagrammeR)

titanicDataTree = createDataTree(titanicH2oTree2)

GetEdgeLabel <- function(node) {return (node$edgeLabel)}
GetNodeShape <- function(node) {switch(node$type, 
                                       split = "diamond", leaf = "oval")}
GetFontName <- function(node) {switch(node$type, 
                                      split = 'Palatino-bold', 
                                      leaf = 'Palatino')}
SetEdgeStyle(titanicDataTree, fontname = 'Palatino-italic', 
             label = GetEdgeLabel, labelfloat = TRUE,
             fontsize = "26", fontcolor='royalblue4')
SetNodeStyle(titanicDataTree, fontname = GetFontName, shape = GetNodeShape, 
             fontsize = "26", fontcolor='royalblue4',
             height="0.75", width="1")

SetGraphStyle(titanicDataTree, rankdir = "LR", dpi=70.)

plot(titanicDataTree, output = "graph")

titanic_2tree

h2o.shutdown()
