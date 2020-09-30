library(h2o)
localH2O = h2o.init()

getwd()
setwd("/Users/bblanchard006/Desktop/SMU/QTW/Case Studies/Unit 6 Case Study")

load("data.Rda") 

spamNotSpam.hex <- as.h2o(emailDFrp)

h2o.shutdown()

