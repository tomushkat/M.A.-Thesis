setwd('D:\\Tom\\ROB23\\ET-Exp\\Data\\Clean united Data')


#################
#   ET data     #
#################

subjectNumber <- 31
subjectName   <- c()
for (i in 1:subjectNumber){
  subjectName <- c(subjectName, paste0("Index", i, ".csv", sep = ''))}
Data <- do.call(rbind, lapply(subjectName, function(x)
  read.csv(x, stringsAsFactors = FALSE)))
Data$Trial.y <- NULL
Data$X <- NULL


#########################
#     Behaioral data    #
#########################

behavioralData <- read.csv('Clean Behavioral.csv')

###############################
#   ET fixed data block 2     #
###############################
subjectNumber <- 31
subjectName   <- c()
for (i in 1:subjectNumber){
  subjectName <- c(subjectName, paste0(i, ' ', 'fixed data.csv'))}
Data <- do.call(rbind, lapply(subjectName, function(x)
  read.csv(x, stringsAsFactors = FALSE)))
Data$Trial.y <- NULL
Data$X <- NULL