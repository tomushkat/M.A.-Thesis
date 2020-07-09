

setwd('D:\\Tom\\ROB23\\ET-Exp\\Data\\Behavioral data')

subjectNumber <- 51
subjectName   <- c()
for (i in 1:subjectNumber){
  subjectName <- c(subjectName, paste0("Log", i, ".csv", sep = ''))}
dataBehavioral <- do.call(rbind, lapply(subjectName, function(x)
  read.csv(x, stringsAsFactors = FALSE)))

#######################3
# changing data        #
#######################
dataBehavioral$Question  <- NULL
dataBehavioral$GisX      <- NULL
dataBehavioral$GisY      <- NULL
dataBehavioral$CenterX   <- NULL
dataBehavioral$CenterY   <- NULL
dataBehavioral$Group     <- NULL
dataBehavioral$Congruent <- NULL
dataBehavioral$Show      <- NULL

dataBehavioral$Gender    <- as.factor(dataBehavioral$Gender)
dataBehavioral$QID       <- as.factor(dataBehavioral$QID)
dataBehavioral$SubBlock  <- as.factor(dataBehavioral$SubBlock)
dataBehavioral$Language  <- as.factor(dataBehavioral$Language)
#t <- ifelse(dataBehavioral$Gender == "F", 
#                                ifelse(dataBehavioral$QID == 1, 2,
#                                       ifelse(dataBehavioral$QID == 2, 1,
#                                              ifelse(dataBehavioral$QID == 11, 12,
#                                                     ifelse(dataBehavioral$QID == 12, 11, dataBehavioral$QID)))), dataBehavioral$QID)
#dataBehavioral$newQID <- t  # QID men same as weman
#dataBehavioral$newQID <- as.factor(dataBehavioral$newQID)

dataBehavioral$newBlock  <- ifelse(dataBehavioral$Block > 1, dataBehavioral$Block - 1, dataBehavioral$Block)

dataBehavioral$QIDUnite <- ifelse(dataBehavioral$QID == 13, 1314,
                                  ifelse(dataBehavioral$QID == 14, 1314,
                                         ifelse(dataBehavioral$QID == 15, 1516,
                                                ifelse(dataBehavioral$QID == 16, 1516,
                                                       ifelse(dataBehavioral$QID == 17, 1718,
                                                              ifelse(dataBehavioral$QID == 18, 1718,
                                                                     ifelse(dataBehavioral$QID == 19, 1920,
                                                                            ifelse(dataBehavioral$QID == 20, 1920,
                                                                                   ifelse(dataBehavioral$QID == 21, 2122,
                                                                                          ifelse(dataBehavioral$QID == 22, 2122, 
                                                                                                 ifelse(dataBehavioral$QID == 23, 2324, 
                                                                                                        ifelse(dataBehavioral$QID == 24, 2324,
                                                                                                               ifelse(dataBehavioral$QID == 25, 2526,
                                                                                                                      ifelse(dataBehavioral$QID == 26, 2526, 
                                                                                                                             ifelse(dataBehavioral$QID == 27, 2728,
                                                                                                                                    ifelse(dataBehavioral$QID == 28, 2728, dataBehavioral$QID))))))))))))))))

dataBehavioral$qType <- ifelse(dataBehavioral$QIDUnite >= 1314 & dataBehavioral$QIDUnite <= 1920, "New",
                                       ifelse(dataBehavioral$QIDUnite >= 2122 & dataBehavioral$QIDUnite <= 2728, "Old", "Other"))
dataBehavioral$QIDUnite <- as.factor(dataBehavioral$QIDUnite)

##### End chaging data  ####


NPp     <- length(unique(dataBehavioral$ID))
NBlocks <- length(unique(dataBehavioral$newBlock))


########################
#  cleaning RT         #
########################
endQuestionTime <- 2350
dataBehavioral$cleanRT <- ifelse(dataBehavioral$newBlock <= 2 , 
                                 ifelse(dataBehavioral$RT < endQuestionTime | dataBehavioral$RT > 10000, NA, dataBehavioral$RT), dataBehavioral$RT)
# same vector - "cleanRT" - w/o 3SD. 
theVector <- c()
for (Pp in unique(dataBehavioral$ID)){
  data <- subset(dataBehavioral, dataBehavioral$ID == Pp)
  d1 <- data$cleanRT[data$newBlock == 1]
  d3 <- data$cleanRT[data$newBlock == 2]
  d4 <- data$cleanRT[data$newBlock == 3]
  d5 <- data$cleanRT[data$newBlock == 4]
  
  Mean1 <- mean(d1, na.rm = TRUE)
  SD1   <- sd(d1, na.rm = TRUE)
  d1  <- ifelse(d1 < (Mean1 - 3 * SD1) | d1 > (Mean1 + 3 * SD1), NA, d1)  
  Mean3 <- mean(d3, na.rm = TRUE)
  SD3   <- sd(d3, na.rm = TRUE)
  d3  <- ifelse(d3 < (Mean3 - 3 * SD3) | d3 > (Mean3 + 3 * SD3), NA, d3)
  D  <- c(d1, d3, d4, d5)
  theVector <- c(theVector, D)
}

dataBehavioral$cleanRT <- theVector 

data <- NULL
D <- NULL
d1 <- NULL
d2 <- NULL
d3 <- NULL
d4 <- NULL
d5 <- NULL


# new vector - "newIsCorrect - is correct w/o bad RT tirals. For OT and catch to use regular IsCorrect
dataBehavioral$newIsCorrect <- ifelse(is.na(dataBehavioral$cleanRT), NA, dataBehavioral$IsCorrect)
dataBehavioral$answerET  <- ifelse(dataBehavioral$Answer == 2, -1, 1)                  ## Yes == 1, No == -1
dataBehavioral$questionTypeET <- ifelse(dataBehavioral$qType == "New", 1, -1) ## newQ == 1, oldQ == -1
dataBehavioral$goodTrials <- ifelse(is.na(dataBehavioral$newIsCorrect), NA, dataBehavioral$BlockTrial)
dataBehavioral$goodBlocks <- ifelse(is.na(dataBehavioral$newIsCorrect), NA, dataBehavioral$newBlock)

Badpp <- c(40, 32, 9, 13, 33, 38, 8, 45, 35, 34, 1, 2, 3, 4, 23, 10, 49, 50)
forMerging <- dataBehavioral[! dataBehavioral$ID %in% Badpp, ]

#setwd('D:\\Tom\\ROB23\\ET-Exp\\Clean Data')
#write.csv(forMerging, 'Clean Behavioral.csv')

dForETtoMerge <- cbind(forMerging$ID, forMerging$goodBlocks, forMerging$SubBlock, forMerging$goodTrials, forMerging$answerET, forMerging$questionTypeET, forMerging$QIDUnite, forMerging$newIsCorrect)
dForETtoMerge <- as.data.frame(dForETtoMerge)    # the data I want to merge with the ET data
dimnames(dForETtoMerge)[[2]] <- c("ID", "newBlock", 'subBlock', "Trial", 'Answer', 'qType', 'qID', 'newIsCorrect')
dForETtoMerge <- subset(dForETtoMerge, !is.na(dForETtoMerge$Trial))          # the data I want to remoev from the ET data
dForETtoMerge$toMerge <- as.numeric(paste0(dForETtoMerge$ID, dForETtoMerge$newBlock, dForETtoMerge$Trial))
dForETtoMerge$toMerge <- ifelse(dForETtoMerge$ID >= 10,  as.numeric(paste0(dForETtoMerge$toMerge, 11)), dForETtoMerge$toMerge)

#forMerging <- NULL
#dForETtoMerge$Trial <- NULL
#for (i in 1:length(unique(dForETtoMerge$ID))){
#  data <- subset(dForETtoMerge, dForETtoMerge$ID == i)
#  Name <- paste0('Pp', i, '.csv')
#  write.csv(data, Name)
#}
#write.csv(dForETtoMerge, 'dForETtoMerge.csv')   # clean data (Pp and trials) to merge with the ET DATA

#unique(dForETtoMerge$ID)

##### End cleaning data ####
#cleanDataBehavioral <- read.csv('dataBehavioral.csv', header = TRUE, sep = ",", stringsAsFactors = FALSE)
#dataBehavioral <- NULL