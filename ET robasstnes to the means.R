
b2Data <- subset(Data, Data$Block == 2)
b2Data$X <- NULL
b2Data$toMerge <- NULL
formixmodelData <- b2Data[b2Data$TrialTime >= 2150 & b2Data$TrialTime <= 2250, ]
formixmodelData$Answer <- ifelse(formixmodelData$Answer == 1, 'Yes', 'No')
formixmodelData$qType <- ifelse(formixmodelData$qType == 1, 'New', 'Old')
formixmodelData$pair <- paste0(formixmodelData$Answer, formixmodelData$qType)


boot <- function(dataBoot){
  Trial <- c()
  Length <- length(dataBoot[, 5])
  for (i in c(1:1000)){
    bootCondition <- sample(dataBoot[, 5], size = 1)
    Trial <- c(Trial, bootCondition)
  }
  return(Trial)
}

doBootstrap <- function(data, strCondition, ID){
  data <- data[data[, 2] == ID, ]
  robustnessMean <- ddply(data, c('Answer', 'qType'), summarise,
                          meanID = mean(gy, na.rm = TRUE))
  robustnessMean$pair <- paste0(robustnessMean$Answer, robustnessMean$qType)
  robustnessMean <- robustnessMean[(robustnessMean[, 4] == strCondition) , 3]
  
  robustnessID <- ddply(data, c('Trial', 'Answer', 'qType', 'pair'), summarise,
                        Mean = mean(gy, na.rm = TRUE))
  robustnessID <- robustnessID[robustnessID[, 4] == strCondition,  ]
  
  bootstrapData <- boot(dataBoot = robustnessID)
  
  #tTestMean <- mean(robustnessID[, 5], na.rm = TRUE)
  #Test <- t.test(bootstrapData, mu = tTestMean)
  #pass <- ifelse(Test$p.value >= 0.05, 1, 0)
  #Answer <- cbind(pass, Test$statistic)
  return(bootstrapData)
}

dataFrame <- data.frame()
Pp <- unique(b2Data$ID)
for (subject in Pp){
  YesNewQ <- doBootstrap(data = formixmodelData, strCondition = 'YesNew', ID = subject)
  YesOldQ <- doBootstrap(data = formixmodelData, strCondition = 'YesOld', ID = subject)
  NoNewQ  <- doBootstrap(data = formixmodelData, strCondition = 'NoNew', ID = subject)
  NoOldQ  <- doBootstrap(data = formixmodelData, strCondition = 'NoOld', ID = subject)
  Pass <- rbind(YesNewQ, YesOldQ, NoNewQ, NoOldQ)
  Condition <- c("YesNew", "YesOld", "NoNew", "NoOld")
  ID <- rep(subject, 4)
  data <- data.frame(ID, Condition, Pass)
  dataFrame <- rbind(dataFrame, data)
}


d <- ddply(formixmodelData, c('ID', 'pair', 'Trial'), summarise,
           Mean = mean(gy, na.rm = TRUE)) 

data <- ddply(d, c('ID', 'pair'), summarise,
              M = round(mean(Mean, na.rm = TRUE), 2), 
              SD   = round(sd(Mean, na.rm = TRUE), 2),
              Max  = max(Mean, na.rm = TRUE),
              Min  = min(Mean, na.rm = TRUE))



for (Participant in unique(formixmodelData$ID)){
  df <- c()
  df2 <- c()
  data1 <- subset(formixmodelData, formixmodelData$ID == Participant)
  for (Condition in unique(formixmodelData$pair)){
    data2 <- subset(data1, data1$pair == Condition)
    data3 <- ddply(data2, c('Trial'), summarise,
                   Mean = mean(gy, na.rm = TRUE))
    scaleMean <- scale(na.omit(data3$Mean))
    nonScaleMean <- na.omit(data3$Mean)
    Mean <- round(mean(data3$Mean, na.rm = TRUE), 1)
    SD   <- round(sd(data3$Mean, na.rm = TRUE), 1)
    Max  <- round(max(data3$Mean, na.rm = TRUE))
    Min  <- round(min(data3$Mean, na.rm = TRUE))
    Name <- paste0(Condition, " ", 'M=', Mean[1], " ", 'SD=', SD[1], " ", 'R=', Min[1], "-", Max[1])
    dataframe <- cbind(Participant, Name, scaleMean, nonScaleMean, Mean, SD, Max, Min)
    dataframe <- as.data.frame(dataframe)
    df <- rbind(df, dataframe)
    df$V3 <- as.numeric(as.character(df$V3)) 
    df$nonScaleMean <- as.numeric(as.character(df$nonScaleMean)) 
    
  }
  Name1 <- paste0('Boxplot ID = ', dataframe$Participant[1])
  
  
  ggplot(df, aes(x = Name, y = nonScaleMean)) +
    geom_boxplot(outlier.color = 'red') +
    xlab('Condition') + ylab('Eye gaze values') + ggtitle(Name1) + 
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text   = element_text(size = 12),
          axis.title  = element_text(size = 14, face = "bold"),
          axis.line.y = element_line(color = "black", size = 1),
          axis.line.x = element_line(color = "black", size = 1)) +
    theme(axis.line.y = element_line(color = "black", size = 1),
          axis.line.x = element_line(color = "black", size = 1)) +
    theme(legend.text = element_text(size = 14))  
  
  ggsave(paste0(Name1, '.pdf'), width = 40, height = 28, units = "cm")
  
  data4 <- ddply(df, c('Name'), summarise,
                 meanEyeGaze = Mean[1],
                 sdEyeGaze = SD[1])
  data4$meanEyeGaze <- as.numeric(as.character(data4$meanEyeGaze))
  data4$sdEyeGaze <- as.numeric(as.character(data4$sdEyeGaze))
  Name2 <- paste0('Bars ID = ', dataframe$Participant[1])
  ggplot(data4, aes(x = Name, y = meanEyeGaze)) +
    geom_point() +
    geom_errorbar(aes(ymin = meanEyeGaze - sdEyeGaze, ymax = meanEyeGaze + sdEyeGaze), width = 0.1,
                  position = position_dodge(0.2), color = "black") +
    xlab('Condition') + ylab('Eye gaze values') + ggtitle(Name1) + 
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text   = element_text(size = 12),
          axis.title  = element_text(size = 14, face = "bold"),
          axis.line.y = element_line(color = "black", size = 1),
          axis.line.x = element_line(color = "black", size = 1)) +
    theme(axis.line.y = element_line(color = "black", size = 1),
          axis.line.x = element_line(color = "black", size = 1)) +
    theme(legend.text = element_text(size = 14))  
  
  ggsave(paste0(Name2, '.pdf'), width = 40, height = 28, units = "cm")
  #df2 <- rbind(df2, df)
}


for (Participant in unique(formixmodelData$ID)){
  data1 <- subset(formixmodelData, formixmodelData$ID == Participant)
  for (Condition in unique(formixmodelData$pair)){
    df <- c()
    data2 <- subset(data1, data1$pair == Condition)
    data3 <- ddply(data2, c('Trial'), summarise,
                   Mean = mean(gy, na.rm = TRUE))
    scaleMean <- scale(na.omit(data3$Mean))
    nonScaleMean <- na.omit(data3$Mean)
    Mean <- round(mean(data3$Mean, na.rm = TRUE), 1)
    SD   <- round(sd(data3$Mean, na.rm = TRUE), 1)
    Max  <- round(max(data3$Mean, na.rm = TRUE))
    Min  <- round(min(data3$Mean, na.rm = TRUE))
    Name <- paste0(Condition, " ", 'M=', Mean[1], " ", 'SD=', SD[1], " ", 'R=', Min[1], "-", Max[1])
    dataframe <- cbind(Participant, Name, scaleMean, nonScaleMean, Mean, SD, Max, Min)
    dataframe <- as.data.frame(dataframe)
    df <- rbind(df, dataframe)
    df$nonScaleMean <- as.numeric(as.character(df$nonScaleMean)) 
    
    Name1 <- paste0('Histogram ID = ', dataframe$Participant[1], " ", 'Condition ', Condition)
    
    ggplot(df, aes(nonScaleMean)) +
      geom_histogram(bins = length(df$nonScaleMean)) +
      xlab(paste0('Eye gaze values Mean = ', Mean, ' ', 'SD = ', SD)) + ylab('Density') + ggtitle(Name1) + 
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(axis.text   = element_text(size = 12),
            axis.title  = element_text(size = 14, face = "bold"),
            axis.line.y = element_line(color = "black", size = 1),
            axis.line.x = element_line(color = "black", size = 1)) +
      theme(axis.line.y = element_line(color = "black", size = 1),
            axis.line.x = element_line(color = "black", size = 1)) +
      theme(legend.text = element_text(size = 14))  
    
    ggsave(paste0(Name1, '.pdf'), width = 40, height = 28, units = "cm")
  }
}

