
readingData <- Data[Data$Block == 1, ]
readingData <- readingData[readingData$TrialTime >= 400 & readingData$TrialTime <= 2100, ]

screenCenter <- 960
dataX <- ddply(readingData, c('ID', 'TrialTime'), summarise,
               Mean = mean(gx, na.rm = TRUE),
               SD = sd(gx, na.rm = TRUE))
#Vector <- c()
#for (i in unique(dataX$ID)){
#  data <- subset(dataX, dataX$ID == i)
#  Max <- max(data$Mean) - data$Mean

#  Vector <- c(Vector, Max)
#}
#dataX$Max <- Vector

dataxx <- ddply(dataX, c('TrialTime'), summarise,
                theMean = mean(Mean, na.rm = TRUE),
                theSD   = sd(Mean, na.rm = TRUE))

ggplot(dataxx, aes(x = theMean, y = TrialTime)) + 
  geom_point(aes(color = theSD)) +
  theme(axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16, face = "bold")) +
  theme(legend.text = element_text(size = 14)) +
  ylim(300, 2200) + ylab('Trial Time') + xlab('X axis') +
  ggtitle('Induction reading')
Name <- paste0('Induction reading.pdf')
ggsave(Name, width = 25, height = 25, units = "cm")






for (i in unique(dataX$ID)){
  Name <- paste0('Induction reading participant ', i)
  ggplot(dataX[dataX$ID == i, ], aes(x = Mean, y = TrialTime)) + 
    geom_point(aes(color = SD)) +
    theme(axis.line.y = element_line(color = "black", size = 1),
          axis.line.x = element_line(color = "black", size = 1)) +
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 16, face = "bold")) +
    theme(legend.text = element_text(size = 14)) +
    ylim(300, 2200) + ylab('Trial Time') + xlab('X axis') +
    ggtitle(Name)
  Name <- paste0('Induction reading Subject', i, ".pdf")
  ggsave(Name, width = 25, height = 25, units = "cm")
}











readingData <- Data[Data$Block == 2, ]
readingData <- readingData[readingData$TrialTime >= 400 & readingData$TrialTime <= 2100, ]

screenCenter <- 960
dataX <- ddply(readingData, c('ID', 'TrialTime'), summarise,
               Mean = mean(gx, na.rm = TRUE),
               SD = sd(gx, na.rm = TRUE))
#Vector <- c()
#for (i in unique(dataX$ID)){
#  data <- subset(dataX, dataX$ID == i)
#  Max <- max(data$Mean) - data$Mean

#  Vector <- c(Vector, Max)
#}
#dataX$Max <- Vector

dataxx <- ddply(dataX, c('TrialTime'), summarise,
                theMean = mean(Mean, na.rm = TRUE),
                theSD = sd(Mean, na.rm = TRUE))

ggplot(dataxx, aes(x = theMean, y = TrialTime)) + 
  geom_point(aes(color = theSD)) +
  theme(axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16, face = "bold")) +
  theme(legend.text = element_text(size = 14)) +
  ylim(300, 2200) + ylab('Trial Time') + xlab('X axis') +
  ggtitle('Experimental reading')
Name <- paste0('Experimental reading.pdf')
ggsave(Name, width = 25, height = 25, units = "cm")






for (i in unique(dataX$ID)){
  Name <- paste0('Experimental reading participant ', i)
  ggplot(dataX[dataX$ID == i, ], aes(x = Mean, y = TrialTime)) + 
    geom_point(aes(color = SD)) +
    theme(axis.line.y = element_line(color = "black", size = 1),
          axis.line.x = element_line(color = "black", size = 1)) +
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 16, face = "bold")) +
    theme(legend.text = element_text(size = 14)) +
    ylim(300, 2200) + ylab('Trial Time') + xlab('X axis') +
    ggtitle(Name)
  Name <- paste0('Experimental reading Subject', i, ".pdf")
  ggsave(Name, width = 25, height = 25, units = "cm")
}
