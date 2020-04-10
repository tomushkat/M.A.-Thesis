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
  ylim(300, 2200) + xlim(1000, 1020) + ylab('Trial Time') + xlab('X axis') +
  ggtitle('Experimental reading')
Name <- paste0('Experimental reading.pdf')
ggsave(Name, width = 25, height = 25, units = "cm")

for (i in unique(dataX$ID)){
  Name = pate0('Experimental reading  participant ', i)
  ggplot(dataX[dataX$ID == i, ], aes(x = Mean, y = TrialTime)) + 
    geom_point(aes(color = SD)) +
    theme(axis.line.y = element_line(color = "black", size = 1),
          axis.line.x = element_line(color = "black", size = 1)) +
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 16, face = "bold")) +
    theme(legend.text = element_text(size = 14)) +
    ylim(300, 2200) + xlim(920, 1040) + ylab('Trial Time') + xlab('X axis') +
    ggtitle(Name)
  Name <- paste0('Experimental reading Subject', i, ".pdf")
  ggsave(Name, width = 25, height = 25, units = "cm")
}
###################
# Pp 25% together #
###################
readingData <- Data[Data$Block == 2, ]
readingData <- readingData[readingData$TrialTime >= 400 & readingData$TrialTime <= 2100, ]
readingData <- readingData[readingData$ID %in% acc$ID, ]

dataX <- ddply(readingData, c('ID', 'TrialTime'), summarise,
               Mean = mean(gx, na.rm = TRUE),
               SD   = sd(gx, na.rm = TRUE))
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
  ylim(300, 2200) + xlim(990, 1020) + ylab('Trial Time') + xlab('X axis') +
  ggtitle('Experimental reading Pp above 25% together')
Name <- paste0('Experimental reading  Pp above 0.25 together.pdf')
ggsave(Name, width = 25, height = 25, units = "cm")

for (i in unique(dataX$ID)){
  Name = paste0('Experimental reading Pp above 25% together participant ', i)
  ggplot(dataX[dataX$ID == i, ], aes(x = Mean, y = TrialTime)) + 
    geom_point(aes(color = SD)) +
    theme(axis.line.y = element_line(color = "black", size = 1),
          axis.line.x = element_line(color = "black", size = 1)) +
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 16, face = "bold")) +
    theme(legend.text = element_text(size = 14)) +
    ylim(300, 2200) + ylab('Trial Time') + xlab('X axis') +
    ggtitle(Name)
  Name <- paste0('Experimental reading Pp above 0.25 together Pp', i, ".pdf")
  ggsave(Name, width = 25, height = 25, units = "cm")
}

###################
# Pp 25% separatly #
###################
readingData <- Data[Data$Block == 2, ]
readingData <- readingData[readingData$TrialTime >= 400 & readingData$TrialTime <= 2100, ]
readingData <- readingData[readingData$ID %in% t, ]

dataX <- ddply(readingData, c('ID', 'TrialTime'), summarise,
               Mean = mean(gx, na.rm = TRUE),
               SD   = sd(gx, na.rm = TRUE))
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
  ggtitle('Experimental reading Pp above 25% separately')
Name <- paste0('Experimental reading  Pp above 0.25 separately.pdf')
ggsave(Name, width = 25, height = 25, units = "cm")

for (i in unique(dataX$ID)){
  Name = paste0('Experimental reading Pp above 25% separately participant ', i)
  ggplot(dataX[dataX$ID == i, ], aes(x = Mean, y = TrialTime)) + 
    geom_point(aes(color = SD)) +
    theme(axis.line.y = element_line(color = "black", size = 1),
          axis.line.x = element_line(color = "black", size = 1)) +
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 16, face = "bold")) +
    theme(legend.text = element_text(size = 14)) +
    ylim(300, 2200) + ylab('Trial Time') + xlab('X axis') +
    ggtitle(Name)
  Name <- paste0('Experimental reading Pp above 0.25 separately Pp', i, ".pdf")
  ggsave(Name, width = 25, height = 25, units = "cm")
}

