
setwd('D:\\Tom\\ROB23\\newETData\\CSVs\\full data\\With pupil size')
### Loading raw Data ###

subjectNumber <- 52
subjectName   <- c()
for (i in 5:subjectNumber){
  subjectName <- c(subjectName, paste0("EDF", i, ".csv", sep = ''))}
dataEyeTraker <- do.call(rbind, lapply(subjectName, function(x)
  read.csv(x, stringsAsFactors = FALSE)))



###################
#  changing data  #
###################

# removing NaN from dataEyeTraker
dataEyeTraker <- dataEyeTraker[complete.cases(dataEyeTraker$ID), ]    
dataEyeTraker <- dataEyeTraker[dataEyeTraker$TrialTime <= 10000, ]    
dataEyeTraker <- dataEyeTraker[dataEyeTraker$Block <= 3, ]
dataEyeTraker <- dataEyeTraker[dataEyeTraker$ID != 17, ]
#dataEyeTraker$gx <- NULL


## making block 1:3
dataEyeTraker$Block <- ifelse(dataEyeTraker$Block == 3, 2, dataEyeTraker$Block)  


# reducing one in ID larger tha 16
dataEyeTraker$ID  <- ifelse(dataEyeTraker$ID > 17, dataEyeTraker$ID - 1, dataEyeTraker$ID) 


#dataEyeTraker$Type <- NULL
Badpp <- c(40, 32, 9, 13, 33, 38, 8, 45, 35, 34, 1, 2, 3, 4, 23, 10, 49, 50)               # for data with suspects

dataEyeTraker <- dataEyeTraker[! dataEyeTraker$ID %in% Badpp, ]
sort(unique(dataEyeTraker$ID))
length(unique(dataEyeTraker$ID))


# removing bad trials
dataEyeTraker$toMerge <- as.numeric(paste0(dataEyeTraker$ID, dataEyeTraker$Block, dataEyeTraker$Trial))
dataEyeTraker$toMerge <- ifelse(dataEyeTraker$ID >= 10,  as.numeric(paste0(dataEyeTraker$toMerge, 11)),
                                dataEyeTraker$toMerge)
#dForETtoMerge <- read.csv('dForETtoMerge.csv')
dataEyeTraker <- dataEyeTraker[dataEyeTraker$toMerge %in% dForETtoMerge$toMerge, ]
sort(unique(dataEyeTraker$ID))
length(unique(dataEyeTraker$ID))

dForETtoMerge$newBlock <- NULL
dForETtoMerge$ID <- NULL
dataBehavioral <- NULL
forMerging <- NULL
Data <- merge(dataEyeTraker, dForETtoMerge, by = 'toMerge')
dataEyeTraker <- NULL
dForETtoMerge <- NULL
#sort(unique(Data$ID))
#length(unique(Data$ID))

setwd('D:\\Tom\\ROB23\\ET-Exp\\Data\\Clean united Data')
Index <- 1
for (i in unique(Data$ID)){
  Name <- paste0('Index', Index, ".csv")
  Index <- Index + 1
  write.csv(Data[Data$ID == i, ], Name)
}
#######################
#  End changing data  #
#######################

### Loading clean and merged data ###



B2 <- subset(Data, Data$Block == 2)
B2 <- B2[B2$TrialTime >= 2150 & B2$TrialTime <= 2300, ]

B2$Answer <- as.factor(ifelse(B2$Answer == 1, 'Yes', ifelse(B2$Answer == -1, "No", NA)))
levels(B2$Answer)
contrasts(B2$Answer)
contrasts(B2$Answer)[1] <- -1
B2$qType <- as.factor(ifelse(B2$qType == 1, 'New', ifelse(B2$qType == -1, "Old", NA)))
levels(B2$qType)[1]
contrasts(B2$qType)[1] <- 1
contrasts(B2$qType)[2] <- -1
B2$ID <- as.character(B2$ID)
B2$qID <- as.factor(as.character(B2$qID))
levels(B2$qID)
contrasts(B2$qID)[1] <- -1

data <- ddply(B2, c('ID', 'Trial.x', 'Answer', 'qType', 'qID'), summarise,
              Mean = mean(gy, na.rm = TRUE))
data <- data[complete.cases(data$Mean), ]
summary(lmer(Mean ~ Answer * qType + Trial.x + (1 + Answer * qType + Trial.x | ID) + (1 | qID), REML = FALSE, data = data))
summary(lmer(Mean ~ Answer * qType + (1 + Answer * qType | ID) + (1 | qID), REML = FALSE, data = data))
summary(lmer(Mean ~ Answer * qType + (1 + Answer * qType | ID), REML = FALSE, data = data))
summary(lmer(Mean ~ Answer * qType + (1 + Answer + Answer:qType | ID), REML = FALSE, data = data))
summary(lmer(Mean ~ Answer * qType + (1 + Answer:qType | ID), REML = FALSE, data = data))

summary(lmer(Mean ~ Answer * qType + (1 + Answer | ID), REML = FALSE, data = data))
summary(lmer(Mean ~ Answer * qType + (1 | ID), REML = FALSE, data = data))
data2 <- ddply(B2, c('ID', 'Answer', 'qType'), summarise,
               Mean = mean(gy, na.rm = TRUE))
data2$Answer <- as.factor(data2$Answer)
data2$qType <- as.factor(data2$qType)
ggplot(data2, aes(x = qType, y = Mean, fill = Answer)) +
  geom_dotplot(binaxis='y', stackdir = 'center', position=position_dodge(0.8))  +
  scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9")) + 
  stat_summary(fun.y = mean, geom = "point", shape = 18, size = 5,  aes(color = Answer) , position = position_dodge(0.8)) + 
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "pointrange", aes(color = Answer), position = position_dodge(0.8)) +
  xlab('Question type') + ylab('converted y axis') + ggtitle('Experimental all questions without 30 and 31 two by two dotplot Accuracy') + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text   = element_text(size = 12),
        axis.title  = element_text(size = 14, face = "bold"),
        axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(legend.text = element_text(size = 14))  
geom_label_repel(aes(label = ID),
                 box.padding   = 0.35, 
                 point.padding = 0.5,
                 segment.color = 'grey50', position=position_dodge(0.8))
ggsave('With names Experimental all questions without 30 and 31 two by two dotplot Accuracy.pdf', width = 40, height = 28, units = "cm")

