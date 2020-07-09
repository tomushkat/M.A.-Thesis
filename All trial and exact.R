
#b2Data <- Data[Data$Block == 2 & Data$ID %in% unique(tt), ]  # exlude failure in conditions seperatly
b2Data <- Data[Data$Block == 2, ]  # exlude failure in conditions together
unique(Data$ID)
#b2Data <- subset(b2Data, b2Data$newIsCorrect == 1)
sort(unique(b2Data$ID))
length(unique(b2Data$ID))
#aovData <- ddply(b2Data, c('ID', 'Answer', 'qType'), summarize,
#                 MeanY = mean(gy, na.rm = TRUE))
#shapiro.test(aovData$MeanY)
#summary(aov(MeanY ~ Answer * qType + Error(ID / Answer * qType), data = aovData))
#b1Data <- subset(Data, Data$Block == 1)
#Data <- NULL
b2Data$X <- NULL
b2Data$toMerge <- NULL

formixmodelData <- b2Data[b2Data$TrialTime >= 2200 & b2Data$TrialTime <= 2300, ]
mixmodelData <- ddply(formixmodelData, c('ID', 'Trial.x', 'Answer', 'qType', 'qID'), summarise,
                      Mean = mean(gy, na.rm = TRUE))
formixmodelData$qID <- as.factor(formixmodelData$qID)
summary(lmer(Mean ~ Answer * qType + (1 + Answer * qType + qID | ID)  + (1 + 1 | qID), REML = FALSE, data = mixmodelData))   
summary(lmer(Mean ~ Answer * qType + (1 + Answer * qType + qID | ID), REML = FALSE, data = mixmodelData))   
summary(lmer(Mean ~ Answer * qType + (1 + Answer * qType | ID), REML = FALSE, data = mixmodelData))   
summary(lmer(Mean ~ Answer * qType + (1 + Answer + Answer:qType | ID), REML = FALSE, data = mixmodelData))   
summary(lmer(Mean ~ Answer * qType + (1 + Answer:qType | ID), REML = FALSE, data = mixmodelData))   



anovalData <- ddply(formixmodelData, c('ID', 'Answer', 'qType'), summarise,
                    Mean = mean(gy, na.rm = TRUE))
yesNew <- anovalData[anovalData$Answer == 1 & anovalData$qType == 1, ]
yesOld <- anovalData[anovalData$Answer == 1 & anovalData$qType == -1, ]
noNew  <- anovalData[anovalData$Answer == -1 & anovalData$qType == 1, ]
noOld  <- anovalData[anovalData$Answer == -1 & anovalData$qType == -1, ]

shapiro.test(yesNew$Mean)
shapiro.test(yesOld$Mean)
shapiro.test(noNew$Mean)
shapiro.test(noOld$Mean)

ezANOVA(data = anovalData, dv = Mean, wid = ID, within = c(Answer, qType), detailed = TRUE)

mixmodelGraph <- ddply(formixmodelData, c('ID', 'Answer', 'qType'), summarise,
                       MeanData = mean(gy, na.rm = TRUE))
mixmodelGraph$Answer <- ifelse(mixmodelGraph$Answer == 1, 'Yes', 'No')
mixmodelGraph$qType <- ifelse(mixmodelGraph$qType == 1, 'New', 'Old')


mixmodelTable <- ddply(mixmodelGraph, c('Answer', 'qType'), summarise,
                       Mean = round(mean(MeanData, na.rm = TRUE), 2),
                       SD   = round(sd(MeanData, na.rm = TRUE), 2))

ggplot(mixmodelGraph, aes(x = qType, y = MeanData, fill = Answer)) +
  geom_dotplot(binaxis='y', stackdir = 'center', position=position_dodge(0.8))  +
  scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9")) + 
  stat_summary(fun.y = mean, geom = "point", shape = 18, size = 5,  aes(color = Answer) , position = position_dodge(0.8)) + 
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "pointrange", aes(color = Answer), position = position_dodge(0.8)) +
  xlab('Question type') + ylab('Converted mean y axis fixation') + ggtitle('Experimental all questions only participants above 40% accuracy') + 
  Theme +
  Lbls

exactGraphData <- b2Data[b2Data$TrialTime <= 3000, ]

Yes             <- 1
colorYes        <- 'blue'
No              <- -1
colorNo         <- 'red' 

# graphs for group
graphTitel <- c("Experimental all questions No")

GraphData <- b2Data %>%
  group_by(ID, TrialTime, Answer) %>%
  summarise(Mean = mean(gy, na.rm = TRUE)) %>%
  filter(Answer == -1) %>%
  ggplot(aes(x = TrialTime, y = Mean)) + 
  geom_point(stat = 'summary', fun = 'mean', color = 'black', size = 1) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
               geom = "errorbar", color = colorNo, width = 0.2, alpha = 0.1) +
  xlab("Time") + ylab("Converted hight") + ggtitle(graphTitel) +
  ylim(0, 1080) +
  Theme



for (Pp in unique((b2Data$ID))){
  data <- exactGraphData[exactGraphData$ID == Pp, ]
  graphTitel <- paste0("Exact all questions paticipant ", Pp)
  graphName <- paste0("Exact all questions participant ", Pp, ".pdf")
  
  ggplot(data, aes(x = TrialTime, y = gy)) + 
    geom_point(stat = 'summary', fun.y = 'mean', color = 'black', size = 1) +
    stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
                 geom = "errorbar", color = 'green', width = 0.2, alpha = 0.1) +
    xlab("Time") + ylab("Converted hight") + ggtitle(graphTitel) +
    ylim(200, 900) +
    Theme
  ggsave(graphName, width = 40, height = 28, units = "cm")
  
}


