
b1Data <- subset(Data, Data$Block == 1)
#b1Data <- subset(b1Data, b1Data$newIsCorrect == 1)

b1Data$X <- NULL
b1Data$toMerge <- NULL
formixmodelData <- b1Data[b1Data$TrialTime >= 2200 & b1Data$TrialTime <= 2550, ]
mixmodelData <- ddply(formixmodelData, c('ID', 'Trial', 'Answer', 'qType', 'qID'), summarise,
                      Mean = mean(gy, na.rm = TRUE))

summary(lmer(Mean ~ Answer * qType + Trial + (1 + Answer * qType + Trial + qID | ID)  + (1 | qID), REML = FALSE, data = mixmodelData))    # Max model
summary(lmer(Mean ~ Answer * qType + Trial + (1 + Answer * qType + qID | ID)  + (1 | qID), REML = FALSE, data = mixmodelData))            # removing Trial as random effect
summary(lmer(Mean ~ Answer * qType + (1 + Answer * qType + qID | ID)  + (1 | qID), REML = FALSE, data = mixmodelData))                    # removing Trial as fix effect
summary(lmer(Mean ~ Answer * qType + (1 + Answer * qType | ID) + (1 | qID), REML = FALSE, data = mixmodelData))                           # removing qID as random 1 effect
summary(lmer(Mean ~ Answer * qType + (1 + Answer * qType | ID), REML = FALSE, data = mixmodelData))                                       # removing qID as random 2 effect
summary(lmer(Mean ~ Answer * qType + (1 + Answer + qType | ID), REML = FALSE, data = mixmodelData))                                       # removing interaction as random 1 effect
summary(lmer(Mean ~ Answer * qType + (1 + Answer | ID), REML = FALSE, data = mixmodelData))                                               # removing qType as random 1 effect

summary(lmer(Mean ~ Answer * qType + (1 + Answer | ID) + (1 | qID), REML = FALSE, data = mixmodelData))                                   
summary(lmer(Mean ~ Answer * qType + (1 + Answer + qID | ID) + (1 | qID), REML = FALSE, data = mixmodelData))                                         
summary(lmer(Mean ~ Answer * qType + Trial + (1 + Answer | ID) + (1 | qID), REML = FALSE, data = mixmodelData))                                         
summary(lmer(Mean ~ Answer * qType + Trial + (1 + Answer + Trial | ID) + (1 | qID), REML = FALSE, data = mixmodelData))                                         


finalModel <- summary(lmer(Mean ~ Answer * qType + Trial + (1 + Answer | ID) + (1 | qID), REML = FALSE, data = mixmodelData))                                         
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
  xlab('Question type') + ylab('Converted mean y axis fixation') + ggtitle('Experimental all questions without 30 and 31 two by two dotplot only incorrect answers') + 
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
ggsave('Experimental all questions without 30 and 31 two by two dotplot only incorrect answers.pdf', width = 40, height = 28, units = "cm")

exactGraphData <- b1Data[b1Data$TrialTime <= 3000, ]

Yes             <- 1
colorYes        <- 'blue'
No              <- -1
colorNo         <- 'red' 

# graphs for group
graphTitel <- c("Experimental all questions without participants 30 and 31")
graphName <- c("Experimental all questions without participants 30 and 31.pdf")
GraphData <- ddply(b1Data, c("ID", 'TrialTime'), summarise,
                   Mean = mean(gy, na.rm = TRUE))
ggplot(GraphData, aes(x = TrialTime, y = Mean)) + 
  geom_point(stat = 'summary', fun.y = 'mean', color = 'black', size = 1) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
               geom = "errorbar", color = 'green', width = 0.2, alpha = 0.1) +
  xlab("Time") + ylab("Converted hight") + ggtitle(graphTitel) +
  ylim(200, 900) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text   = element_text(size = 12),
        axis.title  = element_text(size = 14, face = "bold"),
        axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(legend.text = element_text(size = 14))
ggsave(graphName, width = 40, height = 28, units = "cm")



for (Pp in unique((b1Data$ID))){
  data <- exactGraphData[exactGraphData$ID == Pp, ]
  graphTitel <- paste0("Exact all questions paticipant ", Pp)
  graphName <- paste0("Exact all questions participant ", Pp, ".pdf")
  
  ggplot(data, aes(x = TrialTime, y = gy)) + 
    geom_point(stat = 'summary', fun.y = 'mean', color = 'black', size = 1) +
    stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
                 geom = "errorbar", color = 'green', width = 0.2, alpha = 0.1) +
    xlab("Time") + ylab("Converted hight") + ggtitle(graphTitel) +
    ylim(200, 900) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text   = element_text(size = 12),
          axis.title  = element_text(size = 14, face = "bold"),
          axis.line.y = element_line(color = "black", size = 1),
          axis.line.x = element_line(color = "black", size = 1)) +
    theme(axis.line.y = element_line(color = "black", size = 1),
          axis.line.x = element_line(color = "black", size = 1)) +
    theme(legend.text = element_text(size = 14))
  ggsave(graphName, width = 40, height = 28, units = "cm")
  
}
