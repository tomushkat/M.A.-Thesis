B2 <- subset(Data, Data$Block == 2)
pass25Numeric <- c(5, 6, 7, 11, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22, 24, 25, 26, 27, 29, 37, 39, 42, 43, 44, 47, 48, 51)
#pass25Sig <- c(6, 7, 16, 19, 21, 27, 44)
#B2 <- Data[Data$Block == 2 & Data$ID %in% acc$ID[acc$Binom == 1], ]  # pass significanly all conditions together
#B2 <- Data[Data$Block == 2 & Data$ID %in% pass25Numeric, ]           # pass numeric aseperate conditions
#B2 <- Data[Data$Block == 2 & Data$ID %in% pass25Sig, ]               # pass significantly aseperate conditions

#data <-  ddply(B2[B2$TrialTime == 2250, ], c('ID', 'Trial', 'Answer', 'qType', 'qID'), summarise,
#                      Mean = mean(gy, na.rm = TRUE))

#summary(lmer(Mean ~ Answer * qType + Trial + (1 + Answer * qType + Trial | ID) + (1 + 1 | qID), REML = FALSE, data = data))                                         


B2$gyNo <- ifelse(B2$Answer == -1, B2$gy, NA)
B2$gyYes <- ifelse(B2$Answer == 1, B2$gy, NA)

Yes             <- NULL
colorYes        <- 'blue'
No              <- NULL
colorNo         <- 'red' 

# graphs for group


graphTitel <- c("Experimental all questions both answers")
graphName <- c("Experimental all questions both answers.pdf")
GraphData <- ddply(B2, c("ID", 'TrialTime'), summarise,
                   MeanYes = mean(gyYes, na.rm = TRUE),
                   MeanNo  = mean(gyNo, na.rm = TRUE))


GraphData.1 <- ddply(GraphData, c('TrialTime'), summarise,
                     Yes   = mean(MeanYes, na.rm = TRUE),
                     SDYes = sd(MeanYes, na.rm = TRUE),
                     No    = mean(MeanNo, na.rm = TRUE),
                     SDNo  = sd(MeanNo, na.rm = TRUE))

#GraphData.1$SN <- c(1:length(GraphData.1$TrialTime))
ggplot(GraphData.1, aes(x = TrialTime, y = Yes)) + 
  geom_point(color = 'blue') +
  geom_point(aes(y = No), color = 'red') +
  xlab("Time") + ylab("Converted hight") + ggtitle(graphTitel) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text   = element_text(size = 12),
        axis.title  = element_text(size = 14, face = "bold"),
        axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(legend.text = element_text(size = 14))
ggsave(graphName, width = 40, height = 28, units = "cm")

graphName <- c("Experimental all questions both answers Trial Time lesser than 2500ms.pdf")

ggplot(GraphData.1[GraphData.1$TrialTime < 2500, ], aes(x = TrialTime, y = Yes)) + 
  geom_point(color = 'blue') +
  geom_point(aes(y = No), color = 'red') +
  xlab("Time") + ylab("Converted hight") + ggtitle(graphTitel) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text   = element_text(size = 12),
        axis.title  = element_text(size = 14, face = "bold"),
        axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(legend.text = element_text(size = 14))
ggsave(graphName, width = 40, height = 28, units = "cm")


#graphTitel <- c("Experimental all questions both answers only participants above 25% accuracy seperatly")
#graphName <- c("Experimental all questions both answers only participants above 0.25 together accuracy 2000 to 2300.pdf")

GraphData.1$SDYes <- GraphData.1$SDYes / sqrt(length(unique(GraphData$ID)))  
GraphData.1$SDNo <- GraphData.1$SDNo / sqrt(length(unique(GraphData$ID)))  

graphName <- c("Experimental all questions both answers Trial Time between 2000ms to 2500ms.pdf")
ggplot(GraphData.1[GraphData.1$TrialTime >= 2000 & GraphData.1$TrialTime <= 2500, ], aes(x = TrialTime, y = Yes))  +
  #geom_errorbar(aes(ymin = Yes - SDYes, ymax = Yes + SDYes), width = 0.1,
  #             position = position_dodge(0.2), color = "blue") +
  geom_point(color = 'blue') +
  #geom_errorbar(aes(ymin = No - SDNo, ymax = No + SDNo), width = 0.1,
  #              position = position_dodge(0.2), color = "pink") +
  geom_point(aes(y = No), color = 'red') +
  xlab("Time") + ylab("Converted hight") + ggtitle(graphTitel) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text   = element_text(size = 12),
        axis.title  = element_text(size = 14, face = "bold"),
        axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(legend.text = element_text(size = 14))
ggsave(graphName, width = 40, height = 28, units = "cm")



mean(GraphData.1[GraphData.1 >= 2150 & GraphData.1$TrialTime <= 2200, 2])
sd(GraphData.1[GraphData.1 >= 2150 & GraphData.1$TrialTime <= 2200, 2])
mean(GraphData.1[GraphData.1 >= 2150 & GraphData.1$TrialTime <= 2200, 3])
sd(GraphData.1[GraphData.1 >= 2150 & GraphData.1$TrialTime <= 2200, 3])



graphTitel <- c("Experimental all questions 2150ms to 2300ms")
graphName <- c("Experimental all questions 2150ms to 2300ms.pdf")

Temp <- B2[B2$TrialTime >= 2150 & B2$TrialTime <= 2300, ]

data12 <-  ddply(Temp, c('ID', 'Trial.x', 'Answer', 'qType'), summarise,
                 Mean = mean(gy, na.rm = TRUE))
data12.1 <-  ddply(data12, c('ID', 'Answer', 'qType'), summarise,
                   Mean.1 = mean(Mean, na.rm = TRUE))
data12.1$Answer <- ifelse(data12.1$Answer == 1, 'Yes', 'No')
data12.1$qType <- ifelse(data12.1$qType == 1, 'New', 'Old')
ggplot(data12.1, aes(x = qType, y = Mean.1, fill = Answer)) +
  geom_dotplot(binaxis='y', stackdir = 'center', position = position_dodge(0.8))  +
  scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9")) + 
  stat_summary(fun.y = mean, geom = "point", shape = 18, size = 5, aes(color = Answer) , position = position_dodge(0.8)) + 
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "pointrange", aes(color = Answer), position = position_dodge(0.8)) +
  xlab('Question type') + ylab('Converted mean y axis fixation') + ggtitle(graphTitel) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text   = element_text(size = 12),
        axis.title  = element_text(size = 14, face = "bold"),
        axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(legend.text = element_text(size = 14)) 
ggsave(graphName, width = 40, height = 28, units = "cm")


ddply(data12.1, c('Answer', 'qType'), summarise,
      m = round(mean(Mean.1, na.rm = T), 2),
      S = round(sd(Mean.1, na.rm = T), 2))

data12.2 <-  ddply(data12, c('ID', 'Answer'), summarise,
                   Mean.1 = mean(Mean, na.rm = TRUE))
data12.2$Answer <- ifelse(data12.2$Answer == 1, 'Yes', 'No')
ddply(data12.2, c('Answer'), summarise,
      Mean = round(mean(Mean.1, na.rm = TRUE), 2),
      SD   = round(sd(Mean.1, na.rm = T), 2))

shapiro.test(data12.2$Mean[data12.2$Answer == 'No'])
shapiro.test(data12.2$Mean[data12.2$Answer == 'Yes'])
var.test(data12.2$Mean[data12.2$Answer == 'Yes'], data12.2$Mean[data12.2$Answer == 'No'], paired = TRUE)
t.test(data12.2$Mean[data12.2$Answer == 'Yes'], data12.2$Mean[data12.2$Answer == 'No'],
       paired = TRUE, var.equal = TRUE, alternative = c('less'))
cohen.d(data12.2$Mean[data12.2$Answer == 'Yes'], data12.2$Mean[data12.2$Answer == 'No'], pooled = TRUE, paired = TRUE, hedges.correction = FALSE)

shapiro.test(data12.1$Mean[data12.1$Answer == 'No' & data12.1$qType == 'New'])
shapiro.test(data12.1$Mean[data12.1$Answer == 'Yes'& data12.1$qType == 'New'])
shapiro.test(data12.1$Mean[data12.1$Answer == 'No' & data12.1$qType == 'Old'])
shapiro.test(data12.1$Mean[data12.1$Answer == 'Yes'& data12.1$qType == 'Old'])
ezANOVA(data = data12.1, dv = Mean.1, wid = ID, within = c('Answer', 'qType'), detailed = TRUE)
fit <- aov(Mean.1 ~ Answer * qType + Error(ID / (Answer * qType)), data = data12.1)
summary(fit)
eta_sq(fit, partial = FALSE)

data12 <- data12 %>%
  mutate(Answer = as.factor(ifelse(Answer == 1, 'Yes', ifelse(Answer == -1, 'No', NA))),
         qType  = as.factor(ifelse(qType  == 1, "New", ifelse(qType == -1, 'Old', NA))))
contrasts(data12$Answer)[1] <- -1
contrasts(data12$qType)[1] <- 1
contrasts(data12$qType)[2] <- -1


summary(lmer(Mean ~ Answer * qType + (1 + Answer * qType | ID), data = data12, REML = FALSE))
summary(lmer(Mean ~ Answer * qType + (1 + Answer + Answer:qType | ID), data = data12, REML = FALSE))
summary(lmer(Mean ~ Answer * qType + (1 + Answer:qType | ID), data = data12, REML = FALSE))
summary(lmer(Mean ~ Answer * qType + (1 | ID), data = data12, REML = FALSE))
summary(lmer(Mean ~ Answer * qType + (1 + Answer | ID), data = data12, REML = FALSE))

