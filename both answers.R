# Block 1
B2 <- Data %>%
  filter(Block == 1 & ID %in% onlyB2) %>%
  mutate(gyNo  = ifelse(Answer == -1, gy, NA),
         gyYes = ifelse(Answer == 1, gy, NA),
         Answer = ifelse(Answer == 1, 'Yes', 'No'))

#B2 <- B2[B2$ID %in% pass25Numeric, ]           # pass numeric aseperate conditions
#B2 <- B2[B2$ID %in% pass25Sig, ]               # pass significantly aseperate conditions





Yes             <- NULL
colorYes        <- 'blue'
No              <- NULL
colorNo         <- 'red' 

# graphs for group


GraphData <- B2 %>%
  group_by(ID, TrialTime) %>%
  summarise(MeanYes = mean(gyYes, na.rm = TRUE),
            MeanNo  = mean(gyNo, na.rm = TRUE))

GraphData.1 <- GraphData %>%
  group_by(TrialTime) %>%
  summarise(Yes   = mean(MeanYes, na.rm = TRUE),
            SDYes = sd(MeanYes, na.rm = TRUE),
            No    = mean(MeanNo, na.rm = TRUE),
            SDNo  = sd(MeanNo, na.rm = TRUE))

graphTitel <- c("Experimental all questions both answers")
graphName <- c("Experimental all questions both answers.pdf")
GraphData.1 %>%
  filter(TrialTime < 10000) %>%
  mutate(Pahse = ifelse(TrialTime > 400 & TrialTime < 2100, 'Question',
                        ifelse(TrialTime > 400 &TrialTime < 2200, 'ISI',
                               ifelse(TrialTime > 400 &TrialTime < 2350, 'Target',
                                      ifelse(TrialTime > 400 & TrialTime < 2500, 'BM', 'None'))))) %>%
  ggplot(aes(x = TrialTime, y = Yes)) + 
  geom_line(color = 'blue') +
  geom_line(y = 540, color = "black", size = 1) +
  geom_line(y = 0, aes(color = Pahse), size = 2) + 
  geom_line(aes(y = No), color = 'red') + ylim(0, 1100) + 
  xlab("Time in ms") + ylab("Converted hight") + ggtitle(graphTitel) +
  Theme
ggsave(graphName, width = 40, height = 28, units = "cm")

graphName <- c("Experimental all questions both answers Trial Time lesser than 2500ms.pdf")

GraphData.1 %>%
  filter(TrialTime < endBM) %>%
  mutate(Pahse = ifelse(TrialTime > 400 & TrialTime < 2100, 'Question',
                        ifelse(TrialTime > 400 &TrialTime < 2200, 'ISI',
                               ifelse(TrialTime > 400 &TrialTime < 2350, 'Target',
                                      ifelse(TrialTime > 400 & TrialTime < 2500, 'BM', 'None'))))) %>%
  ggplot(aes(x = TrialTime, y = Yes)) +
  geom_line(color = 'blue') +
  geom_line(y = 540, color = "black", size = 1) +
  geom_line(y = 0, aes(color = Pahse), size = 2) + 
  geom_line(aes(y = No), color = 'red') + ylim(0, 1100) + 
  xlab("Time in ms") + ylab("Converted hight") + ggtitle(graphTitel) +
  Theme
ggsave(graphName, width = 40, height = 28, units = "cm")



#graphTitel <- c("Experimental all questions both answers only participants above 25% accuracy seperatly")
#graphName <- c("Experimental all questions both answers only participants above 0.25 together accuracy 2000 to 2300.pdf")

GraphData.1$SDYes <- GraphData.1$SDYes / sqrt(length(unique(GraphData$ID)))  
GraphData.1$SDNo <- GraphData.1$SDNo / sqrt(length(unique(GraphData$ID)))  

graphName <- c("Experimental all questions both answers Trial Time between 1800ms to 2600ms.pdf")
GraphData.1 %>%
  filter(TrialTime >= 1800 & TrialTime <= 2600) %>%
  mutate(Pahse = ifelse(TrialTime < 2100, 'Question',
                        ifelse(TrialTime < 2200, 'ISI',
                               ifelse(TrialTime < 2350, 'Target',
                                      ifelse(TrialTime < 2500, 'BM', 'None'))))) %>%
  ggplot(aes(x = TrialTime, y = Yes))  +
  geom_line(y = 540, color = "black", size = 1) +
  geom_line(y = 0, aes(color = Pahse), size = 2) + 
  #geom_errorbar(aes(ymin = Yes - SDYes, ym + ax = Yes + SDYes), width = 0.1,
  #             position = position_dodge(0.2), color = "blue") +
  geom_line(color = 'blue') +
  #geom_errorbar(aes(ymin = No - SDNo, ymax = No + SDNo), width = 0.1,
  #              position = position_dodge(0.2), color = "pink") +
  geom_line(aes(y = No), color = 'red')  + ylim(0, 1100) + 
  xlab("Time in ms") + ylab("Converted hight") + ggtitle(graphTitel) +
  Theme
ggsave(graphName, width = 40, height = 28, units = "cm")

GraphData.1 %>%
  filter(TrialTime >= minCriticTime & TrialTime <= maxCriticTime) %>%
  summarise(meanYes = mean(Yes, na.rm = TRUE),
            sdYes = sd(Yes, na.rm = TRUE),
            meanNo = mean(No, na.rm = TRUE),
            sdNo = sd(No, na.rm = TRUE))

graphTitel <- c("Induction all questions 2200ms to 2300ms")
graphName <- c("Induction all questions 2200ms to 2300ms.pdf")

Temp <- B2 %>%
  filter(TrialTime >= minCriticTime & TrialTime <= maxCriticTime)

data12 <-  ddply(Temp, c('ID', 'Trial.x', 'Answer'), summarise,
                 Mean = mean(gy, na.rm = TRUE))
data12.1 <-  ddply(data12, c('ID', 'Answer'), summarise,
                   Mean.1 = mean(Mean, na.rm = TRUE))

data12.1 %>%
  ggplot(aes(x = Answer, y = Mean.1)) +
  geom_dotplot(binaxis = 'y', stackdir = 'center', position = position_dodge(0.8), binwidth = 15)  +
  scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9")) + 
  stat_summary(fun = mean, geom = "point", shape = 18, size = 5, aes(color = Answer) , position = position_dodge(0.8)) + 
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "pointrange", aes(color = Answer), position = position_dodge(0.8)) +
  xlab('Question type') + ylab('Converted mean y axis fixation') + ggtitle(graphTitel) + 
  Theme +
Lbls
ggsave(graphName, width = 20, height = 14, units = "cm")



ddply(data12.1, c('Answer'), summarise,
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
t.test(data12.2$Mean[data12.2$Answer == 'Yes'], data12.2$Mean[data12.2$Answer == 'No'],
       paired = TRUE, alternative = c('less'))
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
  mutate(Answer = as.factor(Answer))
contrasts(data12$Answer) <- c(-1, 1)

data12 <- data12 %>%
  mutate(ifelse(Answer == 1, 'Yes', 'No'))
summary(lmer(Mean ~ Answer + (1 + Answer | ID), data = data12, REML = FALSE))


Data %>%
  filter(ID == 6, Block == 1, Trial.x == 34) %>%
  ggplot(aes(x = TrialTime, y = gy)) + 
  geom_line() + 
  Theme

Data %>%
  filter(ID == 6, Block == 2, Trial.x == 34) %>%
  ggplot(aes(x = TrialTime, y = gy)) + 
  geom_line() + 
  Theme



























# Block 2

B2 <- Data %>%
  filter(Block == 2 & ID %in% onlyB2) %>%
  mutate(gyNo  = ifelse(Answer == -1, gy, NA),
         gyYes = ifelse(Answer == 1, gy, NA),
         Answer = ifelse(Answer == 1, 'Yes', 'No'),
         qType = ifelse(qType == 1, "New", "Old"))

#B2 <- B2[B2$ID %in% pass25Numeric, ]           # pass numeric aseperate conditions
#B2 <- B2[B2$ID %in% pass25Sig, ]               # pass significantly aseperate conditions



  

Yes             <- NULL
colorYes        <- 'blue'
No              <- NULL
colorNo         <- 'red' 

# graphs for group


GraphData <- B2 %>%
  group_by(ID, TrialTime) %>%
  summarise(MeanYes = mean(gyYes, na.rm = TRUE),
            MeanNo  = mean(gyNo, na.rm = TRUE))

GraphData.1 <- GraphData %>%
  group_by(TrialTime) %>%
  summarise(Yes   = mean(MeanYes, na.rm = TRUE),
            SDYes = sd(MeanYes, na.rm = TRUE),
            No    = mean(MeanNo, na.rm = TRUE),
            SDNo  = sd(MeanNo, na.rm = TRUE))

graphTitel <- c("Experimental all questions both answers")
graphName <- c("Experimental all questions both answers.pdf")
GraphData.1 %>%
  filter(TrialTime < 10000) %>%
  mutate(Pahse = ifelse(TrialTime > 400 & TrialTime < 2100, 'Question',
                        ifelse(TrialTime > 400 &TrialTime < 2200, 'ISI',
                               ifelse(TrialTime > 400 &TrialTime < 2350, 'Target',
                                      ifelse(TrialTime > 400 & TrialTime < 2500, 'BM', 'None'))))) %>%
  ggplot(aes(x = TrialTime, y = Yes)) + 
  geom_line(color = 'blue') +
  geom_line(y = 540, color = "black", size = 1) +
  geom_line(y = 0, aes(color = Pahse), size = 2) + 
  geom_line(aes(y = No), color = 'red') + ylim(0, 1100) + 
  xlab("Time in ms") + ylab("Converted hight") + ggtitle(graphTitel) +
  Theme
ggsave(graphName, width = 40, height = 28, units = "cm")

graphName <- c("Experimental all questions both answers Trial Time lesser than 2500ms.pdf")

GraphData.1 %>%
  filter(TrialTime < endBM) %>%
  mutate(Pahse = ifelse(TrialTime > 400 & TrialTime < 2100, 'Question',
                        ifelse(TrialTime > 400 &TrialTime < 2200, 'ISI',
                               ifelse(TrialTime > 400 &TrialTime < 2350, 'Target',
                                      ifelse(TrialTime > 400 & TrialTime < 2500, 'BM', 'None'))))) %>%
  ggplot(aes(x = TrialTime, y = Yes)) +
  geom_line(color = 'blue') +
  geom_line(y = 540, color = "black", size = 1) +
  geom_line(y = 0, aes(color = Pahse), size = 2) + 
  geom_line(aes(y = No), color = 'red') + ylim(0, 1100) + 
  xlab("Time in ms") + ylab("Converted hight") + ggtitle(graphTitel) +
  Theme
ggsave(graphName, width = 40, height = 28, units = "cm")



#graphTitel <- c("Experimental all questions both answers only participants above 25% accuracy seperatly")
#graphName <- c("Experimental all questions both answers only participants above 0.25 together accuracy 2000 to 2300.pdf")

GraphData.1$SDYes <- GraphData.1$SDYes / sqrt(length(unique(GraphData$ID)))  
GraphData.1$SDNo <- GraphData.1$SDNo / sqrt(length(unique(GraphData$ID)))  

graphName <- c("Experimental all questions both answers Trial Time between 1800ms to 2600ms.pdf")
GraphData.1 %>%
  filter(TrialTime >= 1800 & TrialTime <= 2600) %>%
  mutate(Pahse = ifelse(TrialTime < 2100, 'Question',
                        ifelse(TrialTime < 2200, 'ISI',
                               ifelse(TrialTime < 2350, 'Target',
                                      ifelse(TrialTime < 2500, 'BM', 'None'))))) %>%
  ggplot(aes(x = TrialTime, y = Yes))  +
  geom_line(y = 540, color = "black", size = 1) +
  geom_line(y = 0, aes(color = Pahse), size = 2) + 
  #geom_errorbar(aes(ymin = Yes - SDYes, ym + ax = Yes + SDYes), width = 0.1,
  #             position = position_dodge(0.2), color = "blue") +
  geom_line(color = 'blue') +
  #geom_errorbar(aes(ymin = No - SDNo, ymax = No + SDNo), width = 0.1,
  #              position = position_dodge(0.2), color = "pink") +
  geom_line(aes(y = No), color = 'red')  + ylim(0, 1100) + 
  xlab("Time in ms") + ylab("Converted hight") + ggtitle(graphTitel) +
  Theme
ggsave(graphName, width = 40, height = 28, units = "cm")

GraphData.1 %>%
  filter(TrialTime >= minCriticTime & TrialTime <= maxCriticTime) %>%
  summarise(meanYes = mean(Yes, na.rm = TRUE),
            sdYes = sd(Yes, na.rm = TRUE),
            meanNo = mean(No, na.rm = TRUE),
            sdNo = sd(No, na.rm = TRUE))

graphTitel <- c("Experimental all questions 2200ms to 2300ms")
graphName <- c("Experimental all questions 2200ms to 2300ms.pdf")

Temp <- B2 %>%
  filter(TrialTime >= minCriticTime & TrialTime <= maxCriticTime)

data12 <-  ddply(Temp, c('ID', 'Trial.x', 'Answer', 'qType'), summarise,
                 Mean = mean(gy, na.rm = TRUE))
data12.1 <-  ddply(data12, c('ID', 'Answer', 'qType'), summarise,
                   Mean.1 = mean(Mean, na.rm = TRUE))

data12.1 %>%
  ggplot(aes(x = qType, y = Mean.1, fill = Answer)) +
  geom_dotplot(binaxis = 'y', stackdir = 'center', position = position_dodge(0.8), binwidth = 15)  +
  scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9")) + 
  stat_summary(fun = mean, geom = "point", shape = 18, size = 5, aes(color = Answer) , position = position_dodge(0.8)) + 
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "pointrange", aes(color = Answer), position = position_dodge(0.8)) +
  theme(panel.grid.major = element_line(color = 'grey')) + 
  xlab('Question type') + ylab('Converted mean y axis fixation') + ggtitle(graphTitel) + 
  Theme 
  Lbls
ggsave(graphName, width = 20, height = 14, units = "cm")



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
  mutate(Answer = as.factor(Answer),
         qType  = as.factor(qType))
contrasts(data12$Answer) <- c(1, -1)
contrasts(data12$qType) <- c(1, -1)


summary(lmer(Mean ~ Answer * qType + (1 + Answer * qType | ID), data = data12, REML = FALSE))
summary(lmer(Mean ~ Answer * qType + (1 + Answer + Answer:qType | ID), data = data12, REML = FALSE))
summary(lmer(Mean ~ Answer * qType + (1 + Answer:qType | ID), data = data12, REML = FALSE))
summary(lmer(Mean ~ Answer * qType + (1 | ID), data = data12, REML = FALSE))
summary(lmer(Mean ~ Answer * qType + (1 + Answer | ID), data = data12, REML = FALSE))


Data %>%
  filter(ID == 6, Block == 1, Trial.x == 34) %>%
  ggplot(aes(x = TrialTime, y = gy)) + 
  geom_line() + 
  Theme

Data %>%
  filter(ID == 6, Block == 2, Trial.x == 34) %>%
  ggplot(aes(x = TrialTime, y = gy)) + 
  geom_line() + 
  Theme

