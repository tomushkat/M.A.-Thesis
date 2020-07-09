
## Induction ##

B1 <- Data %>%
  filter(Block == 1) %>%
  filter(ID %in% onlyB2)


B1$paNo <- ifelse(B1$Answer == -1, B1$pa, NA)
B1$paYes <- ifelse(B1$Answer == 1, B1$pa, NA)

# graphs for group

colorYes <- 'blue'
colorNo <- 'red'

GraphData <- B1 %>%
  group_by(ID, TrialTime) %>%
  summarise( MeanYes = mean(paYes, na.rm = TRUE),
             MeanNo  = mean(paNo, na.rm = TRUE))

GraphData.1 <- GraphData %>%
  group_by(TrialTime) %>%
  summarise(Yes   = mean(MeanYes, na.rm = TRUE),
             SDYes = sd(MeanYes, na.rm = TRUE),
             No    = mean(MeanNo, na.rm = TRUE),
             SDNo  = sd(MeanNo, na.rm = TRUE))



graphTitel <- c("Pupil size Induction all questions")
graphName <- c("Pupil size Induction all questions.pdf") 
GraphData.1 %>%
  mutate(Phase = ifelse(TrialTime > 400 & TrialTime < 2100, 'Question',
                        ifelse(TrialTime < 2200, 'ISI',
                               ifelse(TrialTime < 2350, 'Target',
                                      ifelse(TrialTime < 2500, 'BM', 'None'))))) %>%
  ggplot(aes(x = TrialTime, y = Yes)) +
  geom_line(color = colorYes) +
  geom_line(aes(y = No), color = colorNo) +
  geom_line(y = 3000, aes(color = Phase), size = 2) + 
  ylim(3000, 6000) +
  xlab("Time") + ylab("Pupil size") + ggtitle(graphTitel) +
  Theme 


GraphData.1 %>%
  mutate(Phase = ifelse(TrialTime > 400 & TrialTime < 2100, 'Question',
                        ifelse(TrialTime > 400 &TrialTime < 2200, 'ISI',
                               ifelse(TrialTime > 400 &TrialTime < 2350, 'Target',
                                      ifelse(TrialTime > 400 & TrialTime < 2500, 'BM', 'None'))))) %>%
  filter(TrialTime < 2500) %>%
  ggplot(aes(x = TrialTime, y = Yes)) +
  geom_line(color = colorYes) +
  geom_line(aes(y = No), color = colorNo) +
  geom_line(y = 3000, aes(color = Phase), size = 2) + 
  ylim(3000, 6000) +
  xlab("Time") + ylab("Pupil size") + ggtitle(graphTitel) +
  Theme 


GraphData.1$SDYes <- GraphData.1$SDYes / sqrt(length(unique(GraphData$ID)))  
GraphData.1$SDNo <- GraphData.1$SDNo / sqrt(length(unique(GraphData$ID)))  
GraphData.1 %>%
  mutate(Pahse = ifelse(TrialTime < 2100, 'Question',
                        ifelse(TrialTime < 2200, 'ISI',
                               ifelse(TrialTime < 2350, 'Target',
                                      ifelse(TrialTime < 2500, 'BM', 'None'))))) %>%
  filter(TrialTime >= 2000 & TrialTime < 2500) %>%
  ggplot(aes(x = TrialTime, y = Yes))  +
  #geom_errorbar(aes(ymin = Yes - SDYes, ymax = Yes + SDYes), width = 0.1,
  #              position = position_dodge(0.2), color = "blue") +
  geom_line(color = colorYes) +
  #geom_errorbar(aes(ymin = No - SDNo, ymax = No + SDNo), width = 0.1,
  #              position = position_dodge(0.2), color = "pink") +
  geom_line(aes(y = No), color = colorNo) +
  geom_line(y = 3000, aes(color = Pahse), size = 2) + 
  ylim(3000, 6000) +
  xlab("Time") + ylab("Pupil size") + ggtitle(graphTitel) +
  Theme

Temp <- B1 %>%
  filter(TrialTime >= 2400 & TrialTime <= 2500)

data12 <-  ddply(Temp, c('ID', 'Trial.x', 'Answer'), summarise,
                 Mean = mean(pa, na.rm = TRUE))
data12.1 <-  ddply(data12, c('ID', 'Answer'), summarise,
                   Mean.1 = mean(Mean, na.rm = TRUE))

data12.1 %>%
  mutate(Answer = ifelse(Answer == 1, "Yes", "No")) %>%
  ggplot(aes(x = Answer, y = Mean.1)) +
  geom_dotplot(binaxis = 'y', stackdir = 'center', position = position_dodge(0.8))  +
  scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9")) + 
  stat_summary(fun = mean, geom = "point", shape = 18, size = 5, aes(color = Answer) , position = position_dodge(0.8)) + 
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "pointrange", aes(color = Answer), position = position_dodge(0.8)) +
  xlab('Answer') + ylab('Converted mean y axis fixation') + ggtitle(graphTitel) + 
  Theme 
  Lbls

ddply(data12.1, c('Answer'), summarise,
        m = round(mean(Mean.1, na.rm = T), 2),
        S = round(sd(Mean.1, na.rm = T), 2))
  
data12.2 <-  ddply(data12, c('ID', 'Answer'), summarise,
                     Mean.1 = mean(Mean, na.rm = TRUE))
data12.2$Answer <- ifelse(data12.2$Answer == 1, 'Yes', 'No')
ddply(data12.2, c('Answer'), summarise,
        Mean = round(mean(Mean.1, na.rm = TRUE), 2),
        SD   = round(sd(Mean.1, na.rm = T), 2))

data12 <- data12 %>%
  mutate(Answer = as.factor(Answer))
contrasts(data12$Answer) <- c(-1, 1)

data12 <- data12 %>%
  mutate(ifelse(Answer == 1, 'Yes', 'No'))  
summary(lmer(Mean ~ Answer + (1 + Answer | ID), data = data12, REML = FALSE))



## Experimental ##

B2 <- Data %>%
  filter(Block == 2) %>%
  filter(ID %in% onlyB2)


B2$paNo <- ifelse(B2$Answer == -1, B2$pa, NA)
B2$paYes <- ifelse(B2$Answer == 1, B2$pa, NA)

# graphs for group

colorYes <- 'blue'
colorNo <- 'red'

GraphData <- B2 %>%
  group_by(ID, TrialTime) %>%
  summarise( MeanYes = mean(paYes, na.rm = TRUE),
             MeanNo  = mean(paNo, na.rm = TRUE))

GraphData.1 <- GraphData %>%
  group_by(TrialTime) %>%
  summarise(Yes   = mean(MeanYes, na.rm = TRUE),
            SDYes = sd(MeanYes, na.rm = TRUE),
            No    = mean(MeanNo, na.rm = TRUE),
            SDNo  = sd(MeanNo, na.rm = TRUE))



graphTitel <- c("Pupil size Experimental all questions")
graphName <- c("Pupil size Experimental all questions.pdf") 
GraphData.1 %>%
  mutate(Pahse = ifelse(TrialTime < 2100, 'Question',
                        ifelse(TrialTime < 2200, 'ISI',
                               ifelse(TrialTime < 2350, 'Target',
                                      ifelse(TrialTime < 2500, 'BM', 'None'))))) %>%
  ggplot(aes(x = TrialTime, y = Yes)) +
  geom_line(color = colorYes) +
  geom_line(aes(y = No), color = colorNo) +
  geom_line(y = 3000, aes(color = Pahse), size = 2) + 
  ylim(3000, 6000) +
  xlab("Time") + ylab("Pupil size") + ggtitle(graphTitel) +
  Theme 


GraphData.1 %>%
  mutate(Phase = ifelse(TrialTime > 400 & TrialTime < 2100, 'Question',
                        ifelse(TrialTime > 400 &TrialTime < 2200, 'ISI',
                               ifelse(TrialTime > 400 &TrialTime < 2350, 'Target',
                                      ifelse(TrialTime > 400 & TrialTime < 2500, 'BM', 'None'))))) %>%
  filter(TrialTime < 2500) %>%
  ggplot(aes(x = TrialTime, y = Yes)) +
  geom_line(color = colorYes) +
  geom_line(aes(y = No), color = colorNo) +
  geom_line(y = 3000, aes(color = Phase), size = 2) + 
  ylim(3000, 6000) +
  xlab("Time") + ylab("Pupil size") + ggtitle(graphTitel) +
  Theme 



GraphData.1$SDYes <- GraphData.1$SDYes / sqrt(length(unique(GraphData$ID)))  
GraphData.1$SDNo <- GraphData.1$SDNo / sqrt(length(unique(GraphData$ID)))  
GraphData.1 %>%
  mutate(Pahse = ifelse(TrialTime < 2100, 'Question',
                        ifelse(TrialTime < 2200, 'ISI',
                               ifelse(TrialTime < 2350, 'Target',
                                      ifelse(TrialTime < 2500, 'BM', 'None'))))) %>%
  filter(TrialTime >= 2000 & TrialTime < 2500) %>%
  ggplot(aes(x = TrialTime, y = Yes))  +
  #geom_errorbar(aes(ymin = Yes - SDYes, ymax = Yes + SDYes), width = 0.1,
  #              position = position_dodge(0.2), color = "blue") +
  geom_line(color = colorYes) +
  #geom_errorbar(aes(ymin = No - SDNo, ymax = No + SDNo), width = 0.1,
  #              position = position_dodge(0.2), color = "pink") +
  geom_line(aes(y = No), color = colorNo) +
  geom_line(y = 3000, aes(color = Pahse), size = 2) + 
  ylim(3000, 6000) +
  xlab("Time") + ylab("Pupil size") + ggtitle(graphTitel) +
  Theme

Temp <- B1 %>%
  filter(TrialTime >= minCriticTime & TrialTime <= maxCriticTime)

data12 <-  ddply(Temp, c('ID', 'Trial.x', 'Answer'), summarise,
                 Mean = mean(pa, na.rm = TRUE))
data12.1 <-  ddply(data12, c('ID', 'Answer'), summarise,
                   Mean.1 = mean(Mean, na.rm = TRUE))

data12.1 %>%
  mutate(Answer = ifelse(Answer == 1, "Yes", "No")) %>%
  ggplot(aes(x = Answer, y = Mean.1)) +
  geom_dotplot(binaxis = 'y', stackdir = 'center', position = position_dodge(0.8))  +
  scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9")) + 
  stat_summary(fun = mean, geom = "point", shape = 18, size = 5, aes(color = Answer) , position = position_dodge(0.8)) + 
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "pointrange", aes(color = Answer), position = position_dodge(0.8)) +
  xlab('Answer') + ylab('Converted mean y axis fixation') + ggtitle(graphTitel) + 
  Theme 
Lbls

ddply(data12.1, c('Answer'), summarise,
      m = round(mean(Mean.1, na.rm = T), 2),
      S = round(sd(Mean.1, na.rm = T), 2))

data12.2 <-  ddply(data12, c('ID', 'Answer'), summarise,
                   Mean.1 = mean(Mean, na.rm = TRUE))
data12.2$Answer <- ifelse(data12.2$Answer == 1, 'Yes', 'No')
ddply(data12.2, c('Answer'), summarise,
      Mean = round(mean(Mean.1, na.rm = TRUE), 2),
      SD   = round(sd(Mean.1, na.rm = T), 2))

data12 <- data12 %>%
  mutate(Answer = as.factor(Answer))
contrasts(data12$Answer) <- c(-1, 1)

data12 <- data12 %>%
  mutate(ifelse(Answer == 1, 'Yes', 'No'))  
summary(lmer(Mean ~ Answer + (1 + Answer | ID), data = data12, REML = FALSE))
