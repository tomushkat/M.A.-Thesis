

############################
#      All Pp              #
############################

B2EyeData <- Data %>%
  filter(Block == 2) %>%
  group_by(ID) %>%
  mutate(gyDiff = gy - 540,
         gyScaled = gyDiff / sqrt(sum((gyDiff)^2, na.rm = TRUE) / length(na.omit(gyDiff))),
         Answer = ifelse(Answer == 1, 'Yes', 'No'),
         qType  = ifelse(qType == 1, 'New', 'Old'),
         gyNo  = ifelse(Answer == 'No', gyScaled, NA),
         gyYes = ifelse(Answer == 'Yes', gyScaled, NA))



Yes             <- NULL
colorYes        <- 'blue'
No              <- NULL
colorNo         <- 'red' 






GraphData <- B2EyeData %>%
  filter(ID %in% pass25Sig) %>%
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
  ggplot(aes(x = TrialTime, y = Yes)) + 
  geom_line(color = 'blue') +
  geom_line(aes(y = No), color = 'red') +
  xlab("Time") + ylab("Hight") + ggtitle(graphTitel) +
  theme(panel.grid.major = element_line(color = 'grey')) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text   = element_text(size = 12),
        axis.title  = element_text(size = 14, face = "bold"),
        axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(legend.text = element_text(size = 14)) 

ggsave(graphName, width = 40, height = 28, units = "cm")


GraphData.1 %>%
  filter(TrialTime < endBM) %>%
  ggplot(aes(x = TrialTime, y = Yes)) + 
  geom_line(color = 'blue') +
  geom_line(aes(y = No), color = 'red') +
  xlab("Time") + ylab("Hight") + ggtitle(graphTitel) +
  theme(panel.grid.major = element_line(color = 'grey')) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text   = element_text(size = 12),
        axis.title  = element_text(size = 14, face = "bold"),
        axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(legend.text = element_text(size = 14))  
ggsave(graphName, width = 40, height = 28, units = "cm")



GraphData.1 %>%
  filter(TrialTime >= 1800 & TrialTime <= endBM) %>%
  ggplot(aes(x = TrialTime, y = Yes))  +
  #geom_errorbar(aes(ymin = Yes - SDYes, ymax = Yes + SDYes), width = 0.1,
  #             position = position_dodge(0.2), color = "blue") +
  geom_line(color = 'blue') +
  #geom_errorbar(aes(ymin = No - SDNo, ymax = No + SDNo), width = 0.1,
  #              position = position_dodge(0.2), color = "pink") +
  geom_line(aes(y = No), color = 'red') +
  xlab("Time") + ylab("Hight") + ggtitle(graphTitel) +
  theme(panel.grid.major = element_line(color = 'grey')) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text   = element_text(size = 12),
        axis.title  = element_text(size = 14, face = "bold"),
        axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(legend.text = element_text(size = 14)) 
ggsave(graphName, width = 40, height = 28, units = "cm")



Temp <- B2EyeData %>%
  filter(TrialTime >= minCriticTime & TrialTime <= maxCriticTime)

data12 <-  ddply(Temp, c('ID', 'Trial.x', 'Answer', 'qType'), summarise,
                 Mean = mean(gyScaled, na.rm = TRUE))
data12.1 <-  ddply(data12, c('ID', 'Answer', 'qType'), summarise,
                   Mean.1 = mean(Mean, na.rm = TRUE))

data12.1 %>%
  ggplot(aes(x = qType, y = Mean.1, fill = Answer)) +
  geom_dotplot(binaxis = 'y', stackdir = 'center', position = position_dodge(0.8), binwidth = 0.15)  +
  theme(panel.grid.major = element_line(color = 'grey')) + 
  scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9")) + 
  stat_summary(fun = mean, geom = "point", shape = 18, size = 5, aes(color = Answer) , position = position_dodge(0.8)) + 
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "pointrange", aes(color = Answer), position = position_dodge(0.8)) +
  xlab('Question type') + ylab('Mean hight') + ggtitle(graphTitel) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text   = element_text(size = 18),
        axis.title  = element_text(size = 14, face = "bold"),
        axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(legend.text = element_text(size = 14))  
ggsave(graphName, width = 20, height = 14, units = "cm")

data12.1 %>%
  group_by(Answer, qType) %>%
  summarise(Mean = round(mean(Mean.1), 2),
            SD   = round(sd(Mean.1), 2))

data12 <- data12 %>%
  mutate(Answer = as.factor(Answer),
         qType  = as.factor(qType))
contrasts(data12$Answer)[1] <- -1
contrasts(data12$qType)[1] <- 1
contrasts(data12$qType)[2] <- -1


summary(lmer(Mean ~ Answer * qType + (1 + Answer * qType | ID), data = data12, REML = FALSE))
summary(lmer(Mean ~ Answer * qType + (1 + Answer + Answer:qType | ID), data = data12, REML = FALSE))
summary(lmer(Mean ~ Answer * qType + (1 + Answer:qType | ID), data = data12, REML = FALSE))
summary(lmer(Mean ~ Answer * qType + (1 | ID), data = data12, REML = FALSE))
summary(lmer(Mean ~ Answer * qType + (1 + Answer | ID), data = data12, REML = FALSE))
summary(lmer(Mean ~ Answer * qType + (1 + qType | ID), data = data12, REML = FALSE))


vGD <- GraphData[GraphData$TrialTime < endBM , ]
df <- unique(vGD$TrialTime)
for (i in unique(vGD$ID)){
  v <- subset(vGD, vGD$ID == i)
  v <- as.data.frame(v[, c(3:4)])
  idYes <- paste0('Yes', i)
  idNo <- paste0('No', i)
  dimnames(v)[[2]] <- c(idYes, idNo)
  df <- cbind(df, v)
}


dimnames(df)[[2]][1] <- c('Time')


ggBoth <- function(data = df, IDY, IDN, ID){
  
  colorYes        <- 'blue'
  colorNo         <- 'red' 
  
  graphTitel <- paste0("scaled Experimental all questions Pp ", ID)
  fig <- data %>%
    ggplot(aes(x = Time, y = IDY)) + 
    geom_line(color = colorYes) +
    geom_line(aes(y = IDN), color = colorNo) +
    xlab("Time") + ylab("Hight") + ggtitle(graphTitel) +
    theme(panel.grid.major = element_line(color = 'grey')) + 
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text   = element_text(size = 12),
          axis.title  = element_text(size = 14, face = "bold"),
          axis.line.y = element_line(color = "black", size = 1),
          axis.line.x = element_line(color = "black", size = 1)) +
    theme(axis.line.y = element_line(color = "black", size = 1),
          axis.line.x = element_line(color = "black", size = 1)) +
    theme(legend.text = element_text(size = 14)) 
  print(fig)
}

a <- 1
for (i in 2:(length(colnames(df)))){
  if (i %% 2 == 0){
    Y <- i 
    N <- Y + 1
    Pp <- unique(GraphData$ID)[a]
    a <- a + 1
    ggBoth(IDY = df[, Y], IDN = df[, N], ID = Pp)
    graphName <- paste0("scaled Accurate Experimental all questions Pp ", Pp, ".pdf")
    ggsave(graphName, width = 40, height = 28, units = "cm")
  }
}


graphTitel <- c("Experimental all questions scaled from center No all Pp")
graphName <- c("Experimental all questions scaled from center No all Pp.pdf")

df %>%
  ggplot(aes(x = df, y = No5)) + 
  geom_line(color = 'lightblue1') +
  geom_line(aes(y = No6), color = 'chocolate') +
  geom_line(aes(y = No7), color = 'yellow4') +
  geom_line(aes(y = No12), color = 'lightsteelblue') +
  geom_line(aes(y = No14),color = 'dodgerblue') +
  geom_line(aes(y = No16), color = 'palevioletred2') +
  geom_line(aes(y = No17), color = 'steelblue') +
  geom_line(aes(y = No18), color = 'lightcyan2') +
  geom_line(aes(y = No19), color = 'tan1') +
  geom_line(aes(y = No21), color = 'tan4') +
  geom_line(aes(y = No22), color = 'cadetblue1') +
  geom_line(aes(y = No24), color = 'mediumpurple') +
  geom_line(aes(y = No25), color = 'orchid1') +
  geom_line(aes(y = No27), color = 'dimgray') +
  geom_line(aes(y = No28), color = 'snow4') +
  geom_line(aes(y = No29), color = 'seagreen1') +
  geom_line(aes(y = No36), color = 'palegreen3') +
  geom_line(aes(y = No37), color = 'deepskyblue1') +
  geom_line(aes(y = No39), color = 'mintcream') +
  geom_line(aes(y = No41), color = 'hotpink1') +
  geom_line(aes(y = No42), color = 'maroon') +
  geom_line(aes(y = No43), color = 'cornsilk2') +
  geom_line(aes(y = No44), color = 'tomato2') +
  geom_line(aes(y = No46), color = 'plum1') +
  geom_line(aes(y = No47), color = 'lavender') +
  geom_line(aes(y = No51), color = 'khaki') +
  xlab("Time") + ylab("Hight") + ggtitle(graphTitel) +
  theme(panel.grid.major = element_line(color = 'grey')) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text   = element_text(size = 12),
        axis.title  = element_text(size = 14, face = "bold"),
        axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(legend.text = element_text(size = 14)) 
ggsave(graphName, width = 40, height = 28, units = "cm")


graphTitel <- c("Experimental all questions scaled from center No 25 sig Pp")
graphName <- c("Experimental all questions scaled from center No 25 sig Pp.pdf")

df %>%
  ggplot(aes(x = df, y = No5)) + 
  geom_line(color = 'lightblue1') +
  geom_line(aes(y = No6), color = 'chocolate') +
  geom_line(aes(y = No7), color = 'yellow4') +
  geom_line(aes(y = No16), color = 'palevioletred2') +
  geom_line(aes(y = No21), color = 'tan4') +
  geom_line(aes(y = No27), color = 'dimgray') +
  geom_line(aes(y = No44), color = 'tomato2') +
  xlab("Time") + ylab("Hight") + ggtitle(graphTitel) +
  theme(panel.grid.major = element_line(color = 'grey')) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text   = element_text(size = 12),
        axis.title  = element_text(size = 14, face = "bold"),
        axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(legend.text = element_text(size = 14)) 
ggsave(graphName, width = 40, height = 28, units = "cm")


graphTitel <- c("Experimental all questions scaled from center No 25 num Pp")
graphName <- c("Experimental all questions scaled from center No 25 num Pp.pdf")

df %>%
  ggplot(aes(x = df, y = No5)) + 
  geom_line(color = 'lightblue1') +
  geom_line(aes(y = No28), color = 'snow4') +
  geom_line(aes(y = No36), color = 'palegreen3') +
  geom_line(aes(y = No41), color = 'hotpink1') +
  geom_line(aes(y = No46), color = 'plum1') +
  xlab("Time") + ylab("Hight") + ggtitle(graphTitel) +
  theme(panel.grid.major = element_line(color = 'grey')) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text   = element_text(size = 12),
        axis.title  = element_text(size = 14, face = "bold"),
        axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(legend.text = element_text(size = 14)) 
ggsave(graphName, width = 40, height = 28, units = "cm")



############################
#      End All Pp          #
############################


############################
#      Numeric pass Pp     #
############################

B2EyeData <- Data %>%
  filter(Block == 2) %>%
  filter(ID %in% pass25Numeric) %>%
  group_by(ID) %>%
  mutate(#minHight = max(na.omit(gy)),
    #sdHight  = sd(gy, na.rm = TRUE),
    gyDiff = gy - 540,
    gyScaled = gyDiff / sqrt(sum((gyDiff)^2, na.rm = TRUE) / length(na.omit(gyDiff))),
    #gyScaled = -1 * ((gy - minHight) / sdHight),
    Answer = ifelse(Answer == 1, 'Yes', 'No'),
    qType  = ifelse(qType == 1, 'New', 'Old'),
    gyNo  = ifelse(Answer == 'No', gyScaled, NA),
    gyYes = ifelse(Answer == 'Yes', gyScaled, NA))


Yes             <- NULL
colorYes        <- 'blue'
No              <- NULL
colorNo         <- 'red' 

# graphs for group


graphTitel <- c("scaled Experimental all questions both answers 25 num")
graphName <- c("scaled Experimental all questions both answers 25 num.pdf")
#GraphData <- ddply(B2EyeData, c("ID", 'TrialTime'), summarise,
#                   MeanYes = mean(gyYes, na.rm = TRUE),
#                   MeanNo  = mean(gyNo, na.rm = TRUE))

#GraphData.1 <- ddply(GraphData, c('TrialTime'), summarise,
#                    Yes   = mean(MeanYes, na.rm = TRUE),
#                     SDYes = sd(MeanYes, na.rm = TRUE),
#                     No    = mean(MeanNo, na.rm = TRUE),
#                     SDNo  = sd(MeanNo, na.rm = TRUE))

GraphData <- B2EyeData %>%
  group_by(ID, TrialTime) %>%
  summarise(MeanYes = mean(gyYes, na.rm = TRUE),
            MeanNo  = mean(gyNo, na.rm = TRUE))

GraphData.1 <- GraphData %>%
  group_by(TrialTime) %>%
  summarise(Yes   = mean(MeanYes, na.rm = TRUE),
            SDYes = sd(MeanYes, na.rm = TRUE),
            No    = mean(MeanNo, na.rm = TRUE),
            SDNo  = sd(MeanNo, na.rm = TRUE))

#GraphData.1$SN <- c(1:length(GraphData.1$TrialTime))
GraphData.1 %>%
  ggplot(aes(x = TrialTime, y = Yes)) + 
  geom_point(color = 'blue') +
  geom_point(aes(y = No), color = 'red') +
  xlab("Time") + ylab("Hight") + ggtitle(graphTitel) +
  theme(panel.grid.major = element_line(color = 'grey')) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text   = element_text(size = 12),
        axis.title  = element_text(size = 14, face = "bold"),
        axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) 
  theme(legend.text = element_text(size = 14)) + 
ggsave(graphName, width = 40, height = 28, units = "cm")


GraphData.1 %>%
  filter(TrialTime < endBM) %>%
  ggplot(aes(x = TrialTime, y = Yes)) + 
  geom_point(color = 'blue') +
  geom_point(aes(y = No), color = 'red') +
  xlab("Time") + ylab("Hight") + ggtitle(graphTitel) +
  theme(panel.grid.major = element_line(color = 'grey')) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text   = element_text(size = 12),
        axis.title  = element_text(size = 14, face = "bold"),
        axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(legend.text = element_text(size = 14)) 
ggsave(graphName, width = 40, height = 28, units = "cm")



GraphData.1 %>%
  filter(TrialTime >= 2000 & TrialTime <= endBM) %>%
  ggplot(aes(x = TrialTime, y = Yes))  +
  #geom_errorbar(aes(ymin = Yes - SDYes, ymax = Yes + SDYes), width = 0.1,
  #             position = position_dodge(0.2), color = "blue") +
  geom_point(color = 'blue') +
  #geom_errorbar(aes(ymin = No - SDNo, ymax = No + SDNo), width = 0.1,
  #              position = position_dodge(0.2), color = "pink") +
  geom_point(aes(y = No), color = 'red') +
  xlab("Time") + ylab("Hight") + ggtitle(graphTitel) +
  theme(panel.grid.major = element_line(color = 'grey')) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text   = element_text(size = 12),
        axis.title  = element_text(size = 14, face = "bold"),
        axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(legend.text = element_text(size = 14)) 

ggsave(graphName, width = 40, height = 28, units = "cm")


Temp <- B2EyeData %>%
  filter(TrialTime >= minCriticTime & TrialTime <= maxCriticTime)

data12 <-  ddply(Temp, c('ID', 'Trial.x', 'Answer', 'qType'), summarise,
                 Mean = mean(gyScaled, na.rm = TRUE))
data12.1 <-  ddply(data12, c('ID', 'Answer', 'qType'), summarise,
                   Mean.1 = mean(Mean, na.rm = TRUE))

data12.1 %>%
  ggplot(aes(x = qType, y = Mean.1, fill = Answer)) +
  geom_dotplot(binaxis = 'y', stackdir = 'center', position = position_dodge(0.8), binwidth = 0.25)  +
  theme(panel.grid.major = element_line(color = 'grey')) + 
  scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9")) + 
  stat_summary(fun = mean, geom = "point", shape = 18, size = 5, aes(color = Answer) , position = position_dodge(0.8)) + 
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "pointrange", aes(color = Answer), position = position_dodge(0.8)) +
  xlab('Question type') + ylab('Mean hight') + ggtitle(graphTitel) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text   = element_text(size = 18),
        axis.title  = element_text(size = 14, face = "bold"),
        axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(legend.text = element_text(size = 14)) + 
ggsave(graphName, width = 20, height = 14, units = "cm")

data12.1 %>%
  group_by(Answer, qType) %>%
  summarise(Mean = round(mean(Mean.1), 2),
            SD   = round(sd(Mean.1), 2))

data12 <- data12 %>%
  mutate(Answer = as.factor(Answer),
         qType  = as.factor(qType))
contrasts(data12$Answer)[1] <- -1
contrasts(data12$qType)[1] <- 1
contrasts(data12$qType)[2] <- -1


summary(lmer(Mean ~ Answer * qType + (1 + Answer * qType | ID), data = data12, REML = FALSE))
summary(lmer(Mean ~ Answer * qType + (1 + Answer + Answer:qType | ID), data = data12, REML = FALSE))
summary(lmer(Mean ~ Answer * qType + (1 + Answer:qType | ID), data = data12, REML = FALSE))
summary(lmer(Mean ~ Answer * qType + (1 | ID), data = data12, REML = FALSE))
summary(lmer(Mean ~ Answer * qType + (1 + Answer | ID), data = data12, REML = FALSE))
summary(lmer(Mean ~ Answer * qType + (1 + qType | ID), data = data12, REML = FALSE))


############################
#   End Numeric pass Pp    #
############################


############################
#      Sig pass Pp         #
############################

B2EyeData <- Data %>%
  filter(Block == 2) %>%
  filter(ID %in% pass25Sig) %>%
  group_by(ID) %>%
  mutate(#minHight = max(na.omit(gy)),
    #sdHight  = sd(gy, na.rm = TRUE),
    gyDiff = gy - 540,
    gyScaled = gyDiff / sqrt(sum((gyDiff)^2, na.rm = TRUE) / length(na.omit(gyDiff))),
    #gyScaled = -1 * ((gy - minHight) / sdHight),
    Answer = ifelse(Answer == 1, 'Yes', 'No'),
    qType  = ifelse(qType == 1, 'New', 'Old'),
    gyNo  = ifelse(Answer == 'No', gyScaled, NA),
    gyYes = ifelse(Answer == 'Yes', gyScaled, NA))


Yes             <- NULL
colorYes        <- 'blue'
No              <- NULL
colorNo         <- 'red' 

# graphs for group


graphTitel <- c("scaled sig 25 Experimental all questions both answers")
graphName <- c("scaled sig 25 Experimental all questions both answers.pdf")
#GraphData <- ddply(B2EyeData, c("ID", 'TrialTime'), summarise,
#                   MeanYes = mean(gyYes, na.rm = TRUE),
#                   MeanNo  = mean(gyNo, na.rm = TRUE))

#GraphData.1 <- ddply(GraphData, c('TrialTime'), summarise,
#                    Yes   = mean(MeanYes, na.rm = TRUE),
#                     SDYes = sd(MeanYes, na.rm = TRUE),
#                     No    = mean(MeanNo, na.rm = TRUE),
#                     SDNo  = sd(MeanNo, na.rm = TRUE))

GraphData <- B2EyeData %>%
  group_by(ID, TrialTime) %>%
  summarise(MeanYes = mean(gyYes, na.rm = TRUE),
            MeanNo  = mean(gyNo, na.rm = TRUE))

GraphData.1 <- GraphData %>%
  group_by(TrialTime) %>%
  summarise(Yes   = mean(MeanYes, na.rm = TRUE),
            SDYes = sd(MeanYes, na.rm = TRUE),
            No    = mean(MeanNo, na.rm = TRUE),
            SDNo  = sd(MeanNo, na.rm = TRUE))

#GraphData.1$SN <- c(1:length(GraphData.1$TrialTime))
GraphData.1 %>%
  ggplot(aes(x = TrialTime, y = Yes)) + 
  geom_point(color = 'blue') +
  geom_point(aes(y = No), color = 'red') +
  xlab("Time") + ylab("Hight") + ggtitle(graphTitel) +
  theme(panel.grid.major = element_line(color = 'grey')) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text   = element_text(size = 12),
        axis.title  = element_text(size = 14, face = "bold"),
        axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(legend.text = element_text(size = 14)) + 
ggsave(graphName, width = 40, height = 28, units = "cm")


GraphData.1 %>%
  filter(TrialTime < endBM) %>%
  ggplot(aes(x = TrialTime, y = Yes)) + 
  geom_point(color = 'blue') +
  geom_point(aes(y = No), color = 'red') +
  xlab("Time") + ylab("Hight") + ggtitle(graphTitel) +
  theme(panel.grid.major = element_line(color = 'grey')) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text   = element_text(size = 12),
        axis.title  = element_text(size = 14, face = "bold"),
        axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(legend.text = element_text(size = 14)) + 
ggsave(graphName, width = 40, height = 28, units = "cm")



GraphData.1 %>%
  filter(TrialTime >= 2000 & TrialTime <= endBM) %>%
  ggplot(aes(x = TrialTime, y = Yes))  +
  #geom_errorbar(aes(ymin = Yes - SDYes, ymax = Yes + SDYes), width = 0.1,
  #             position = position_dodge(0.2), color = "blue") +
  geom_point(color = 'blue') +
  #geom_errorbar(aes(ymin = No - SDNo, ymax = No + SDNo), width = 0.1,
  #              position = position_dodge(0.2), color = "pink") +
  geom_point(aes(y = No), color = 'red') +
  xlab("Time") + ylab("Hight") + ggtitle(graphTitel) +
  theme(panel.grid.major = element_line(color = 'grey')) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text   = element_text(size = 12),
        axis.title  = element_text(size = 14, face = "bold"),
        axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(legend.text = element_text(size = 14)) 

  ggsave(graphName, width = 40, height = 28, units = "cm")



Temp <- B2EyeData %>%
  filter(TrialTime >= minCriticTime & TrialTime <= maxCriticTime)

data12 <-  ddply(Temp, c('ID', 'Trial.x', 'Answer', 'qType'), summarise,
                 Mean = mean(gyScaled, na.rm = TRUE))
data12.1 <-  ddply(data12, c('ID', 'Answer', 'qType'), summarise,
                   Mean.1 = mean(Mean, na.rm = TRUE))

data12.1 %>%
  ggplot(aes(x = qType, y = Mean.1, fill = Answer)) +
  geom_dotplot(binaxis = 'y', stackdir = 'center', position = position_dodge(0.8), binwidth = 0.07)  +
  theme(panel.grid.major = element_line(color = 'grey')) + 
  scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9")) + 
  stat_summary(fun = mean, geom = "point", shape = 18, size = 5, aes(color = Answer) , position = position_dodge(0.8)) + 
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "pointrange", aes(color = Answer), position = position_dodge(0.8)) +
  xlab('Question type') + ylab('Mean hight') + ggtitle(graphTitel) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text   = element_text(size = 18),
        axis.title  = element_text(size = 14, face = "bold"),
        axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(legend.text = element_text(size = 14)) 

  ggsave(graphName, width = 20, height = 14, units = "cm")

data12.1 %>%
  group_by(Answer, qType) %>%
  summarise(Mean = round(mean(Mean.1), 2),
            SD   = round(sd(Mean.1), 2))

data12 <- data12 %>%
  mutate(Answer = as.factor(Answer),
         qType  = as.factor(qType))
contrasts(data12$Answer)[1] <- -1
contrasts(data12$qType)[1] <- 1
contrasts(data12$qType)[2] <- -1


summary(lmer(Mean ~ Answer * qType + (1 + Answer * qType | ID), data = data12, REML = FALSE))
summary(lmer(Mean ~ Answer * qType + (1 + Answer + Answer:qType | ID), data = data12, REML = FALSE))
summary(lmer(Mean ~ Answer * qType + (1 + Answer:qType | ID), data = data12, REML = FALSE))
summary(lmer(Mean ~ Answer * qType + (1 | ID), data = data12, REML = FALSE))
summary(lmer(Mean ~ Answer * qType + (1 + Answer | ID), data = data12, REML = FALSE))
summary(lmer(Mean ~ Answer * qType + (1 + qType | ID), data = data12, REML = FALSE))

finalModelSig <- summary(lmer(Mean ~ Answer * qType + (1 | ID), data = data12, REML = FALSE))

############################
#   End sig pass Pp        #
############################



a <- c(11, 9, 9, 11, 12, 13, 8, 8, 9)
b <- c(14, 6, 6, 14, 15, 16, 5, 5, 6)
an <- a + 3
bn <- b + 3

A <- c(a, an)
B <- c(b, bn)

Sa <- (a - 2) / sqrt(sum((A - 2)^2, na.rm = TRUE) / length(na.omit(A)))
Sb <- (b - 2) / sqrt(sum((A - 2)^2, na.rm = TRUE) / length(na.omit(A)))
San <- (an - 2) /  sqrt(sum((B - 2)^2, na.rm = TRUE) / length(na.omit(B)))
Sbn <- (bn - 2) /  sqrt(sum((B - 2)^2, na.rm = TRUE) / length(na.omit(B)))


(mean(a) - mean(b)) / sd(c(a, b))

(mean(Sa) - mean(Sb)) / sd(c(Sa, Sb))





