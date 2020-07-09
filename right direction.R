#### Induction ####
B1 <- Data %>%
  filter(Block == 1) %>%
  filter(ID %in% onlyB2)


B1$gyNo <- ifelse(B1$Answer == -1, B1$gy, NA)
B1$gyYes <- ifelse(B1$Answer == 1, B1$gy, NA)

Yes             <- NULL
colorYes        <- 'blue'
No              <- NULL
colorNo         <- 'red' 

criticalTime <- B1 %>%
  filter(TrialTime > minCriticTime & TrialTime < maxCriticTime) %>%
  mutate(Answer = ifelse(Answer == 1, "Yes", "No"),
         qType  = ifelse(qType == 1, 'New', 'Old'))

data <- criticalTime %>%
  group_by(ID, Trial.x) %>%
  mutate(Y = mean(gy, na.rm = T))



data1.1 <- unique(data[ , c('ID', 'Trial.x', 'Answer', 'newIsCorrect', 'Y')])


data.1 <- data1.1 %>%
  mutate(Direction = ifelse((Answer == 'Yes' & Y < 540) | (Answer == 'No' & Y > 540), "rightD", 'wrongD'))

data.2 <- ddply(data.1, c('ID', 'Direction', 'Answer'), summarise,
                Frecuency = length(Direction))
data.2 <- data.2[complete.cases(data.2), ]
#data.2 <- data.1 %>%
#  group_by(ID, Direction, Answer) %>%
#  mutate(d = length(Direction))

data.3 <- data.2 %>%
  group_by(ID, Answer) %>%
  mutate(sumFrecuency = sum(Frecuency),
         Ratio = round(100 * Frecuency / sumFrecuency, 2))


data.3 %>%
  filter(Direction == 'rightD', ID != 37) %>%
  group_by(Answer) %>%
  summarise(Mean = mean(Ratio),
            SD   = sd(Ratio))

dforWill <- data.3 %>%
  filter(Direction == 'rightD', ID != 37)
shapiro.test(dforWill$Ratio[dforWill$Answer == 'No'])
shapiro.test(dforWill$Ratio[dforWill$Answer == 'Yes'])


wilcox.test(dforWill$Ratio[dforWill$Answer == 'Yes'], dforWill$Ratio[dforWill$Answer == 'No'], paired = TRUE)

dforWill %>%
  ggplot(aes(x = Answer, y = Ratio)) +
  geom_dotplot(binaxis = 'y', stackdir = 'center', position = position_dodge(0.8), binwidth = 2)  +
  scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9")) + 
  stat_summary(fun = mean, geom = "point", shape = 18, size = 5,  aes(color = Answer) , position = position_dodge(0.8)) + 
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "pointrange", aes(color = Answer), position = position_dodge(0.8)) +
  Theme + ylim(0, 110) + 
  xlab('Answer') + ylab('Ratio for right direction')  +
  Lbls



  asSaccade <- data1.1 %>%
    mutate(rightDirection = ifelse((Answer == 'Yes' & Y < 540) | (Answer == 'No' & Y > 540), 1,
                              ifelse((Answer == 'Yes' & Y > 540) | (Answer == 'No' & Y < 540) , 0, NA))) %>%
    group_by(ID) %>%
    summarise(right = sum(rightDirection, na.rm = TRUE),
              wrong = length(na.omit(rightDirection)) - right, 
              Diff = right - wrong, 
              ratioRight = round(sum(rightDirection, na.rm = TRUE) / length(na.omit(rightDirection)), 2),
              ratioWrong = round(1 - ratioRight, 2),
              diffRatios = ratioRight - ratioWrong, 
              dontKnow = length(rightDirection) - length(na.omit(rightDirection)),
              ratioKnow = 1 - round(dontKnow / length(rightDirection), 2),
              meanIscorrect = mean(newIsCorrect, na.rm = TRUE))
    
    
    
  shapiro.test(asSaccade$ratioRight)
  shapiro.test(asSaccade$meanIscorrect)
  
  cor <- cor.test(asSaccade$ratioRight, asSaccade$meanIscorrect, method = 'spearman')
  
  p <- ifelse(cor$p.value < .01, '< .01', round(cor$p.value, 2))
  r <- round(cor$estimate, 2)
  Rsquared <- round(r ^ 2, 2)
  Tilte <- paste0('Induction Right direction for target time: rho  =  ', r, ',  p  =  ', p, ',  R-squared = ', Rsquared)
  
  
  asSaccade %>%
    ggplot(aes(x = ratioRight, y = meanIscorrect)) + 
    geom_point(aes(color = ratioKnow, size = ratioKnow)) + 
    geom_smooth(method = 'lm', se = TRUE) + 
    xlim(0, 1) + ylim(0, 1) + 
    xlab('Right direction ratio') + ylab('Accuracy') + ggtitle(Tilte) +
    Theme +
    Lbls  





data.1.2 <- data1 %>%
  mutate(Direction = ifelse(Y < 540, "lookedUp", ifelse(Y > 540, 'LookedDown', NA)))

data.2.2 <- ddply(data.1.2, c('ID', 'Direction', 'Answer'), summarise,
                  d = length(Direction))
data.2.2 <- data.2.2[complete.cases(data.2.2), ]
#data.2 <- data.1 %>%
#  group_by(ID, Direction, Answer) %>%
#  mutate(d = length(Direction))

data.3.1 <- data.2.2 %>%
  group_by(ID) %>%
  mutate(sumd = sum(d),
         Ratio = round(100 * d / sumd, 2))

data.3.1 %>%
  group_by(Answer, Direction) %>%
  summarise(Mean = mean(Ratio),
            SD   = sd(Ratio))

data.3.1 %>%
  ggplot(aes(x = Answer, y = Ratio, fill = Direction)) +
  geom_dotplot(binaxis = 'y', stackdir = 'center', position = position_dodge(0.8), binwidth = 2)  +
  scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9")) + 
  stat_summary(fun = mean, geom = "point", shape = 18, size = 5,  aes(color = Direction), position = position_dodge(0.8)) + 
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "pointrange", aes(color = Direction), position = position_dodge(0.8)) +
  xlab('Answer') + ylab('Converted mean y axis fixation') + ggtitle('Induction') + 
  Theme + ylim(0, 100)
  Lbls

  




CrossTable(data.1.2$Direction[data.1.2$ID == 51], data.1.2$Answer[data.1.2$ID == 51], chisq = T, format = 'SPSS')

bi <- c()
for (i in unique(data.1.2$ID)){
  t <- chisq.test(data.1.2$Direction[data.1.2$ID == i], data.1.2$Answer[data.1.2$ID == i], correct = FALSE)
  t1 <- ifelse(t$p.value < .05, 1, 0)
  bi <- c(bi, t1)
  if(t$p.value < .05){
    print(i)
  }
}
bi[13] <- 0
t1 <- binom.test(sum(bi), length(bi), 0.5)

#### End induction ####













#### Experimental ####
B2 <- Data %>%
  filter(Block == 2) %>%
  filter(ID %in% onlyB2)


B2$gyNo <- ifelse(B2$Answer == -1, B2$gy, NA)
B2$gyYes <- ifelse(B2$Answer == 1, B2$gy, NA)

Yes             <- NULL
colorYes        <- 'blue'
No              <- NULL
colorNo         <- 'red' 

criticalTime <- B2 %>%
  filter(TrialTime > minCriticTime & TrialTime < maxCriticTime) %>%
  mutate(Answer = ifelse(Answer == 1, "Yes", "No"),
         qType  = ifelse(qType == 1, 'New', 'Old'))

data <- criticalTime %>%
  group_by(ID, Trial.x) %>%
  mutate(Y = mean(gy, na.rm = T))



data1 <- unique(data[ , c('ID', 'Trial.x', 'Answer', 'qType', 'newIsCorrect', 'Y')])


data.1 <- data1 %>%
  mutate(Direction = ifelse((Answer == 'Yes' & Y < 540) | (Answer == 'No' & Y > 540), "rightD", 'wrongD'))

data.2 <- ddply(data.1, c('ID', 'Direction','Answer', 'qType'), summarise,
                Frecuency = length(Direction))
data.2 <- data.2[complete.cases(data.2), ]
#data.2 <- data.1 %>%
#  group_by(ID, Direction, Answer) %>%
#  mutate(d = length(Direction))

data.3 <- data.2 %>%
  group_by(ID) %>%
  mutate(sumFrecuency = sum(Frecuency),
         Ratio = round(100 * Frecuency / sumFrecuency, 2),
         RatioReal = round(100 * Frecuency / 24, 2) )



data.3 %>%
  filter(Direction == 'rightD') %>%
  group_by(Answer, qType) %>%
  summarise(Mean = mean(Ratio),
            SD   = sd(Ratio))
shapiro.test(data.3$RatioReal[data.3$Direction == 'rightD' & data.3$Answer == 'No'  & data.3$qType == 'New'])
shapiro.test(data.3$RatioReal[data.3$Direction == 'rightD' & data.3$Answer == 'No'  & data.3$qType == 'Old'])
shapiro.test(data.3$RatioReal[data.3$Direction == 'rightD' & data.3$Answer == 'Yes' & data.3$qType == 'New'])
shapiro.test(data.3$RatioReal[data.3$Direction == 'rightD' & data.3$Answer == 'Yes' & data.3$qType == 'Old'])

summary(aov(RatioReal ~ Answer * qType + Error(ID / (Answer * qType)), data = data.3[data.3$Direction == 'rightD', ]))

ggplot(data.3[data.3$Direction == 'rightD', ], aes(x = qType, y = RatioReal, fill = Answer)) +
  geom_dotplot(binaxis = 'y', stackdir = 'center', position = position_dodge(0.8), binwidth = 2)  +
  scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9")) + 
  stat_summary(fun = mean, geom = "point", shape = 18, size = 5,  aes(color = Answer) , position = position_dodge(0.8)) + 
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "pointrange", aes(color = Answer), position = position_dodge(0.8)) +
  Theme + ylim(0, 100) + 
  xlab('Question type') + ylab('Ratio for right direction')  
  Lbls
  
ggsave('Experimental all questions without two by two dotplot only participant above 0.4 accuracy.pdf', width = 40, height = 28, units = "cm")







data.1.2 <- data1 %>%
  mutate(Direction = ifelse(Y < 540, "lookedUp", ifelse(Y > 540, 'LookedDown', NA)))

data.2.2 <- ddply(data.1.2, c('ID', 'Direction', 'Answer'), summarise,
                d = length(Direction))
data.2.2 <- data.2.2[complete.cases(data.2.2), ]
#data.2 <- data.1 %>%
#  group_by(ID, Direction, Answer) %>%
#  mutate(d = length(Direction))

data.3.1 <- data.2.2 %>%
  group_by(ID) %>%
  mutate(sumd = sum(d),
         Ratio = round(100 * d / sumd, 2))

data.3.1 %>%
  group_by(Answer, Direction) %>%
  summarise(Mean = mean(Ratio),
            SD   = sd(Ratio))


ggplot(data.3.1, aes(x = Answer, y = Ratio, fill = Direction)) +
  geom_dotplot(binaxis = 'y', stackdir = 'center', position = position_dodge(0.8), binwidth = 2)  +
  scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9")) + 
  stat_summary(fun = mean, geom = "point", shape = 18, size = 5,  aes(color = Direction), position = position_dodge(0.8)) + 
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "pointrange", aes(color = Direction), position = position_dodge(0.8)) +
  ylim(0, 100) + 
  xlab('Question type') + ylab('Converted mean y axis fixation') + ggtitle('Experimental all questions only participants above 40% accuracy') +
  Theme + 
  Lbls 
  
  ggsave('Experimental all questions without two by two dotplot only participant above 0.4 accuracy.pdf', width = 40, height = 28, units = "cm")




CrossTable(data.1.2$Direction[data.1.2$ID == 51], data.1.2$Answer[data.1.2$ID == 51], chisq = T, format = 'SPSS')

bi <- c()
for (i in unique(data.1.2$ID)){
  t <- chisq.test(data.1.2$Direction[data.1.2$ID == i], data.1.2$Answer[data.1.2$ID == i], correct = FALSE)
  t1 <- ifelse(t$p.value < .05, 1, 0)
  bi <- c(bi, t1)
  if(t$p.value < .05){
    print(i)
  }
}
bi[13] <- 0
t1 <- binom.test(sum(bi), length(bi), 0.5)

#### End experimental ####





### Together ###


B2 <- Data %>%
  filter(ID %in% onlyB2)


B2$gyNo <- ifelse(B2$Answer == -1, B2$gy, NA)
B2$gyYes <- ifelse(B2$Answer == 1, B2$gy, NA)


criticalTime <- B2 %>%
  filter(TrialTime > minCriticTime & TrialTime < maxCriticTime) %>%
  mutate(Answer = ifelse(Answer == 1, "Yes", "No"))

data <- criticalTime %>%
  group_by(ID, Block, Trial.x) %>%
  mutate(Y = mean(gy, na.rm = T))


data1 <- unique(data[ , c('ID', 'Block', 'Trial.x', 'Answer', 'newIsCorrect', 'Y')])


data.1 <- data1 %>%
  mutate(Direction = ifelse((Answer == 'Yes' & Y < 540) | (Answer == 'No' & Y > 540), "rightD", 'wrongD'))

data.2 <- ddply(data.1, c('ID', 'Block', 'Direction'), summarise,
                Frecuency = length(Direction))
data.2 <- data.2[complete.cases(data.2), ]
#data.2 <- data.1 %>%
#  group_by(ID, Direction, Answer) %>%
#  mutate(d = length(Direction))

data.3 <- data.2 %>%
  group_by(ID, Block) %>%
  mutate(sumFrecuency = sum(Frecuency),
         Ratio = round(100 * Frecuency / sumFrecuency, 2)) 


data.4 <- data.3 %>%
  filter(Direction == 'rightD') 


data.4.1 <- data.4 %>% filter(Block == '1')
data.4.2 <- data.4 %>% filter(Block == '2')

data.4.3 <- cbind(data.4.1, data.4.2)




shapiro.test(data.4.3$Ratio...6)
shapiro.test(data.4.3$Ratio...12)

Cor <- cor.test(data.4.3$Ratio...6, data.4.3$Ratio...12, method = 'spearman')
p <- round(Cor$p.value, 2)
r <- round(Cor$estimate, 2)
Rsquared <- round(r ^ 2, 2)
Title <- paste0('rho  =  ', r, ',  p  =  ', p, ',  R-squared = ', Rsquared)


data.4.3 %>%
  mutate(confindance = (sumFrecuency...5 + sumFrecuency...11) / 2) %>%
  ggplot(aes(x = Ratio...6, y = Ratio...12)) + 
  geom_point(aes(size = confindance, color = confindance)) + 
  geom_smooth(method = 'lm', se = TRUE, color = 'black') + 
  xlim(0, 100) + ylim(0, 100) +
  xlab('Right direction ratio Induction') + ylab('Right direction ratio Experimental') + 
  Theme + ggtitle(Title) 
  geom_label_repel(aes(label = ID...1),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50', position = position_dodge(0.8))
  
colnames(data.4.3)
