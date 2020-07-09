

#### Block 2 ####

behaviorlB2 <- behavioralData %>% 
  filter(newBlock == 2) %>%
  filter(ID %in% onlyB2) %>%
  mutate(Answer = ifelse(Answer == 1, 'Yes', 'No'),
         Group  = paste0(Answer, qType))


dataForAnova <- behaviorlB2 %>%
  group_by(ID, Answer, qType, Group) %>%
  summarise(meanAccuracy = mean(newIsCorrect, na.rm = TRUE))
                      
dataForAnova <- dataForAnova[complete.cases(dataForAnova), ]



shapiro.test(dataForAnova$meanAccuracy[dataForAnova$Group == 'YesNew'])
shapiro.test(dataForAnova$meanAccuracy[dataForAnova$Group == 'YesOld'])
shapiro.test(dataForAnova$meanAccuracy[dataForAnova$Group == 'NoNew'])
shapiro.test(dataForAnova$meanAccuracy[dataForAnova$Group == 'NoOld'])

rAnova <- aov(meanAccuracy ~ Answer * qType + Error(ID / (Answer * qType)), data = dataForAnova)
summary(rAnova)
ezANOVA(data = dataForAnova, wid = ID, dv = meanAccuracy, within = c('Answer', 'qType'), detailed = TRUE)
pairwise.t.test(dataForAnova$meanAccuracy, dataForAnova$Group, paired = TRUE, p.adj = "hochberg")

ddply(dataForAnova, c('Group'), summarise,
      averageAccuracy = round(mean(meanAccuracy), 2),
      SDAccuracy   = round(sd(meanAccuracy), 2))

dataForAnova %>%
  ggplot(aes(x = qType, y = meanAccuracy, fill = Answer)) +
  geom_dotplot(binaxis = 'y', stackdir = 'center', position = position_dodge(0.8), binwidth = 0.025)  +
  scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9")) + 
  stat_summary(fun = mean, geom = "point", shape = 18, size = 5,  aes(color = Answer) , position = position_dodge(0.8)) + 
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "pointrange", aes(color = Answer), position = position_dodge(0.8)) +
  xlab('Question type') + ylab('Accuracy') + ggtitle('Experimental all questions Answer by qType dotplot Accuracy') +
  Theme + ylim(0, 1) 
  Lbls
ggsave('Experimental all questions Answer by qType dotplot Accuracy.pdf', width = 30, height = 19, units = "cm")




regressiongg <- data.frame(ID = unique(dataForAnova$ID), Yes_New = dataForAnova$meanAccuracy[dataForAnova$Group == 'YesNew'], Yes_Old = dataForAnova$meanAccuracy[dataForAnova$Group == 'YesOld'],
                           No_New = dataForAnova$meanAccuracy[dataForAnova$Group == 'NoNew'], No_Old = dataForAnova$meanAccuracy[dataForAnova$Group == 'NoOld'])

write.csv(regressiongg, 'toPlot2.csv')

ggplot(regressiongg, aes(x = Yes_New, y = Yes_Old)) +
  geom_point(aes(color = No_New, size = No_Old)) +
  ggtitle('Accuracy by Yes new vs Yes old vs No new vs No old') + 
  Theme + 
  ylim(0, 1) + xlim(0, 1) +
  Lbls
ggsave('With namnes Experimental all questions without 30 and 31 two by two accuracy correlation.pdf', width = 24, height = 20, units = "cm")

regressiongg <- regressiongg[, c('Yes_New', 'Yes_Old', 'No_New', 'No_Old')]


shapiro.test(dataForAnova$meanAccuracy[dataForAnova$Group == 'YesNew'])
shapiro.test(dataForAnova$meanAccuracy[dataForAnova$Group == 'YesOld'])
shapiro.test(dataForAnova$meanAccuracy[dataForAnova$Group == 'NoNew'])
shapiro.test(dataForAnova$meanAccuracy[dataForAnova$Group == 'NoOld'])

corr <- round(cor(regressiongg, method = 'pearson'), 2)
ggcorrplot(corr, hc.order = TRUE, type = "lower",
           lab = TRUE) + ggtitle('Accuracy correlation Experimental') 
ggsave('cor.pdf')
apa.cor.table(regressiongg, filename = "Accuracy correlation plot.doc", table.number = 1)


rcorr(as.matrix(regressiongg), type = c("pearson"))



acc <- behaviorlB2 %>%
  group_by(ID, Answer, qType) %>%
  summarise(lCorrect = sum(newIsCorrect, na.rm = TRUE),
            Total    = length(na.omit(newIsCorrect)),
            Ratio    = lCorrect / Total) %>%
  mutate(numericPass = ifelse(Ratio > 0.25, 1, 0))

totalacc <- acc %>%
  group_by(ID) %>%
  summarise(total = sum(Total),
            correct = sum(lCorrect),
            ratio = correct / total) %>%
  mutate(numericPass = ifelse(ratio > 0.25, 1, 0))


acc$SN <- c(1:length(acc$ID))
TT <- c()
for (i in acc$SN){
  
  t <- binom.test(acc$lCorrect[acc$SN == i], acc$Total[acc$SN == i],  p = 0.25, alternative = c("greater"))
  t1 <- ifelse(t$p.value < 0.05, 1, 0)
  TT <- c(TT, t1)
  
}

acc$sigPass <- TT

pass25Sig <- c(6, 7, 16, 19, 21, 27, 44)
pass25Numeric <- unique(behaviorlB2$ID[! behaviorlB2$ID %in% c(28, 36, 41, 46)])

acc2 <- behaviorlB2 %>%
  group_by(ID) %>%
  summarise(lCorrect = sum(newIsCorrect, na.rm = TRUE),
            Total    = length(na.omit(newIsCorrect)))

acc2$SN <- c(1:length(acc2$ID))
TT <- c()
for (i in acc2$SN){
  
  t <- binom.test(acc2$lCorrect[acc2$SN == i], acc2$Total[acc2$SN == i],  p = 0.25, alternative = c("greater"))
  t1 <- ifelse(t$p.value < 0.05, 1, 0)
  TT <- c(TT, t1)
  
}
acc2 <- acc2 %>%
  mutate(Binom = TT)
acc2 <- acc2[acc2$Binom == 1, ]

acc2$ID





dataForAnova2 <- behaviorlB2 %>%
  group_by(ID, Answer) %>%
  summarise(meanAccuracy = mean(newIsCorrect, na.rm = TRUE))

regressiongg <- data.frame(ID = unique(dataForAnova2$ID),
                           Yes = dataForAnova2$meanAccuracy[dataForAnova2$Answer == 'Yes'],
                           No = dataForAnova2$meanAccuracy[dataForAnova2$Answer == 'No'])
shapiro.test(regressiongg$Yes)
shapiro.test(regressiongg$No)
cor <- cor.test(regressiongg$Yes, regressiongg$No, method = 'pearosn')

p <- ifelse(cor$p.value < .01, '< .01', round(cor$p.value, 2))
r <- round(cor$estimate, 2)
Rsquared <- round(r ^ 2, 2)
Tilte <- paste0('Accuracy Yes vs No r  =  ', r, ',  p  =  ', p, ',  R-squared = ', Rsquared)


regressiongg %>%
  ggplot(aes(x = Yes, y = No)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) + 
  ggtitle(Tilte) + xlab('Yes') + ylab("No") +
  Theme + 
  ylim(0, 1) + xlim(0, 1) +
  Lbls

#### End Block 2 ####


#### Block 1 ####

behaviorlB1 <- behavioralData %>% 
  filter(Block == 1) %>%
  filter(ID %in% onlyB2) %>%
  mutate(Answer = ifelse(Answer == 1, 'Yes', 'No'))



dataForAnova <- behaviorlB1 %>%
  group_by(ID, Answer) %>%
  summarise(meanAccuracy = mean(newIsCorrect, na.rm = TRUE))

dataForAnova <- dataForAnova[complete.cases(dataForAnova), ]



shapiro.test(log(dataForAnova$meanAccuracy[dataForAnova$Answer == 'Yes']))  # before log .37, after, .41
shapiro.test(log(dataForAnova$meanAccuracy[dataForAnova$Answer == 'No']))   # before log .03, after .054
var.test(log(dataForAnova$meanAccuracy[dataForAnova$Answer == 'Yes']), log(dataForAnova$meanAccuracy[dataForAnova$Answer == 'No']),
                                                                           paired = TRUE)

t.test(log(dataForAnova$meanAccuracy[dataForAnova$Answer == 'Yes']), log(dataForAnova$meanAccuracy[dataForAnova$Answer == 'No']),
       paired = TRUE, var.equal = TRUE)

ddply(dataForAnova, c('Answer'), summarise,
      averageAccuracy = round(mean(meanAccuracy), 2),
      SDAccuracy      = round(sd(meanAccuracy), 2))



dataForAnova %>%
  ggplot(aes(x = Answer, y = meanAccuracy)) +
  geom_dotplot(binaxis = 'y', stackdir = 'center', position = position_dodge(0.8), binwidth = 0.025)  +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 5 , position = position_dodge(0.8)) + 
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "pointrange", aes(color = Answer), position = position_dodge(0.8)) +
  xlab('Answer') + ylab('Accuracy') + ggtitle('Induction all questions by Answer dotplot Accuracy') +
  Theme + ylim(0, 1) 
  Lbls
ggsave('Experimental all questions Answer by qType dotplot Accuracy.pdf', width = 30, height = 19, units = "cm")

regressiongg <- data.frame(ID = unique(dataForAnova$ID), Yes = dataForAnova$meanAccuracy[dataForAnova$Answer == 'Yes'], No = dataForAnova$meanAccuracy[dataForAnova$Answer == 'No'])

#write.csv(regressiongg, 'toPlot2.csv')
cor.test(regressiongg$Yes, regressiongg$No)


ggplot(regressiongg, aes(x = Yes, y = No)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  Theme + xlab("Accuracy Yes") + ylab('Accuracy No') 
  ylim(0, 1) + xlim(0, 1) 
  Lbls
ggsave('With namnes Experimental all questions without 30 and 31 two by two accuracy correlation.pdf', width = 24, height = 20, units = "cm")

regressiongg <- regressiongg[, c('Yes_New', 'Yes_Old', 'No_New', 'No_Old')]

corr <- round(cor(regressiongg, method = 'pearson'), 2)
ggcorrplot(corr, hc.order = TRUE, type = "lower",
           lab = TRUE) + ggtitle('Accuracy correlation Experimental')
ggsave('cor.pdf')
apa.cor.table(regressiongg, filename = "Accuracy correlation plot.doc", table.number = 1)


rcorr(as.matrix(regressiongg), type = c("pearson"))



acc <- behaviorlB2 %>%
  group_by(ID, Answer, qType) %>%
  summarise(lCorrect = sum(newIsCorrect, na.rm = TRUE),
            Total    = length(na.omit(newIsCorrect)),
            Ratio    = lCorrect / Total) %>%
  mutate(numericPass = ifelse(Ratio > 0.25, 1, 0))

totalacc <- acc %>%
  group_by(ID) %>%
  summarise(total = sum(Total),
            correct = sum(lCorrect),
            ratio = correct / total) %>%
  mutate(numericPass = ifelse(ratio > 0.25, 1, 0))


acc$SN <- c(1:length(acc$ID))
TT <- c()
for (i in acc$SN){
  
  t <- binom.test(acc$lCorrect[acc$SN == i], acc$Total[acc$SN == i],  p = 0.25, alternative = c("greater"))
  t1 <- ifelse(t$p.value < 0.05, 1, 0)
  TT <- c(TT, t1)
  
}

acc$sigPass <- TT

pass25Sig <- c(6, 7, 16, 19, 21, 27, 44)
pass25Numeric <- unique(behaviorlB2$ID[! behaviorlB2$ID %in% c(28, 36, 41, 46)])

acc2 <- behaviorlB2 %>%
  group_by(ID) %>%
  summarise(lCorrect = sum(newIsCorrect, na.rm = TRUE),
            Total    = length(na.omit(newIsCorrect)))

acc2$SN <- c(1:length(acc2$ID))
TT <- c()
for (i in acc2$SN){
  
  t <- binom.test(acc2$lCorrect[acc2$SN == i], acc2$Total[acc2$SN == i],  p = 0.25, alternative = c("greater"))
  t1 <- ifelse(t$p.value < 0.05, 1, 0)
  TT <- c(TT, t1)
  
}
acc2 <- acc2 %>%
  mutate(Binom = TT)
acc2 <- acc2[acc2$Binom == 1, ]

acc2$ID


#### End Block 1 ####
