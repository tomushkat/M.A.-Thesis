
accData <- behavioralData %>%
  mutate(Answer = ifelse(Answer == 1, 'Yes', 'No')) %>%
  filter(newBlock == 2) %>%
  filter(ID %in% onlyB2) %>%
  group_by(ID, Answer) %>%
  summarise(meanAccuracy = mean(newIsCorrect, na.rm = TRUE)) %>%
  mutate(Group = paste0(ID, Answer))

accData <- accData[complete.cases(accData), ]

B2EyeData <- Data %>%
  filter(Block == 1) %>%
  filter(TrialTime > 2200 & TrialTime < 2300) %>%
  group_by(ID, Trial.x, Answer) %>%
  summarise(meanEye = mean(gy, na.rm = TRUE)) %>%
  group_by(ID, Answer) %>%
  summarise(MeanEyegaze = mean(meanEye, na.rm = TRUE)) %>%
  mutate(Answer = ifelse(Answer == 1, 'Yes', 'No'),
         Group  = paste0(ID, Answer)) 

finalData <- merge(accData, B2EyeData, by = c('Group'))
finalData <- finalData %>%
  mutate(ID.y = NULL,
         Answer.y = NULL)

diffAcc <- finalData$meanAccuracy[finalData$Answer.x == 'Yes'] - finalData$meanAccuracy[finalData$Answer.x == 'No']
diffMeanEyegaze <- finalData$MeanEyegaze[finalData$Answer.x == 'Yes'] - finalData$MeanEyegaze[finalData$Answer.x == 'No']

corData <- data.frame(ID = unique(finalData$ID), yesAcc = finalData$meanAccuracy[finalData$Answer.x == 'Yes'], noAcc = finalData$meanAccuracy[finalData$Answer.x == 'No'], diffAcc,
                      YesGaze = finalData$MeanEyegaze[finalData$Answer.x == 'Yes'], noGaze = finalData$MeanEyegaze[finalData$Answer.x == 'No'],  diffMeanEyegaze)

corTable <- apa.cor.table(corData[ , c(2:7)], filename="Table2_APA.doc", table.number=1)
corData <- round(corData, 2)

corData$directionEye1 <- ifelse(corData$diffMeanEyegaze < 0, 'rightDirection', 'wrongDirection')

Title <- c('Accuracy and Eye-gaze differences')
Name <- c('Block 2 Accuracy and Eye-gaze differences.pdf')
corData %>%
  ggplot(aes(x = diffAcc, y = diffMeanEyegaze)) + 
  geom_point() +
  #scale_color_manual(breaks = c('rightDirection', 'wrongDirection'), values = c('blue', 'red')) +
  ggtitle(Title) + xlab('Accuracy: Yes - No') + ylab('ET: Yes - No') +
  Theme + 
  ylim(-1000, 500) + xlim(-1, 1) 
  Lbls
ggsave(Name, width = 24, height = 20, units = "cm")


corData <- corData %>%
  mutate(sdETDistance = diffMeanEyegaze / sd(diffMeanEyegaze),
         sdDiffAcc = diffAcc / sd(diffAcc))

Title <- c('SD Accuracy and Eye-gaze differences')
Name <- c('Block 2 SD Accuracy and Eye-gaze differences.pdf')

corData %>%
  ggplot(aes(x = sdDiffAcc, y = sdETDistance)) + 
  geom_point() +
  #scale_color_manual(breaks = c('rightDirection', 'wrongDirection'), values = c('blue', 'red')) +
  ggtitle(Title) + xlab('Accuracy: Yes - No') + ylab('ET: Yes - No') +
  Theme + ylim(-3.5, 3.5) + xlim(-3.5, 3.5)
Lbls
ggsave(Name, width = 24, height = 20, units = "cm")


B2 <- Data %>%
  filter(ID %in% onlyB2) %>%
  filter(Block == 2) %>%
  mutate(gyNo  = ifelse(Answer == -1, gy, NA),
         gyYes = ifelse(Answer == 1, gy, NA),
         Answer = ifelse(Answer == 1, 'Yes', 'No'),
         qType = ifelse(qType == 1, "New", "Old"))


ET1 <- B2 %>%
  filter(TrialTime >= minCriticTime & TrialTime <= maxCriticTime) %>%
  group_by(ID, TrialTime) %>%
  summarise(MeanYes = mean(gyYes, na.rm = TRUE),
            MeanNo  = mean(gyNo, na.rm = TRUE),
            meanAccuracy = mean(newIsCorrect, na.rm = TRUE))

ET1.1 <- ET1 %>%
  group_by(ID) %>%
  summarise(Yes   = mean(MeanYes, na.rm = TRUE),
            No    = mean(MeanNo, na.rm = TRUE),
             Accuracy = mean(meanAccuracy, na.rm = TRUE))

ET1.2 <- ET1.1 %>%
  mutate(Diff = Yes - No)

shapiro.test(ET1.2$Accuracy)
shapiro.test(ET1.2$Diff)


Cor <- cor.test(ET1.2$Diff, ET1.2$Accuracy, method = 'spearman')
p <- round(Cor$p.value, 2)
r <- round(Cor$estimate, 2)
Rsquared <- round(r ^ 2, 2)
Tilte <- paste0('rho  =  ', r, ',  p  =  ', p, ',  R-squared = ', Rsquared)

ET1.2 %>%
  ggplot(aes(x = Diff, y = Accuracy)) +
  geom_point() +
  geom_smooth(method = 'lm', se = TRUE) + 
  xlab('Eye Yes - No') + ylab('Accuracy') + 
  Theme + ggtitle(Tilte) + ylim(0, 1)  
  Lbls
ggsave('Induction accuracy ET correlation.pdf',  width = 40, height = 28, units = "cm")





B2 <- Data %>%
  filter(ID %in% onlyB2) %>%
  mutate(gyNo  = ifelse(Answer == -1, gy, NA),
         gyYes = ifelse(Answer == 1, gy, NA),
         Answer = ifelse(Answer == 1, 'Yes', 'No'),
         qType = ifelse(qType == 1, "New", "Old"))


ET1 <- B2 %>%
  filter(TrialTime >= minCriticTime & TrialTime <= maxCriticTime) %>%
  group_by(ID, Block, Trial.x) %>%
  summarise(MeanYes = mean(gyYes, na.rm = TRUE),
            MeanNo  = mean(gyNo, na.rm = TRUE))

ET1.1 <- ET1 %>%
  group_by(ID, Block) %>%
  summarise(Yes   = mean(MeanYes, na.rm = TRUE),
            No    = mean(MeanNo, na.rm = TRUE))

ET1.2 <- ET1.1 %>%
  mutate(Diff = Yes - No)


shapiro.test(ET1.2$Diff[ET1.2$Block == 1])
shapiro.test(ET1.2$Diff[ET1.2$Block == 2])

Cor <- cor.test(ET1.2$Diff[ET1.2$Block == 1], ET1.2$Diff[ET1.2$Block == 2], method = 'spearman')
df <- data.frame(b1 = ET1.2$Diff[ET1.2$Block == 1], b2 = ET1.2$Diff[ET1.2$Block == 2])
p <- round(Cor$p.value, 2)
r <- round(Cor$estimate, 2)
Rsquared <- round(r ^ 2, 2)
Tilte <- paste0('rho  =  ', r, ',  p  =  ', p, ',  R-squared = ', Rsquared)

df %>%
  ggplot(aes(x = b1, y = b2)) +
  geom_point() +
  geom_smooth(method = 'lm', se = TRUE) + 
  xlab('Induction ET Yes - No') + ylab('Experimental ET Yes - No') + 
  Theme  + ggtitle(Tilte)
ggsave('ET incution - experimental Yes vs No.pdf',  width = 40, height = 28, units = "cm")







ET2 <- Data %>%
  filter(ID %in% onlyB2) %>%
  filter(Block == 2 & TrialTime >= minCriticTime & TrialTime <= maxCriticTime) %>%
  group_by(ID, Trial.x, newIsCorrect) %>%
  summarise(Mean = mean(gy, na.rm = TRUE))


ET2.1 <- ET2 %>%
  group_by(ID) %>%
  summarise(meanAccuracy = mean(newIsCorrect, na.rm = TRUE),
            sdET         = sd(Mean, na.rm = TRUE))


shapiro.test(ET2.1$meanAccuracy)
shapiro.test(ET2.1$sdET)

Cor1 <- cor.test(ET2.1$meanAccuracy, ET2.1$sdET, method = "spearman")
p <- ifelse(Cor1$p.value < .001, '< .001', round(Cor1$p.value, 2))
r <- round(Cor1$estimate, 2)
Rsquared <- round(r ^ 2, 2)
Tilte <- paste0('rho  =  ', r, ',  p  =  ', p, ',  R-squared = ', Rsquared)

ET2.1 %>%
  ggplot(aes(x = sdET, y = meanAccuracy)) +
  geom_point() +
  geom_smooth(method = 'lm', se = TRUE) + 
  xlab('SD ET') + ylab('Accuracy') + 
  ggtitle(Tilte) + ylim(0, 1) + xlim(0, 600) +  Theme +
  Lbls
ggsave('Accuracy SDET correlation experimental.pdf',  width = 40, height = 28, units = "cm")


ET2 <- Data %>%
  filter(ID %in% onlyB2) %>%
  filter(Block == 2 & TrialTime >= minCriticTime & TrialTime <= maxCriticTime) %>%
  group_by(ID, Trial.x, Answer, newIsCorrect) %>%
  summarise(Mean = mean(gy, na.rm = TRUE))

ET2.1 <- ET2 %>%
  group_by(ID, Answer) %>%
  summarise(meanAccuracy = mean(newIsCorrect, na.rm = TRUE),
  sdET         = sd(Mean, na.rm = TRUE))

shapiro.test(ET2.1$meanAccuracy[ET2.1$Answer == 1])
shapiro.test(ET2.1$sdET[ET2.1$Answer == 1])
Cor2 <- cor.test(ET2.1$meanAccuracy[ET2.1$Answer == 1], ET2.1$sdET[ET2.1$Answer == 1])
p <- ifelse(Cor2$p.value < .001, '< .001', round(Cor2$p.value, 2))
r <- round(Cor2$estimate, 2)
Rsquared <- round(r ^ 2, 2)
Tilte <- paste0('r  =  ', r, ',  p  =  ', p, ',  R-squared = ', Rsquared)

ET2.1 %>%
  filter(Answer == 1) %>%
  ggplot(aes(x = sdET, y = meanAccuracy)) +
  geom_point() +
  geom_smooth(method = 'lm', se = TRUE) + 
  Theme + 
  ggtitle(Tilte) + ylim(0, 1) + xlim(0, 500) +
 Lbls
ggsave('Accuracy SDET correlation experimental Yes.pdf',  width = 40, height = 28, units = "cm")


shapiro.test(ET2.1$meanAccuracy[ET2.1$Answer == -1])
shapiro.test(ET2.1$sdET[ET2.1$Answer == -1])
Cor3 <- cor.test(ET2.1$meanAccuracy[ET2.1$Answer == -1], ET2.1$sdET[ET2.1$Answer == -1])
p <- ifelse(Cor3$p.value < .001, '< .001', round(Cor3$p.value, 2))
r <- round(Cor3$estimate, 2)
Rsquared <- round(r ^ 2, 2)
Tilte <- paste0('r  =  ', r, ',  p  =  ', p, ',  R-squared = ', Rsquared)

ET2.1 %>%
  filter(Answer == -1) %>%
  ggplot(aes(x = sdET, y = meanAccuracy)) +
  geom_point() +
  geom_smooth(method = 'lm', se = TRUE) + 
  xlab('SD ET') + ylab('Accuracy') + 
  Theme + 
   ggtitle(Tilte) + ylim(0, 1) + xlim(0, 500) +
  Lbls

ggsave('Accuracy SDET correlation experimental No.pdf',  width = 40, height = 28, units = "cm")
