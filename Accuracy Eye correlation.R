accData <- behavioralData %>%
  mutate(Answer = ifelse(Answer == 1, 'Yes', 'No')) %>%
  filter(newBlock == 2) %>%
  filter(ID != 30) %>% filter(ID != 31) %>%
  group_by(ID, Answer) %>%
  summarise(meanAccuracy = mean(newIsCorrect, na.rm = TRUE)) %>%
  mutate(Group = paste0(ID, Answer))

accData <- accData[complete.cases(accData), ]

B2EyeData <- Data %>%
  filter(Block == 2) %>%
  filter(TrialTime > 2150 & TrialTime < 2300) %>%
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
Name <- c('Accuracy and Eye-gaze differences.pdf')
corData %>%
  ggplot(aes(x = diffAcc, y = diffMeanEyegaze)) + 
  geom_point(aes(shape = directionEye1), size = 3) +
  #scale_color_manual(breaks = c('rightDirection', 'wrongDirection'), values = c('blue', 'red')) +
  ggtitle(Title) + xlab('Accuracy: Yes - No') + ylab('ET: Yes - No') +
  theme_classic() + 
  theme(panel.grid.major = element_line(color = 'grey')) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text   = element_text(size = 12),
        axis.title  = element_text(size = 14, face = "bold"),
        axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1))  + 
geom_label_repel(aes(label = ID),
                 box.padding   = 0.35, 
                 point.padding = 0.5,
                 segment.color = 'grey50', position = position_dodge(0.8))
ggsave(Name, width = 24, height = 20, units = "cm")


corData <- corData %>%
  mutate(sdETDistance = diffMeanEyegaze / sd(diffMeanEyegaze),
         sdDiffAcc = diffAcc / sd(diffAcc))

Title <- c('SD Accuracy and Eye-gaze differences')
Name <- c('SD Accuracy and Eye-gaze differences.pdf')

corData %>%
  ggplot(aes(x = sdDiffAcc, y = sdETDistance)) + 
  geom_point(aes(shape = directionEye1), size = 3) +
  #scale_color_manual(breaks = c('rightDirection', 'wrongDirection'), values = c('blue', 'red')) +
  ggtitle(Title) + xlab('Accuracy: Yes - No') + ylab('ET: Yes - No') +
  theme_classic() + 
  theme(panel.grid.major = element_line(color = 'grey')) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text   = element_text(size = 12),
        axis.title  = element_text(size = 14, face = "bold"),
        axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1))  
  geom_label_repel(aes(label = ID),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50', position = position_dodge(0.8))
ggsave(Name, width = 24, height = 20, units = "cm")
