
accEyeData <- ddply(B2, c('ID', 'Answer'), summarise,
                    meanAccuracy = mean(newIsCorrect, na.rm = TRUE))
accEyeData <- accEyeData[complete.cases(accEyeData), ]
accEyeData$Answer <- ifelse(accEyeData$Answer == 1, 'Yes', 'No')
accEyeData$Group <- paste0(accEyeData$ID, accEyeData$Answer)

B2EyeData <- B2[B2$TrialTime > 2150 & B2$TrialTime < 2300, ]
eyeAccData <- ddply(B2EyeData, c('ID', 'Trial.x',  'Answer'), summarise,
                    Mean = mean(gy, na.rm = TRUE))
eyeAccData2 <- ddply(eyeAccData, c('ID',  'Answer'), summarise,
                     MeanEyegaze = mean(Mean, na.rm = TRUE))
eyeAccData2$Answer <- ifelse(eyeAccData2$Answer == 1, 'Yes', 'No')
eyeAccData2$Group <- paste0(eyeAccData2$ID, eyeAccData2$Answer)

finalData <- merge(accEyeData, eyeAccData2, by = c('Group'))
finalData$ID.y <- NULL
finalData$Answer.y <- NULL
diffAcc <- finalData$meanAccuracy[finalData$Answer.x == 'Yes'] - finalData$meanAccuracy[finalData$Answer.x == 'No']
diffMeanEyegaze <- finalData$MeanEyegaze[finalData$Answer.x == 'Yes'] - finalData$MeanEyegaze[finalData$Answer.x == 'No']

corData <- data.frame(ID = unique(finalData$ID), yesAcc = finalData$meanAccuracy[finalData$Answer.x == 'Yes'], noAcc = finalData$meanAccuracy[finalData$Answer.x == 'No'], diffAcc,
                      YesGaze = finalData$MeanEyegaze[finalData$Answer.x == 'Yes'], noGaze = finalData$MeanEyegaze[finalData$Answer.x == 'No'],  diffMeanEyegaze)

corTable <- apa.cor.table(corData[ , c(2:7)], filename="Table2_APA.doc", table.number=1)
corData <- round(corData, 2)

corData$directionEye <- as.factor(ifelse(corData$diffMeanEyegaze > 0, TRUE, FALSE))
ggplot(corData, aes(x = yesAcc, y = noAcc)) + 
  geom_point(aes(color = YesGaze, size = noGaze, shape = directionEye)) +
  ggtitle('Accuracy and Eye-gaze correlation') + xlab('Yes accuracy') + ylab('No accuracy') + 
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
                 segment.color = 'grey50', position=position_dodge(0.8))
ggsave('Accuracy and Eye-gaze correlation.pdf', width = 24, height = 20, units = "cm")


corData$sdETDistance <- corData$diffMeanEyegaze / sd(corData$diffMeanEyegaze)

corData$sdDiffAcc <- corData$diffAcc / sd(corData$diffAcc)


ggplot(corData, aes(x = sdDiffAcc, y = sdETDistance)) + 
  geom_point() +
  ggtitle('Distance Accuracy difference and Eye-gaze difference correlation 2150-2300ms') + xlab('Accuracy difference') + ylab('Eye gaze difference') + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text   = element_text(size = 12),
        axis.title  = element_text(size = 14, face = "bold"),
        axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  geom_label_repel(aes(label = ID),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50', position=position_dodge(0.8))
ggsave('With names SD distance Accuracy difference and Eye-gaze difference correlation 2150-2300ms.pdf', width = 24, height = 20, units = "cm")
