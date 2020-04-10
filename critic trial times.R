data <- dataBehavioral[dataBehavioral$newBlock == 2, ]
data <- data[! data$ID %in% Badpp, ]
data2 <- ddply(data, c('ID', 'Answer'), summarise,
               Mean = mean(IsCorrect, na.rm = T))
var.test(data2$Mean[data2$Answer == 1], data2$Mean[data2$Answer == 2])
shapiro.test(data2$Mean)
t.test(data2$Mean[data2$Answer == 1], data2$Mean[data2$Answer == 2], var.equal = TRUE, paired = TRUE, alternative = "greater")

data3 <- ddply(data2, c("Answer"), summarise,
               newMean = round(mean(Mean), 2),
               SD      = round(sd(Mean), 2))

newData <- b2Data[b2Data$TrialTime == 400 | b2Data$TrialTime == 1500 | b2Data$TrialTime == 1900 | b2Data$TrialTime == 2100 | b2Data$TrialTime == 2200 | b2Data$TrialTime == 2350 | b2Data$TrialTime == 2500, ]
toTest <- ddply(newData, c('TrialTime', "Answer"), summarise,
                Mean = mean(gy, na.rm = T))
toTest$TrialTime <- as.factor(toTest$TrialTime)
toTest$Answer <- ifelse(toTest$Answer == 1, "Yes", "No")
#toTest$TrialTime <- ifelse(toTest$TrialTime == 400, '400ms Q start', 
#                           ifelse(toTest$TrialTime == 1500, '1500ms Q',
#                                  ifelse(toTest$TrialTime == 1900, '1900ms Q',
#                                         ifelse(toTest$TrialTime == 2100, '2100ms ISI start',
#                                                ifelse(toTest$TrialTime == 2350, '2350ms Target start',
#                                                      ifelse(toTest$TrialTime == 2500, '2500ms BM strat', 1))))))
ggplot(toTest, aes(x = TrialTime, y = Mean)) +
  geom_point(aes(color = Answer)) +
  xlab('Time') + ylab('Converted mean y axis fixation') + ggtitle('Experimental all questions without 30 and 31 critic trial times') + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text   = element_text(size = 12),
        axis.title  = element_text(size = 14, face = "bold"),
        axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(legend.text = element_text(size = 14)) 
ggsave('Experimental all questions without 30 and 31 critic trial times.pdf', width = 40, height = 28, units = "cm")

