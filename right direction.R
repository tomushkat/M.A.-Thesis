B2 <- subset(Data, Data$Block == 2)

B2$gyNo <- ifelse(B2$Answer == -1, B2$gy, NA)
B2$gyYes <- ifelse(B2$Answer == 1, B2$gy, NA)

Yes             <- NULL
colorYes        <- 'blue'
No              <- NULL
colorNo         <- 'red' 

criticalTime <- B2 %>%
  filter(TrialTime > 2250 & TrialTime < 2300)
criticalTime <- criticalTime %>%
  mutate(Answer = ifelse(Answer == 1, "Yes", "No"),
         qType  = ifelse(qType == 1, 'New', 'Old'))

data <- criticalTime %>%
  group_by(ID, Trial.x) %>%
  mutate(Y = mean(gy, na.rm = T))

data <- data[complete.cases(data), ]


data.1 <- unique(data[ , c('ID', 'Trial.x', 'Answer', 'qType', 'newIsCorrect', 'Y')])


data.1 <- data.1 %>%
  mutate(Direction = ifelse((Answer == 'Yes' & Y < 540) | (Answer == 'No' & Y > 540), "rightD", 'wrongD'))

data.2 <- ddply(data.1, c('ID', 'Direction','Answer', 'qType'), summarise,
                d = length(Direction))
#data.2 <- data.1 %>%
#  group_by(ID, Direction, Answer) %>%
#  mutate(d = length(Direction))

data.3 <- data.2 %>%
  group_by(ID) %>%
  mutate(sumd = sum(d),
         Ratio = round(100 * d / sumd, 2))

CrossTable(data.1$newIsCorrect[data.1$ID == 18], data.1$Direction[data.1$ID == 18], chisq = T, format = 'SPSS')


ggplot(data.3[data.3$Direction == 'rightD', ], aes(x = qType, y = Ratio, fill = Answer)) +
  geom_dotplot(binaxis='y', stackdir = 'center', position=position_dodge(0.8))  +
  scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9")) + 
  stat_summary(fun.y = mean, geom = "point", shape = 18, size = 5,  aes(color = Answer) , position = position_dodge(0.8)) + 
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "pointrange", aes(color = Answer), position = position_dodge(0.8)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text   = element_text(size = 12),
        axis.title  = element_text(size = 14, face = "bold"),
        axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(legend.text = element_text(size = 14)) +
  geom_label_repel(aes(label = ID),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50', position=position_dodge(0.8))
xlab('Question type') + ylab('Converted mean y axis fixation') + ggtitle('Experimental all questions only participants above 40% accuracy') + 
  
  ggsave('Experimental all questions without two by two dotplot only participant above 0.4 accuracy.pdf', width = 40, height = 28, units = "cm")
