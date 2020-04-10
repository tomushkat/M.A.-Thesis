newData <- Data[Data$TrialTime > 2150 & Data$TrialTime < 2300, ] %>%
  filter(Block <= 2) %>%
  filter(ID %in% unique(B2$ID)) %>%
  mutate(Answer = ifelse(Answer == 1, 'Yes', 'No'),
         Block = ifelse(Block == 1, 'Induction', 'Experimental')) %>%
  group_by(ID, Block, Answer) %>%
  summarise(Mean = mean(newIsCorrect, na.rm = T),
            Eye  = mean(gy, na.rm = T))

ggplot(newData, aes(x = Block, y = Mean, fill = Answer)) +
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
xlab('Question type') + ylab('Converted mean y axis fixation') + ggtitle('Experimental all questions only participants above 40% accuracy') 

ggplot(newData, aes(x = Block, y = Eye, fill = Answer)) +
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
xlab('Question type') + ylab('Converted mean y axis fixation') + ggtitle('Experimental all questions only participants above 40% accuracy') 



newDataY <-  newData %>%
  filter(Answer == 'Yes')


newDatan <-  newData %>%
  filter(Answer == 'No') 

newDatan$Yes <- newDataY$Mean
newDatan$YesEye <- newDataY$Eye


newDatan <- newDatan %>%
  mutate(sub = Yes - Mean,
         subEye = YesEye - Eye)

d <- data.frame(ID = unique(newDatan$ID),
                InductionAcc = newDatan$sub[newDatan$Block == 'Induction'],
                ExperimentalAcc = newDatan$sub[newDatan$Block == 'Experimental'],
                InductionEye = newDatan$subEye[newDatan$Block == 'Induction'],
                ExperimentalEye = newDatan$subEye[newDatan$Block == 'Experimental'])


shapiro.test(d$InductionAcc)
shapiro.test(d$ExperimentalAcc)
cor.test(d$InductionAcc, d$ExperimentalAcc)

d %>%
  ggplot(aes(x = InductionAcc / sd(InductionAcc), y = ExperimentalAcc / sd(ExperimentalAcc))) + 
  geom_point() +
  geom_smooth(method = lm, se = TRUE) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text   = element_text(size = 12),
        axis.title  = element_text(size = 14, face = "bold"),
        axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(legend.text = element_text(size = 14)) +
  ggtitle('Induction Yes - NO accuracy by Experimental Yes - NO accuracy') + xlab('Induction Yes - NO') + ylab('Experimental Yes - NO')


d1 <- d[complete.cases(d), ]
shapiro.test(d1$InductionEye)
shapiro.test(d$ExperimentalEye)
cor.test(d1$InductionEye, d1$ExperimentalEye)

d1 %>%
  ggplot(aes(x = InductionEye / sd(InductionEye), y = ExperimentalEye / sd(ExperimentalEye))) + 
  geom_point() +
  geom_smooth(method = lm, se = TRUE) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text   = element_text(size = 12),
        axis.title  = element_text(size = 14, face = "bold"),
        axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size =
                                     
                                     +
                                     theme(axis.line.y = element_line(color = "black", size = 1),
                                           axis.line.x = element_line(color = "black", size = 1)) +
                                     theme(legend.text = element_text(size = 14)) +
                                     ggtitle('Induction Yes - NO eye (sd) by Experimental Yes - NO eye (sd)') + xlab('Induction Yes - NO') + ylab('Experimental Yes - NO')
                                   
                                   
                                   