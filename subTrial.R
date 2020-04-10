

B2 <- subset(Data, Data$Block == 2)
B2$subTrial <- ifelse(B2$TrialTime >= 400 & B2$TrialTime < 850, 1,
                      ifelse(B2$TrialTime >= 850 & B2$TrialTime < 1300, 2,
                             ifelse(B2$TrialTime >= 1300 & B2$TrialTime < 1750, 3,
                                    ifelse(B2$TrialTime >= 1750 & B2$TrialTime < 2200, 4, NA))))

f400t825   <- B2[B2$subTrial == 1, ]
f825t1250  <- B2[B2$subTrial == 2, ]
f1250t1675 <- B2[B2$subTrial == 3, ]
f1675t2100 <- B2[B2$subTrial == 4, ]


dd <- function(data){
  Data <- ddply(data, c('ID', 'Trial', 'Answer', 'qType', 'subTrial'), summarise,
                Mean = mean(gy, na.rm = TRUE))
  return(Data)
}
data1 <- B2[complete.cases(B2$subTrial), ]
data1 <- dd(data1)
data2 <- dd(f400t825)
data3 <- dd(f825t1250)
data4 <- dd(f1250t1675)
data5 <- dd(f1675t2100)
data1$subTrial <- as.factor(data1$subTrial)

data1$ID <- as.character(data1$ID)
summary(lmer(Mean ~ Answer:subTrial:qType  + (1 + 1 | ID), data = data1))
data1$Answer <- ifelse(data1$Answer == 1, 'Yes', 'No')
data1$qType <- ifelse(data1$qType == 1, 'New', 'Old')
d1 <- ddply(data1, c('ID', 'Answer', 'qType', 'subTrial'), summarise,
            meanGy = mean(Mean, na.rm = TRUE))
d1.1 <- ddply(d1, c('qType', 'Answer', 'subTrial'), summarise,
              meanGy.1 = round(mean(meanGy, na.rm = TRUE), 2),
              SD       = round(sd(meanGy, na.rm = TRUE), 2))
ggplot(data1, aes(x = subTrial, y = Mean)) +
  geom_point(aes(color = Answer), position=position_dodge(0.8))  +
  scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9")) + 
  stat_summary(fun.y = mean, geom = "point", shape = 18, size = 5,  aes(color = Answer) , position = position_dodge(0.8)) + 
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "pointrange", aes(color = Answer), position = position_dodge(0.8)) +
  xlab('Question type') + ylab('converted y axis') + ggtitle('Experimental all questions without 30 and 31 two by two Answer by subTrial') + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text   = element_text(size = 12),
        axis.title  = element_text(size = 14, face = "bold"),
        axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(legend.text = element_text(size = 14))  
ggsave('Experimental all questions without 30 and 31 two by two Answer by subTrial.pdf', width = 40, height = 28, units = "cm")

ggplot(data1[data1$qType == 1, ], aes(x = subTrial, y = Mean)) +
  geom_point(aes(color = Answer), position=position_dodge(0.8))  +
  scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9")) + 
  stat_summary(fun.y = mean, geom = "point", shape = 18, size = 5,  aes(color = Answer) , position = position_dodge(0.8)) + 
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "pointrange", aes(color = Answer), position = position_dodge(0.8)) +
  xlab('Question type') + ylab('converted y axis') + ggtitle('Experimental new questions without 30 and 31 two by two Answer by subTrial') + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text   = element_text(size = 12),
        axis.title  = element_text(size = 14, face = "bold"),
        axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(legend.text = element_text(size = 14))  
ggsave('Experimental new questions without 30 and 31 two by two Answer by subTrial.pdf', width = 40, height = 28, units = "cm")

ggplot(data1[data1$qType == -1, ], aes(x = subTrial, y = Mean)) +
  geom_point(aes(color = Answer), position=position_dodge(0.8))  +
  scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9")) + 
  stat_summary(fun.y = mean, geom = "point", shape = 18, size = 5,  aes(color = Answer) , position = position_dodge(0.8)) + 
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "pointrange", aes(color = Answer), position = position_dodge(0.8)) +
  xlab('Question type') + ylab('converted y axis') + ggtitle('Experimental old questions without 30 and 31 two by two Answer by subTrial') + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text   = element_text(size = 12),
        axis.title  = element_text(size = 14, face = "bold"),
        axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(legend.text = element_text(size = 14))  
ggsave('Experimental old questions without 30 and 31 two by two Answer by subTrial.pdf', width = 40, height = 28, units = "cm")


