#B2 <- subset(Data, Data$Block == 2)
pass25Numeric <- c(5, 6, 7, 11, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22, 24, 25, 26, 27, 29, 37, 39, 42, 43, 44, 47, 48, 51)
pass25Sig <- c(6, 7, 16, 19, 21, 27, 44)
#B2 <- Data[Data$Block == 2 & Data$ID %in% acc$ID[acc$Binom == 1], ]  # pass significanly all conditions together
#B2 <- Data[Data$Block == 2 & Data$ID %in% pass25Numeric, ]           # pass numeric aseperate conditions
#B2 <- Data[Data$Block == 2 & Data$ID %in% pass25Sig, ]               # pass significantly aseperate conditions

#data <-  ddply(B2[B2$TrialTime == 2250, ], c('ID', 'Trial', 'Answer', 'qType', 'qID'), summarise,
#                      Mean = mean(gy, na.rm = TRUE))

#summary(lmer(Mean ~ Answer * qType + Trial + (1 + Answer * qType + Trial | ID) + (1 + 1 | qID), REML = FALSE, data = data))                                         


B2$paNo <- ifelse(B2$Answer == -1, B2$pa, NA)
B2$paYes <- ifelse(B2$Answer == 1, B2$pa, NA)

Yes             <- NULL
colorYes        <- 'blue'
No              <- NULL
colorNo         <- 'red' 


# graphs for group



GraphData <- ddply(B2, c("ID", 'TrialTime'), summarise,
                   MeanYes = mean(paYes, na.rm = TRUE),
                   MeanNo  = mean(paNo, na.rm = TRUE))


GraphData.1 <- ddply(GraphData, c('TrialTime'), summarise,
                     Yes   = mean(MeanYes, na.rm = TRUE),
                     SDYes = sd(MeanYes, na.rm = TRUE),
                     No    = mean(MeanNo, na.rm = TRUE),
                     SDNo  = sd(MeanNo, na.rm = TRUE))

graphTitel <- c("Pupil size Experimental all questions only participants above sig 25% accuracy seperatly")
graphName <- c("Pupil size Experimental all questions both answers only participants above sig 0.25 accuracy seperatly.pdf") 
ggplot(GraphData.1, aes(x = TrialTime, y = Yes)) +
  geom_point(color = colorYes) +
  geom_point(aes(y = No), color = colorNo) +
  xlab("Time") + ylab("Pupil size") + ggtitle(graphTitel) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text   = element_text(size = 12),
        axis.title  = element_text(size = 14, face = "bold"),
        axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(legend.text = element_text(size = 14))
ggsave(graphName, width = 40, height = 28, units = "cm")

graphTitel <- c("Pupil size Experimental all questions only participants above sig 25% accuracy seperatlyl < 2500ms")
graphName <- c("Pupil size Experimental all questions both answers only participants above sig 0.25 accuracy seperatly less than 2500ms.pdf")
ggplot(GraphData.1[GraphData.1$TrialTime < 2500, ], aes(x = TrialTime, y = Yes)) + 
  geom_point(color = colorYes) +
  geom_point(aes(y = No), color = colorNo) +
  xlab("Time") + ylab("Pupil size") + ggtitle(graphTitel) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text   = element_text(size = 12),
        axis.title  = element_text(size = 14, face = "bold"),
        axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(legend.text = element_text(size = 14))
ggsave(graphName, width = 40, height = 28, units = "cm")

graphTitel <- c("Pupil size Experimental all questions only participants above sig 25% accuracy seperatly")
graphName <- c("Pupil size Experimental all questions both answers only participants above sig 0.25  accuracy 2000 to 2300 seperatly.pdf")

GraphData.1$SDYes <- GraphData.1$SDYes / sqrt(length(unique(GraphData$ID)))  
GraphData.1$SDNo <- GraphData.1$SDNo / sqrt(length(unique(GraphData$ID)))  

ggplot(GraphData.1[GraphData.1$TrialTime >= 2000 & GraphData.1$TrialTime <= 2300, ], aes(x = TrialTime, y = Yes))  +
  #geom_errorbar(aes(ymin = Yes - SDYes, ymax = Yes + SDYes), width = 0.1,
  #              position = position_dodge(0.2), color = "blue") +
  geom_point(color = colorYes) +
  #geom_errorbar(aes(ymin = No - SDNo, ymax = No + SDNo), width = 0.1,
  #              position = position_dodge(0.2), color = "pink") +
  geom_point(aes(y = No), color = colorNo) +
  xlab("Time") + ylab("Pupil size") + ggtitle(graphTitel) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text   = element_text(size = 12),
        axis.title  = element_text(size = 14, face = "bold"),
        axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(legend.text = element_text(size = 14))
ggsave(graphName, width = 40, height = 28, units = "cm")


B1 <- Data %>%
  filter(Data$Block == 1 & Data$ID %in% unique(B2$ID))

B1$paNo <- ifelse(B1$Answer == -1, B1$pa, NA)
B1$paYes <- ifelse(B1$Answer == 1, B1$pa, NA)

Yes             <- NULL
colorYes        <- 'blue'
No              <- NULL
colorNo         <- 'red' 

# graphs for group



GraphDataB1 <- ddply(B1, c("ID", 'TrialTime'), summarise,
                     MeanYes = mean(paYes, na.rm = TRUE),
                     MeanNo  = mean(paNo, na.rm = TRUE))


GraphDataB1.1 <- ddply(GraphDataB1, c('TrialTime'), summarise,
                       Yes   = mean(MeanYes, na.rm = TRUE),
                       SDYes = sd(MeanYes, na.rm = TRUE),
                       No    = mean(MeanNo, na.rm = TRUE),
                       SDNo  = sd(MeanNo, na.rm = TRUE))

graphTitel <- c("Pupil size Induction all questions only participants who past induction")
graphName <- c("Pupil size Induction all questions both answers only participants who past induction.pdf") 
ggplot(GraphData.1, aes(x = TrialTime, y = Yes)) +
  geom_point(color = colorYes) +
  geom_point(aes(y = No), color = colorNo) +
  xlab("Time") + ylab("Pupil size") + ggtitle(graphTitel) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text   = element_text(size = 12),
        axis.title  = element_text(size = 14, face = "bold"),
        axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(legend.text = element_text(size = 14))
ggsave(graphName, width = 40, height = 28, units = "cm")

graphTitel <- c("Pupil size Induction all questions only participants who past induction < 2500ms")
graphName <- c("Pupil size Induction all questions both answers only participants who past induction seperatly less than 2500ms.pdf")
ggplot(GraphData.1[GraphData.1$TrialTime < 2500, ], aes(x = TrialTime, y = Yes)) + 
  geom_point(color = colorYes) +
  geom_point(aes(y = No), color = colorNo) +
  xlab("Time") + ylab("Pupil size") + ggtitle(graphTitel) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text   = element_text(size = 12),
        axis.title  = element_text(size = 14, face = "bold"),
        axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(legend.text = element_text(size = 14))
ggsave(graphName, width = 40, height = 28, units = "cm")

graphTitel <- c("Pupil size Induction all questions only participants who past induction")
graphName <- c("Pupil size Induction all questions both answers only participants who past induction 2000 to 2300.pdf")

GraphData.1$SDYes <- GraphData.1$SDYes / sqrt(length(unique(GraphData$ID)))  
GraphData.1$SDNo <- GraphData.1$SDNo / sqrt(length(unique(GraphData$ID)))  

ggplot(GraphData.1[GraphData.1$TrialTime >= 2000 & GraphData.1$TrialTime <= 2300, ], aes(x = TrialTime, y = Yes))  +
  #geom_errorbar(aes(ymin = Yes - SDYes, ymax = Yes + SDYes), width = 0.1,
  #              position = position_dodge(0.2), color = "blue") +
  geom_point(color = colorYes) +
  #geom_errorbar(aes(ymin = No - SDNo, ymax = No + SDNo), width = 0.1,
  #              position = position_dodge(0.2), color = "pink") +
  geom_point(aes(y = No), color = colorNo) +
  xlab("Time") + ylab("Pupil size") + ggtitle(graphTitel) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text   = element_text(size = 12),
        axis.title  = element_text(size = 14, face = "bold"),
        axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(legend.text = element_text(size = 14))
ggsave(graphName, width = 40, height = 28, units = "cm")



colorExpYes     <- 'blue'
colorIndYes     <- 'lightblue' 
colorExpNo     <- 'red'
colorIndNo     <- 'pink' 

g12Data <- Data[Data$ID %in% pass25Sig, ]               # pass significantly aseperate conditions
g12Data$paNo <- ifelse(g12Data$Answer == -1, g12Data$pa, NA)
g12Data$paYes <- ifelse(g12Data$Answer == 1, g12Data$pa, NA)

GraphDataB12 <- ddply(g12Data, c("ID", 'Block', 'TrialTime'), summarise,
                      MeanYes = mean(paYes, na.rm = TRUE),
                      MeanNo  = mean(paNo, na.rm = TRUE))


GraphDataB12.1 <- ddply(GraphDataB12, c('Block', 'TrialTime'), summarise,
                        Yes   = mean(MeanYes, na.rm = TRUE),
                        SDYes = sd(MeanYes, na.rm = TRUE),
                        No    = mean(MeanNo, na.rm = TRUE),
                        SDNo  = sd(MeanNo, na.rm = TRUE))

d1 <- GraphDataB12.1 %>% filter(Block == 1)
d2 <- GraphDataB12.1 %>% filter(Block == 2)
d2$TrialTime <- NULL
d2$Block <- NULL

dimnames(d2)[[2]] <- c('yesExp', 'yesSDExp', 'noExp', 'noSDExp')
GraphDataB12.1 <- cbind(d1, d2)

graphTitel <- c("Pupil size Ind vs Exp all questions Yes only 25 sig")
graphName <- c("Pupil size Ind vs Exp all questions Yes only 25 sig.pdf") 
ggplot(GraphDataB12.1, aes(x = TrialTime, y = Yes)) +
  geom_point(color = colorIndYes) +
  geom_point(aes(y = yesExp), color = colorExpYes) +
  xlab("Time") + ylab("Pupil size") + ggtitle(graphTitel) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text   = element_text(size = 12),
        axis.title  = element_text(size = 14, face = "bold"),
        axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(legend.text = element_text(size = 14))
ggsave(graphName, width = 40, height = 28, units = "cm")

graphTitel <- c("Pupil size Ind vs Exp all questions Yes only 25 sig")
graphName <- c("Pupil size Ind vs Exp all questions Yes only 25 sig less than 2500 Trial time.pdf") 
ggplot(GraphDataB12.1[GraphDataB12.1$TrialTime < 2500, ], aes(x = TrialTime, y = Yes)) +
  geom_point(color = colorIndYes) +
  geom_point(aes(y = yesExp), color = colorExpYes) +
  xlab("Time") + ylab("Pupil size") + ggtitle(graphTitel) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text   = element_text(size = 12),
        axis.title  = element_text(size = 14, face = "bold"),
        axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(legend.text = element_text(size = 14))
ggsave(graphName, width = 40, height = 28, units = "cm")

graphTitel <- c("Pupil size Ind vs Exp all questions No only 25 sig")
graphName <- c("Pupil size Ind vs Exp all questions No only 25 sig.pdf") 
ggplot(GraphDataB12.1, aes(x = TrialTime, y = No)) +
  geom_point(color = colorIndNo) +
  geom_point(aes(y = noExp), color = colorExpNo) +
  xlab("Time") + ylab("Pupil size") + ggtitle(graphTitel) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text   = element_text(size = 12),
        axis.title  = element_text(size = 14, face = "bold"),
        axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(legend.text = element_text(size = 14))
ggsave(graphName, width = 40, height = 28, units = "cm")

graphTitel <- c("Pupil size Ind vs Exp all questions No only 25 sig")
graphName <- c("Pupil size Ind vs Exp all questions No only 25 sig less than 2500 Trial time.pdf") 
ggplot(GraphDataB12.1[GraphDataB12.1$TrialTime < 2500, ], aes(x = TrialTime, y = No)) +
  geom_point(color = colorIndNo) +
  geom_point(aes(y = noExp), color = colorExpNo) +
  xlab("Time") + ylab("Pupil size") + ggtitle(graphTitel) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text   = element_text(size = 12),
        axis.title  = element_text(size = 14, face = "bold"),
        axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(legend.text = element_text(size = 14))
ggsave(graphName, width = 40, height = 28, units = "cm")

