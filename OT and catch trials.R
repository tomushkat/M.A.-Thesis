########################
#     OT               #
########################
E <- c(74, 24, 83, 23, 19, 17, 43, 91, 93, 28, 18, 21, 30, 78, 86, 77, 89, 27, 29, 84, 75, 95, 44, 85, 41, 20, 31, 25, 94, 81, 48, 80, 82, 76, 79, 47, 87, 90, 26, 96, 92, 88, 42, 22, 32, 46, 45, 73, 7, 9, 10, 6, 8)
E <- unique(E)
OTData <- subset(dataBehavioral, dataBehavioral$newBlock == 3)
OTData$OTLanguage  <- ifelse(as.numeric(OTData$QID) %in%  E, 'E', 'H')
OTData$newIsCorrect  <- ifelse((OTData$Response == "S" & OTData$OTLanguage == 'H') | (OTData$Response == "K" & OTData$OTLanguage == 'E'), 1,
                               ifelse((OTData$Response == "S" & OTData$OTLanguage == 'E') | (OTData$Response == "K" & OTData$OTLanguage == 'H'), 0, NA))





dataOT <- ddply(OTData, c("ID"), summarise,
                Mean = mean(newIsCorrect, na.rm = TRUE))
index <- 1
didSee <- c()
for (i in unique(OTData$ID)) {
  t1 <- binom.test(sum(na.omit(OTData$newIsCorrect[OTData$ID == i])), length(na.omit(OTData$newIsCorrect[OTData$ID == i])), 0.5)
  didSee[index] <- ifelse(t1$p.value < 0.05, "Awere", "notAwere")
  index <- index + 1
}
dataOT$didSee <- didSee
dataOT <- dataOT[-c(1:3), ] 


round(mean(dataOT$Mean), 2)
round(sd(dataOT$Mean), 2)

only16 <- OTData[OTData$ID == 50, ] 
only16$newIsCorrect <- ifelse(only16$newIsCorrect == 1, 0, 
                              ifelse( only16$newIsCorrect == 0, 1, NA))
binom.test(sum(na.omit(only16$newIsCorrect)), length(na.omit(only16$newIsCorrect)), 0.5)
#ggplot(dataOT, aes(x = as.factor(ID), y = Mean)) +
#  geom_point(aes(color = didSee)) + 
#  ylab("Objective test Accuracy") + xlab("ID") + ggtitle("Objective test")

########################
#   End OT             #
########################


########################
#     Catch            #
########################

catchData <- subset(dataBehavioral, dataBehavioral$newBlock == 4)
catchData$OTLanguage <- ifelse(as.numeric(catchData$QID) %in%  E, 'E', 'H')
catchData$newCorrect2 <- ifelse((catchData$Response == "S" & catchData$OTLanguage == 'H') | (catchData$Response == "K" & catchData$OTLanguage == 'E'), 1,
                                ifelse((catchData$Response == "S" & catchData$OTLanguage == 'E') | (catchData$Response == "K" & catchData$OTLanguage == 'H'), 0, NA))


dataCatch <- ddply(catchData, c("ID"), summarise,
                   Mean = mean(newCorrect2, na.rm = TRUE))
dataCatch$passCatch <- as.factor(ifelse(dataCatch$Mean >= 0.8, "Yes", "No"))
dataCatch <- dataCatch[-c(1:3), ] 

dataCatch$didSee <- dataOT$Mean
dataCatch$didSeeDict <- dataOT$didSee

#ggplot(dataCatch, aes(x = as.factor(ID), y = Mean)) +
#  geom_point(aes(color = passCatch)) + 
#  xlab("ID") + ylab('Catch tails accuracy') + ggtitle('Catch tails')

dataCatch <- dataCatch[complete.cases(dataCatch), ]

ggplot(dataCatch[dataCatch$ID > 4, ], aes(x = didSee , y = Mean)) +
  geom_point(aes(color = didSeeDict)) + 
  xlab("Abjective test accuracy") + ylab('Catch tails accuracy') + ggtitle("Objective test + catch trials") +
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
                   segment.color = 'grey50')
ggsave('Objective test by catch trials.pdf', width = 40, height = 30, units = "cm")

########################
#   End Catch          #
########################

only50 <- dataBehavioral[dataBehavioral$ID == 50, ]
only50$newIsCorrect <- ifelse(only50$newIsCorrect == 1, 0, ifelse(only50$newIsCorrect == 0, 1, NA))
only50 <- only50[only50$newBlock == 3, ]
binom.test(sum(na.omit(only50$newIsCorrect)), length(na.omit(only50$newIsCorrect)), 0.5)

only16 <- dataBehavioral[dataBehavioral$ID == 16, ]
only16$newIsCorrect <- ifelse(only16$newIsCorrect == 1, 0, ifelse(only16$newIsCorrect == 0, 1, NA))
only16 <- only16[only16$newBlock == 3, ]
binom.test(sum(na.omit(only16$newIsCorrect)), length(na.omit(only16$newIsCorrect)), 0.5)