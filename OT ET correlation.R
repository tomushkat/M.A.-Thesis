B2 <- Data %>%
  filter(Block == 2) %>%
  mutate(gyNo  = ifelse(Answer == -1, gy, NA),
         gyYes = ifelse(Answer == 1, gy, NA),
         Answer = ifelse(Answer == 1, 'Yes', 'No'),
         qType = ifelse(qType == 1, "New", "Old"))


ET1 <- B2 %>%
  filter(TrialTime >= minCriticTime & TrialTime <= maxCriticTime) %>%
  group_by(ID, TrialTime) %>%
  summarise(MeanYes = mean(gyYes, na.rm = TRUE),
            MeanNo  = mean(gyNo, na.rm = TRUE))

ET1.1 <- ET1 %>%
  group_by(ID) %>%
  summarise(Yes   = mean(MeanYes, na.rm = TRUE),
            No    = mean(MeanNo, na.rm = TRUE))

ET1.2 <- ET1.1 %>%
  mutate(Diff = Yes - No)

length(ET1.2$ID)

########################
#     OT               #
########################

setwd('D:\\Tom\\ROB23\\ET-Exp\\Data\\Behavioral data')

subjectNumber <- 51
subjectName   <- c()
for (i in 1:subjectNumber){
  subjectName <- c(subjectName, paste0("Log", i, ".csv", sep = ''))}
dataBehavioral <- do.call(rbind, lapply(subjectName, function(x)
  read.csv(x, stringsAsFactors = FALSE)))

E <- c(74, 24, 83, 23, 19, 17, 43, 91, 93, 28, 18, 21, 30, 78, 86, 77, 89, 27, 29, 84, 75, 95, 44, 85, 41, 20, 31, 25, 94, 81, 48, 80, 82, 76, 79, 47, 87, 90, 26, 96, 92, 88, 42, 22, 32, 46, 45, 73, 7, 9, 10, 6, 8)
E <- unique(E)
Badpp <- c(40, 32, 9, 13, 33, 38, 8, 45, 35, 34, 1, 2, 3, 4, 23, 10, 49, 50)
OTData <- dataBehavioral %>%
  filter(Block == 4) %>%
  filter(! ID %in% Badpp) %>%
  mutate(OTLanguage = ifelse(as.numeric(QID) %in%  E, 'E', 'H'),
         newIsCorrect = ifelse((Response == "S" & OTLanguage == 'H') | (Response == "K" & OTLanguage == 'E'), 1,
                               ifelse((Response == "S" & OTLanguage == 'E') | (Response == "K" & OTLanguage == 'H'), 0, NA)))
  
dataOT <- OTData %>%
  group_by(ID) %>%
  summarise(Mean = mean(newIsCorrect, na.rm = TRUE))





dataTest <- merge(ET1.2, dataOT, by = 'ID')
dataTest <- dataTest %>%
  mutate(scaledDiff = scale(Diff),
         scaledMean = scale(Mean),
         scaledOTcorrected = scale(Mean))

length(dataTest$ID)

shapiro.test(dataTest$Diff)
shapiro.test(dataTest$Mean)
Cor <- cor.test(dataTest$Diff, dataTest$Mean)
p <- round(Cor$p.value, 2)
r = round(Cor$estimate, 2)
Tilte <- paste0('r  =  ', r, ',  p  =  ', p)

dataTest %>%
  ggplot(aes(x = Diff, y = Mean)) +
  geom_point() +
  geom_smooth(method = 'lm', se = TRUE) + 
 xlab('Eye Yes - No') + ylab('OT accuracy') + 
  Theme +  ggtitle(Tilte) + ylim(0, 1) 
  Lbls
ggsave('OT ET corr.pdf',  width = 40, height = 28, units = "cm")
