
Pp <- 5
block <- 2
data <- Data
trial <- 7
dataPp <- data

fixingData <- function(data){
  setwd('D:\\Tom\\ROB23\\ET-Exp\\Data\\fixed data')
  
  #if(pass25SigPp == TRUE){
   # data <- data %>% filter(Block == 2 & TrialTime < 4000 & ID %in% pass25Sig)
  #}else{
  #  data <- data %>% filter(Block == 2 & TrialTime < 4000)
  #}
   
  Index <- 1                          # on if I want to write csv
  for (Pp in unique(data$ID)){
    dataPp <- data %>% filter(ID == Pp)
    if (2 %in% unique((dataPp$Block))){
      Ppdata <- c()                      # on if I want to write csv
      data1 <- dataPp %>% filter(Block == 1)
      data2 <- dataPp %>% filter(Block == 2)
      data3 <- dataPp %>% filter(Block > 2)
      for(trial in unique(data2$Trial.x)){
        dataTrial <- data2 %>% filter(Trial.x == trial)

        #Table <- table(dataTrial$TrialTime)
        #Double <- sum(as.numeric(Table == 2))
        #t0 <- dataTrial$TrialTime[1 : Double]
        #t1 <- dataTrial$TrialTime[(Double + 1) : length(dataTrial$TrialTime)]
        #t1 <- t1 + Double
        #t2 <- c(t0, t1)
        dataTrial$TrialTime <- c(0:(length(dataTrial$TrialTime) - 1))
        table(dataTrial$TrialTime)
        
        Ppdata <- rbind(Ppdata, dataTrial)  

        #newData <- rbind(newData, dataTrial)
      }
      data4 <- rbind(data1, Ppdata, data3)
      Name <- paste0(Index, ' ', 'fixed data.csv')     # on if I want to write csv
      write.csv(data4, Name)                          # on if I want to write csv
    }else{
      Name <- paste0(Index, ' ', 'fixed data.csv')
      write.csv(dataPp, Name)
    }
    Index <- Index + 1
  }
}

fixingData(Data)




t <- read_csv('D:\\Tom\\ROB23\\ET-Exp\\Data\\fixed data\\1 fixed data.csv')
table(t$Block)
table(Data$Block[Data$ID == 5])
t1 <- read_csv('D:\\Tom\\ROB23\\ET-Exp\\Data\\ET data\\EDF6.csv')

table(t1$Block)


g <- fixedData %>%
  mutate(gyNo  = ifelse(Answer == -1, gy, NA),
         gyYes = ifelse(Answer == 1, gy, NA),
         Answer = ifelse(Answer == 1, 'Yes', 'No'),
         qType = ifelse(qType == 1, "New", "Old"))

colorYes        <- 'blue'
colorNo         <- 'red' 

GraphData <- g %>%
  group_by(ID, TrialTime) %>%
  summarise(MeanYes = mean(gyYes, na.rm = TRUE),
            MeanNo  = mean(gyNo, na.rm = TRUE)) %>%
  mutate(Diff = MeanNo - MeanYes)



GraphData %>%
  ggplot(aes(x = TrialTime, y = Diff)) + 
  geom_line(aes(color = absDiff)) + 
  xlab("Time") + ylab("Difference") + ggtitle(graphTitel) +
  theme(panel.grid.major = element_line(color = 'grey')) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text   = element_text(size = 12),
        axis.title  = element_text(size = 14, face = "bold"),
        axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(legend.text = element_text(size = 14))



GraphData.1 <- GraphData %>%
  group_by(TrialTime) %>%
  summarise(meanDiff = mean(Diff, na.rm = TRUE),
            sdtDiff   = sd(Diff, na.rm = TRUE) / sqrt(length(unique(ID))),
            absDiff  = if_else(meanDiff > 0, 1, 0))


GraphData.1 %>%
  ggplot(aes(x = TrialTime, y = meanDiff)) + 
  geom_line(aes(color = absDiff)) + 
  xlab("Time") + ylab("Difference") + ggtitle(graphTitel) +
  theme(panel.grid.major = element_line(color = 'grey')) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text   = element_text(size = 12),
        axis.title  = element_text(size = 14, face = "bold"),
        axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(legend.text = element_text(size = 14))

graphName <- c('Diff with std only significat Pp after correcting 2.pdf')
graphTitel <- ('Diff with std only significat Pp after correcting 2')
GraphData.1 %>%
  filter(TrialTime > 1500 & TrialTime < 2300) %>%
  ggplot(aes(x = TrialTime, y = meanDiff)) + 
  geom_line(aes(color = absDiff), size = 1) + 
  geom_errorbar(aes(ymin = meanDiff - sdtDiff, ymax = meanDiff + sdtDiff), width = 0.05,
               position = position_dodge(0.2), color = "grey", alpha = 0.5) +
  xlab("Time") + ylab("Difference") + ggtitle(graphTitel) +
  theme(panel.grid.major = element_line(color = 'grey')) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text   = element_text(size = 12),
        axis.title  = element_text(size = 14, face = "bold"),
        axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(axis.line.y = element_line(color = "black", size = 1),
        axis.line.x = element_line(color = "black", size = 1)) +
  theme(legend.text = element_text(size = 14))
ggsave(graphName, width = 40, height = 28, units = "cm")



GraphData.2 <- g %>%
  group_by(ID, TrialTime) %>%
  summarise(MeanYes = mean(gyYes, na.rm = TRUE),
            MeanNo  = mean(gyNo, na.rm = TRUE)) %>%
  mutate(Diff = MeanNo - MeanYes,
         absDiff = ifelse(Diff > 0, 1, 0))

for (Pp in unique(GraphData.2$ID)){
  
  graphName <- paste0(Pp, ' ' ,'Diff after correcting 2.pdf')
  graphTitel <- paste0(Pp, ' ' ,'Diff  after correcting 2')
  
  GraphData.2 %>%
    filter(ID == Pp) %>%
    filter(TrialTime > 1500 & TrialTime < 2500) %>%
    ggplot(aes(x = TrialTime, y = Diff)) + 
    geom_line(aes(color = absDiff)) + 
    xlab("Time") + ylab("Difference") + ggtitle(graphTitel) +
    theme(panel.grid.major = element_line(color = 'grey')) + 
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text   = element_text(size = 12),
          axis.title  = element_text(size = 14, face = "bold"),
          axis.line.y = element_line(color = "black", size = 1),
          axis.line.x = element_line(color = "black", size = 1)) +
    theme(axis.line.y = element_line(color = "black", size = 1),
          axis.line.x = element_line(color = "black", size = 1)) +
    theme(legend.text = element_text(size = 14))
  ggsave(graphName, width = 40, height = 28, units = "cm")
  
}



