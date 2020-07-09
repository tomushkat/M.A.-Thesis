
B2 <- Data %>%
  filter(Block == 2) %>%
  filter(ID %in% onlyB2) %>%
  mutate(gyNo  = ifelse(Answer == -1, gy, NA),
         gyYes = ifelse(Answer == 1, gy, NA),
         Answer = ifelse(Answer == 1, 'Yes', 'No'))
         #qType = ifelse(qType == 1, "New", "Old"))

colorYes        <- 'blue'
colorNo         <- 'red' 

GraphData <- B2 %>%
  group_by(ID, TrialTime) %>%
  summarise(MeanYes = mean(gyYes, na.rm = TRUE),
            MeanNo  = mean(gyNo, na.rm = TRUE),
            SDYes   = sd(gyYes, na.rm = TRUE),
            SDNo    = sd(gyNo, na.rm =  TRUE)) %>%
  mutate(diffET = MeanYes - MeanNo,
         dffSD  = (SDYes + SDNo) / 2,
         Color = ifelse(diffET > 0, 1, 0))




for(i in unique(GraphData$ID)){
  
  graphTitel <- paste0("Experimental Pp ", i)
  GraphData %>%
    filter(ID == i & TrialTime > 1200 &  TrialTime < 3000) %>%
    mutate(Pahse = ifelse(TrialTime < EndQuestion, 'Question',
                          ifelse(TrialTime < endISI, 'ISI',
                                 ifelse(TrialTime < endTarget, 'Target',
                                        ifelse(TrialTime < endBM, 'BM', 'None'))))) %>%
    ggplot(aes(x = TrialTime, y = diffET)) +
    geom_line(aes(y = diffET)) + 
    geom_errorbar(aes(ymin = diffET - dffSD, ymax = diffET + dffSD), width = 0.1,
                  position = position_dodge(0.2), color = "red", alpha = 0.05) + 
    geom_line(y = 0, aes(color = Pahse), size = 2) + 
    xlab("Time in ms") + ylab("Difference ET Yes - NO") + ggtitle(graphTitel) +
    ylim(-1000, 800) + 
    Theme
  
  graphName <- paste0("Experimental Pp ", i, '.pdf')
  
  ggsave(graphName, width = 40, height = 28, units = "cm")
  
}


graphTitel <- paste0("Experimental all questions Pp ", Pp)
graphName <- paste0("Eexact experimental all questions Pp ", Pp, '.pdf')


GraphData %>%
  filter(ID == Pp & TrialTime > 1000 &  TrialTime < 2500) %>%
  mutate(Pahse = ifelse(TrialTime < EndQuestion, 'Question',
                        ifelse(TrialTime < endISI, 'ISI',
                               ifelse(TrialTime < endTarget, 'Target',
                                      ifelse(TrialTime < endBM, 'BM', 'None'))))) %>%
  ggplot(aes(x = TrialTime, y = diffET)) +
  geom_line(aes(color = Color)) + 
  ylim(-570, 570) + 
  geom_line(y = 0, aes(color = Pahse), size = 2) + 
  xlab("Time") + ylab("Difference ET Yes - NO") + ggtitle(graphTitel) +
  Theme



ggsave(graphName, width = 40, height = 28, units = "cm")












vGD <- GraphData %>%
  filter(TrialTime < 2500)
df <- unique(vGD$TrialTime)

for (i in unique(vGD$ID)){
  v <- vGD %>% filter(ID == i)
  v <- as.data.frame(v[, c(3:4)])
  idYes <- paste0('Yes', i)
  idNo <- paste0('No', i)
  dimnames(v)[[2]] <- c(idYes, idNo)
  df <- cbind(df, v)
}

dimnames(df)[[2]][1] <- c('Time')


ggBoth <- function(data = df, IDY, IDN, ID){
  
  colorYes        <- 'blue'
  colorNo         <- 'red' 
  
  graphTitel <- paste0("Experimental all questions Pp ", ID)
  fig <- data %>%
    ggplot(aes(x = Time, y = IDY)) + 
    geom_line(color = colorYes) +
    geom_line(aes(y = IDN), color = colorNo) +
    xlab("Time") + ylab("Hight") + ggtitle(graphTitel) +
    Theme
  print(fig)
}

graphTitel <- paste0("Experimental all questions Pp ", 5)
fig <- df %>%
  filter(Time < 3000) %>%
  ggplot(aes(x = Time, y = Yes5)) + 
  geom_line(color = colorYes) +
  geom_line(aes(y = No5), color = colorNo) +
  xlab("Time") + ylab("Hight") + ggtitle(graphTitel) +
  Theme
print(fig)

a <- 1
for (i in 2:(length(colnames(df)))){
  if (i %% 2 == 0){
    Y <- i 
    N <- Y + 1
    Pp <- unique(GraphData$ID)[a]
    a <- a + 1
    ggBoth(IDY = df[, Y], IDN = df[, N], ID = Pp)
    graphName <- paste0("Accurate Experimental all questions Pp ", Pp, ".pdf")
    ggsave(graphName, width = 40, height = 28, units = "cm")
  }
}

ggBoth(IDY = df$Yes51, IDN = df$No51, ID = 51)

graphTitel <- c("Experimental all questions No all Pp")
graphName <- c("Experimental all questions No all Pp.pdf")
df %>%
  ggplot(aes(x = Time, y = No5)) + 
  geom_line(color = 'lightblue1') +
  geom_line(aes(y = No6), color = 'chocolate') +
  geom_line(aes(y = No7), color = 'yellow4') +
  geom_line(aes(y = No12), color = 'lightsteelblue') +
  geom_line(aes(y = No14),color = 'dodgerblue') +
  geom_line(aes(y = No16), color = 'palevioletred2') +
  geom_line(aes(y = No17), color = 'steelblue') +
  geom_line(aes(y = No18), color = 'lightcyan2') +
  geom_line(aes(y = No19), color = 'tan1') +
  geom_line(aes(y = No21), color = 'tan4') +
  geom_line(aes(y = No22), color = 'cadetblue1') +
  geom_line(aes(y = No24), color = 'mediumpurple') +
  geom_line(aes(y = No25), color = 'orchid1') +
  geom_line(aes(y = No27), color = 'dimgray') +
  geom_line(aes(y = No28), color = 'snow4') +
  geom_line(aes(y = No29), color = 'seagreen1') +
  geom_line(aes(y = No36), color = 'palegreen3') +
  geom_line(aes(y = No37), color = 'deepskyblue1') +
  geom_line(aes(y = No39), color = 'mintcream') +
  geom_line(aes(y = No41), color = 'hotpink1') +
  geom_line(aes(y = No42), color = 'maroon') +
  geom_line(aes(y = No43), color = 'cornsilk2') +
  geom_line(aes(y = No44), color = 'tomato2') +
  geom_line(aes(y = No46), color = 'plum1') +
  geom_line(aes(y = No47), color = 'lavender') +
  geom_line(aes(y = No51), color = 'khaki') +
  xlab("Time") + ylab("Hight") + ggtitle(graphTitel) +
  Theme
ggsave(graphName, width = 40, height = 28, units = "cm")


graphTitel <- c("Experimental all questions No 25 sig Pp")
graphName <- c("Experimental all questions No 25 sig Pp.pdf")

df %>%
  ggplot(aes(x = Time, y = No5)) + 
  geom_line(color = 'lightblue1') +
  geom_line(aes(y = No6), color = 'chocolate') +
  geom_line(aes(y = No7), color = 'yellow4') +
  geom_line(aes(y = No16), color = 'palevioletred2') +
  geom_line(aes(y = No21), color = 'tan4') +
  geom_line(aes(y = No27), color = 'dimgray') +
  geom_line(aes(y = No44), color = 'tomato2') +
  xlab("Time") + ylab("Hight") + ggtitle(graphTitel) +
  Theme
ggsave(graphName, width = 40, height = 28, units = "cm")


graphTitel <- c("Experimental all questions No 25 num Pp")
graphName <- c("Experimental all questions No 25 num Pp.pdf")

df %>%
  ggplot(aes(x = Time, y = No5)) + 
  geom_line(color = 'lightblue1') +
  geom_line(aes(y = No28), color = 'snow4') +
  geom_line(aes(y = No36), color = 'palegreen3') +
  geom_line(aes(y = No41), color = 'hotpink1') +
  geom_line(aes(y = No46), color = 'plum1') +
  xlab("Time") + ylab("Hight") + ggtitle(graphTitel) +
  Theme
ggsave(graphName, width = 40, height = 28, units = "cm")
