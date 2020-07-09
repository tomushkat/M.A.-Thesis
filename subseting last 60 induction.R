dataPp <- c()
for (Pp in unique(behavioralData$ID)){
  B1 <- behavioralData %>% filter(ID == Pp & Block == 1)
  B2 <- behavioralData %>% filter(ID == Pp & Block > 1)
  trialsVector <- unique(B1$Trial)
  if (length(trialsVector) > 60){
    trialsVector <- trialsVector[(length(trialsVector) - 59):length(trialsVector)]
    B1 <- B1 %>% filter(Trial %in% trialsVector)
    dataPp <- rbind(dataPp, B1, B2)
  }else{
    dataPp <- rbind(dataPp, B1, B2)
  }
  
}

behavioralData <- dataPp



dataPp <- c()
for (Pp in unique(Data$ID)){
  B1 <- Data %>% filter(ID == Pp & Block == 1)
  B2 <- Data %>% filter(ID == Pp & Block > 1)
  trialsVector <- unique(B1$Trial.x)
  if (length(trialsVector) > 60){
    trialsVector <- trialsVector[(length(trialsVector) - 59):length(trialsVector)]
    B1 <- B1 %>% filter(Trial.x %in% trialsVector)
    dataPp <- rbind(dataPp, B1, B2)
  }else{
    dataPp <- rbind(dataPp, B1, B2)
  }
  
}

Data <- dataPp
