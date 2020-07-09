



MOVING <- function(vector, n_neighbors){
  n <- length(vector)
  newVector <- c(rep(vector[1], n_neighbors), vector, rep(vector[length(vector)], n_neighbors))
  L = c()
  for (timeTrial in c(n_neighbors:(n + n_neighbors - 1))){
    toSmooth <- newVector[c(timeTrial + 1 - n_neighbors):c(timeTrial + 1 + n_neighbors)]
    if(sum(as.numeric(is.na(toSmooth))) < (n_neighbors * 2 + 1)){
      Mean <- mean(toSmooth, na.rm = TRUE)
      L <- c(L, Mean)
      }else{
        Mean <- NA
        L <- c(L, Mean)
    }
  }
  return (L)
}



newData <- Data %>% filter(2 %in% unique(Data$Block))
newData$correctdGY <- NA
newData$correctdGX <- NA
Index <- 0
numberOfNeibors <- 5

setwd('D:\\Tom\\ROB23\\ET-Exp\\Data\\smoothed')
for(Pp in unique(newData$ID)){
  Index <- Index + 1
  b1 <- newData %>% filter(ID == Pp & Block == 1)
  b2 <- newData %>% filter(ID == Pp & Block == 2)
  for (trial in unique(b1$Trial.x)){
    Trial <- b1 %>% filter(Trial.x == trial)
    b1$correctdGY[b1$Trial.x == trial] <- MOVING(vector = Trial$gy, numberOfNeibors)
    b1$correctdGX[b1$Trial.x == trial] <- MOVING(vector = Trial$gx, numberOfNeibors)
    b1$TrialTime[b1$Trial.x == trial] <- c(0:(length(b1$TrialTime[b1$Trial.x == trial]) - 1))
  }
  for (trial in unique(b2$Trial.x)){
    Trial <- b2 %>% filter(Trial.x == trial)
    b2$correctdGY[b2$Trial.x == trial] <- MOVING(Trial$gy, numberOfNeibors)
    b2$correctdGX[b2$Trial.x == trial] <- MOVING(Trial$gx, numberOfNeibors)
    b2$TrialTime[b2$Trial.x == trial] <- c(0:(length(b2$TrialTime[b2$Trial.x == trial]) - 1))
  }
  data <- rbind(b1, b2)
  Name <- paste0('Smoothed and fixed Pp ', Index, '.csv')
  write.csv(data, Name)
}
