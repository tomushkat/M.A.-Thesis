


chackY <- function(data, block){
  numberof <- c()
  Min1.1 <- c()
  for (i in unique(data$ID)){
    PpData <- data %>% filter(ID == i & Block == block)
    Count <- 0
    Min1 <- c()
    for (j in unique(PpData$Trial.x)){
      Trial <- PpData %>% filter(Trial.x == j)
      if(min(na.omit(Trial$gy)) < 0){
        Count <- Count + 1
        Min1 <- c(Min1, min(na.omit(Trial$gy))) 
      }
    }
    numberof <- c(numberof, Count)
    Minnnn <- min(na.omit(Min1))
    Min1.1 <- c(Min1.1, Minnnn)
  }
  L <- list(numberof, Min1.1)
  return(L)
}


chackx <- function(data, block){
  numberof <- c()
  Min1.1 <- c()
  for (i in unique(data$ID)){
    PpData <- data %>% filter(ID == i & Block == block)
    Count <- 0
    Min1 <- c()
    for (j in unique(PpData$Trial.x)){
      Trial <- PpData %>% filter(Trial.x == j)
      if(min(na.omit(Trial$gx)) < 0){
        Count <- Count + 1
        Min1 <- c(Min1, min(na.omit(Trial$gx))) 
      }
    }
    numberof <- c(numberof, Count)
    Minnnn <- min(na.omit(Min1))
    Min1.1 <- c(Min1.1, Minnnn)
  }
  L <- list(numberof, Min1.1)
  return(L)
}


yuc

b1y <- chackY(Data, 1)

b2y <- chackY(Data, 2)

b1x <- chackx(Data, 1)

b2x <- chackx(Data, 2)
