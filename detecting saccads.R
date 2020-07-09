

  

doSaccade <- function(data, startTimeSaccade, endTimeSaccade, block, startTimeTrial, endTimeTrial){
  
  # detect sacade for all Pp in specific block and specicif trail time
  
  #data <- Data
  #startTimeSaccade <- 1000
  #endTimeSaccade <- 2500
  #block <- 1
  #startTimeTrial <- 2150
  #endTimeTrial <- 2300
  #Pp <- 6
  
  summarisedData <- data %>%
    filter(Block == block & ID %in% onlyB2) %>%
    filter(TrialTime >= startTimeTrial & TrialTime <= endTimeTrial) %>%
    group_by(ID, Trial.x, Answer, qType, newIsCorrect) %>%
    summarise(Meangy = mean(gy, na.rm = TRUE))
  
  
  dimnames(summarisedData)[[2]] <- c('ID', 'trial', 'Answer', 'qType', 'isCorrect', 'meanEye')
  
  ETdata <- data %>%
    filter(Block == block) %>%
    filter(TrialTime >= startTimeSaccade & TrialTime <= endTimeSaccade)
    
  finalData <- c()  
  for (Pp in unique(ETdata$ID)){
    PpData <- summarisedData %>% filter(ID == Pp)
    saccadeData.1 <- ETdata %>%
      filter(ID == Pp) 
    
    saccadeData.1 <- saccadeData.1[, c('TrialTime', 'Trial.x', 'correctdGX', 'correctdGY')]  
    dimnames(saccadeData.1)[[2]] <- c('time', 'trial', 'x', 'y')
    Saccades <- detect.fixations(saccadeData.1, smooth.coordinates = TRUE, smooth.saccades = TRUE, lambda = 15)
    PpData <- merge(Saccades, PpData, by = 'trial')
    finalData <- rbind(finalData, PpData)
  }
  return(finalData)  
  }


summaryFixation <- function(Fixation_Data = fixaitionData, ID){
  subfixationData <- Fixation_Data[Fixation_Data$ID == ID, c('trial', 'start', 'end', 'x', 'y', 'sd.x', "sd.y", "peak.vx", "peak.vy", "dur")]
  subfixationData$ID <- NULL
  fixationsSummary <- calculate.summary(subfixationData)
  
  return(fixationsSummary)
}
# do summary for saccade in for specific ID and trial

singelData <- function(dataFull, dataFix, id, Trial, testedBlock){
  # takes ID, Trial and block 
  # extract the data from Data and fixaitionData
  # return data before the target and the fixsations before the target
  
  
  
  
  firstPp <- id             # the number number
  first.trial <- Trial    # the trial number
  
  first.trial.samples <- dataFull %>%    # Data
    filter(ID %in% onlyB2) %>%
    filter(ID == firstPp & Block == testedBlock & Trial.x == first.trial)
  
  first.trial.fixations <- dataFix %>%
    filter(ID %in% onlyB2) %>%
    filter(ID == firstPp & trial == first.trial)
  
  first.trial.fixations$beforTarget <- NA
  for (i in c(1:length(row.names(first.trial.fixations)))){
    if(first.trial.fixations$start[i] < 2200){
      first.trial.fixations$beforTarget[i] <- i
    }else{
      first.trial.fixations$beforTarget[i] <- 0
    }
  }
  first.trial.fixations <- first.trial.fixations %>% filter(beforTarget > 0)
  L <- list(first.trial.samples, first.trial.fixations)
  return(L)
}

testedBlock <- 2
Pp <- 12
trial.y <- 29

fixaitionData <- doSaccade(data = Data, startTimeSaccade = 0, endTimeSaccade = 2500, block = testedBlock, startTimeTrial = 2200, endTimeTrial = 2300)
#dataSacade <- someSaccade(data = fixaitionData, endFinalSaccade = 2500)



doGraphTlast <- function(toGraph = tograph){
  Title <- paste0('Participant ', Pp, ', trial ', trial.y, ', Answer = ',  tograph[1, c('Answer')])
  Xlab <- c('Time in ms')
  Ylab <- c('Converted hight')
  
  #testedBlock <- 1
  #Pp <- 6
  #trial.y <- 29
  
  
  if(max(toGraph$beforTarget) >= 2){
    Data %>%
      filter(ID == Pp & Trial.x == trial.y & Block == testedBlock & TrialTime < 4000) %>%
      mutate(beforTarget = ifelse(TrialTime >= toGraph$start[max(toGraph$beforTarget) - 1] & TrialTime <= toGraph$end[max(toGraph$beforTarget) - 1], toGraph$y[max(toGraph$beforTarget) - 1],
                          ifelse(TrialTime >= toGraph$start[max(toGraph$beforTarget)] & TrialTime <= toGraph$end[max(toGraph$beforTarget)], toGraph$y[max(toGraph$beforTarget)], NA))) %>%
      ggplot(aes(x = TrialTime, y = correctdGY)) + 
      geom_line() + 
      geom_point(aes(y = beforTarget), color = 'red', size = 1) +
      Theme + 
      ggtitle(Title) + ylab(Ylab) + xlab(Xlab)
  }else{
    Data %>%
      filter(ID == Pp & Trial.x == theTtial & Block == testedBlock & TrialTime < 4000) %>%
      mutate(beforTarget = ifelse(TrialTime <= toGraph$end[max(toGraph$beforTarget)], toGraph$y[max(toGraph$beforTarget)], NA)) %>%
      ggplot(aes(x = TrialTime, y = correctdGY)) + 
      geom_line() + 
      geom_point(aes(y = beforTarget), color = 'red', size = 1) +
      Theme + 
      ggtitle(Title) + ylab(Ylab) + xlab(Xlab)
  }
  
}

tep <- singelData(Data, fixaitionData, Pp, trial.y, testedBlock)
with(as.data.frame(tep[1]), plot(gx, gy, pch = 20, cex = 0.02, col = "black"))
with(as.data.frame(tep[2]), points(x, y, pch = 19, cex = 1 + sqrt(dur / 10000), col = 'red', bg = 22))


tograph <- as.data.frame(tep[2])
doGraphTlast()

#PpFixations <- summaryFixation(fixaitionData, ID = Pp)
#PpFixations



##### First saccade after question
firstSaccadeAfterQeustion <- function(data = fixaitionData, startQuestion = timeblanckBeforeQuestion){
  
  # remove the saccades that starts before the apearence of the question
  
  fixaitionData.1 <- data %>%
    filter(start > startQuestion)
  
  return(fixaitionData.1)
  
}
firstSaccadeDirection <- function(data = fixaitionData.1){
  
  # return full data with only the first fixation after question and with the direction of the fixation
  
  fixaitionData_firstAfterQ <- c()
  for(i in unique(data$ID)){
    dataPp <- data %>% filter(ID == i)
    for(j in unique(dataPp$trial)){
      dataTrial <- dataPp %>% filter(trial == j)
      fixaitionData_firstAfterQ <- rbind(fixaitionData_firstAfterQ, dataTrial[1, ])
    }
  }
  
  fixaitionData_firstAfterQ <- fixaitionData_firstAfterQ %>%
    mutate(rightDirection = ifelse(is.na(y), NA,
                                   ifelse((Answer == 1 & y < 540) | (Answer == -1 & y > 540), 1,
                                          ifelse((Answer == 1 & y > 540) | (Answer == -1 & y < 540), 0, NA))))
  return(fixaitionData_firstAfterQ)
  
}
sumByID <- function(data = fixaitionData_firstAfterQ){
  
  # takes firstSaccadeDirection data and returns summarized data per Pp for the ratio to the
  #fixations who were in the right or wrong direction
  # ratioKnow is how much hte value is robust - using how many NA I have
  
  fixaitionData_firstAfterQ.1 <- data %>%
    group_by(ID) %>%
    summarise(right = sum(na.omit(rightDirection)),
              wrong = length(na.omit(rightDirection)) - right, 
              Diff = right - wrong, 
              ratioRight = round(sum(na.omit(rightDirection)) / length(na.omit(rightDirection)), 2),
              ratioWrong = round(1 - ratioRight, 2),
              diffRatios = ratioRight - ratioWrong, 
              dontKnow = length(rightDirection) - length(na.omit(rightDirection)),
              ratioKnow = 1 - round(dontKnow / length(rightDirection), 2),
              meanIscorrect = mean(isCorrect, na.rm = TRUE))
  return(fixaitionData_firstAfterQ.1)
  
  
}


fixaitionData.1 <- firstSaccadeAfterQeustion(data = fixaitionData, startQuestion = timeblanckBeforeQuestion)
fixaitionData_firstAfterQ <- firstSaccadeDirection(fixaitionData.1)
fixaitionData_firstAfterQ.1 <- sumByID(fixaitionData_firstAfterQ)


shapiro.test(fixaitionData_firstAfterQ.1$ratioRight)
shapiro.test(fixaitionData_firstAfterQ.1$meanIscorrect)

cor <- cor.test(fixaitionData_firstAfterQ.1$ratioRight, fixaitionData_firstAfterQ.1$meanIscorrect, method = 'spearman')

p <- ifelse(cor$p.value < .01, '< .01', round(cor$p.value, 2))
r <- round(cor$estimate, 2)
Rsquared <- round(r ^ 2, 2)
Tilte <- paste0('Right direction for first saccade: rho  =  ', r, ',  p  =  ', p, ',  R-squared = ', Rsquared)


fixaitionData_firstAfterQ.1 %>%
  ggplot(aes(x = ratioRight, y = meanIscorrect)) + 
  geom_point(aes(color = ratioKnow, size = ratioKnow)) + 
  geom_smooth(method = 'lm', se = TRUE) + 
  xlim(0, 1) + ylim(0, 1) + 
  xlab('Right direction ratio') + ylab('Accuracy') + ggtitle(Tilte) +
  Theme + Lbls  


##### Second saccade after question
secondSaccadeAfterQeustion <- function(data = fixaitionData, startQuestion = timeblanckBeforeQuestion){
  
  # remove the saccades that starts before the apearence of the question
  
  fixaitionData.1 <- data %>%
    filter(start > startQuestion)
  
  return(fixaitionData.1)
  
}
secondSaccadeDirection <- function(data = fixaitionData.1){
  
  # return full data with only the first fixation after question and with the direction of the fixation
  
  #i <- 12
  #j <- 29
  
  fixaitionData_firstAfterQ <- c()
  for(i in unique(data$ID)){
    dataPp <- data %>% filter(ID == i)
    for(j in unique(dataPp$trial)){
      dataTrial <- dataPp %>% filter(trial == j)
      fixaitionData_firstAfterQ <- rbind(fixaitionData_firstAfterQ, dataTrial[2, ])
    }
  }
  
  fixaitionData_firstAfterQ <- fixaitionData_firstAfterQ %>%
    mutate(rightDirection = ifelse(is.na(y), NA,
                                   ifelse((Answer == 1 & y < 540) | (Answer == -1 & y > 540), 1,
                                          ifelse((Answer == 1 & y > 540) | (Answer == -1 & y < 540), 0, NA))))
  return(fixaitionData_firstAfterQ)
  
}
sumByID.2 <- function(data = fixaitionData_secondAfterQ){
  
  # takes firstSaccadeDirection data and returns summarized data per Pp for the ratio to the
  #fixations who were in the right or wrong direction
  # ratioKnow is how much hte value is robust - using how many NA I have
  
  fixaitionData_firstAfterQ.1 <- data %>%
    group_by(ID) %>%
    summarise(right = sum(na.omit(rightDirection)),
              wrong = length(na.omit(rightDirection)) - right, 
              Diff = right - wrong, 
              ratioRight = round(sum(na.omit(rightDirection)) / length(na.omit(rightDirection)), 2),
              ratioWrong = round(1 - ratioRight, 2),
              diffRatios = ratioRight - ratioWrong, 
              dontKnow = length(rightDirection) - length(na.omit(rightDirection)),
              ratioKnow = 1 - round(dontKnow / length(rightDirection), 2),
              meanIscorrect = mean(isCorrect, na.rm = TRUE))
  return(fixaitionData_firstAfterQ.1)
  
  
}


fixaitionData.1 <- secondSaccadeAfterQeustion(data = fixaitionData, startQuestion = timeblanckBeforeQuestion)
fixaitionData_secondAfterQ <- secondSaccadeDirection(fixaitionData.1)
fixaitionData_secondAfterQ.1 <- sumByID.2(fixaitionData_secondAfterQ)


shapiro.test(fixaitionData_secondAfterQ.1$ratioRight)
shapiro.test(fixaitionData_secondAfterQ.1$meanIscorrect)

cor <- cor.test(fixaitionData_secondAfterQ.1$ratioRight, fixaitionData_secondAfterQ.1$meanIscorrect, method = 'spearman')

p <- ifelse(cor$p.value < .01, '< .01', round(cor$p.value, 2))
r <- round(cor$estimate, 2)
Rsquared <- round(r ^ 2, 2)
Tilte <- paste0('Right direction for second saccade: rho  =  ', r, ',  p  =  ', p, ',  R-squared = ', Rsquared)


fixaitionData_secondAfterQ.1 %>%
  ggplot(aes(x = ratioRight, y = meanIscorrect)) + 
  geom_point(aes(color = ratioKnow, size = ratioKnow)) + 
  geom_smooth(method = 'lm', se = TRUE) + 
  xlim(0, 1) + ylim(0, 1) + 
  xlab('Right direction ratio') + ylab('Accuracy') + ggtitle(Tilte) +
  Theme +
Lbls  



  ##### last saccade before target
  
  lastSaccadeAfterQeustion <- function(data = fixaitionData, startQuestion = timeblanckBeforeQuestion){
    
    # remove the saccades that starts before the apearence of the question
    
    fixaitionData <- data %>%
      filter(start < endISI)
    
    return(data)
    
  }
  lastSaccadeDirection <- function(data = fixaitionData.2){
    
    # return full data with only the last fixation before the target and with the direction of the fixation
    
   
    fixaitionData_firstAfterQ <- c()
    for(i in unique(data$ID)){
      dataPp <- data %>% filter(ID == i)
      for(j in unique(dataPp$trial)){
        dataTrial <- dataPp %>% filter(trial == j)
        fixaitionData_firstAfterQ <- rbind(fixaitionData_firstAfterQ, dataTrial[length(dataTrial[, 1]), ])
      }
    }
    
    fixaitionData_firstAfterQ <- fixaitionData_firstAfterQ %>%
      mutate(rightDirection = ifelse(is.na(y), NA,
                                     ifelse((Answer == 1 & y < 540) | (Answer == -1 & y > 540), 1,
                                            ifelse((Answer == 1 & y > 540) | (Answer == -1 & y < 540), 0, NA))))
    return(fixaitionData_firstAfterQ)
    
  }
  sumByID.2 <- function(data = fixaitionData_lastBeforeTarget){
    
    # takes firstSaccadeDirection data and returns summarized data per Pp for the ratio to the
    #fixations who were in the right or wrong direction
    # ratioKnow is how much hte value is robust - using how many NA I have
    
    fixaitionData_firstAfterQ.1 <- data %>%
      group_by(ID) %>%
      summarise(right = sum(na.omit(rightDirection)),
                wrong = length(na.omit(rightDirection)) - right, 
                Diff = right - wrong, 
                ratioRight = round(sum(na.omit(rightDirection)) / length(na.omit(rightDirection)), 2),
                ratioWrong = round(1 - ratioRight, 2),
                diffRatios = ratioRight - ratioWrong, 
                dontKnow = length(rightDirection) - length(na.omit(rightDirection)),
                ratioKnow = 1 - round(dontKnow / length(rightDirection), 2),
                meanIscorrect = mean(isCorrect, na.rm = TRUE))
    return(fixaitionData_firstAfterQ.1)
    
    
  }
  
  
  fixaitionData.2 <- lastSaccadeAfterQeustion(data = fixaitionData, startQuestion = timeblanckBeforeQuestion)
  fixaitionData_lastBeforeTarget <- lastSaccadeDirection(fixaitionData.2)
  fixaitionData_lastBeforeTarget.1 <- sumByID.2(fixaitionData_lastBeforeTarget)
  
  
  shapiro.test(fixaitionData_lastBeforeTarget.1$ratioRight)
  shapiro.test(fixaitionData_lastBeforeTarget.1$meanIscorrect)
  
  cor <- cor.test(fixaitionData_lastBeforeTarget.1$ratioRight, fixaitionData_lastBeforeTarget.1$meanIscorrect, method = 'spearman')
  
  p <- ifelse(cor$p.value < .01, '< .01', round(cor$p.value, 2))
  r <- round(cor$estimate, 2)
  Rsquared <- round(r ^ 2, 2)
  Tilte <- paste0('Right direction for last saccade: rho  =  ', r, ',  p  =  ', p, ',  R-squared = ', Rsquared)
  
  
  fixaitionData_lastBeforeTarget.1 %>%
    ggplot(aes(x = ratioRight, y = meanIscorrect)) + 
    geom_point(aes(color = ratioKnow, size = ratioKnow)) + 
    geom_smooth(method = 'lm', se = TRUE) + 
    xlim(0, 1) + ylim(0, 1) + 
    xlab('Right direction ratio') + ylab('Accuracy') + ggtitle(Tilte) +
    Theme +
    Lbls  
  

  