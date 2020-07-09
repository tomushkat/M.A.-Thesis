

##### Induction ####


Temp <- Data %>%
  filter(Block == 1 & ID %in% onlyB2) %>%
  filter(TrialTime >= minCriticTime & TrialTime <= maxCriticTime)

data12 <-  Temp %>%
  group_by(ID, Trial.x, Answer) %>%
  summarise(Mean = mean(gy, na.rm = TRUE)) %>%
  mutate(Answer = ifelse(Answer == 1, 1, ifelse(Answer == -1, 0, NA)),
         Direction = ifelse((Answer == 1 & Mean < 540) | (Answer == 0 & Mean > 540), 1,
                            ifelse((Answer == 1 & Mean > 540) | (Answer == 0 & Mean < 540), 0, NA)),
         Directionwhere  = ifelse(Mean < 540, 1, ifelse(Mean > 540, 0, NA)))
data12$Direction <- as.factor(data12$Direction)
data12$Directionwhere <- as.factor(data12$Directionwhere)
data12$Answer <- as.factor(data12$Answer)
contrasts(data12$Answer) <- c(-1, 1)
#contrasts(data12$Direction) <- c(0, 1)
#contrasts(data12$Directionwhere) <- c(0, 1)

#data12 <- unique(data[ , c('ID', 'Trial.x', 'Answer', 'Y')])


data12.2 <- data12[complete.cases(data12$Directionwhere), ]
set.seed(2)


dataSplit <- data12.2 %>%
  as_tibble() %>%
  initial_split(prop = 0.7, strata = Directionwhere)

Train <- training(dataSplit)
Test <- testing(dataSplit)


Model <- glmer(Directionwhere ~ Answer + (1 | ID), data = Train, family = binomial(link = 'logit'))  
summary(Model)


S <- predict(Model, Test, type = 'response')
original <- Test$Directionwhere
predicted <- round(S)

trainTab <- as.matrix(table(predicted, original))
dimnames(trainTab)[[1]] <- c('Down', 'Up')
dimnames(trainTab)[[2]] <- c('Down', 'Up')


t <- as.matrix(table(as.vector(predicted), Test$Answer))
dimnames(t)[[2]] <- c('No', 'Yes')
dimnames(t)[[1]] <- c('Down', 'Up')
CrossTable(as.vector(predicted), Test$Answer, chisq = TRUE)



head(original)
head(Test$Answer.1)
head(predicted)


Acc <- round((trainTab[2, 2] + trainTab[1, 1]) / sum(trainTab), 2)
Specificity <- round(trainTab[2, 2] / (trainTab[2, 2] + trainTab[1, 2]), 2)
Sensitivity <- round(trainTab[1, 1] / (trainTab[1, 1] + trainTab[2, 1]), 2)

tx <- trainTab %>%
  confusionMatrix()




data12.1 <- data12[complete.cases(data12$Direction), ]
set.seed(2)

dataSplit <- data12.1 %>%
  as_tibble() %>%
  initial_split(prop = 0.7, strata = Direction)

Train <- training(dataSplit)
Test <- testing(dataSplit)


Model <- glmer(Direction ~ Answer + (1 | ID), data = Train, family = binomial(link = 'logit'))  
summary(Model)


S <- predict(Model, Test, type = 'response')
original <- Test$Direction
predicted <- round(S)

trainTab <- as.matrix(table(predicted, original))
dimnames(trainTab)[[1]] <- c('Wrong', 'Right')
dimnames(trainTab)[[2]] <- c('Wrong', 'Right')


t <- as.matrix(table(as.vector(predicted), Test$Answer))
dimnames(t)[[2]] <- c('No', 'Yes')
dimnames(t)[[1]] <- c('Wrong', 'Right')

head(original)
head(Test$Answer)
head(predicted)


Acc <- round((trainTab[2, 2] + trainTab[1, 1]) / sum(trainTab), 2)
Sensitivity <- round(trainTab[2, 2] / (trainTab[2, 2] + trainTab[1, 2]), 2)
Specificity <- round(trainTab[1, 1] / (trainTab[1, 1] + trainTab[2, 1]), 2)

tx <- trainTab %>%
  confusionMatrix()


as.matrix(table(predicted, Test$Answer))

##### End induction ####




##### Experimental ####


Temp <- Data %>%
  filter(Block == 2) %>%
  filter(TrialTime >= minCriticTime & TrialTime <= maxCriticTime)

data12 <-  Temp %>%
  group_by(ID, Trial.x, Answer) %>%
  summarise(Mean = mean(gy, na.rm = TRUE)) %>%
  mutate(Answer = ifelse(Answer == 1, 1, ifelse(Answer == -1, 0, NA)),
         Direction = ifelse((Answer == 1 & Mean < 540) | (Answer == 0 & Mean > 540), 1,
                            ifelse((Answer == 1 & Mean > 540) | (Answer == 0 & Mean < 540), 0, NA)),
         Directionwhere  = ifelse(Mean < 540, 1, ifelse(Mean > 540, 0, NA)))
data12$Direction <- as.factor(data12$Direction)
data12$Directionwhere <- as.factor(data12$Directionwhere)
data12$Answer <- as.factor(data12$Answer)
contrasts(data12$Answer) <- c(-1, 1)
contrasts(data12$Direction) <- c(0, 1)
contrasts(data12$Directionwhere) <- c(1, 0)

#data12 <- unique(data[ , c('ID', 'Trial.x', 'Answer', 'Y')])


data12.2 <- data12[complete.cases(data12$Directionwhere), ]
set.seed(2)


dataSplit <- data12.2 %>%
  as_tibble() %>%
  initial_split(prop = 0.7, strata = Directionwhere)

Train <- training(dataSplit)
Test <- testing(dataSplit)


Model <- glmer(Directionwhere ~ Answer + (1 | ID), data = Train, family = binomial(link = 'logit'))  
summary(Model)


S <- predict(Model, Test, type = 'response')
original <- Test$Directionwhere
predicted <- round(S)

trainTab <- as.matrix(table(predicted, original))
dimnames(trainTab)[[1]] <- c('Down', 'Up')
dimnames(trainTab)[[2]] <- c('Down', 'Up')


t <- as.matrix(table(as.vector(predicted), Test$Answer))
dimnames(t)[[2]] <- c('No', 'Yes')
dimnames(t)[[1]] <- c('Down', 'Up')

head(original)
head(Test$Answer.1)
head(predicted)


Acc <- round((trainTab[2, 2] + trainTab[1, 1]) / sum(trainTab), 2)
Specificity <- round(trainTab[2, 2] / (trainTab[2, 2] + trainTab[1, 2]), 2)
Sensitivity <- round(trainTab[1, 1] / (trainTab[1, 1] + trainTab[2, 1]), 2)

tx <- trainTab %>%
  confusionMatrix()




data12.1 <- data12[complete.cases(data12$Direction), ]
set.seed(2)

dataSplit <- data12.1 %>%
  as_tibble() %>%
  initial_split(prop = 0.7, strata = Direction)

Train <- training(dataSplit)
Test <- testing(dataSplit)


Model <- glmer(Direction ~ Answer + (1 | ID), data = Train, family = binomial(link = 'logit'))  
summary(Model)


S <- predict(Model, Test, type = 'response')
original <- Test$Direction
predicted <- round(S)

trainTab <- as.matrix(table(predicted, original))
dimnames(trainTab)[[1]] <- c('Wrong', 'Right')
dimnames(trainTab)[[2]] <- c('Wrong', 'Right')


t <- as.matrix(table(as.vector(predicted), Test$Answer))
dimnames(t)[[2]] <- c('No', 'Yes')
dimnames(t)[[1]] <- c('Wrong', 'Right')

head(original)
head(Test$Answer.1)
head(predicted)


Acc <- round((trainTab[2, 2] + trainTab[1, 1]) / sum(trainTab), 2)
Sensitivity <- round(trainTab[2, 2] / (trainTab[2, 2] + trainTab[1, 2]), 2)
Specificity <- round(trainTab[1, 1] / (trainTab[1, 1] + trainTab[2, 1]), 2)

tx <- trainTab %>%
  confusionMatrix()



##### End experimental ####
