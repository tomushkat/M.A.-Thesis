
newData <- Data %>%
  filter(TrialTime > minCriticTime & TrialTime < maxCriticTime) %>%
  filter(Block <= 2) %>%
  filter(ID %in% onlyB2) %>%
  mutate(Answer = ifelse(Answer == 1, 'Yes', 'No'),
         Block = ifelse(Block == 1, 'Induction', 'Experimental')) %>%
  group_by(ID, Block, Answer) %>%
  summarise(meanAccurary = mean(newIsCorrect, na.rm = T),
            meanEyeDirection  = mean(gy, na.rm = T))


newData %>%
  ggplot(aes(x = Block, y = meanAccurary, fill = Answer)) +
  geom_dotplot(binaxis = 'y', stackdir = 'center', position = position_dodge(0.8), binwidth = .02)  +
  scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9")) + 
  stat_summary(fun = mean, geom = "point", shape = 18, size = 5,  aes(color = Answer) , position = position_dodge(0.8)) + 
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "pointrange", aes(color = Answer), position = position_dodge(0.8)) +
  Theme + 
  xlab('Question type') + ylab('Accuracy') + ggtitle('Accuracy by block') +
  Lbls

ggsave('accuracyCompFigure.pdf')

newData %>%
  ggplot(aes(x = Block, y = meanEyeDirection, fill = Answer)) +
  geom_dotplot(binaxis = 'y', stackdir = 'center', position = position_dodge(0.8), binwidth = .02)  +
  scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9")) + 
  stat_summary(fun = mean, geom = "point", shape = 18, size = 5,  aes(color = Answer) , position = position_dodge(0.8)) + 
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "pointrange", aes(color = Answer), position = position_dodge(0.8)) +
  Theme + 
  xlab('Question type') + ylab('Converted mean y axis fixation') + ggtitle('Eye direction by block') 
  Lbls

ggsave('ETCompFigure.pdf')


newDataY <-  newData %>%
  filter(Answer == 'Yes')


newDatan <-  newData %>%
  filter(Answer == 'No') 

newDatan$Yes <- newDataY$meanAccurary
newDatan$YesEye <- newDataY$meanEyeDirection

dimnames(newDatan)[[2]] <- c('ID', 'Block', 'Answer', 'accuracyNo', 'ETnO', 'accuracyYes', 'ETYes')


newDatan <- newDatan %>%
  mutate(subAccuracy = accuracyYes - accuracyNo,
         subEye = ETYes - ETnO)

d <- data.frame(ID = unique(newDatan$ID),
                inductionAccuray = newDatan$subAccuracy[newDatan$Block == 'Induction'],
                experimentalAccuray = newDatan$subAccuracy[newDatan$Block == 'Experimental'],
                inductionEye = newDatan$subEye[newDatan$Block == 'Induction'],
                experimentalEye = newDatan$subEye[newDatan$Block == 'Experimental'])


shapiro.test(d$inductionAccuray)
shapiro.test(d$experimentalAccuray)
c1 <- cor.test(d$inductionAccuray, d$experimentalAccuray)

d$experimentalAccurayScaled <- scale(d$experimentalAccuray)
d$inductionAccurayScaled <- scale(d$inductionAccuray)

Name <- c('Induction Yes - NO accuracy by Experimental Yes - NO accuracy.pdf')
Title <- paste0('Induction Yes - NO accuracy by Experimental Yes - NO accuracy ', 'r = ', round(c1$estimate, 2),  ' ', 'p = ', round(c1$p.value, 2))
d %>%
  ggplot(aes(x = experimentalAccuray, y = inductionAccuray)) + 
  geom_point() +
  geom_smooth(method = lm, se = TRUE) + 
  Theme + xlim(-0.5, 0.5) + ylim(-0.5, 0.5) + 
  ggtitle(Title) + xlab('Induction Yes - NO') + ylab('Experimental Yes - NO') 
  Lbls
ggsave(Name, width = 30, height = 25, units = "cm")


d1 <- d[complete.cases(d), ]
shapiro.test(d1$inductionEye)
shapiro.test(d1$experimentalEye)
c2 <- cor.test(d1$inductionEye, d1$experimentalEye, method = 'spearman')

Name <- c('Induction Yes - NO by Experimental Yes - NO ET.pdf')
Title <- paste0('Induction Yes - NO by Experimental Yes - NO ET ',  'rho = ', round(c2$estimate, 2),  ' ', 'p = ', round(c2$p.value, 2))

d1 %>%
  ggplot(aes(x = inductionEye, y = experimentalEye)) + 
  geom_point() +
  geom_smooth(stat = 'smooth', method = lm, se = TRUE) +
  Theme + ylim(-1000, 1000) + xlim(-1000, 1000) + 
  ggtitle(Title) + xlab('Induction Yes - NO') + ylab('Experimental Yes - NO') +
  Lbls
                                   
ggsave(Name, width = 30, height = 25, units = "cm")

                                   