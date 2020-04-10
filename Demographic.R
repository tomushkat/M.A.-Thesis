
d2 <- c()
for (sub in 5:NPp){
  d = subset(dataBehavioral, dataBehavioral$ID == sub)
  d1 = d[1, c(1:3)]
  d2 = rbind(d2, d1)
}

CrossTable(d2$Gender, chisq = TRUE, format = c("SPSS"))
chisq.test(c(length(d2$Gender[d2$Gender == 'Female']), length(d2$Gender[d2$Gender == 'Male'])),  p = c(0.5, 0.5))
round(mean(d2$Age), 2)
round(sd(d2$Age), 2)
max(d2$Age)
min(d2$Age)
d <- NULL
d1 <- NULL
d2 <- NULL
