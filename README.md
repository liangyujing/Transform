```{R}
#reverse ACC score
mixedef_correct_incorrect<-mixedef_correct_incorrect%>%
  mutate(ACC_reverse = if_else(ACC == 0, 1, 0))

#create factor of the reverse ACC score
mixedef_correct_incorrect$ACC_reverse<-factor(mixedef_correct_incorrect$ACC_reverse, levels = c(0, 1), labels = c("correct", "Incorrect"))

```

```{R}
#Transform HRV measures, to increase progressing to normal distribution, so that model estimation is beter afterwards
#correct incorrect
mixedef_correct_incorrect<-mixedef_correct_incorrect%>%
  mutate(lnRT = log10(RT),
         lnconfidence = 1/(max(confidence+1) - confidence), 
         lnRMSSD = log10(baseline_RMSSD))
         
#check normality again, with adjusted predictor variables
#histogram to see how RMSSD 
hist(mixedef_correct_incorrect$lnRMSSD,probability=T, main="Histogram of distributed RMSSD
     data",xlab="distributed RMSSD data")
lines(density(mixedef_correct_incorrect$lnRMSSD),col=2)
#qqplot RT
qqPlot(mixedef_correct_incorrect$lnRMSSD)
#not normal distributed what is to be expected based on the literature, right skewed

#histogram to see how RT
hist(mixedef_correct_incorrect$lnRT,probability=T, main="Histogram of distributed RT
     data",xlab="distributed RT data")
lines(density(mixedef_correct_incorrect$RT),col=2)
#qqplot RT
qqPlot(mixedef_correct_incorrect$lnRT)
#It is right skewed

#histogram to see how confidence 
hist(mixedef_correct_incorrect$lnconfidence,probability=T, main="Histogram of distributed confidence data",
     xlab="distributed confidence data")
lines(density(mixedef_correct_incorrect$lnconfidence),col=2)
#qqplot RT
qqPlot(mixedef_correct_incorrect$lnconfidence)
#It is left skewed
```

```{R}
#center RT and confidence at the participant level
#for lnconfidence
mixedef_correct_incorrect<- mixedef_correct_incorrect %>%
  group_by(subject)%>%
  mutate(lnconfidence_mean = mean(lnconfidence),
         lnconfidence_sd = sd(lnconfidence),
         lnconfidence_zscore = (lnconfidence - lnconfidence_mean)/lnconfidence_sd)

#For RT
mixedef_correct_incorrect<- mixedef_correct_incorrect %>%
  group_by(subject)%>%
  mutate(lnRT_mean = mean(lnRT),
         lnRT_sd = sd(lnRT),
         lnRT_zscore = (lnRT - lnRT_mean)/lnRT_sd)
```
