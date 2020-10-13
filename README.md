```{R}
#reverse ACC score
mixedef_correct_incorrect<-mixedef_correct_incorrect%>%
  mutate(ACC_reverse = if_else(ACC == 0, 1, 0))

#create factor of the reverse ACC score
mixedef_correct_incorrect$ACC_reverse<-factor(mixedef_correct_incorrect$ACC_reverse, levels = c(0, 1), labels = c("correct", "Incorrect"))

```
