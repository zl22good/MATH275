ch = chisq.test(c(23,50,20),p=c(0.25,0.5,0.25))
ch
ch$expected

desipramine <- read.csv("desipramine.csv")
attach(desipramine)
table(Treatment,Success)
summary(desipramine)
chisq.test(Treatment,Success)



