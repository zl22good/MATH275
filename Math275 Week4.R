a =1
die = 1:6
2*die
factorial(7)
mean(die)
sd(die)
round(sd(die))
round(sd(die), digits = 3)
A = matrix(c(-1,0,3,7,1,-3), nrow = 3, ncol = 2, byrow = FALSE)
A
A = matrix(c(-1,0,3,7,1,-3), nrow = 3, ncol = 2, byrow = TRUE)
A
A[3,2]
A[1,]
A[,2]
B = matrix(3:6, nrow = 2, ncol = 2)
B
t(B)
det(B)
eigen(B)
A%*%B
cbind(c(1:4),c(5:8))
rbind(c(1:4),c(5:8))
A
B
rbind(A,B)
rbind(B,A)
#\u2265 = '>='
dimnames(B) = list(c("GPA < 3.70", "GPA \u2265 3.70"),c("Loudonville","Saratoga Springs"))
B
###############
#9/26

table = cbind(c(114,323),c(36,36))
rownames(table)=c("Female","Male")
colnames(table)=c("Yes","No")
table

OSASdata <- read.csv("OSASdata.csv")
View(OSASdata)

table(OSASdata$sex,OSASdata$OSAS)

attach(OSASdata)
round(prop.table(table(sex,OSAS),2),digits = 2)
#?prop.table
chisq.test(sex,OSAS)

dice <- sample(x=1:6,size=5, rep = T)
dice
sum(dice)

roll <- function(){sample(x=1:6,size=2, rep = T)}
roll()

roll2 <- function(bones=1:6){sample(bones,size=2, rep = T)}
roll2(1:20)
roll2()

