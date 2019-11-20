library('ggplot2')
x <- seq(-1,1,by = 0.2)
x
y <- -x^3
y
plot(x,y)
qplot(x,y)
qplot(x, binwidth = 1)

replicate(3, 4)
replicate(3, 4*2+3)
replicate(3, "Siena")
roll <- function(die = 1:6){
  dice <- sample(die, size = 2, replace = TRUE)
  sum(dice)
}
replicate(10,roll())
rolls <- replicate(100000,roll())
qplot(rolls, binwidth = 1)
hist(rolls)


wroll <- function(die = 1:6){
  wdice <- sample(die, size = 2, replace = TRUE, prob = c(1/8,1/8,1/8,1/8,1/8,3/8))
  sum(wdice)
}
replicate(10,wroll())
wrolls <- replicate(100000,wroll())
qplot(wrolls, binwidth =1)
hist(wrolls)


dbinom(2, size = 50, prob = 0.066) + 
  dbinom(3, size = 50, prob = 0.066)

pbinom(2049, size = 32000, prob = 0.066)
?pbinom
1-pbinom(0, size = 6, prob = 0.08)
pbinom(0, size = 6, prob = 0.08, lower.tail = FALSE) 
dpois(x=3, lambda = 2.74)
dpois(x=3, lambda = 2.74) * 365

ppois(3,2.75)
ppois(2,2.75)
ppois(4,2.75, lower.tail = FALSE)
365 * (1-ppois(4,2.75))

pnorm(172,mean = 222, sd = 37)
