?pnorm
pnorm(240, mean = 222, sd = 37, lower.tail = FALSE)

1 - pnorm(200, mean = 222, sd = 37)-
  pnorm(240, mean = 222, sd = 37, lower.tail = FALSE)

pnorm(240, mean = 222, sd = 37)-
  pnorm(200, mean = 222, sd = 37)

qnorm(.90)
qnorm(0.10, lower.tail = F)

qnorm(0.75, mean = 222, sd = 37)
###################

sample(1:6, 30, rep = T)
die = function(n) {sample(1:6, n, rep = T)}
d = die(10000)
sum(d==5)
sum(d==5)/10000

sample(c("Head","Tail"), 10, rep = T)
coin = function(n) {sample(c("Head","Tail"), n, rep = T)}
c = coin(300)
sum(c == "Tail")/300

hist(d, breaks = seq(0.5, 6.5, 1), ylim = c(0.0, 0.2), col = "green", prob = T)

runif(4)

runif(4, min = 2, max = 4)

waitTime = rexp(1000, rate = 1/30)

mean(waitTime)
sd(waitTime)

hist(waitTime)
summary(waitTime)
round(summary(waitTime))

x = rnorm(n = 200, m = 10, sd = 2)
hist(x, main = "Histogram of observed data")
plot(density(x), main = "Density estimate of data")



###########################

pnorm(3.5, mean = 3.1, sd = 0.2) - pnorm(2.7, mean = 3.1, sd = 0.2)

#####################################

myCI <- function(data, c.level = 0.95){
  t = qt((1-c.level)/2,
         df = length(data)-1,
         lower.tail = FALSE)
  xbar = mean(data)
  se = sd(data)/sqrt(length(data))
  me = t * se
  c(xbar-me, xbar+me)
}
