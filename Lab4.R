#Zac Goodsell
gender <- c(rep("Female", 10), rep("Male", 10)) 
group <- rep(rep(c("Treatment", "Control"), each = 5), 2)
worms <- c(1, 2, 2, 10, 7, 16, 10, 10, 7, 17, 3, 5, 9, 10, 6, 31, 26, 28, 13, 47)
schis <- data.frame(gender, group, worms)
rm(gender, group, worms)
head(schis, n = 3)

schis

library(ggplot2)
p <- ggplot(data = schis, aes(group, worms)) + 
  geom_point(position = "jitter", aes(color = group)) + 
  facet_grid(. ~ gender) + 
  theme_bw()
p

# Classical approach with order()
with(data = schis, 
     schis[order(gender, group, worms),]
)

# Tidyverse Approach
library(dplyr)
schis <- schis %>%
  arrange(gender, group, worms)
schis

with(data = schis, 
     tapply(worms, list(gender, group), median)    
)


with(data = schis, 
     tapply(worms, list(gender, group), mean)   
)

with(data = schis, 
     tapply(worms, list(gender, group), sd)    
)

# Tidyverse Approach
schis %>%
  group_by(gender, group) %>%
  summarize(Md = median(worms), Mean = mean(worms), SD = sd(worms), N = n())

ND <- schis[schis$gender=="Female", ]
ND

tapply(ND$worms, ND$group, mean)

# OR
ANS1 <- with(data = ND,
             tapply(worms, group, mean)
)
ANS1

observed <- ANS1[1] - ANS1[2]
observed

names(observed) <- NULL
observed

# Tidyverse approach
ND2 <- schis %>%
  filter(gender == "Female")
ND2

IA <- ND2 %>%
  group_by(group) %>%
  summarize(MeanWorms = mean(worms))
IA

obs_diff <- IA[1, 2] - IA[2, 2]
obs_diff

Worms <- ND$worms
Worms

# Another way:
Worms2 <- subset(ND, select = worms, drop = TRUE)
Worms2

N <- 10^4 - 1         # number of times to repeat the process
result <- numeric(N)  # space to save the random differences
set.seed(5)
for (i in 1:N){
  # sample of size 5, from 1 to 10, without replacement
  index <- sample(10, size = 5, replace = FALSE)
  result[i] <- mean(Worms2[index]) - mean(Worms2[-index])
}
hist(result, col = "blue", freq = FALSE, main = "", breaks = "Scott")

d.res <- density(result)
plot(d.res, main ="", xlab = "", ylab = "")
polygon(d.res, col ="pink")
xs <- c(7.6, d.res$x[d.res$x >= 7.6])
ys <- c(0, d.res$y[d.res$x>=7.6])
polygon(xs, ys, col = "red")

pvalue <- (sum(result >= observed) + 1)/(N + 1) # p-value
pvalue  # results will vary

############################################################
# Yet another approach - 
ND2
N <- 10^4 - 1          # number of times to repeat the process
result2 <- numeric(N)  # space to save the random differences
set.seed(5)
for (i in 1:N){
  # sample of size 5, from 1 to 10, without replacement
  index <- sample(10, size = 5, replace = FALSE)
  result2[i] <- mean(ND2$worms[index]) - mean(ND2$worms[-index])
}
# Graph Now
# ggplot2 approach now
DF <- data.frame(x = result)
p <- ggplot(data = DF) + geom_density(aes(x = x, y = ..density..), fill = 'pink', alpha = 0.4) + 
  theme_bw()
p

x.dens <- density(result)
df.dens <- data.frame(x = x.dens$x, y = x.dens$y)
p + geom_area(data = subset(df.dens, x >= 7.6 & x <= max(DF$x)), aes(x = x, y = y), fill = 'blue', alpha = 0.4) + 
  labs(x = expression(bar(x)[Control] - bar(x)[Treatment]), y = '', title = "Randomization Distribution") + 
  theme_bw()

# PROGRAMMING IS THE BEST WAY TO DEBUG YOUR THINKING!
# Theoretical Answer
library(PASWR2)
DATA <- c(1, 2, 2, 10, 7, 16, 10, 10, 7, 17)
DATA

OBS <- mean(DATA[6:10]) - mean(DATA[1:5])
OBS

#
# ANS <- t(Combinations(10, 5))
ANS <- t(combn(10, 5))
head(ANS)

nn <- dim(ANS)[1]
nn

means <- numeric(nn)
for (i in 1:nn){
  means[i] <- mean(DATA[ANS[i,]]) - mean(DATA[-ANS[i,]])
}
sort(means)

#
sum(means >= OBS)

pvalue <- sum(means >= OBS)/nn
pvalue

# 7/252
DF <- data.frame(x = means)
p <- ggplot(data = DF) + 
  geom_density(aes(x = x, y = ..density..), fill = "pink", alpha = 0.4) + 
  theme_bw()
p

x.dens <- density(means)
df.dens <- data.frame(x = x.dens$x, y = x.dens$y)
p + geom_area(data = subset(df.dens, x >= 7.6 & x <= max(DF$x)), aes(x = x, y = y), fill = 'blue', alpha = 0.4) + 
  labs(x = '', y = '') 

# Another approach ....
#
P3 <- t(srs(DATA, n = 5))
P2 <- t(srs(DATA, n = 5))
# Note need to reorder the P2 values
P2R <- P2[, 252:1]
apply(P2R, 2, mean)

apply(P3, 2, mean)

DiffMeans <- apply(P3, 2, mean) - apply(P2R, 2, mean)
sort(DiffMeans)

sum(DiffMeans >= 7.6)

# Note the following:
obs <- mean(DATA[6:10])
sort(apply(P3, 2, mean))

sum(apply(P3, 2, mean) >= obs)


ND <- schis[schis$gender=="Female", ]
ND

tapply(ND$worms, ND$group, mean)


# OR
ANS1 <- with(data = ND,
             tapply(worms, group, mean)
)
ANS1

observed <- ANS1[1] - ANS1[2]
observed

names(observed) <- NULL
observed

Worms2 <- subset(ND, select = worms, drop = TRUE)
Worms2

N <- 10^5 - 1         # number of times fo repeat the process
set.seed(13)
result <- numeric(N)  # space to save the random differences
for (i in 1:N){
  # sample of size 5, from 1 to 10, without replacement
  index <- sample(10, size = 5, replace = FALSE)
  result[i] <- mean(Worms2[index]) - mean(Worms2[-index])
}
hist(result, col = "blue", main = "", freq = FALSE, breaks = "Scott")
d.res <- density(result)
plot(d.res, main ="", xlab = "", ylab = "")
polygon(d.res, col ="pink")
xsr <- c(7.6, d.res$x[d.res$x >= 7.6])
ysr <- c(0, d.res$y[d.res$x>=7.6])
xsl <- c(-7.6, d.res$x[d.res$x <= -7.6])
ysl <- c(0, d.res$y[d.res$x <= -7.6])
polygon(xsr, ysr, col = "red")
polygon(xsl, ysl, col = "red")

pvalue <- (sum(result >= observed) + sum(result <= -observed) + 1)/(N + 1) # p-value
pvalue  # results will vary

# ggplot2 approach now
DF <- data.frame(x = result)
p <- ggplot(data = DF) + 
  geom_density(aes(x = x, y = ..density..), fill = 'pink', alpha = 0.4) + 
  theme_bw()
p

x.dens <- density(result)
df.dens <- data.frame(x = x.dens$x, y = x.dens$y)
p + geom_area(data = subset(df.dens, x >= 7.6 & x <= max(DF$x)), aes(x = x, y = y), fill = 'blue', alpha = 0.4) + 
  geom_area(data = subset(df.dens, x <= -7.6 & x >= min(DF$x)), aes(x = x, y = y), fill = 'blue', alpha = 0.4) +
  labs(x = expression(bar(x)[Control] - bar(x)[Treatment]), y = '', title = "Randomization Distribution") + 
  theme(plot.title = element_text(hjust = 0.5)) 

sum(result >= observed)
sum(result <= -observed)