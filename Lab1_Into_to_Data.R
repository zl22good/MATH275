source("http://www.openintro.org/stat/data/cdc.R")
names(cdc)
#Question 1 - There are 20,000 cases and 9 variables
#for each of these cases. Data types - genhlth:categorical,
#exerany:binary, hlthplan:binary,smoke100:binary,
#height:continuous,weight:continuous,wtdesire:continuous,
#age:continuous,gender:binary
head(cdc)
tail(cdc)
summary(cdc$weight)
190-140
mean(cdc$weight)
var(cdc$weight)
median(cdc$weight)
table(cdc$smoke100)
table(cdc$smoke100)/20000
barplot(table(cdc$smoke100))
smoke <- table(cdc$smoke100)
barplot(smoke)
#Question 2 -
summary(cdc$height)
summary(cdc$age)
iqr_height=summary(cdc$height)[5]-summary(cdc$height)[2]
iqr_height
#Interquartile range of height is 6
iqr_age=summary(cdc$age)[5]-summary(cdc$age)[2]
iqr_age
#Interquartile range of age is 26
gender=table(cdc$gender)
genderFreq = gender/20000
genderFreq
barplot(genderFreq)
exerany=table(cdc$exerany)
exeranyFreq = exerany/20000
exeranyFreq
barplot(exeranyFreq)
table(cdc$gender)
numMales = table(cdc$gender)[1]
numMales
table(cdc$genhlth)/20000
table(cdc$genhlth)[1]/20000
#Question 3
table(cdc$gender,cdc$smoke100)
mosaicplot(table(cdc$gender,cdc$smoke100),main="Mosaic Plot of Gender vs. smoke100")
title(ylab="Person Has Smoked More Than 100 Cigarettes in Lifetime",
      xlab="Persons Gender")
dim(cdc)
cdc[567,6]
names(cdc)
cdc[1:10,6]
cdc[1:10,]
cdc[,6]
cdc$weight
cdc$weight[567]
cdc$gender == "m"
cdc$age > 30
mdata <- subset(cdc, cdc$gender == "m")
head(mdata)
m_and_over30 <- subset(cdc, gender == "m" & age > 30)
m_or_over30 <- subset(cdc, gender == "m" | age > 30)
#Question 4
under23_and_smoke = subset(cdc, smoke100==1 & age<23)
under23_and_smoke
boxplot(cdc$height)
summary(cdc$height)
boxplot(cdc$height ~ cdc$gender)
bmi <- (cdc$weight / cdc$height^2) * 703
boxplot(bmi ~ cdc$genhlth)
#Question 5
bmi = (cdc$weight/cdc$height^2)*703
boxplot(bmi ~ cdc$exerany,
        main="Relationship Between BMI and Monthly Exercise",xlab="Whether or Not Person Has Exercised in Past Month",ylab="Persons BMI")
#On your own
#1
plot(cdc$weight, cdc$wtdesire,
     xlab="Weight",ylab="Desired Weight",main="Plot of Persons Actual Weights vs. Desired Weights")
#2
wdiff = abs(cdc$wtdesire-cdc$weight)
#3
#wdiff is a continuse variable. If it is 0 that means that the persons actual weight and desiered weight are the same. If wdiff isn't 0 that is now far the person is from there desired weight. 
#4
summary(wdiff)
hist(wdiff)
wdiff_under100=subset(wdiff, wdiff<100)
summary(wdiff_under100)
hist(wdiff_under100)
#The histogram is right skewed. The mean of 10 shows that 50% of the people are with in 10 pounds of there desired weight. Further more, the histogram shows that roughly 80% of the people are within 30 pounds of there desired weight
#5
cdc_under100=subset(cdc,
                   abs(cdc$wtdesire-cdc$weight)<=100)
cdc_under100_male=subset(cdc_under100, gender=='m')
cdc_under100_female=subset(cdc_under100, gender=='f')
summary(abs(cdc_under100_male$wtdesire-cdc_under100_male$weight))
summary(abs(cdc_under100_female$wtdesire-cdc_under100_female$weight))
boxplot(abs(cdc_under100$wtdesire-cdc_under100$weight) ~ cdc_under100$gender, 
        main="Difference Between Actual and Desired Weight, by Gender",
        xlab="Gender",ylab="Difference Between Actual and Desired Weight")
#The plot shows that men are closer to there desired weight. The women have more out lying points.
#6
weight_mean = mean(cdc$weight)
weight_sd = sd(cdc$weight)
weight_mean
weight_sd
cdc_one_sd=subset(cdc, 
                  cdc$weight>(weight_mean-weight_sd) 
                  & cdc$weight<weight_mean+weight_sd)
nrow(cdc_one_sd)/nrow(cdc)
#70.76% of people weight fall within one sd pf the mean weight