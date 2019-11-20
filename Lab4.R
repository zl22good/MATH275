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

