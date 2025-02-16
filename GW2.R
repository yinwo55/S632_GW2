# Team Member: Ambuj Sahu, Inwoo Lee, Shiv Anand Chhabria

library(faraway)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(nnet)


###############
##### A. Make a plot showing how the proportion on miners in the three categories at each year 
# point varies over time. Comment on the relationship.

attach(pneumo)

pneumo_wide <- pneumo |> 
  pivot_wider(names_from = status, values_from = Freq) |> 
  mutate(p.normal = normal/(normal+mild+severe),
         p.mild = mild/(normal+mild+severe),
         p.severe = severe/(normal+mild+severe))

pneumo <- pneumo |> 
  group_by(year) |> 
  mutate(gtotal=sum(Freq), proportion = Freq/gtotal)

ggplot(pneumo, aes(year, proportion, group = status, color = status)) +
  geom_line()

###############
###### B. Use the pneumonoconiosis status as the response variable. Build a model for predicting the frequency of the three outcomes in terms of length of service. 
###### Interpret the coefficient for year for someone with a severe condition vs a normal condition.

#Change reference category to democrats
pneumo$status <- relevel(factor(pneumo$status), ref = "normal")

m1 = multinom(status ~ Freq + year, pneumo)
m1_step <- step(m1, trace = 0)
m1_step$call
summary(m1_step)

# Coefficient of year on severe is -0.703 which means that for each additional year, 
# the log-odds of being in the severe category (relative to normal) decrease by 0.703.


###############
###### C. Would it be better to use log(year) as the predictor? Explain.

summary(pneumo)

# No. According to the range rule, if the range of a variable is considerably less than one order of magnitude, 
# then any transformation of that variable is unlikely to be helpful. Since the range of year in the dataset is not large, 
# a log transformation is unlikely to be helpful.

###############
###### D. What is the probability that an individual will have a severe condition after working for 31 years?

result <- predict(m1_step, data.frame(Freq = mean(pneumo$Freq), year = 31), type = "probs")
print(result)

# The probability that an individual will have a severe condition after working for 31 years is 67.37% 

###### E. Produce a plot of the predicted probabilities in the same format as part A.
qyear_25 <- quantile(pneumo$year, 0.25)
qyear_75 <- quantile(pneumo$year, 0.75)

ylevels = round(qyear_25):round(qyear_75)

preds = data.frame(year=ylevels,
                   predict(m1_step, data.frame(Freq = mean(pneumo$Freq), year=ylevels),type="probs"))

lpred = pivot_longer(preds, cols = -year, names_to = "status", values_to = "probability")

ggplot(lpred, aes(year, probability, group = status, color = status)) +
  geom_line()
