library(ggplot2)
library(dplyr)

# A. Make a plot showing how the proportion on miners in the three categories at each year 
# point varies over time. Comment on the relationship.

pneumo <- pneumo |> 
  group_by(status, year) |> 
  summarise(count = n()) |> 
  mutate(total_f=sum(Freq), proportion=(count/total_f))
) 

### above is my code

pneumo_wide <- pneumo |> 
  pivot_wider(names_from = status, values_from = Freq) |> 
  mutate(p.normal = normal/(normal+mild+severe),
         p.mild = mild/(normal+mild+severe),
         p.severe = severe/(normal+mild+severe))

pneumod <- pneumo |> 
  group_by(year) |> 
  mutate(gtotal=sum(Freq), proportion = Freq/gtotal)

ggplot(pneumod, aes(year, proportion, group = status, color = status)) +
  geom_line()

# B. Use the pneumonoconiosis status as the response variable. Build a model for predicting the frequency of the three outcomes in terms of length of service. 
# Interpret the coefficient for year for someone with a severe condition vs a normal condition.

m1 = multinom(status ~ Freq + year, pneumod)
#Change reference category to democrats
pneumod$status <- relevel(factor(pneumod$status), ref = 2)
#Run model
m2 <- multinom(status ~ Freq + year, pneumod)
summary(m2)
