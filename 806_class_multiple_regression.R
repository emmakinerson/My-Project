
#more regression

library(tidyverse)
library(palmerpenguins)

ggplot(data = penguins, aes(x = bill_length_mm, y = body_mass_g))+ 
  geom_point() + geom_smooth(method='lm')

mod1 <- lm(body_mass_g ~ bill_length_mm, data = penguins)
summary(mod1)

#for every one mm increase in bill length the body mass increases by 87.415 grams

plot(mod1)
#in the residuals vs leverage graph it will label the data points that are concerning and it'll be the row in the data that is the concern  


str(mod1)
hist(mod1$residuals)


#Multiple regression

mod2 <- lm(body_mass_g ~ bill_length_mm + flipper_length_mm, data = penguins)
summary(mod2)

#model always gets better as you add more variables, the model can be over fitted though for when you want to fit to a new data point 

mod3 <- lm(body_mass_g ~ flipper_length_mm, data = penguins)
summary(mod3)

mod4 <- lm(body_mass_g ~ flipper_length_mm + sex, data = penguins)
summary(mod4)
#means that if you have two penguins with the same flipper length than it is expected that the male will weigh 347.85 grams more than the female

mod5 <- lm(body_mass_g ~ flipper_length_mm + sex + bill_depth_mm + bill_length_mm + year + island + species, data = penguins)
summary(mod5)

AIC(mod1, mod2, mod3, mod4, mod5)
#for AIC comparisons you want the lowest score compared to the other groups 
#should drop NAs or data you are worried about before doing AIC because it should be against the same number of observations
#if the model is overfitted it should then have a higher AIC

#should talk about estimate, P value, r squared, AIC value 

