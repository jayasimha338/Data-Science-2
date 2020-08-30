lt <- read.csv(file.choose())
View(lt)
# Hypothesis Testing
#Where x-discrete,y-contionous
#x1-l1,x2-l2,x3-l3,x4-l4,y-average of tat 
Stacked_lab <- stack(lt)
View(Stacked_lab)
attach(Stacked_lab)
#Normality test
shapiro.test(lt$Laboratory.1)
# p-value = 0.5508 >0.05 so p high null fly => It follows normal distribution
shapiro.test(lt$Laboratory.2)
# p-value = 0.8637 >0.05 so p high null fly => It follows normal distribution
shapiro.test(lt$Laboratory.3)
# p-value = 0.4205 >0.05 so p high null fly => It follows normal distribution
shapiro.test(lt$Laboratory.4)
# p-value = 0.6619 >0.05 so p high null fly => It follows normal distribution
# Another way of normality test
install.packages("ANOM")
library(nortest)
ad.test(Stacked_lab$values) 
aov()
# Variance Test
library(car)
leveneTest(values~ ind, data = Stacked_lab)
# p-value = 0.05161 > 0.05 
#HO: var of x1= var of x2 =>Avona
#Ha: var of x1!= var of x2=>Anom
#Anova Test
Anova_results <- aov(values~ind,data = Stacked_lab)
summary(Anova_results)
#HO: var of x1= var of x2 =>Avona test
#Ha: var of x1!= var of x2=>Anom test
# p-value = 2e-16 accept alternative hypothesis
#From business case the average of turn around time has different reports of the laborarties on their preffered list