library(moments)
ct <- read.csv(file.choose())
View(ct)
# Hypothesis Testing
#Where x-discrete,y-contionous
#x1-unitA,x2-unitB,y-diameter of cutlets
#Normality test
shapiro.test(ct$Unit.A)
# p-value = 0.3200 >0.05 so p high null fly => It follows normal distribution

shapiro.test(ct$Unit.B)
# p-value = 0.5225 >0.05 so p high null fly => It follows normal distribution
### External conditions are same- Paired T test
#case(1)
#HO: mu of x1= mu of x2
#Ha: mu of x1!= mu of x2


t.test(ct$Unit.A,ct$Unit.B,alternative = "two.sided",conf.level = 0.95,paired = TRUE)
#p value=0.4552>0.05=> fail to reject HO=> mu of x1=mu of x2
## From above business case there is no significance difference of diameter of the cutlet between two units
# Then accept null hypothesis
