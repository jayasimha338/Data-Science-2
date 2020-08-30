cf <- read.csv(file.choose())
View(cf)
# Hypothesis Testing
#Where x-discrete,y-discrete
#x1-p,x2-id,x3-m,x4-ind,y- % defective 
p <- ifelse(cf$Phillippines=="Error Free",0,1)
id <- ifelse(cf$Indonesia=="Error Free",0,1)
m <- ifelse(cf$Malta=="Error Free",0,1)
ind <- ifelse(cf$India=="Error Free",0,1)
cf1 <- cbind.data.frame(p,id,m,ind)
View(cf1)
str(cf1)
stacked_cf1<-stack(cf1)
attach(stacked_cf1)
View(stacked_cf1)
table(stacked_cf1)
#chisq.test
chisq.test(table(stacked_cf1$ind,stacked_cf1$values))
# Ho -> % defective for different centers are same
# Ha -> % defective for different centers are not same
# Since manager wants to check whether the % defective in differnt centres,but it has equal proportions
#where P-vale = 0.2771 accept null hypothesis