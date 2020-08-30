br <- read.csv(file.choose())
View(br)
# Hypothesis Testing
#Where x-discrete,y-discrete
#x1-e,x2-w,x3-n,x4-s(sales of products in regions),y-buyer rations are similar or not
br1 <- subset(br,select = -1) 
View(br1)
str(br1)
attach(br1)
row.names(br1) <- c("Males","Females")
View(br1)
str(br1)
#chisq.test
chisq.test(br1)
# Ho ->   All proportions are equal
# Ha ->  All proportions are not equal
#Since male-female buyer rations are similar across regions
#Where P-value > 0.05 accept null hypothesis

