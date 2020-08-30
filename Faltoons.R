ft <- read.csv(file.choose())
View(ft)
# Hypothesis Testing
#Where x-discrete,y-discrete
#x1-e,x2-w,x3-n,x4-s(sales of products in different regions),y- ratios  of males versus females
wd <- ifelse(ft$Weekdays=="Male",1,0)
we <- ifelse(ft$Weekend=="Male",1,0)
ft1 <- cbind.data.frame(wd,we)
View(ft1)
str(ft1)
stacked_ft1<-stack(ft1)
attach(stacked_ft1)
View(stacked_ft1)
table(stacked_ft1)
#chisq.test
chisq.test(table(stacked_ft1))
# Ho ->  % of males versus females walking in to the store  won't differ based on day of the week
# Ha ->  % of males versus females walking in to the store differ based on day of the week
# Since the males versus females walking in to the store will differ based on day of the week,so further actions to be performed
#where P-vale < 0.05 accept alternative hypothesis