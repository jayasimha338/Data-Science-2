bank <- read.table(file.choose(),header = T,sep = ";")
View(bank)
str(bank)
sum(is.na(bank))
bd <- bank
head(bd)
tail(bd)
View(bd)

table(bd$y) # tabular representation of the Y categories

?Mode # learn more about the dataset

bd.y <- multinom(y~.,data=bd)
summary(bd.y)

##### Significance of Regression Coefficients###
z <- summary(bd.y)$coefficients / summary(bd.y)$standard.errors
p_value <- (1-pnorm(abs(z),0,1))*2

summary(bd.y)$coefficients
p_value

# odds ratio 
exp(coef(bd.y))

# predict probabilities
prob <- fitted(bd.y)
prob

# Find the accuracy of the model

class(prob)
prob <- data.frame(prob)
View(prob)
prob["pred"] <- NULL

# Custom function that returns the predicted value based on probability
get_names <- function(i){
  return (names(which.max(i)))
}

pred_name <- apply(prob,1,get_names)
?apply
prob$pred <- pred_name
View(prob)
table(bd$y)
# Confusion matrix
table(pred_name,bd$y)

# confusion matrix visualization
barplot(table(pred_name,bd$y),beside = T,col=c("red","blue"),legend=c("no","yes"),main = "Predicted(X-axis) - Legends(Actual)",ylab ="count")


# Accuracy 
mean(pred_name==bd$y) # 69.31 %
#Error
1-mean(pred_name==bd$y)
