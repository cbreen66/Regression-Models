data(mtcars)
mtcars$am <- ifelse(mtcars$am == 1, c("manual"), c("automatic"))
mtcars$am <- as.factor(mtcars$am)

t.test(mpg~am, data=mtcars)
boxplot(mpg~am, data=mtcars)

t.test(wt~am, data=mtcars)

Allin <- lm(mpg ~ ., data=mtcars)
summary(Allin)
library(car)
vif(Allin)

Model1 <- step(Allin, k=log(nrow(mtcars)))
summary(Model1)

fit1 <- lm(mpg ~ wt, data = mtcars)
fit2 <- update(fit1, mpg ~ wt + qsec)
fit3 <- update(fit2, mpg ~ wt + qsec + am)
anova(fit1, fit2, fit3)

summary(Model1)
vif(Model1)
mean(Model1$residuals)
par(mfrow=c(2,2)); plot(Model1)

library(ggplot2)
g <- ggplot(mtcars, aes(mpg, wt))
g + geom_point(colour="blue") + facet_grid(.~am)

h <- ggplot(mtcars, aes(mpg, qsec))
h + geom_point(colour="red") + facet_grid(.~am)