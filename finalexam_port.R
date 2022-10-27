rm(list=ls())
data<- read.csv(file.choose(), header=FALSE)
View(data)

y <- data$V1
x1 <- data$V2
x2 <- data$V3
#1.
reg <- lm(y~x1+x2)
summary(reg)


#Two-Sided T-Test : Y~X2
testx2 <- lm(y~x2)
summary(testx2)
#Test Stat = 11.61, df = 2997
# Critical Value = 1.960756
qt(1-(0.05/2), 2997)
(1-pt(11.61, 2997))*2
#P = 0


#Two-Sided T-Test : Y~X1
testx1 <- lm(y~x1)
summary(testx1)
#Test Stat = 10.585, df = 2997
# Critical Value = 1.960756
qt(1-(0.05/2), 2997)
(1-pt(10.585, 2997))*2
#P = 0

summary(reg)


x2sqr <- data$V3^2
View(data)
#1.
reg2 <- lm(y~x1+x2+x2sqr)
summary(reg2)

#9
x3 <- data$V4
x4 <- data$V5
x5 <- data$V6
x6 <- data$V7
x7 <- data$V8
x8 <- data$V9
x9 <- data$V10
x10 <- data$V11
x11 <- data$V12

reg3 <- lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11)
summary(reg3)






unrestricted <- lm(y~x1+x2+x2sqr)
summary(unrestricted)
restricted <- lm(y~x1+x2)
summary(restricted)
rss.r <- sum(residuals(restricted)^2)
rss.ur <- sum(residuals(unrestricted)^2)
test.stat <- ((rss.r-rss.ur)/1/rss.ur/(2999-4))
CV <- qf((1-0.01), 1, (2999-(3+1)))
p <- (1-pf(test.stat, 1, 2999 -(3+1)))
test.stat
CV
p

#F Test 2
unrestricted <- lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11)
summary(unrestricted)
restricted <- lm(y~x1+x2)
summary(restricted)
rss.r <- sum(residuals(restricted)^2)
rss.ur <- sum(residuals(unrestricted)^2)
test.stat <- ((rss.r-rss.ur)/9/rss.ur/(2999-4))
CV <- qf((1-0.05), 9, (2999-(11+1)))
p <- (1-pf(test.stat, 9, 2999 -(11+1)))
test.stat
CV
p

var(x1)
reg5<- lm(y~x1)
summary(reg5)
plot(x1)
hist(x1)
