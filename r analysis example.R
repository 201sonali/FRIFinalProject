setwd("/Users/Sonali/Desktop/biofuels") #setting my working directory

?setwd

Beauty <- read.csv("Beauty.csv")
v <- c(1,2,3,4,5,6,7,8)
m.v1 <- matrix(v, ncol = 4)
m.v2 <- matrix(v, ncol = 4, byrow=TRUE)
v <- c(1,2,3,4,5,6,7,8)
m.v2 <- matrix(v, ncol = 4, byrow=TRUE)
v <- c(1,2,3,4,5,6,7,8)
m.v2 <- matrix(v, ncol = 4, byrow=TRUE)
?matrix 
m.v1
rm(m.v1)
m.v1
ls()
rm(list = ls())
ls()
beauty <- read.csv(Beauty.csv)
beauty <- read.csv("Beauty.csv")
setwd("/Users/Sonali/Desktop/biofuels")
beauty <- read.csv("Beauty.csv")
write.csv(m.v3, file = 'MyFirstMatrix.csv')
v <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
m.v3 <- matrix(v, ncol = 5, nrow = 3, byrow=TRUE)
write.csv(m.v3, file = 'MyFirstMatrix.csv')
write.csv(m.v3, file = 'MyFirstMatrix.csv')
m.v3 [1,]
m.v3 [3,]
m.v3 [,3]
m.v3 [3,3:5]
m.v3 [1:2,3:5]
names(beauty)
names(beauty)[18:47]
head(beauty)
beauty$minority[366]
which (beauty $ minority ==1)
which (beauty $ minority != 1)
which (beauty $ profevaluation > 4)
m.v3 [2,4]
minorities <- which (beauty $ minority == 1 & beauty $ age > 50)
beauty [minorities,]
which (beauty $ minority ==1)
minorities <- which (beauty $ minority == 1 & beauty $ age > 50)
beauty [minorities,]
elders <- which (beauty $ age > 50 & beauty $ minority == 1)
beauty [elders,]
age.order <- order (beauty $ age)
age.order
beauty.ageSorted <- beauty[age.order,]
head(beauty.ageSorted)
x <- 5
v+x
v[1]+x
mean (v)
median (v)
var (v)
sd (v)
sd(v)/sqrt(length(v))
mean(beauty$age, rm.na=TRUE)
var(beauty$age)
sd(beauty$age)
sd(beauty$age)/sqrt(length(beauty$age))
max(beauty$age)
min(beauty$age)
range(beauty$age)
install.packages ('spuRs')
library('spuRs')
data (ufc)
print (ufc)
head (ufc)
pairs (ufc)
plot (ufc $ plot, ufc $ tree)
plot (ufc $ plot, ufc $ tree, xlab = 'Plot', ylab = 'Species', main = 'Plot vs Tree')
plot (ufc $ plot, ufc $ tree, xlab = 'Plot', ylab = 'Species', main = 'Plot vs Tree', pch=16)
plot (ufc $ plot, ufc $ tree, xlab = 'Plot', ylab = 'Species', main = 'Plot vs Tree', pch=12)
plot (ufc $ plot, ufc $ tree, xlab = 'Plot', ylab = 'Species', main = 'Plot vs Tree', pch=22)
plot (ufc $ plot, ufc $ tree, xlab = 'Plot', ylab = 'Species', main = 'Plot vs Tree', pch=16, col='green')

install.packages("psych")
library("psych")

describe(ufc)
describeBy(ufc, group = "species")

min.yes <- which(beauty$minority ==1)
min.no <- which(beauty$minority != 1)

t.test (beauty$age [min.yes], beauty$age [min.no])


boxplot(beauty$age[min.yes], beauty$age[min.no])

t.test (beauty$beautyflowerdiv, beauty$beautymlowerdiv)

t.test (beauty$beautyflowerdiv, beauty$beautymlowerdiv)

boxplot (beauty$beautyflowerdiv, beauty$beautymlowerdiv)

boxplot (beauty$beautyflowerdiv, beauty$beautymlowerdiv, notch=TRUE)

'pairs(ufc)'
lm (ufc$height.m ~ ufc$dbh.cm)
summary (lm (ufc$height.m ~ ufc$dbh.cm))

plot (ufc$height.m ~ ufc$dbh.cm)
abline (lm(ufc$dbh.cm~ufc$height.m))

mydata <- read.csv ("Tolerance.csv")

setwd("/Users/Sonali/Desktop/biofuels")

mydata <- read.csv ("Tolerance.csv")
names(mydata)
head(mydata)

sex <- mydata$Sex 
sulfide <- mydata$Sulfide 
survival <- mydata$Time.to.loss.of.equilibrium 
fit <- aov (survival ~ sex*sulfide, data = mydata) 
summary (fit)


boxplot(survival~Sex*Sulfide, data=mydata)

traits <- read.csv("Phys lab 2018_small vs large.csv")
describeBy(traits, group = "C1")

setwd("/Users/Sonali/Desktop/biofuels")
data <- read.csv ("Phys lab 2018_wet vs dry.csv”")

setwd("/Users/Sonali/Desktop/biofuels")
data <- read.csv ("Phys lab 2018_wet vs dry-2.csv")

library("spuRS")
install.packages('spuRs')
library("spuRs")
install.packages('psych')
library("psych")

names(data)

plot(data)

t.test (data$SoilMoist[2:46], data$SoilMoist[47:91])

t.test (data$SoilMoist, data$Treatment)
t.test(data$SoilMoist,data$Treatment=="wet", data$SoilMoist,data$Treatment=="dry")

min.yes <- which(data$Treatment =="wet") 
min.no <- which(data$Treatment =="dry")

t.test (data$SoilMoist [min.yes], data$SoilMoist [min.no])

boxplot(data$SoilMoist [min.yes], data$SoilMoist [min.no])

t.test (data$SoilMoist[1:45], data$SoilMoist[46:90])

t.test (data$SPAD [min.yes], data$SPAD [min.no])
boxplot(data$SPAD [min.yes], data$SPAD [min.no])

t.test (data$RWC [min.yes], data$RWC [min.no])
t.test (data$WP [min.yes], data$WP [min.no])
t.test (data$OP [min.yes], data$OP [min.no])

boxplot(data$RWC [min.yes], data$RWC [min.no])
boxplot(data$WP [min.yes], data$WP [min.no])
boxplot(data$OP [min.yes], data$OP [min.no])

t.test (data$Photo [min.yes], data$Photo [min.no])
t.test (data$Cond [min.yes], data$Cond [min.no])
t.test (data$Trans [min.yes], data$Trans [min.no])
t.test (data$Ci [min.yes], data$Ci [min.no])

boxplot (data$Photo [min.yes], data$Photo [min.no])
boxplot (data$Cond [min.yes], data$Cond [min.no])
boxplot (data$Trans [min.yes], data$Trans [min.no])
boxplot (data$Ci [min.yes], data$Ci [min.no])


plot (data$Photo ~ data$Cond)
summary(lm (data$Photo ~ data$Cond))

plot (data$Photo ~ data$Trans)
summary(lm (data$Photo ~ data$Trans))

plot (data$Photo ~ data$Ci)
summary(lm (data$Photo ~ data$Ci))

plot (data$Cond ~ data$Trans)
summary(lm (data$Cond ~ data$Trans))

plot (data$Cond ~ data$Ci)
summary(lm (data$Cond ~ data$Ci))

plot (data$Trans ~ data$Ci)
summary(lm (data$Trans ~ data$Ci))


size <- data$Size 
treatment <- data$Treatment 
soil <- data$SoilMoist
fit <- aov (soil ~ size*treatment, data = data) 
summary (fit)

boxplot (soil ~ size*treatment, data = data)

size <- data$Size 
treatment <- data$Treatment 
photo <- data$Photo
fit <- aov (photo ~ size*treatment, data = data) 
summary (fit)

boxplot (photo ~ size*treatment, data = data)

setwd("/Users/Sonali/Desktop/biofuels")
data <- read.csv ("elevated carbon.csv")

names(data)

plot(data)

t.test (data$Carbon, data$"Plant diameter 3 mm"")
t.test(data$Carbon,data$"Plant diameter 3 mm"=="wet", data$SoilMoist,data$Treatment=="dry") 


