# Salt experiment analysis 2018

setwd("C:\\Users\\Sonali Singh\\Documents\\biofuels\\r project")

mydata <- read.csv("SaltData.csv")
mydata

FILonly <- read.csv("FILonly.csv")
FILonly

HALonly <- read.csv("HALonly.csv")
HALonly

all <- read.csv("AllVarieties.csv")
all

install.packages('psych')
library(psych)

################################### using all data ###################################

describe(mydata)
describeBy(mydata, group = "Treatment")
describeBy(mydata, group = c("Treatment","Subspecies"))
describeBy(mydata, group = "Subspecies")

level1 <- which(mydata$Treatment == "150mM")
level2 <- which(mydata$Treatment == "200mM")
level3 <- which(mydata$Treatment == "250mM")
control <- which(mydata$Treatment == "Control")

summary(aov(WP ~ Treatment, data = mydata))
boxplot(WP ~ Treatment, data = mydata, xlab="Treatment Group", ylab="Water Potential (MPa)")

summary(aov(RWC ~ Treatment, data = mydata))
boxplot(RWC ~ Treatment, data = mydata, xlab="Treatment Group", ylab="RWC (%)")

summary(aov(Total_Chlor ~ Treatment, data = mydata))
boxplot(Total_Chlor ~ Treatment, data = mydata, xlab="Treatment Group", ylab="Total Chlorophyll (mg/g)")

summary(aov(Shoot_Mass ~ Treatment, data = mydata))
boxplot(Shoot_Mass ~ Treatment, data = mydata, xlab="Treatment Group", ylab="Shoot Mass (g)")

summary(aov(Osmolarity ~ Treatment, data = mydata))
boxplot(Osmolarity ~ Treatment, data = mydata, xlab="Treatment Group", ylab="Osmotic Potential (mmol/kg)")

t.test(mydata$WP[control], mydata$WP[level1])
t.test(mydata$WP[control], mydata$WP[level2])
t.test(mydata$WP[control], mydata$WP[level3])

t.test(mydata$Shoot_Mass[control], mydata$Shoot_Mass[level1])
t.test(mydata$Shoot_Mass[control], mydata$Shoot_Mass[level2])
t.test(mydata$Shoot_Mass[control], mydata$Shoot_Mass[level3])

t.test(mydata$RWC[control], mydata$RWC[level1])
t.test(mydata$RWC[control], mydata$RWC[level2])
t.test(mydata$RWC[control], mydata$RWC[level3])

t.test(mydata$Total_Chlor[control], mydata$Total_Chlor[level1])
t.test(mydata$Total_Chlor[control], mydata$Total_Chlor[level2])
t.test(mydata$Total_Chlor[control], mydata$Total_Chlor[level3])

t.test(mydata$Osmolarity[control], mydata$Osmolarity[level1])
t.test(mydata$Osmolarity[control], mydata$Osmolarity[level2])
t.test(mydata$Osmolarity[control], mydata$Osmolarity[level3])

################################## FIL only analysis ###################################

FILonly

level1 <- which(FILonly$Treatment == "150mM")
level2 <- which(FILonly$Treatment == "200mM")
level3 <- which(FILonly$Treatment == "250mM")
control <- which(FILonly$Treatment == "Control")

boxplot(WP ~ Treatment, data = FILonly, xlab="Treatment Group", ylab="Water Potential (MPa)")

boxplot(RWC ~ Treatment, data = FILonly, xlab="Treatment Group", ylab="RWC (%)")

boxplot(Total_Chlor ~ Treatment, data = FILonly, xlab="Treatment Group", ylab="Total Chlorophyll (mg/g)")

boxplot(Shoot_Mass ~ Treatment, data = FILonly, xlab="Treatment Group", ylab="Shoot Mass (g)")

boxplot(Osmolarity ~ Treatment, data = FILonly, xlab="Treatment Group", ylab="Osmotic Potential (mmol/kg)")

t.test(FILonly$WP[control], FILonly$WP[level1])
summary(aov(WP ~ Treatment, data = FILonly))

t.test(FILonly$Shoot_Mass[control], FILonly$Shoot_Mass[level1])
summary(aov(Shoot_Mass ~ Treatment, data = FILonly))

t.test(FILonly$RWC[control], FILonly$RWC[level1])
summary(aov(RWC ~ Treatment, data = FILonly))

t.test(FILonly$Total_Chlor[control], FILonly$Total_Chlor[level1])
summary(aov(Total_Chlor ~ Treatment, data = FILonly))

t.test(FILonly$Osmolarity[control], FILonly$Osmolarity[level1])
summary(aov(Osmolarity ~ Treatment, data = FILonly))


################### Compare all varieties in control and 150mM only ###################

FIL <- which(all$Subspecies == "FIL")
FIL
HAL <- which(all$Subspecies == "HAL")
HAL

t.test(all$WP[FIL], all$WP[HAL])
summary(aov(WP ~ Treat_SS, data = all))
boxplot(WP ~ Treat_SS, data = all, xlab="Treatment Group", ylab="Water Potential (MPa)")

t.test(all$Shoot_Mass[FIL], all$Shoot_Mass[HAL])
summary(aov(Shoot_Mass ~ Treat_SS, data = all))
boxplot(Shoot_Mass ~ Treat_SS, data = all, xlab="Treatment Group", ylab="Shoot Mass (g)")

t.test(all$RWC[FIL], all$RWC[HAL])
summary(aov(RWC ~ Treat_SS, data = all))
boxplot(RWC ~ Treat_SS, data = all, xlab="Treatment Group", ylab="RWC (%)")

t.test(all$Total_Chlor[FIL], all$Total_Chlor[HAL])
summary(aov(Total_Chlor ~ Treat_SS, data = all))
boxplot(Total_Chlor ~ Treat_SS, data = all, xlab="Treatment Group", ylab="Total Chlorophyll (mg/g)")

t.test(all$Osmolarity[FIL], all$Osmolarity[HAL])
summary(aov(Osmolarity ~ Treat_SS, data = all))
boxplot(Osmolarity ~ Treat_SS, data = all, xlab="Treatment Group", ylab="Osmotic Potential (mmol/kg)")

HAL1 <- which(all$Treat_SS == "Control_HAL")
FIL1 <- which(all$Treat_SS == "Control_FIL")

t.test(all$WP[HAL1], all$WP[FIL1])
t.test(all$RWC[HAL1], all$RWC[FIL1])
t.test(all$Shoot_Mass[HAL1], all$Shoot_Mass[FIL1])
t.test(all$Total_Chlor[HAL1], all$Total_Chlor[FIL1])
t.test(all$Osmolarity[HAL1], all$Osmolarity[FIL1])

HAL2 <- which(all$Treat_SS == "150mM_HAL")
FIL2 <- which(all$Treat_SS == "150mM_FIL")

t.test(all$WP[HAL2], all$WP[FIL2])
t.test(all$RWC[HAL2], all$RWC[FIL2])
t.test(all$Shoot_Mass[HAL2], all$Shoot_Mass[FIL2])
t.test(all$Total_Chlor[HAL2], all$Total_Chlor[FIL2])
t.test(all$Osmolarity[HAL2], all$Osmolarity[FIL2])

################################## HAL only analysis ###################################

HALonly
level2 <- which(HALonly$Treatment == "150mM")
level1 <- which(HALonly$Treatment == "Control")

t.test(HALonly$WP[level1], HALonly$WP[level2])
summary(aov(WP ~ Treat_Var, data = HALonly))
boxplot(WP ~ Treat_Var, data = HALonly, xlab="Treatment Group", ylab="Water Potential (MPa)", col=c("pink2","steelblue4","palegreen4"))

t.test(HALonly$RWC[level1], HALonly$RWC[level2])
summary(aov(RWC ~ Treat_Var, data = HALonly))
boxplot(RWC ~ Treat_Var, data = HALonly, xlab="Treatment Group", ylab="RWC (%)", col=c("pink2","steelblue4","palegreen4"))

t.test(HALonly$Shoot_Mass[level1], HALonly$Shoot_Mass[level2])
summary(aov(Shoot_Mass ~ Treat_Var, data = HALonly))
boxplot(Shoot_Mass ~ Treat_Var, data = HALonly, xlab="Treatment Group", ylab="Shoot Mass (g)", col=c("pink2","steelblue4","palegreen4"))

t.test(HALonly$Total_Chlor[level1], HALonly$Total_Chlor[level2])
summary(aov(Total_Chlor ~ Treat_Var, data = HALonly))
boxplot(Total_Chlor ~ Treat_Var, data = HALonly, xlab="Treatment Group", ylab="Total Chlorophyll (mg/g)", col=c("pink2","steelblue4","palegreen4"))

t.test(HALonly$Osmolarity[level1], HALonly$Osmolarity[level2])
summary(aov(Osmolarity ~ Treat_Var, data = HALonly))
boxplot(Osmolarity ~ Treat_Var, data = HALonly, xlab="Treatment Group", ylab="Osmotic Potential (mmol/kg)", col=c("pink2","steelblue4","palegreen4"))
