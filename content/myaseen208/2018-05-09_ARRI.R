##----reg.df1----
# Fertilizer (Kg/acre)
# Production (Bushels/acre)
Fertilizer <- c(100, 200, 400, 500)
Production <- c( 70,  70,  80, 100)
df1        <- data.frame(Fertilizer, Production)
df1


##----reg.p1----
library(ggplot2)
# if (!require("ggplot2")) install.packages("ggplot2")
p1 <- 
  ggplot(data = df1, mapping = aes(x = Fertilizer, y = Production)) + 
  geom_point() +
  labs(x = "Fertilizer", y = "Production") +
  theme_bw()
print(p1)


##----reg.fm1----
fm1 <- lm(formula = Production ~ Fertilizer, data = df1)
summary(fm1)

##----reg.fm1anova----
anova(fm1)


##----reg.df2----
Fert  <- c(0.3, 0.6, 0.9, 1.2, 1.5, 1.8, 2.1, 2.4)
Yield <- c(10, 15, 30, 35, 25, 30, 50, 45)
df2   <- data.frame(Fert, Yield)
df2

##----reg.p2----
library(ggplot2)
# if (!require("ggplot2")) install.packages("ggplot2")
p2 <- 
  ggplot(data = df2, mapping = aes(x = Fert, y = Yield)) + 
  geom_point() +
  labs(x = "Fertilizer", y = "Yield") +
  theme_bw()
print(p2)

##----reg.fm2----
fm2 <- lm(formula = Yield ~ Fert, data = df2)
summary(fm2)

##----reg.fm2anova----
anova(fm2)

##----reg.df3----
# Fertilizer (Kg)
# Rainfall (mm)
# Yield (Kg)
Fertilizer <- seq(from = 100, to = 700, by = 100)
Rainfall   <- c(10, 20, 10, 30, 20, 20, 30)
Yield      <- c(40, 50, 50, 70, 65, 65, 80)
df3        <- data.frame(Fertilizer, Rainfall, Yield)
df3

##----reg.p3----
library(car)
car::scatter3d(formula = Yield ~ Fertilizer + Rainfall, data = df3)

##----reg.fm3----
fm3 <- lm(formula = Yield ~ Fertilizer + Rainfall, data = df3)
summary(fm3)

##----reg.fm3anova----
anova(fm3)

##----reg.fm3sbeta----
library(lm.beta)
# if (!require("lm.beta")) install.packages("lm.beta")
lm.beta(fm3)

##----reg.df4----
Nitrogen <- c(0, 0, 10, 10, 20, 20)
Yield    <- c(5, 7, 15, 17,  9, 11)
df4      <- data.frame(Nitrogen, Yield)
df4


##----reg.p4----
library(ggplot2)
# if (!require("ggplot2")) install.packages("ggplot2")
p4 <- 
  ggplot(data = df4, mapping = aes(x = Nitrogen, y = Yield)) + 
  geom_point() +
  labs(x = "Nitrogen", y = "Yield") +
  theme_bw()
print(p4)


##----reg.fm4----
fm4 <- lm(formula = Yield ~ Nitrogen + I(Nitrogen^2), data = df4)
# fm4 <- lm(formula = Yield ~ poly(x = Nitrogen, degree = 2, raw = TRUE), data = df4)
summary(fm4)

##----reg.fm4anova----
anova(fm4)

