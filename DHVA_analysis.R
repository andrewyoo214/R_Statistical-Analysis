################################################################################
################# Digital Healthcare Virtual Agent Research ####################

## Dataset: Responses from after DHVA experiments questionnaires.
## 




################################################################################

library(ggplot2)
library(dplyr)
library(psych)

getwd()

################################################################################
################################################################################

## Basic data preprocessing
data <- read.csv("./data/AR_response.csv")  # original data
data <- read.csv("./data/AR_response2.csv") # participant 1~3 edited
data <- read.csv("./data/AR_response2_adjusted.csv") # participant 1~3 edited + data adjusted
data <- read.csv("./data/AR_response2_adjusted_copy.csv") # participant 1~3 edited + data adjusted + n




head(data)
colnames(data)[1] <- "Group"
colnames(data)

## Factorizing categorical variables
data$Group <- as.factor(data$Group)
data$Age <- as.factor(data$Age)
data$Gender <- as.factor(data$Gender)

str(data)

## Adding new variables
# 1. Column to recognize AR or NonAR
AR_cat <- ifelse(data$Group=="AR - Pro"|data$Group=="AR - Zoo", "AR", "NAR")
data$AR <- AR_cat
data$AR <- as.factor(data$AR)

# 2. Column to recognize Pro or Zoo
Zoo_cat <- ifelse(data$Group=="AR - Zoo"|data$Group=="NonAR - Zoo", "Zoo", "Pro")
data$Zoo <- Zoo_cat
data$Zoo <- as.factor(data$Zoo)

# 3. Columns to sum up the questionnaires

# ATT(Attitude), ITU(Intention of Use), PENJ(Perceived Enjoyment), PEOU(Perceived Ease of Use)
# PS(Perceived Sociability), PU(Perceived Usefulness), SP(Social Presence), TRU(Trust)
# ANX(Anxiety), SI(Social Influence)
data <- data %>% 
  mutate(ATT = Q1 + Q2 + Q3, ITU = Q4 + Q5 + Q6, PENJ = Q7 + Q8 + Q9, 
         PEOU = Q10 + Q11 + Q12, PS = Q13 + Q14 + Q15, PU = Q16 + Q17 + Q18,
         SP = Q19 + Q20 + Q21, TRU = Q22 + Q23 + Q24, ANX = Q25 + Q26 + Q27, 
         SI = Q28 + Q29 + Q30)

data <- data %>% 
  mutate(Total = ATT + ITU + PENJ + PEOU + PS + PU + SP + TRU + ANX + SI)

head(data)
summary(data)


################################################################################
## Cronbach Alpha test

head(data)

alpha(data[,4:6])   # Q1~Q3: ATT(Attitude)             
alpha(data[,7:9])   # Q4~Q6: ITU(Intention of Use)            
alpha(data[,10:12]) # Q7~Q9: PENJ(Perceived Enjoyment)
alpha(data[,13:15]) # Q10~Q12: PEOU(Perceived Ease of Use)
alpha(data[,16:18]) # Q13~Q15: PS(Perceived Sociability) - low
alpha(data[,16:17]) # Q13~Q15: PS(Perceived Sociability) --> Q15 droped.
alpha(data[,19:21]) # Q16~Q18: PU(Perceived Usefulness)
alpha(data[,22:24]) # Q19~Q21: SP(Social Presence)
alpha(data[,25:27]) # Q22~Q24: TRU(Trust) - low
alpha(data[,28:30]) # Q25~Q27: ANX(Anxiety)
alpha(data[,31:33]) # Q28~Q30: SI(Social Influence)


################################################################################

## Simple check for the box plot and interaction plot
boxplot(Total ~ Group, data=data)

## MEAN & SD VALUE COMPARISON ##
ggplot(data, aes(x = AR, y = PEOU, fill = Zoo)) + geom_boxplot()

## Interaction Effect
interaction.plot(data$AR, data$Zoo, data$SP, 
                 main = "Interaction Effect on AR & Zoomorphic",
                 xlab = "AR", ylab = "Score", type = "b",
                 pch = c(19, 22), col=c("blue", "red"), legend=TRUE)



# ATT(Attitude), ITU(Intention of Use), PENJ(Perceived Enjoyment), PEOU(Perceived Ease of Use)
# PS(Perceived Sociability), PU(Perceived Usefulness), SP(Social Presence), TRU(Trust)
# ANX(Anxiety), SI(Social Influence)


## One-way ANOVA test for each variable
result_AR <-aov(PEOU ~ AR, data = data)
anova(result_AR)

result_zoo <- aov(PEOU ~ Zoo, data = data)
anova(result_zoo)


## Two-way ANOVA test for both variables
result_both <- aov(SI ~ Zoo + AR, data = data)
anova(result_both)


## TukeyHSD test for multiple comparisons - post hoc test
TukeyHSD(result_both, conf.level=0.95)

par(mar=c(3.1, 9, 4.1, 2.1))
plot(TukeyHSD(result_both, conf.level = 0.95), las=2)




################################################################################
## ANOVA test with interaction effect from variables






## ANOVA test without interaction effect from variables






