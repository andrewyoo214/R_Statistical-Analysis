################################################################################
##################### EEG Brainwave with Classical Music #######################

## Dataset: Processed EEG brainwave dataset from 20 different subjects. 
## Classical Music: Rite of Spring (by Igor Stravinsky)




################################################################################

library(ggplot2)
library(dplyr)
library(psych)

getwd()

################################################################################
################################################################################

## Basic data preprocessing

data <- read.csv("./data/EEG_data_summary.csv")  # original data


head(data)
colnames(data)[1] <- "sub"
colnames(data)


## Factorizing categorical variables
data$sub <- as.factor(data$sub)
data$familiar <- as.factor(data$familiar)
data$gender <- as.factor(data$gender)
data$channel <- as.factor(data$channel)

str(data)



## Adding new variables
# 1. New column for Column to recognize AR or NonAR
new_familiar <- ifelse(data$score<=3, "nf", "f")
data$familiar_n <- new_familiar
data$familiar_n <- as.factor(data$familiar_n)

str(data) 

################################################################################
################################################################################

## what if delete the recognition score 3 participants?
strict_familiar <- ifelse(data$score<3, "NF", ifelse(data$score>3, "F", "NONE"))
data$familiar_s <- strict_familiar
data$familiar_s <- as.factor(data$familiar_s)

strict_data = data[data$familiar== "NF" | data$familiar_s=="F", ]

################################################################################
################################################################################


# 2. New columns for 
# Part 1: (section 1~section3)  / Part 2: (section4~section6) / Part3: (section7)
# Part 4: (section 8~section10) / Part 5: (section11~section12)
data <- data %>% 
  mutate(RA_part1_ = (RA_s1_+RA_s2_+RA_s3_)/3,
         RA_part2_ = (RA_s4_+RA_s5_+RA_s6_)/3,
         RA_part3_ = (RA_s7_),
         RA_part4_ = (RA_s8_+RA_s9_+RA_s10_)/3,
         RA_part5_ = (RA_s11_+RA_s12_)/2,
         
         RB_part1_ = (RB_s1_+RB_s2_+RB_s3_)/3,
         RB_part2_ = (RB_s4_+RB_s5_+RB_s6_)/3,
         RB_part3_ = (RB_s7_),
         RB_part4_ = (RB_s8_+RB_s9_+RB_s10_)/3,
         RB_part5_ = (RB_s11_+RB_s12_)/2,
         
         RT_part1_ = (RT_s1_+RT_s2_+RT_s3_)/3,
         RT_part2_ = (RT_s4_+RT_s5_+RT_s6_)/3,
         RT_part3_ = (RT_s7_),
         RT_part4_ = (RT_s8_+RT_s9_+RT_s10_)/3,
         RT_part5_ = (RT_s11_+RT_s12_)/2,
         
         RG_part1_ = (RG_s1_+RG_s2_+RG_s3_)/3,
         RG_part2_ = (RG_s4_+RG_s5_+RG_s6_)/3,
         RG_part3_ = (RG_s7_),
         RG_part4_ = (RG_s8_+RG_s9_+RG_s10_)/3,
         RG_part5_ = (RG_s11_+RG_s12_)/2,
         
         RSMR_part1_ = (RSMR_s1_+RSMR_s2_+RSMR_s3_)/3,
         RSMR_part2_ = (RSMR_s4_+RSMR_s5_+RSMR_s6_)/3,
         RSMR_part3_ = (RSMR_s7_),
         RSMR_part4_ = (RSMR_s8_+RSMR_s9_+RSMR_s10_)/3,
         RSMR_part5_ = (RSMR_s11_+RSMR_s12_)/2
         )



head(data)





## Data split
ch1_data = data[data$channel==1, ]
ch2_data = data[data$channel==2, ]
ch3_data = data[data$channel==3, ]
ch4_data = data[data$channel==4, ]
ch5_data = data[data$channel==5, ]
ch6_data = data[data$channel==6, ]
ch7_data = data[data$channel==7, ]
ch8_data = data[data$channel==8, ]
ch9_data = data[data$channel==9, ]
ch10_data = data[data$channel==10, ]
ch11_data = data[data$channel==11, ]
ch12_data = data[data$channel==12, ]
ch13_data = data[data$channel==13, ]
ch14_data = data[data$channel==14, ]
ch15_data = data[data$channel==15, ]
ch16_data = data[data$channel==16, ]
ch17_data = data[data$channel==17, ]
ch18_data = data[data$channel==18, ]
ch19_data = data[data$channel==19, ]



################################################################################
################################################################################

### Analysis ideas / Research hypothesis

# 1. Brainwave differences between familiar / unfamiliar group 
#    (by channel, overall RA, RB, RT, RG, RSMR comparison)
# 2. Brainwave differences between familiar / unfamiliar group
#    (by classical music section (section 1~12))









################################################################################
################################################################################

## Independent sample T-TEST

channel_data = ch1_data
# channel_data = ch2_data
# channel_data = ch3_data
# channel_data = ch4_data
# channel_data = ch5_data
# channel_data = ch6_data
# channel_data = ch7_data
# channel_data = ch8_data
# channel_data = ch9_data
# channel_data = ch10_data
# channel_data = ch11_data
# channel_data = ch12_data
# channel_data = ch13_data
# channel_data = ch14_data
# channel_data = ch15_data
# channel_data = ch16_data
# channel_data = ch17_data
# channel_data = ch18_data
# channel_data = ch19_data


## with original familiar group division (1,2 = NF, 3,4,5 = F)

# Overall brainwave comparison (Total section Average)
var.test(channel_data$RA_T, channel_data$RA_T)
t.test(data = channel_data, RA_T ~ familiar, var.equal=TRUE)
t.test(data = channel_data, RB_T ~ familiar, var.equal=TRUE)
t.test(data = channel_data, RT_T ~ familiar, var.equal=TRUE)
t.test(data = channel_data, RG_T ~ familiar, var.equal=TRUE)
t.test(data = channel_data, RSMR ~ familiar, var.equal=TRUE)


# Part 1: section 1, section 2, section 3
var.test(channel_data$RA_part1_, channel_data$RA_part1_)
t.test(data = channel_data, RA_part1_ ~ familiar, var.equal=TRUE)
t.test(data = channel_data, RB_part1_ ~ familiar, var.equal=TRUE)
t.test(data = channel_data, RT_part1_ ~ familiar, var.equal=TRUE)
t.test(data = channel_data, RG_part1_ ~ familiar, var.equal=TRUE)
t.test(data = channel_data, RSMR_part1_ ~ familiar, var.equal=TRUE)


# Part 2: section 4, section 5, section 6
var.test(channel_data$RA_part2_, channel_data$RA_part2_)
t.test(data = channel_data, RA_part2_ ~ familiar, var.equal=TRUE)
t.test(data = channel_data, RB_part2_ ~ familiar, var.equal=TRUE)
t.test(data = channel_data, RT_part2_ ~ familiar, var.equal=TRUE)
t.test(data = channel_data, RG_part2_ ~ familiar, var.equal=TRUE)
t.test(data = channel_data, RSMR_part2_ ~ familiar, var.equal=TRUE)


# Part 3: section 7
var.test(channel_data$RA_part3_, channel_data$RA_part3_)
t.test(data = channel_data, RA_part3_ ~ familiar, var.equal=TRUE)
t.test(data = channel_data, RB_part3_ ~ familiar, var.equal=TRUE)
t.test(data = channel_data, RT_part3_ ~ familiar, var.equal=TRUE)
t.test(data = channel_data, RG_part3_ ~ familiar, var.equal=TRUE)
t.test(data = channel_data, RSMR_part3_ ~ familiar, var.equal=TRUE)


# Part 4: section 8, section 9, section 10
var.test(channel_data$RA_part4_, channel_data$RA_part4_)
t.test(data = channel_data, RA_part4_ ~ familiar, var.equal=TRUE)
t.test(data = channel_data, RB_part4_ ~ familiar, var.equal=TRUE)
t.test(data = channel_data, RT_part4_ ~ familiar, var.equal=TRUE)
t.test(data = channel_data, RG_part4_ ~ familiar, var.equal=TRUE)
t.test(data = channel_data, RSMR_part4_ ~ familiar, var.equal=TRUE)


# Part 5: section 11, section 12
var.test(channel_data$RA_part5_, channel_data$RA_part5_)
t.test(data = channel_data, RA_part5_ ~ familiar, var.equal=TRUE)
t.test(data = channel_data, RB_part5_ ~ familiar, var.equal=TRUE)
t.test(data = channel_data, RT_part5_ ~ familiar, var.equal=TRUE)
t.test(data = channel_data, RG_part5_ ~ familiar, var.equal=TRUE)
t.test(data = channel_data, RSMR_part5_ ~ familiar, var.equal=TRUE)






################################################################################
################################################################################

## One-way ANOVA test for each variable
# test brainwaves from RA_T to RSMR_part5_

result_eeg <-aov(RA_part5_ ~ familiar, data = data)
anova(result_eeg)
result_eeg <-aov(RB_part5_ ~ familiar, data = data)
anova(result_eeg)
result_eeg <-aov(RT_part5_ ~ familiar, data = data)
anova(result_eeg)
result_eeg <-aov(RG_part5_ ~ familiar, data = data)
anova(result_eeg)
result_eeg <-aov(RSMR_part5_ ~ familiar, data = data)
anova(result_eeg)













################################################################################

## Two-way ANOVA test for both variables
result_both <- aov(RA_T ~ channel + familiar, data = data)
anova(result_both)


## TukeyHSD test for multiple comparisons - post hoc test
TukeyHSD(result_eeg, conf.level=0.95)
TukeyHSD(result_both, conf.level=0.95)


par(mar=c(3.1, 9, 4.1, 2.1))
plot(TukeyHSD(result_eeg, conf.level = 0.95), las=2)




################################################################################

## Simple check for the box plot and interaction plot
boxplot(RA_T ~ familiar, data=data)

## MEAN & SD VALUE COMPARISON ##
ggplot(data, aes(x = familiar, y = RA_T, fill = channel)) + geom_boxplot()










