# ADMISSION-PREDICTING-DATA-ANALYSIS-

library(tidyverse)
list.files(path = "F:\\SEM V\\DM\\SEM-V Project")

df <- read.csv('F:\\SEM V\\DM\\SEM-V Project\\Admission_Predict.csv')
df1 <- read.csv('F:\\SEM V\\DM\\SEM-V Project\\Admission_Predict_Ver1.1.csv')

head(df)
head(df1)
str(df)
str(df1)
summary(df)
summary(df1)



df$University.Rating <- factor(df$University.Rating)
df$Research <- factor(df$Research)

#str(df)

any(is.na(df))

colnames(df)

library(ggplot2)
pl <- ggplot(df, aes(Research, color='black', fill='white'))
pl + geom_bar()


#STUDY CORRELATION AMONG NUMERIC COLUMNS
num.cols <- sapply(df, is.numeric)
num.cols
cors <-cor(df[,num.cols])


#VISUALIZE CORRELATION
library(corrplot)
corrplot(cors)

#SCATTER PLOT OF RESEARCH VS CHANCE OF ADMISSION
pl <- ggplot(df, aes(Chance.of.Admit, TOEFL.Score))
pl + geom_point(aes(color=factor(Research), alpha=0.5, size=2.0)) + theme_bw()


summary(df$TOEFL.Score)

pl <- ggplot(df, aes(Chance.of.Admit,GRE.Score, color=factor(University.Rating), alpha=0.5, size=1.5))
pl + geom_point() + theme_classic() + ggtitle('GRE SCORE VS CHANCE OF ADMISSION')

library(caTools)
#TRAIN MODEL
sample <- sample.split(df$GRE.Score, SplitRatio = 0.7)
train <- subset(df, sample == T)
test <- subset(df, sample == F)
colnames(df)
model <- lm(Chance.of.Admit ~ GRE.Score + TOEFL.Score + University.Rating + SOP + LOR + CGPA + Research, data = train)
summary(model)


predicted <- predict(model, test)
df2 <- data.frame(predicted, test$Chance.of.Admit)
head(df2)
