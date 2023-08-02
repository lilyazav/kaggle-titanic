library(ggplot2)
library(dplyr)

train <- read.csv("./data/train.csv")

# Notable findings:
# 74.2% of women survived, compared to only 18.9% of men
# Passengers in higher class/with higher fare were more likely to survive

# Exploratory Data Analysis
summary(train)
# check that all rows are unique. length of dataset equals length of unique values
unique(train$PassengerId) %>% length

# Survived. 
# 38.4% survived

# Pclass. Ticket Class. 
# 1st class - 216; 2nd class - 184; 3rd class - 491 
ggplot(train, aes(x=Pclass)) + geom_bar() +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -1)

(survived$Pclass == 1)/count(survived)

# any connection between survival and ticket class?
train %>% group_by(Pclass) %>% summarise(survivalRate = mean(Survived))
# Survival rate is 63% for 1, 47.3% for 2, 24.2% for 3. 

# Sex 
# male, 577, or female, 314
ggplot(train, aes(x=Sex)) + geom_bar()
train %>% group_by(Sex) %>% summarise(survivalRate = mean(Survived))
# Survival rate for female 74.2% (wow!) and 18.9%

# Age
# Mean is 29.7, median 28, so right-skewed distribution
# Min is 0.42 and max is 80, 3rd quart is 38. 
# A pretty young group
# 177 NA's
ggplot(train, aes(x=Age)) + geom_histogram(binwidth=10) 
train %>% group_by(Survived) %>% summarise(avgAge = mean(Age, na.rm=T))
# survivors avg 28.3 years, those that died 30.6

#SibSp - number of siblings/spouses aboard
# mean was 0.52, max was 8!
# Those that died had slightly more siblings/spouses (0.554 on avg) than those that lived (0.474)
train %>% group_by(Survived) %>% summarise(meanSib = mean(SibSp))

# Parch- number of parents/childr aboard
# mean is 0.38, max was 6!
# Those that lived had slightly more parents/ch (0.465) than those that died (0.330)
train %>% group_by(Survived) %>% summarise(meanPar = mean(Parch))

# Ticket - ticket number, I'm assuming we can safely ignore.

# Fare - how much they paid? 
# Survivors paid more (48.4, compared to 22.1)
train %>% group_by(Survived) %>% summarise(avgFare = mean(Fare))

# Cabin is a letter followed by a number. Lots missing values ("")
sum(train$Cabin == "")
# 687 of them are NA

#Embarked
# 2 - "", C - 168, Q - 77, S - 644
ggplot(data = train, aes(x = Embarked)) + geom_bar() +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -2)
# 55.4% of Cherbourg-embarkers survived, compared to 39% of Queenslanders, and 33.7% of Southamptons
train %>% group_by(Embarked) %>% summarise(survived = mean(Survived))
# Cherbourg tended to be higher fare passenger
train %>% group_by(Embarked) %>% summarise(class = mean(Pclass))
  