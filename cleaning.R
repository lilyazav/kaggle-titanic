library(ggplot2)
library(dplyr)

train <- read.csv("./data/train.csv")

train$Cabin = replace(train$Cabin, train$Cabin == "", NA)