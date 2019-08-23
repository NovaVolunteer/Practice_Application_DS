library(tidyverse)
library(matchingR)

data <- read_csv("Capstone Ranking Survey_August 23, 2019_13.27.csv")

data <- data[-c(1,2),] %>%
      dplyr::select(starts_with("Q"))

data <- t(data)
students <- data[1,]
data <- data[-1,]
colnames(data) <- students
data <- apply(data, 2, as.numeric)

studentPref <- data
collegeUtils <- matrix(1, ncol(data), nrow(data))

results.studentoptimal = galeShapley.collegeAdmissions(studentPref = studentPref,
                             collegeUtils = collegeUtils,
                             slots = 3,
                             studentOptimal = TRUE)
results.studentoptimal
