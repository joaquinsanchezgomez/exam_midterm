#title: "exam 1"
#Name: Joaquin Sanchez Gomez
---
  
  ### Econ B2000, Statistics and Introduction to Econometrics
  ### Kevin R Foster, Colin Powell School, the City College of New York, CUNY
  ### Oct 14 2021
  
  
#First Question
  In this section I only decided to consider the people who said they were vaccinated or not; 
I left out the unanswered observations, because they are a low proportion and do not detract 
from the significance of the analysis. People with the highest vaccination rate are found in 
the west of the country; Among people with less education, 79% have been vaccinated, while people with 
advanced degrees reach about 95%. To a lesser extent, this pattern is replicated for the rest of the regions. 
Obviously the educational level is associated with the decision to get vaccinated. 
As part of a deeper analysis, I would choose to normalize the set of people vaccinated by region. 
To do this, would have to assign values ​​to each category. 
One way to do it would be: less than high school "1", High school Diploma "2", Some college "3", Associate degree "4", Bachelor degree "5" and advanced degree "6". 
So the question would be what each region would be: What is the probability of 1, 2 ..., n?
  
#Second Question
  This exercise is very similar to the previous one. In this regard, it shows us the percentage 
of people who are vaccinated according to their gender. According to the information, 
transgender people are the ones who most choose to be vaccinated, followed by women and - lastly - men, 
who have a lower proportion.

  
  
#Third Question

library(tidyverse)

data_base <- Household_Pulse_data%>%mutate(
    INCOME1=as.numeric(INCOME),INCOME1=case_when(INCOME1==1~NA_integer_,TRUE~as.integer(INCOME1)))
  
data_base <- Household_Pulse_data%>%mutate(
  ANX1=as.numeric(ANXIOUS), ANX1=case_when(ANX1==1~NA_integer_,TRUE~as.integer(ANX1)))


data_base <- Household_Pulse_data%>%mutate(
  SEXO=as.numeric(SEXUAL_ORIENTATION), SEXO=case_when(SEXO==1~NA_integer_,TRUE~as.integer(SEXO)))
  

norm_varb <- function(X_in) {
  (X_in - min(X_in, na.rm = TRUE))/( max(X_in, na.rm = TRUE) - min(X_in, na.rm = TRUE) )
}


norm_sex <- norm_varb(SEXO)
norm_inc <- norm_varb(INCOME1)

data_use_prelim <- tibble(norm_sex,norm_inc)
data_use_prelim <- as.data.frame(data_use_prelim)

good_obs_data_use <- complete.cases(data_use_prelim,ANX1)
dat_use <- subset(data_use_prelim,good_obs_data_use)
y_use <- subset(ANX1,good_obs_data_use)

set.seed(12345)
NN_obs <- sum(good_obs_data_use == 1)
select1 <- (runif(NN_obs) < 0.8)
train_data <- subset(dat_use,select1)
test_data <- subset(dat_use,(!select1))
cl_data <- y_use[select1]
true_data <- y_use[!select1]


summary(cl_data)
prop.table(summary(cl_data))
summary(train_data)
require(class)
for (indx in seq(1, 9, by= 2)) {
  pred_anxiety <- knn(train_data, test_data, cl_data, k = indx, l = 0, prob = FALSE, use.all = TRUE)
  num_correct_labels <- sum(pred_anxiety == true_data)
  correct_rate <- num_correct_labels/length(true_data)
  print(c(indx,correct_rate))
