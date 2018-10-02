# Employee-Attrition-Problem-Trying-to-Solve-by-merge-two-theories-of-Machine-learning-and-Survival-An
Employee Attrition Problem Trying to Solve by merging of two theories of Machine learning and Survival Analysis and tried to explain prediction of single Variable Explanation by graph 





library(breakDown)
str(HR_data)
names(HR_data)
HR_data<-HR_data[,-c(9)]
HR_data$Work_accident<-as.factor(HR_data$Work_accident)
HR_data$left<-as.numeric(HR_data$left)
HR_data$promotion_last_5years<-as.factor(HR_data$promotion_last_5years)
str(HR_data)

smp_size <- floor(0.5 * nrow(HR_data))
set.seed(123)
train_ind <- sample(seq_len(nrow(HR_data)), size = smp_size)

train <- HR_data[train_ind, ]
nrow(train)
test <- HR_data[-train_ind, ]
nrow(test)

Cox Proportional Hazards Model
names(HR_data)
library(rms)

cph_model <- cph(Surv(time_spend_company, left)~., data = train, surv = TRUE, x = TRUE, y=TRUE)


#Random Forests for Survival

library(randomForestSRC)
set.seed(1994)
rf_model <- rfsrc(Surv(time_spend_company, left)~., data = train)

library(survival)
reg_model <- survreg(Surv(time_spend_company, left)~., data = train, x = TRUE)

#Explanation

library(survxai)
names(HR_data)
pbc_smaller[,-c(1,5)]
surve_cph <- explain(model = cph_model,
                     data = test[,-c(5,7)], 
                     y = Surv(test$time_spend_company, test$left))

print(surve_cph)
names(HR_data)
surve_rf <- explain(model = rf_model, 
                    label = "random forest",
                    data = test[,-c(5,7)], 
                    y = Surv(test$time_spend_company, test$left))

print(surve_rf)


 Sur_Regr

library(CFC)

custom_predict <- function(model, newdata, times){
  times <- sort(times)
  vars <- all.vars(model$call[[2]][[2]])
  n_vars <- which(colnames(newdata) %in% vars)
  if(length(n_vars)>0){
    newdata <- newdata[,-c(n_vars)]
  }
  model$x <- model.matrix(~., newdata)
  res <- matrix(ncol = length(times), nrow = nrow(newdata))
  for(i in 1:nrow(newdata)) {
    res[i,] <- cfc.survreg.survprob(t = times, args = model, n = i)    
  }
  return(res)
}

surve_reg <- explain(model = reg_model,
                     data = test[,-c(5,7)], 
                     y = Surv(test$time_spend_company, test$left),
                     predict_function = custom_predict)
print(surve_reg)
names(test[,-c(5,7)])
Model performance

mp_cph1 <- model_performance(surve_cph)
mp_rf1 <- model_performance(surve_rf)
mp_reg1 <- model_performance(surve_reg)
plot(mp_cph1,mp_rf1, mp_reg1) + ylim(c(0,0.40))

  #Variable response
#rDEA

names(HR_data)
vr_cph_sex <- variable_response(surve_cph, "salary")
vr_rf_sex <- variable_response(surve_rf, "salary")
vr_reg_sex <- variable_response(surve_reg, "salary")
plot(vr_cph_sex, vr_rf_sex, vr_reg_sex)

 #By Vairable 
plot(vr_cph_sex, vr_rf_sex, vr_reg_sex, split = "variable")

Continuous variable
names(HR_data)
vr_cph_bili <- variable_response(surve_cph, "number_project")
vr_rf_bili <- variable_response(surve_rf, "number_project")
vr_reg_bili <- variable_response(surve_reg, "number_project")

plot(vr_cph_bili, vr_rf_bili, vr_reg_bili)
plot(vr_cph_bili, vr_rf_bili, vr_reg_bili, split = "variable")



names(HR_data)
vr_cph_satisfaction_level <- variable_response(surve_cph, "satisfaction_level")
vr_rf_satisfaction_level <- variable_response(surve_rf, "satisfaction_level")
vr_reg_satisfaction_level <- variable_response(surve_reg, "satisfaction_level")

plot(vr_cph_satisfaction_level,vr_reg_satisfaction_level, vr_rf_satisfaction_level)
plot(vr_cph_satisfaction_level, vr_reg_satisfaction_level,vr_rf_satisfaction_level, split = "variable",Type1Font=10)

#the explanations for one, new observation.

single_observation <- test[1,-c(5,7)]
single_observation

cp_cph <- ceteris_paribus(surve_cph, single_observation)
cp_rf <- ceteris_paribus(surve_rf, single_observation)
cp_reg <- ceteris_paribus(surve_reg, single_observation)

plot(cp_cph, scale_type = "gradient", scale_col = c("red", "blue"))

plot(cp_rf, scale_type = "gradient", scale_col = c("red", "blue"))


plot(cp_reg, scale_type = "gradient", scale_col = c("red", "blue"))
 

#Prediction breakdown
broken_prediction_cph <- prediction_breakdown(surve_cph, single_observation)
broken_prediction_rf <- prediction_breakdown(surve_rf, single_observation)
broken_prediction_reg <- prediction_breakdown(surve_reg, single_observation)

 #Plots
plot(broken_prediction_cph, scale_col = c("red", "blue"), lines_type = 2)
plot(broken_prediction_rf, scale_col = c("red", "blue"), lines_type = 2)
plot(broken_prediction_reg, scale_col = c("red", "blue"), lines_type = 4)
@This plots helps to understand the factors that drive survival probability for a single observation.     


broken_prediction_cph <- prediction_breakdown(surve_cph, single_observation, prob = 0.8)
broken_prediction_rf <- prediction_breakdown(surve_rf, single_observation, prob = 0.8)
broken_prediction_reg <- prediction_breakdown(surve_reg, single_observation, prob = 0.8)

broken_prediction_cph

plot(broken_prediction_cph, scale_col = c("red", "blue"), lines_type = 2)
plot(broken_prediction_rf, scale_col = c("red", "blue"), lines_type = 2)
plot(broken_prediction_reg, scale_col = c("red", "blue"), lines_type = 2)


##We can compare this models on one plot.

plot(broken_prediction_cph, broken_prediction_rf, broken_prediction_reg, scale_col = c("red", "blue"), lines_type = 2)

library(survxai)
plot(broken_prediction_rf, scale_col = c("red", "blue"), lines_type = 2)

plot(broken_prediction_cph, broken_prediction_rf, broken_prediction_reg, scale_col = c("red", "blue"), lines_type = 2)


print(broken_prediction_cph$position)
