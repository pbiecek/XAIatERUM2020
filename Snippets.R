#
# 0. Make sure that you have all package installed

library("DALEX")
library("ranger")
library("ggplot2")
library("rms")

#
# 1. Look at the data we'll be working on

head(titanic_imputed)

#
# 2. Let's train two classification models
# you can try also other models if you like

model_ranger <- ranger(survived ~ ., data = titanic_imputed, 
                       classification = TRUE, probability = TRUE)

model_rms <- lrm(survived ~ rcs(age)*gender + rcs(fare) + 
                   class, data = titanic_imputed)

#
# 3. The models are different, to work with them in a uniform way we need an adapter
# created with explain() function

exp_ranger <- explain(model_ranger,
                      data = titanic_imputed[,1:7],
                      y = titanic_imputed$survived)

predict(exp_ranger, titanic_imputed[1,])



#
# 4. Other arguments are useful too

exp_rms <- explain(model_rms,
                   data = titanic_imputed[,1:7], 
                   y = titanic_imputed$survived,
                   predict_function = function(m, x) 
                     predict(m, x, type = "fitted"),
                   label = "Logistic with splines")

#
# 5. The adapter stores some useful information about the model

exp_rms$model_info

#
# 6. First step in the exploration are measures of performance
# performance is calculated on data in explainer
# use update_data to set a new dataset

mp_ranger <- model_performance(exp_ranger)
mp_ranger

mp_rms <- model_performance(exp_rms)
mp_rms

#
# 7. it is easier to compare models with charts

plot(mp_ranger, mp_rms, geom = "boxplot")

plot(mp_ranger, mp_rms, geom = "roc")

plot(mp_ranger, mp_rms, geom = "lift")

#
# 8. Let's focus on a single observations

henry <- titanic_imputed[1,]

predict(exp_ranger, henry)

#
# 9. Variable attributions - Shapley valyes

sh_ranger <- predict_parts(exp_ranger, henry, 
                           type = "shap", B = 1)

plot(sh_ranger, show_boxplots = FALSE) + 
   ggtitle("Shapley values for Henry","")

#
# 10. Variable attributions - Break-down values

bd_ranger <- predict_parts(exp_ranger, henry, 
                        type = "break_down_interactions")
bd_ranger
plot(bd_ranger, show_boxplots = FALSE) + 
  ggtitle("Break down values for Henry","") + 
  scale_y_continuous("",limits = c(0.09,0.33))

#
# 11. Variable attributions - Break-down with specified order

bd_ranger <- predict_parts(exp_ranger, henry, 
            order = c("age", "gender", "fare", "class",
                    "parch", "sibsp", "embarked"))

plot(bd_ranger)  + 
  scale_y_continuous("",limits = c(0.09,0.33))

#
# 11. Variable importance 

mp_ranger <- model_parts(exp_ranger, type = "difference")

plot(mp_ranger, show_boxplots = FALSE) + 
  ggtitle("Variable importance","")

#
# 12. Variable importance for two models

mp_ranger <- model_parts(exp_ranger, B = 1)

mp_rms <- model_parts(exp_rms, B = 1)

plot(mp_ranger, mp_rms, show_boxplots = FALSE, bar_width = 5) + 
  ggtitle("Variable importance","")

#
# 13. Profiles for a single observation

cp_ranger <- predict_profile(exp_ranger, henry)

plot(cp_ranger, variables = c("age", "fare"))

#
# 14. Profiles for two models

cp_rms <- predict_profile(exp_rms, henry)

plot(cp_ranger, cp_rms, variables = "age", color = "_label_") + 
  ggtitle("Ceteris Paribus for Henry")

#
# 15. How important are variables

cp_rms <- predict_parts(exp_rms, henry, type = "oscillations")

#
# 16. For categorical variables

plot(cp_ranger, variables = "class", categorical_type = "bars") + 
  ggtitle("Ceteris Paribus for Henry")

#
# 17. Model profile - aveage across individual CPs

mp_ranger <- model_profile(exp_ranger)

plot(mp_ranger, variables = "age")

plot(mp_ranger, variables = "age", geom = "points") + 
   ggtitle("Partial dependence","")

#
# 18. Model profile - aveage calculated in groups

mp_ranger <- model_profile(exp_ranger, groups = "gender")

plot(mp_ranger, variables = "age") + 
   ggtitle("Partial dependence","")

#
# 19. Model profile - aveage calculated in clusters

mp_ranger <- model_profile(exp_ranger, k = 3, center = TRUE)

plot(mp_ranger, variables = "age") + 
   ggtitle("Partial dependence 3 clusters","")

#
# 20. modelStudio dashboard

library(modelStudio)

modelStudio(exp_ranger)















model_apart <- ranger(m2.price ~ ., data = apartments)

exp_ranger <- explain(model_apart,
                      data = apartments_test,
                      y = apartments_test$m2.price)

predict(exp_ranger, apartments_test[1,])


mp_ranger <- model_performance(exp_ranger)

predict_parts(exp_ranger, apartments[1,])


plot(mp_ranger, geom = "boxplot")




mp_ranger <- model_profile(exp_ranger)

plot(mp_ranger)
