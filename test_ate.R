
# packages

library(MatchIt)
library(mice)
library(MatchIt)
library(cobalt)

# Standarization

model_ols <- glm(heart_attack_incident ~ statin, data = df_work, family = binomial)

jtools::summ(model_ols, exp = T)[[1]]["statin1","exp(Est.)",c("2.5%","97.5%")]

model_adj <- glm(heart_attack_incident ~ statin + diet + smoking_stat + health_insurace + physical_act +
                   income + bmxbmi + age + sex + educ, data = df_work, family = binomial)

jtools::summ(model_adj, exp = T)

model_a1 <- predict(model_adj, newdata = df_work %>% mutate(statin = 1), type = "response")
model_a0 <- predict(model_adj, newdata = df_work %>% mutate(statin = 0), type = "response")

mean(model_a1, na.rm = T) - mean(model_a0, na.rm = T)



###########

# MICE imputation

df_work_exp <- df_work %>%  select(statin,diet,smoking_stat,health_insurace,physical_act,income,bmxbmi,age,sex,educ)


imp <- mice(df_work_exp, m = 5, method = "pmm", seed = 123)

# datasets imputed 

df1 <- complete(imp, 1)
df2 <- complete(imp, 2)
df3 <- complete(imp, 3)
df4 <- complete(imp, 4)
df5 <- complete(imp, 5)


ipw_mice <- function(data,data2){

  data$heart_attack_incident <- data2$heart_attack_incident
  
  model_ps <- glm(statin ~ diet + smoking_stat + health_insurace + physical_act +
                    income + bmxbmi + age + sex + educ, 
                  
                  data = data, family = binomial)
  
  
  data$ps <- predict(model_ps, type = "response")
  
  p_statin <- mean(data$statin == 1, na.rm = TRUE)
  
  
  data <- data %>%
    mutate(
      w_stab = case_when(
        statin == 1 ~ p_statin / ps,
        statin == 0 ~ (1 - p_statin) / (1 - ps),
        TRUE ~ NA
      )
    )
  
  model <-jtools::summ(glm(heart_attack_incident ~ statin, data = data , weights = w_stab, family = binomial), exp =T)[[1]]

  model["statin1", ]
}


map_df(
  .x = list(df1,df2,df3,df4,df5),
  .f = ~ipw_mice(data = .x, data2 = df_work)
)



