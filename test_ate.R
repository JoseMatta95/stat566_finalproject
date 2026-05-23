df_work <- df_work %>% mutate(smok = ifelse(smoking_stat == 0, 0, 1))

# Standarization

model_ols <- glm(heart_attack_incident ~ statin, data = df_work, family = binomial)

jtools::summ(model_ols, exp = T)

model_adj <- glm(heart_attack_incident ~ (statin) + age + sex + educ +
                   physical_act, data = df_work, family = binomial)

jtools::summ(model_adj, exp = T)

model_a1 <- predict(model_adj, newdata = df_work %>% mutate(statin = 1), type = "response")
model_a0 <- predict(model_adj, newdata = df_work %>% mutate(statin = 0), type = "response")

mean(model_a1, na.rm = T) - mean(model_a0, na.rm = T)

# IPW

model_ps <- glm(statin ~ age + sex + educ +
                  physical_act, data = df_work, family = binomial,
                na.action = na.exclude)

df_work$ps <- predict(model_ps, type = "response")

p_statin <- mean(df_work$statin == 1, na.rm = TRUE)

df_work <- df_work %>%
  mutate(
    w_stab = case_when(
      statin == 1 ~ p_statin / ps,
      statin == 0 ~ (1 - p_statin) / (1 - ps),
      TRUE ~ NA
    )
  )

df_work %>% filter(ps>=.1) %>% 
  ggplot(aes(x = ps, y = statin)) +
  geom_jitter()

jtools::summ(glm(heart_attack_incident ~ statin, data = df_work , weights = w_stab, family = binomial), exp = T)
