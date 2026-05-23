library(tidyverse)
library(haven)
# install.packages("pak")
# pak::pak("kyleGrealis/nhanesdata")

library(nhanesdata)
library(janitor)

# 1. Data----

## Statins----
### Module: Prescription medications: RXQ_RX, RXDD_DRUG

rxq_rx  <- 
  read_nhanes("rxq_rx") %>%
  clean_names() %>%
  filter(year == 2015) %>%
  mutate(
    statins_use    = as.integer(str_detect(rxddrug, regex('atorvastatin|simvastatin|fluvastatin|lovastatin|
                                             pitavastatin|pravastatin|rosuvastatin', ignore_case = T))),
    #rxddays_statin = ifelse(statins_use == 1, rxddays, NA_real_)
  ) %>%   
  select(year, seqn, statins_use, rxddays) %>%
  group_by(year, seqn) %>%
  summarise(
    statin = as.integer(any(statins_use == 1, na.rm = TRUE)),
    
    statin_days = ifelse(
      any(statins_use == 1, na.rm = TRUE),
      max(rxddays[statins_use == 1], na.rm = TRUE),
      NA_real_
    ),
    
    .groups = "drop"
  )
table(rxq_rx$statin)

### Module: Blood Pressure & Cholesterol Questionnaire BPQ

#### use to filter previous dx of hypercholesterolemia
bpq<- read_nhanes("bpq") %>%  # blood pressure and cholesterol 
  filter(year == 2015) %>% 
  select(year,seqn,bpq080) %>% 
  filter(bpq080 == "Yes")


#### final exposure data --> all data have to fit this----
exposure_df<-
bpq %>% 
  left_join(rxq_rx)

## CVD and mortality----
mcq     <- read_nhanes("mcq") 

# "mcq160e",  # heart attack
# "mcq160f",  # stroke
# "mcq160d",  # angina
# "mcq160c",  # coronary heart disease
# "mcq160b"  # congestive heart failure

cvd <- mcq %>%
  filter(year == 2015) %>%
  select(seqn, year, mcq160e, mcq180e) %>%
  mutate(
    heart_attack = ifelse(mcq160e == "Yes", 1, 0),
    age_first_ha = mcq180e
  ) %>%
  select(year, seqn, heart_attack, age_first_ha)


## Final data exposure + otucome----

final_df<-
  exposure_df %>% 
  left_join(cvd)

## Confounders

### Age, sex, race, Income, edu level
demo_conf <-
  read_nhanes("demo") %>% 
  select(year,seqn,ridageyr,riagendr,ridreth3,dmdeduc2,indfmpir) %>% 
  filter(year==2015) %>% 
  mutate(
    dmdeduc2 = case_when(
      dmdeduc2 == "Less than 9th grade"~ "less_than_9th",
      dmdeduc2 == "9-11th grade (Includes 12th grade with no diploma)"~ "9th_to_11th",
      dmdeduc2 == "High school graduate/GED or equivalent"~ "high_school",
      dmdeduc2 == "Some college or AA degree"~ "some_college",
      dmdeduc2 == "College graduate or above"~ "college_or_above",
      dmdeduc2 %in% c("Don't know", "Refused")~ NA_character_,
      TRUE ~ NA_character_
    )
  ) %>% 
  rename(
    age = ridageyr,
    sex = riagendr,
    race = ridreth3,
    educ = dmdeduc2,
    income = indfmpir
  )

### High blood pressure 

bloodpress_conf <-
  read_nhanes("bpx") %>% 
  filter(year==2015) %>% 
  rename(
    systolic1 = bpxsy1,
    diastolic1 = bpxdi1,
    systolic2 = bpxsy2,
    diastolic2 = bpxdi2
  ) %>% 
  mutate(
    
    sys_mean = (systolic1+systolic2)/2,
    diat_mean =(diastolic1+diastolic2)/2 ,
    hypertension = ifelse(sys_mean >= 140 | diat_mean >= 90,1,0)
  ) %>% 
  select(year,seqn,hypertension)

### Diabetes

diabetes_conf<-
  read_nhanes("ghb") %>% 
  filter(year == 2015) %>% 
  mutate(
    diabetes_hbg = ifelse(lbxgh >=6.5,1,0)
  ) %>% 
  select(
    year,seqn,diabetes_hbg
  )

### Cholesterol 

chol_conf<-
  read_nhanes("tchol") %>% 
  filter(year == 2015) %>% 
  rename(
    total_chol = lbxtc
  ) %>% 
  select(
    year,seqn,total_chol
  )

### IMC

imc_conf<-
  read_nhanes("bmx") %>% 
  select(
    year,seqn,bmxbmi,bmxht,bmxwt
  ) %>% 
  filter(
    year == 2015
  )
### Smoking status

smoking_comf <-
  read_nhanes("smq") %>%  # smq020: >100 cig/lifetime, smq040: current
  filter(
    year == 2015
  ) %>% 
  mutate(
    smoking_stat = case_when(
      smq020 == "No" ~ 0, # Never
      smq020=="Yes" & smq040=="Not at all" ~ 1, # Former
      smq020=="Yes" & smq040 %in% c("Every day","Some days") ~ 2, # Current
      T ~ NA
    )
  ) %>% 
  select(year,seqn,smoking_stat)

### Physical act

phys_conf<-
  read_nhanes("paq") %>% 
  filter(year ==2015) %>% 
  mutate(
    physical_act = ifelse(paq650 == "No" & paq665 == "No",0,1)
  ) %>%
  select(year,seqn,physical_act)

### Health insurance

insurance_conf<-
  read_nhanes("hiq") %>% 
  filter(
    year == 2015
  ) %>% 
  rename(
    health_insurace = hiq011
  ) %>% 
  select(
    year,seqn,health_insurace
  )

# Final data ----

df_work<-
  final_df %>% 
  left_join(demo_conf) %>% 
  left_join(bloodpress_conf) %>% 
  left_join(diabetes_conf) %>% 
  left_join(chol_conf) %>% 
  left_join(imc_conf) %>% 
  left_join(smoking_comf) %>% 
  left_join(phys_conf) %>% 
  left_join(insurance_conf) %>% 
  
  mutate(
    statin_start = age-(statin_days/365),
    heart_attack_incident = case_when(
      statin == 1 ~ as.integer(heart_attack == 1 & age_first_ha > statin_start),
      statin == 0 ~ heart_attack,
      TRUE        ~ NA_integer_
    )
  ) %>%
  filter(
    !health_insurace %in% c("Don't know","Refused")
  )


