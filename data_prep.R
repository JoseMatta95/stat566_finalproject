library(tidyverse)
library(haven)
# install.packages("pak")
# pak::pak("kyleGrealis/nhanesdata")

library(nhanesdata)
library(janitor)

# data

## Statins
### Module: Prescription medications: RXQ_RX, RXDD_DRUG

# rxq_rx  <- 
#   read_nhanes("rxq_rx") %>%  # data
#   clean_names() %>% 
#   
#   filter(year == 2017) %>% 
#   mutate(
#     statins_use = str_detect(rxddrug,regex('statin',ignore_case = T))
#   ) %>% 
#   select(year,seqn,rxddrug,statins_use) %>% 
#   filter(statins_use ==T)
# 
# table(rxq_rx$rxddrug)

rxq_rx  <- 
  read_nhanes("rxq_rx") %>%  # data
  clean_names() %>% 
  
  filter(year == 2017) %>% 
  mutate(
    statins_use = str_detect(rxddrug,regex('atorvastatin|simvastatin|fluvastatin|lovastatin|
                                           pitavastatin|pravastatin|rosuvastatin',ignore_case = T))
  ) %>% 
  select(year,seqn,rxddrug,statins_use) %>% 
  
  group_by(year,seqn) %>% 
  summarise(
    statin = as.integer(any(statins_use, na.rm = T),
    
    .groups = 'drop'
  ))
table(rxq_rx$statin)

### Module: Blood Pressure & Cholesterol Questionnaire BPQ

#### use to filter previous dx of hypercholesterolemia
bpq<- read_nhanes("bpq") %>%  # blood pressure and cholesterol 
  filter(year == 2017) %>% 
  select(year,seqn,bpq080) %>% 
  filter(bpq080 == "Yes")


#### final exposure data --> all data have to fit this
exposure_df<-
bpq %>% 
  left_join(rxq_rx)

## CVD and mortality
mcq     <- read_nhanes("mcq") 

# "mcq160e",  # heart attack
# "mcq160f",  # stroke
# "mcq160d",  # angina
# "mcq160c",  # coronary heart disease
# "mcq160b"  # congestive heart failure

cvd <- mcq %>% 
  filter(year == 2017) %>% 
  select(seqn, year,mcq160e,mcq160f,mcq160d,mcq160c,mcq160b ) |>
  mutate(across(.cols = c(mcq160e:mcq160b), .fns = ~ifelse(.x == "Yes",1,0) )) %>% 
  mutate(
    cvd = ifelse(mcq160e+mcq160f+mcq160d+mcq160c+mcq160b >0,1,0)
  ) %>% 
  select(
    year,seqn,cvd
  )


## Final data exposure + otucome

final_df<-
  exposure_df %>% 
  left_join(cvd)

## Confounders

### Age, sex, race

### High blood pressure 

### Diabetes

### Cholesterol 

### IMC

### Smoking status

### Physical act

### Income, edu level

### Health insurance

