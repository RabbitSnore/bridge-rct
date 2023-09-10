################################################################################

# 

################################################################################

# Set up environment -----------------------------------------------------------

packages <- c("tidyverse", "readxl", "lme4", "lmerTest", "janitor")

lapply(packages, library, character.only = TRUE)

# Load data --------------------------------------------------------------------

# Pilot data

# Load pilot data session by session and then stitch it together

pilot_01 <- read_xlsx("data/Bridge_surveys_230906_152943.xlsx",
                      range = cell_cols("B:FF"))

pilot_01 <- clean_names(pilot_01)

pilot_01$waitlist    <- as.numeric(str_detect(pilot_01$groups, "WAITLIST"))
pilot_01$treatment   <- as.numeric(str_detect(pilot_01$groups, "TREATMENT"))
pilot_01$bridge      <- as.numeric(str_detect(pilot_01$groups, "Bridge"))
pilot_01$redirection <- as.numeric(str_detect(pilot_01$groups, "ReDirection"))
pilot_01$visit       <- 1

pilot_01$motivation <- pilot_01$motivation_to_seek_care_151

pilot_01_clean <- pilot_01 %>% 
  select(
    study_code, waitlist, treatment, bridge, redirection, visit, motivation
  )

pilot_02 <- read_xlsx("data/Bridge_surveys_230906_152943.xlsx",
                      range = cell_cols("FH:LH"))

pilot_02 <- clean_names(pilot_05)

pilot_02$waitlist    <- as.numeric(str_detect(pilot_02$groups, "WAITLIST"))
pilot_02$treatment   <- as.numeric(str_detect(pilot_02$groups, "TREATMENT"))
pilot_02$bridge      <- as.numeric(str_detect(pilot_02$groups, "Bridge"))
pilot_02$redirection <- as.numeric(str_detect(pilot_02$groups, "ReDirection"))
pilot_02$visit       <- 2

pilot_02$motivation <- pilot_02$motivation_to_seek_care_156

pilot_02_clean <- pilot_02 %>% 
  select(
    study_code, waitlist, treatment, bridge, redirection, visit, motivation
  )

pilot_03 <- read_xlsx("data/Bridge_surveys_230906_152943.xlsx",
                      range = cell_cols("LJ:RJ"))

pilot_03 <- clean_names(pilot_03)

pilot_03$waitlist    <- as.numeric(str_detect(pilot_03$groups, "WAITLIST"))
pilot_03$treatment   <- as.numeric(str_detect(pilot_03$groups, "TREATMENT"))
pilot_03$bridge      <- as.numeric(str_detect(pilot_03$groups, "Bridge"))
pilot_03$redirection <- as.numeric(str_detect(pilot_03$groups, "ReDirection"))
pilot_03$visit       <- 3

pilot_03$motivation <- pilot_03$motivation_to_seek_care_156

pilot_03_clean <- pilot_03 %>% 
  select(
    study_code, waitlist, treatment, bridge, redirection, visit, motivation
  )

pilot_04 <- read_xlsx("data/Bridge_surveys_230906_152943.xlsx",
                      range = cell_cols("RL:XL"))

pilot_04 <- clean_names(pilot_04)

pilot_04$waitlist    <- as.numeric(str_detect(pilot_04$groups, "WAITLIST"))
pilot_04$treatment   <- as.numeric(str_detect(pilot_04$groups, "TREATMENT"))
pilot_04$bridge      <- as.numeric(str_detect(pilot_04$groups, "Bridge"))
pilot_04$redirection <- as.numeric(str_detect(pilot_04$groups, "ReDirection"))
pilot_04$visit       <- 4

pilot_04$motivation <- pilot_04$motivation_to_seek_care_156

pilot_04_clean <- pilot_04 %>% 
  select(
    study_code, waitlist, treatment, bridge, redirection, visit, motivation
  )

pilot_05 <- read_xlsx("data/Bridge_surveys_230906_152943.xlsx",
                      range = cell_cols("XN:ADN"))

pilot_05 <- clean_names(pilot_05)

pilot_05$waitlist    <- as.numeric(str_detect(pilot_05$groups, "WAITLIST"))
pilot_05$treatment   <- as.numeric(str_detect(pilot_05$groups, "TREATMENT"))
pilot_05$bridge      <- as.numeric(str_detect(pilot_05$groups, "Bridge"))
pilot_05$redirection <- as.numeric(str_detect(pilot_05$groups, "ReDirection"))
pilot_05$visit       <- 5

pilot_05$motivation <- pilot_05$motivation_to_seek_care_156

pilot_05_clean <- pilot_05 %>% 
  select(
    study_code, waitlist, treatment, bridge, redirection, visit, motivation
  )

pilot_06 <- read_xlsx("data/Bridge_surveys_230906_152943.xlsx",
                      range = cell_cols("ADP:AKX"))

pilot_06 <- clean_names(pilot_06)

pilot_06$waitlist    <- as.numeric(str_detect(pilot_06$groups, "WAITLIST"))
pilot_06$treatment   <- as.numeric(str_detect(pilot_06$groups, "TREATMENT"))
pilot_06$bridge      <- as.numeric(str_detect(pilot_06$groups, "Bridge"))
pilot_06$redirection <- as.numeric(str_detect(pilot_06$groups, "ReDirection"))
pilot_06$visit       <- 6

pilot_06$motivation <- pilot_06$motivation_to_seek_care_190

pilot_06_clean <- pilot_06 %>% 
  select(
    study_code, waitlist, treatment, bridge, redirection, visit, motivation
  )

pilot_07 <- read_xlsx("data/Bridge_surveys_230906_152943.xlsx",
                      range = cell_cols("AKZ:ASH"))

pilot_07 <- clean_names(pilot_07)

pilot_07$waitlist    <- as.numeric(str_detect(pilot_07$groups, "WAITLIST"))
pilot_07$treatment   <- as.numeric(str_detect(pilot_07$groups, "TREATMENT"))
pilot_07$bridge      <- as.numeric(str_detect(pilot_07$groups, "Bridge"))
pilot_07$redirection <- as.numeric(str_detect(pilot_07$groups, "ReDirection"))
pilot_07$visit       <- 7

pilot_07$motivation <- pilot_07$motivation_to_seek_care_190

pilot_07_clean <- pilot_07 %>% 
  select(
    study_code, waitlist, treatment, bridge, redirection, visit, motivation
  )

pilot <- bind_rows(pilot_01_clean, 
                   pilot_02_clean,
                   pilot_03_clean,
                   pilot_04_clean,
                   pilot_05_clean,
                   pilot_06_clean,
                   pilot_07_clean)

pilot_clean <- pilot %>% 
  filter(complete.cases(motivation)) %>% 
  filter(study_code != "1032tphf")

bridge <- pilot_clean %>% 
  filter(bridge == 1)

redirection <- pilot_clean %>% 
  filter(redirection == 1)

# Study 1b ---------------------------------------------------------------------

## Sample size planning

## Primary analyses

### Treatment acceptance (care seeking)

#### Rated willingness to seek treatment

lmm_1b_seekcare_base <- lmer(treat_seeking
                             ~ 1
                             + mi_treat
                             + time
                             + time_after
                             + (1 | id),
                             data = study_1b)

### Motivation for change

lmm_1b_change_base   <- lmer(change_sum
                             ~ 1
                             + mi_treat
                             + time
                             + time_after
                             + (1 | id),
                             data = study_1b)

## Secondary analyses

### Do you think you will actually seek care? (dichotomous)

glmm_1b_seekcare_base <- glmer(treat_seeking_01
                               ~ 1
                               + mi_treat
                               + time
                               + time_after
                               + (1 | id),
                               data = study_1b,
                               family = binomial(link = "logit"))

### Sexual urges (SSAS)

lmm_1b_ssas_base     <- lmer(ssas_sum
                             ~ 1
                             + mi_treat
                             + time
                             + time_after
                             + (1 | id),
                             data = study_1b)

### CSAM usage

lmm_1b_csam_base     <- lmer(csam_sum
                             ~ 1
                             + mi_treat
                             + time
                             + time_after
                             + (1 | id),
                             data = study_1b)

### Depression (PHQ-9)

lmm_1b_phq9_base     <- lmer(phq9_sum
                             ~ 1
                             + mi_treat
                             + pre_post
                             + (1 | id),
                             data = study_1b)

lmm_1b_phq9_int      <- lmer(phq9_sum
                             ~ 1
                             + mi_treat
                             * pre_post
                             + (1 | id),
                             data = study_1b)

lrt_1b_phq9          <- anova(lmm_1b_phq9_base, lmm_1b_phq9_int)

### Mediating effect of MI on primary outcomes through motivational talk

#### Rated willingness to seek treatment - a and b path models

lmm_seekcare_a       <- lmer(motivational_talk_pmc # Person mean centered
                             ~ 1
                             + mi_treat
                             + time
                             + time_after
                             + (1 | id),
                             data = study_1b)

lmm_seekcare_b       <- lmer(treat_seeking
                             ~ 1
                             + mi_treat
                             + time
                             + time_after
                             + motivational_talk_pmc
                             + (1 | id),
                             data = study_1b)

#### Motivation for change - a and b path models

lmm_change_a         <- lmer(motivational_talk_pmc
                             ~ 1
                             + mi_treat
                             + time
                             + time_after
                             + (1 | id),
                             data = study_1b)

lmm_change_b         <- lmer(change_sum
                             ~ 1
                             + mi_treat
                             + time
                             + time_after
                             + motivational_talk_pmc
                             + (1 | id),
                             data = study_1b)

#### Bootstrapping coefficients

set.seed(9998)

boot_seekcare_a <- bootstrap(lmm_seekcare_a, fixef, type = "parametric", B = 10000)

set.seed(9998)

boot_seekcare_b <- bootstrap(lmm_seekcare_b, fixef, type = "parametric", B = 10000)

set.seed(1212)

boot_change_a <- bootstrap(lmm_change_a, fixef, type = "parametric", B = 10000)

set.seed(1212)

boot_change_b <- bootstrap(lmm_change_b, fixef, type = "parametric", B = 10000)

##### Calculated bootstrapped indirect effects

boot_seekcare_indirect <- 
  boot_seekcare_a$replicates$time_after * boot_seekcare_b$replicates$motivational_talk

boot_change_indirect <- 
  boot_change_a$replicates$time_after * boot_change_b$replicates$motivational_talk

#### Percentile confidence interval

boot_seekcare_ci_perc <- quantile(boot_seekcare_indirect, c(.055, .975))

boot_change_ci_perc   <- quantile(boot_change_indirect, c(.055, .975))

### Dynamic risk (ACUTE-2007)

lmm_1b_risk_base     <- lmer(acute_2007_sum
                             ~ 1
                             + mi_treat
                             + time
                             + time_after
                             + (1 | id),
                             data = study_1b)

### Hypersexuality (HBI-19)

lmm_1b_hbi_base      <- lmer(hbi_sum
                             ~ 1
                             + mi_treat
                             + pre_post
                             + (1 | id),
                             data = study_1b)

lmm_1b_hbi_int       <- lmer(hbi_sum
                             ~ 1
                             + mi_treat
                             * pre_post
                             + (1 | id),
                             data = study_1b)

lrt_1b_hib           <- anova(lmm_1b_hbi_base, lmm_1b_hbi_int)

# Study 2b ---------------------------------------------------------------------

## Sample size planning

## Primary analyses

### Sexual urges (SSAS)

lmm_2b_ssas_base     <- lmer(ssas_sum
                             ~ 1
                             + rd_treat
                             + time
                             + time_after
                             + (1 | id),
                             data = study_2b)

### CSAM usage

lmm_2b_csam_base     <- lmer(csam_sum
                             ~ 1
                             + rd_treat
                             + time
                             + time_after
                             + (1 | id),
                             data = study_2b)

### SChiMRA B (Behaviors against children)

#### Socialize

lmm_2b_social_base     <- lmer(socialize_sum
                             ~ 1
                             + rd_treat
                             + time
                             + time_after
                             + (1 | id),
                             data = study_2b)

#### Interact

lmm_2b_interact_base <- lmer(interact_sum
                             ~ 1
                             + rd_treat
                             + time
                             + time_after
                             + (1 | id),
                             data = study_2b)

### Depression (PHQ-9)

lmm_2b_phq9_base     <- lmer(phq9_sum
                             ~ 1
                             + rd_treat
                             + pre_post
                             + (1 | id),
                             data = study_1b)

lmm_2b_phq9_int      <- lmer(phq9_sum
                             ~ 1
                             + rd_treat
                             * pre_post
                             + (1 | id),
                             data = study_1b)

lrt_2b_phq9          <- anova(lmm_2b_phq9_base, lmm_2b_phq9_int)

## Secondary analyses

### Dynamic risk (ACUTE-2007)

lmm_2b_risk_base     <- lmer(acute_2007_sum
                             ~ 1
                             + rd_treat
                             + time
                             + time_after
                             + (1 | id),
                             data = study_2b)

### Hypersexuality (HBI-19)

lmm_2b_hbi_base      <- lmer(hbi_sum
                             ~ 1
                             + rd_treat
                             + pre_post
                             + (1 | id),
                             data = study_2b)

lmm_2b_hbi_int       <- lmer(hbi_sum
                             ~ 1
                             + rd_treat
                             * pre_post
                             + (1 | id),
                             data = study_2b)

lrt_2b_hbi           <- anova(lmm_2b_hbi_base, lmm_2b_hbi_int)

### Mediating effect of treatment on CSAM usage through sexual urges (SSAS)

#### b path models

lmm_csam_a       <- lmer(ssas_sum_pmc # Person mean centered
                         ~ 1
                         + rd_treat
                         + time
                         + time_after
                         + ssas_sum
                         + (1 | id),
                         data = study_1b)

lmm_csam_b       <- lmer(csam_sum
                             ~ 1
                             + rd_treat
                             + time
                             + time_after
                             + ssas_sum_pmc
                             + (1 | id),
                             data = study_1b)

#### Bootstrapping coefficients

set.seed(1964)

boot_csam_a <- bootstrap(lmm_csam_a, fixef, type = "parametric", B = 10000)

set.seed(1964)

boot_csam_b <- bootstrap(lmm_csam_b, fixef, type = "parametric", B = 10000)

##### Calculated bootstrapped indirect effects

boot_csam_indirect <- 
  lmm_csam_a$replicates$time_after * lmm_csam_b$replicates$ssas_sum

#### Percentile confidence interval

boot_csam_ci_perc <- quantile(boot_csam_indirect, c(.055, .975))
