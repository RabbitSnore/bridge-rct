################################################################################

# 

################################################################################

# Set up environment -----------------------------------------------------------

packages <- c("tidyverse", "readxl", "lme4", "lmerTest")

lapply(packages, library, character.only = TRUE)

# Load data --------------------------------------------------------------------

# Pilot data

# Load pilot data session by session and then stitch it together

pilot_01 <- read_xlsx("data/Bridge_surveys_230906_152943.xlsx",
                      range = cell_cols("B:FF"))

pilot_02 <- read_xlsx("data/Bridge_surveys_230906_152943.xlsx",
                      range = cell_cols("FH:LH"))

pilot_03 <- read_xlsx("data/Bridge_surveys_230906_152943.xlsx",
                      range = cell_cols("LJ:RJ"))

pilot_04 <- read_xlsx("data/Bridge_surveys_230906_152943.xlsx",
                      range = cell_cols("RL:XL"))

pilot_05 <- read_xlsx("data/Bridge_surveys_230906_152943.xlsx",
                      range = cell_cols("XN:ADN"))

pilot_06 <- read_xlsx("data/Bridge_surveys_230906_152943.xlsx",
                      range = cell_cols("ADP:AKX"))

pilot_07 <- read_xlsx("data/Bridge_surveys_230906_152943.xlsx",
                      range = cell_cols("AKZ:ASH"))

pilot <- bind_rows(pilot_01, 
                   pilot_02,
                   pilot_03,
                   pilot_04,
                   pilot_05,
                   pilot_06)

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

# a paths

lmm_1b_motive_a      <- lmer(motivational_talk
                             ~ 1
                             + mi_treat
                             + time
                             + time_after
                             + (1 | id),
                             data = study_1b)

#### Willingness to seek treatment

# b paths

lmm_1b_seekcare_b    <- lmer(treat_seeking
                             ~ 1
                             + mi_treat
                             + time
                             + time_after
                             + motivational_talk
                             + (1 | id),
                             data = study_1b)

#### Motivation for change

# b paths

lmm_1b_change_b      <- lmer(change_sum
                             ~ 1
                             + mi_treat
                             + time
                             + time_after
                             + motivational_talk
                             + (1 | id),
                             data = study_1b)


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

lmm_2b_csam_base     <- lmer(socialize_sum
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

lrt_2b_hib           <- anova(lmm_2b_hbi_base, lmm_2b_hbi_int)

### Mediating effect of treatment on CSAM usage through sexual urges (SSAS)