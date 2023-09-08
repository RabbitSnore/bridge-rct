################################################################################

# Bridge RCT - Analysis Plan

################################################################################

# Set up environment -----------------------------------------------------------

packages <- c("tidyverse", "readxl", "lme4", "lmerTest", "lmeresampler")

lapply(packages, library, character.only = TRUE)

# Load data --------------------------------------------------------------------

# Study 1b ---------------------------------------------------------------------

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

boot_seekcare_ci_perc <- quantile(boot_seekcare_indirect, c(.025, .975))

boot_change_ci_perc   <- quantile(boot_change_indirect, c(.025, .975))

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

boot_csam_ci_perc <- quantile(boot_csam_indirect, c(.025, .975))
