################################################################################

# Bridge RCT - Analysis Plan

################################################################################

# Set up environment -----------------------------------------------------------

packages <- c("tidyverse", "readxl", "lme4", "lmerTest", "lmeresampler", "simr")

lapply(packages, library, character.only = TRUE)

# Load data --------------------------------------------------------------------

# Power analysis ---------------------------------------------------------------

# Design

wait_arm_n     <- 30
treatment_n    <- 30

measurements_k <- 5

## waitlist arm structure

wait_structure <- data.frame(
  time       = 0:(measurements_k*2 - 1),
  treatment  = c(rep(0, measurements_k), rep(1, measurements_k)),
  time_after = c(rep(0, measurements_k), 1:measurements_k)
)

## Treatment arm structure

treat_structure <- data.frame(
  time       = 0:(measurements_k - 1),
  treatment  = rep(1, measurements_k),
  time_after = 1:measurements_k
)

# Power simulation

# Motivation to seek care

## Fixed and random effects

fixed_design <- c(
  0.00,  # intercept
  0.00,  # treatment 
  0.25,  # time 
  0.25   # time_after
)

varcor_design <- list(
  1.50
)

sigma_design <- 1.60

## Simulated data set without outcome variable

wait_design  <- map_dfr(seq_len(wait_arm_n), ~ wait_structure)

wait_design$id <- sort(rep(1:wait_arm_n, measurements_k*2))

treat_design <- map_dfr(seq_len(treatment_n), ~ treat_structure)

treat_design$id <- sort(rep(1:treatment_n, measurements_k)) + max(wait_design$id)

data_design <- bind_rows(wait_design, treat_design)

data_design <- data_design %>% 
  select(id, everything())

## Simulated model

design_lmm <- makeLmer(
  formula =
  treat_seeking
  ~ 1
  + treatment
  + time
  + time_after
  + (1 | id),
  fixef   = fixed_design,
  VarCorr = varcor_design,
  sigma   = sigma_design,
  data    = data_design)

## Simulation

if (!file.exists("./output/bridge-rct_power-sim.rds")) {
  
  power_simulation <- powerSim(
    fit = design_lmm,
    test = fixed("time_after", method = "t"),
    nsim = 1000,
    seed = 1978
  )
  
  saveRDS(power_simulation, "./output/bridge-rct_power-sim.rds")
  
} else {
  
  power_simulation <- readRDS("./output/bridge-rct_power-sim.rds")
  
}

# SSAS

fixed_design_ssas <- c(
  0.00,  # intercept
  0.00,  # treatment 
  0.25,  # time 
  1.50   # time_after
)

varcor_design_ssas <- list(
  16.50
)

sigma_design_ssas <- 7.60

## Simulated model

design_lmm_ssas <- makeLmer(
  formula =
    treat_seeking
  ~ 1
  + treatment
  + time
  + time_after
  + (1 | id),
  fixef   = fixed_design_ssas,
  VarCorr = varcor_design_ssas,
  sigma   = sigma_design_ssas,
  data    = data_design)

## Simulation

if (!file.exists("./output/bridge-rct_ssas_power-sim.rds")) {
  
  power_simulation_ssas <- powerSim(
    fit = design_lmm_ssas,
    test = fixed("time_after", method = "t"),
    nsim = 1000,
    seed = 1978
  )
  
  saveRDS(power_simulation_ssas, "./output/bridge-rct_ssas_power-sim.rds")
  
} else {
  
  power_simulation_ssas <- readRDS("./output/bridge-rct_ssas_power-sim.rds")
  
}

# Study 1b ---------------------------------------------------------------------

## Primary analyses

### Treatment acceptance (care seeking)

#### Rated motivation to seek treatment

lmm_1b_seekcare_base <- lmer(treat_seeking
                             ~ 1
                             + mi_treat
                             + time
                             + time_after
                             + (1 | id),
                             data = study_1b)

#### Motivation for change

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

### Mediating effect of MI on seeking care through motivation to change

#### Rated willingness to seek treatment - a and b path models

lmm_seekcare_a       <- lmer(change_sum_pmc # Person mean centered
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
                             + change_sum_pmc
                             + (1 | id),
                             data = study_1b)

#### Motivation for change - a and b path models

glmm_seekcare_b     <- glmer(treat_seeking_01
                             ~ 1
                             + mi_treat
                             + time
                             + time_after
                             + change_sum_pmc
                             + (1 | id),
                             data = study_1b,
                             family = binomial(link = "logit"))

#### Bootstrapping coefficients

set.seed(9998)

boot_seekcare_a   <- bootstrap(lmm_seekcare_a, fixef, type = "parametric", B = 10000)

set.seed(9998)

boot_seekcare_b   <- bootstrap(lmm_seekcare_b, fixef, type = "parametric", B = 10000)

set.seed(1212)

boot_seekcare_2_a <- bootstrap(lmm_seekcare_a, fixef, type = "parametric", B = 10000)

set.seed(1212)

boot_seekcare_2_b <- bootstrap(glmm_seekcare_b, fixef, type = "parametric", B = 10000)

##### Calculated bootstrapped indirect effects

boot_seekcare_indirect <- 
  boot_seekcare_a$replicates$time_after * boot_seekcare_b$replicates$motivational_talk

boot_seekcare_2_indirect <- 
  boot_seekcare_2_a$replicates$time_after * boot_seekcare_2_b$replicates$motivational_talk

#### Percentile confidence interval

boot_seekcare_ci_perc     <- quantile(boot_seekcare_indirect, c(.025, .975))

boot_seekcare_2_ci_perc   <- quantile(boot_change_indirect, c(.025, .975))

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

### Moderating effect of autistic traits (RAADS-14) on treatment effectiveness

#### Rated motivation to seek treatment

lmm_1b_seekcare_asd     <- lmer(treat_seeking
                                ~ 1
                                + mi_treat
                                + time
                                + time_after
                                + raads_14_sum_mc # Grand mean centered
                                + (1 | id),
                                data = study_1b)

lmm_1b_seekcare_asd_int <- lmer(treat_seeking
                                ~ 1
                                + time
                                (+ mi_treat
                                 + time_after
                                 + raads_14_sum_mc)^2 # Grand mean centered
                                + (1 | id),
                                data = study_1b)

#### Motivation for change

lmm_1b_change_asd       <- lmer(change_sum
                                ~ 1
                                + mi_treat
                                + time
                                + time_after
                                + raads_14_sum_mc # Grand mean centered
                                + (1 | id),
                                data = study_1b)

lmm_1b_change_asd_int   <- lmer(change_sum
                                ~ 1
                                + time
                                (+ mi_treat
                                 + time_after
                                 + raads_14_sum_mc)^2 # Grand mean centered
                                + (1 | id),
                                data = study_1b)

#### Model comparisons

lrt_asd_seekcare <- anova(lmm_1b_seekcare_base, 
                          lmm_1b_seekcare_asd, 
                          lmm_1b_seekcare_asd_int)

lrt_asd_change   <- anova(lmm_1b_change_base,
                          lmm_1b_change_asd,
                          lmm_1b_change_asd_int)

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

### SChiMRA B (Other behaviors related to sexual interest in children)

#### Socialize

lmm_2b_schimra_base   <- lmer(schimra_other_sum
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
                             data = study_2b)

lmm_2b_phq9_int      <- lmer(phq9_sum
                             ~ 1
                             + rd_treat
                             * pre_post
                             + (1 | id),
                             data = study_2b)

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
                         data = study_2b)

lmm_csam_b       <- lmer(csam_sum
                         ~ 1
                         + rd_treat
                         + time
                         + time_after
                         + ssas_sum_pmc
                         + (1 | id),
                         data = study_2b)

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

### Moderating effect of autistic traits (RAADS-14) on treatment effectiveness

lmm_2b_ssas_asd     <- lmer(ssas_sum
                            ~ 1
                            + mi_treat
                            + time
                            + time_after
                            + raads_14_sum_mc # Grand mean centered
                            + (1 | id),
                            data = study_2b)

lmm_2b_ssas_asd_int <- lmer(ssas_sum
                            ~ 1
                            + time
                            (+ mi_treat
                             + time_after
                             + raads_14_sum_mc)^2 # Grand mean centered
                            + (1 | id),
                            data = study_2b)

#### Model comparisons

lrt_asd_ssas <- anova(lmm_2b_ssas_base, 
                      lmm_2b_ssas_asd, 
                      lmm_2b_ssas_asd_int)
