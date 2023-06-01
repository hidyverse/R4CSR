#### Chapter 5: Efficacy Table
#### Heidi Steiner
#### 01-06-2023

#### locker #####
#### library
library(haven) # Read SAS data
library(dplyr) # Manipulate data
library(tidyr) # Manipulate data
library(r2rtf) # Reporting in RTF format
library(emmeans) # LS mean estimation

#### data store
adsl <- read_sas("R/data/adsl.sas7bdat")
adlb <- read_sas("R/data/adlbc.sas7bdat")

#### functions
source("R/src/functions.R")

#### Wrangle analysis dataset ####
gluc <- adlb %>%
  left_join(adsl %>% select(USUBJID, EFFFL), by = "USUBJID") %>%
  # PARAMCD is parameter code and here we focus on Glucose (mg/dL)
  filter(EFFFL == "Y" & PARAMCD == "GLUC") %>%
  arrange(TRTPN) %>%
  mutate(TRTP = factor(TRTP, levels = unique(TRTP)))

ana <- gluc %>%
  filter(AVISITN > 0 & AVISITN <= 24) %>% # avisitn = numerical study visit
  arrange(AVISITN) %>%
  mutate(AVISIT = factor(AVISIT, levels = unique(AVISIT)))

#### imputate ####
ana_locf <- ana %>%
  group_by(USUBJID) %>%
  mutate(locf = AVISITN == max(AVISITN)) %>%
  filter(locf)


#### summarize data ####
t11 <- gluc %>%
  filter(AVISITN %in% c(0, 24)) %>% ## summarize baseline and week 24
  group_by(TRTPN, TRTP, AVISITN) %>%
  summarise(
    n = n(),
    mean_sd = fmt_est(mean(AVAL), sd(AVAL))
  ) %>%
  pivot_wider(
    id_cols = c(TRTP, TRTPN),
    names_from = AVISITN,
    values_from = c(n, mean_sd)
  )


t12 <- gluc %>%
  filter(AVISITN %in% 24) %>% ## summarize change from baseline
  group_by(TRTPN, AVISITN) %>%
  summarise(
    n_chg = n(),
    mean_chg = fmt_est(
      mean(CHG, na.rm = TRUE),
      sd(CHG, na.rm = TRUE)
    )
  )

### model ####

fit <- lm(CHG ~ BASE + TRTP, data = ana_locf)
summary(fit)

fit_within <- emmeans(fit, "TRTP")
fit_within

t13 <- fit_within %>%
  as_tibble() %>%
  mutate(ls = fmt_ci(emmean, lower.CL, upper.CL)) %>%
  select(TRTP, ls)


fit_between <- pairs(fit_within, reverse = TRUE)

t2 <- fit_between %>%
  as_tibble() %>%
  mutate(
    ls = fmt_ci(
      estimate,
      estimate - 1.96 * SE,
      estimate + 1.96 * SE
    ),
    p = fmt_pval(p.value)
  ) %>%
  filter(stringr::str_detect(contrast, "- Placebo")) %>%
  select(contrast, ls, p)

### build report table ####
##### table 1  #####

t1 <- cbind(
  t11 %>% ungroup() %>% select(TRTP, ends_with("0"), ends_with("24")),
  t12 %>% ungroup() %>% select(ends_with("chg")),
  t13 %>% ungroup() %>% select(ls)
)


t1_rtf <- t1 %>%
  data.frame() %>%
  rtf_title(c(
    "ANCOVA of Change from Baseline Glucose (mmol/L) at Week 24",
    "LOCF",
    "Efficacy Analysis Population"
  )) %>%
  rtf_colheader("| Baseline | Week 24 | Change from Baseline",
                col_rel_width = c(2.5, 2, 2, 4)
  ) %>%
  rtf_colheader(
    paste(
      "Treatment |",
      paste0(rep("N | Mean (SD) | ", 3), collapse = ""),
      "LS Mean (95% CI){^a}"
    ),
    col_rel_width = c(2.5, rep(c(0.5, 1.5), 3), 2)
  ) %>%
  rtf_body(
    text_justification = c("l", rep("c", 7)),
    col_rel_width = c(2.5, rep(c(0.5, 1.5), 3), 2)
  ) %>%
  rtf_footnote(c(
    "{^a}Based on an ANCOVA model after adjusting baseline value. LOCF approach is used to impute missing values.",
    "ANCOVA = Analysis of Covariance, LOCF = Last Observation Carried Forward",
    "CI = Confidence Interval, LS = Least Squares, SD = Standard Deviation"
  ))

# t1_rtf %>%
#   rtf_encode() %>%
#   write_rtf("tlf/tlf_eff1.rtf")

##### table 2 #####
t2_rtf <- t2 %>%
  data.frame() %>%
  rtf_colheader("Pairwise Comparison | Difference in LS Mean (95% CI){^a} | p-Value",
                col_rel_width = c(4.5, 4, 2)
  ) %>%
  rtf_body(
    text_justification = c("l", "c", "c"),
    col_rel_width = c(4.5, 4, 2)
  )

t2_rtf %>%
  rtf_encode() %>%
  write_rtf("tlf/tlf_eff2.rtf")
