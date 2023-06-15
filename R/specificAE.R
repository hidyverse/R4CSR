#### creating specific AEs table
#### heidiesteiner@arizona.edu
#### Jun 15 2023

#### locker #####
#### library
library(dplyr) # Manipulate data
library(haven) # Read SAS data
library(tidyr) # Manipulate data
library(r2rtf) # Reporting in RTF format


#### data store
adsl <- read_sas("R/data/adsl.sas7bdat") # observation data
adae <- read_sas("R/data/adae.sas7bdat") # feature (AE) data

#### functions
source("R/src/functions.R")


#### create analysis data
ana <- adae %>%
  mutate(
    AESOC = tools::toTitleCase(tolower(AESOC)),
    AEDECOD = tools::toTitleCase(tolower(AEDECOD))
  )

## create dataframes for tables
## table 1
t1 <- ana %>%
  group_by(TRTAN, AESOC) %>% # group by treatment group and organ class
  summarise(n = fmt_num(n_distinct(USUBJID), digits = 0)) %>%
  mutate(AEDECOD = AESOC, order = 0)

## table 2
t2 <- ana %>%
  group_by(TRTAN, AESOC, AEDECOD) %>%
  summarise(n = fmt_num(n_distinct(USUBJID), digits = 0)) %>%
  mutate(order = 1)


### combine AE tables
t_ae <- bind_rows(t1, t2) %>%
  pivot_wider(
    id_cols = c(AESOC, order, AEDECOD),
    names_from = TRTAN,
    names_prefix = "n_",
    values_from = n,
    values_fill = fmt_num(0, digits = 0)
  ) %>%
  arrange(AESOC, order, AEDECOD) %>%
  select(AESOC, AEDECOD, starts_with("n"))

## create population table
t_pop <- adsl %>%
  filter(SAFFL == "Y") %>%
  count_by("TRT01AN", "SAFFL",
           var_label = "Participants in population"
  ) %>%
  mutate(
    AESOC = "pop",
    AEDECOD = var_label
  ) %>%
  select(AESOC, AEDECOD, starts_with("n_"))


## create AE Table

tbl_ae_spec <- bind_rows(
  t_pop,
  data.frame(AESOC = "pop"),
  t_ae
) %>%
  mutate(AEDECOD = ifelse(AEDECOD == AESOC,
                          AEDECOD, paste0("  ", AEDECOD)
  ))


## format RTF output
n_row <- nrow(tbl_ae_spec)
n_col <- ncol(tbl_ae_spec)
id <- tbl_ae_spec$AESOC == tbl_ae_spec$AEDECOD
id <- ifelse(is.na(id), FALSE, id)

text_format <- ifelse(id, "b", "")


tbl_ae_spec %>%
  rtf_title(
    "Analysis of Participants With Specific Adverse Events",
    "(Safety Analysis Population)"
  ) %>%
  rtf_colheader(" | Placebo | Xanomeline Low Dose| Xanomeline High Dose",
                col_rel_width = c(3, rep(1, 3))
  ) %>%
  rtf_colheader(" | n |  n | n ",
                border_top = "",
                border_bottom = "single",
                col_rel_width = c(3, rep(1, 3))
  ) %>%
  rtf_body(
    col_rel_width = c(1, 3, rep(1, 3)),
    text_justification = c("l", "l", rep("c", 3)),
    text_format = matrix(text_format, nrow = n_row, ncol = n_col),
    page_by = "AESOC", # grouping output of table
    pageby_row = "first_row" # add header to first row of every new page
  ) %>%
  rtf_footnote("Every subject is counted a single time for each applicable row and column.") %>%
  rtf_encode() %>%
  write_rtf("tlf/tlf_spec_ae.rtf")



