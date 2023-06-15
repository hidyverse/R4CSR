#### specific AEs
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
adae <- read_sas("") # feature data

#### functions
source("R/src/functions.R")

ana <- adae %>%
  mutate(
    AESOC = tools::toTitleCase(tolower(AESOC)),
    AEDECOD = tools::toTitleCase(tolower(AEDECOD))
  )

t1 <- ana %>%
  group_by(TRTAN, AESOC) %>%
  summarise(n = fmt_num(n_distinct(USUBJID), digits = 0)) %>%
  mutate(AEDECOD = AESOC, order = 0)

