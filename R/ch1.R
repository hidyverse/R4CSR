## learning r2rtf
## heidi steiner
## 05/11/23

## library 
library(dplyr) # Manipulate data
library(tidyr) # Manipulate data
library(r2rtf) # Reporting in RTF format

## start rtf 
r2rtf_adae %>%
  select(USUBJID, TRTA, AEDECOD) %>%
  head(4)

## count aes/id
tbl <- 
  r2rtf_adae %>%
  count(TRTA, AEDECOD) %>%
  pivot_wider(names_from = TRTA, values_from = n, values_fill = 0)

tbl %>% 
  head(4)

head(tbl) %>%
  rtf_body() %>% # Step 1 Add table attributes
  rtf_encode() %>% # Step 2 Convert attributes to RTF encode
  write_rtf("tlf/intro-ae1.rtf") # Step 3 Write to a .rtf file
