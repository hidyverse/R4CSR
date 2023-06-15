#### Functions for TFLs


## formatting functions

fmt_num <- function(x, digits, width = digits + 4) {
  formatC(
    x,
    digits = digits,
    format = "f",
    width = width
  )
}


fmt_est <- function(.mean,
                    .sd,
                    digits = c(1, 2)) {
  .mean <- fmt_num(.mean, digits[1], width = digits[1] + 4)
  .sd <- fmt_num(.sd, digits[2], width = digits[2] + 3)
  paste0(.mean, " (", .sd, ")")
}


fmt_ci <- function(.est,
                   .lower,
                   .upper,
                   digits = 2,
                   width = digits + 3) {
  .est <- fmt_num(.est, digits, width)
  .lower <- fmt_num(.lower, digits, width)
  .upper <- fmt_num(.upper, digits, width)
  paste0(.est, " (", .lower, ",", .upper, ")")
}


fmt_pval <- function(.p, digits = 3) {
  scale <- 10^(-1 * digits)
  p_scale <- paste0("<", digits)
  if_else(.p < scale, p_scale, fmt_num(.p, digits = digits))
}



count_by <- function(data, # Input data set
                     grp, # Group variable
                     var, # Analysis variable
                     var_label = var, # Analysis variable label
                     id = "USUBJID") { # Subject ID variable
  data <- data %>% rename(grp = !!grp, var = !!var, id = !!id)

  left_join(
    count(data, grp, var),
    count(data, grp, name = "tot"),
    by = "grp",
  ) %>%
    mutate(
      pct = fmt_num(100 * n / tot, digits = 1),
      n = fmt_num(n, digits = 0),
      npct = paste0(n, " (", pct, ")")
    ) %>%
    pivot_wider(
      id_cols = var,
      names_from = grp,
      values_from = c(n, pct, npct),
      values_fill = list(n = "0", pct = fmt_num(0, digits = 0))
    ) %>%
    mutate(var_label = var_label)
}
