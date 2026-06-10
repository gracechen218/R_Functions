# ---- Program Header ----
# Project: Utility Program
# Program Name: Summary_Stats_Function
# Author: Grace Chen
# Created: 3/10/2026
# Purpose: Generate RTF ready output data frame for summary statistics (continous variable and cateogrical variable)
# Revision History:
# Date        Author        Revision

# ---- Initialize Libraries ----
library(openxlsx)
library(dplyr)
library(lubridate)
library(haven)
library(openxlsx)
library(tidyverse)
library(knitr)
library(labelled)


# ---- Example Usage

# pr_c <- sum_cat(
#   base = adpr9,
#   var       = "AVALC",
#   var_label = "Number of Paradise Catheters Categories",
# )
# 
# pr_n <- bind_rows(
#   sum_cont(base=adpr1,var= "AVAL",var_label = "Procedure Time (min)"),
#   sum_cont(base=adpr2,var= "AVAL",var_label = "Device Time (min)"),
#   sum_cont(base=adpr3,var= "AVAL",var_label = "Total Emission Time (seconds)"),
#   sum_cont(base=adpr7,var= "AVAL",var_label = "    Left Side Total Emission Time (seconds)"),
#   sum_cont(base=adpr8,var= "AVAL",var_label = "    Right Side Total Emission Time (seconds)"),
#   sum_cont(base=adpr4,var= "AVAL",var_label = "Contrast Volume (mL)"),
#   sum_cont(base=adpr5,var= "AVAL",var_label = "Fluoroscopy Exposure (min)"),
#   sum_cont(base=adpr6,var= "AVAL",var_label = "Number of Paradise Catheters used during procedure"),
# )


#---- Function sum_cat: Generate summary statistics for character variables

sum_cat <- function(base, var, var_label, by = NULL) {
  
  var <- rlang::sym(var)
  by  <- rlang::enquo(by)
  
  ## Source data always = base
  data <- base %>%
    dplyr::filter(!is.na(!!var))
  
  if (rlang::quo_is_null(by)) {
    
    ## No BY variable
    cnt <- data %>%
      dplyr::summarise(cnt = dplyr::n_distinct(USUBJID)) %>%
      dplyr::pull(cnt)
    
    summ_var <- data %>%
      dplyr::group_by(!!var) %>%
      dplyr::summarise(n = dplyr::n_distinct(USUBJID), .groups = "drop") %>%
      dplyr::mutate(
        pct   = (n / cnt) * 100,
        c_pct = formatC(pct, digits = 1, format = "f"),
        c     = paste0(c_pct, "% (", n, "/", cnt, ")")
      ) %>%
      dplyr::arrange(desc(pct)) %>%
      dplyr::rename(desc = !!var) %>%
      dplyr::select(desc, c)
    
  } else {
    
    ## With BY variable
    denom <- data %>%
      dplyr::group_by(!!by) %>%
      dplyr::summarise(cnt = dplyr::n_distinct(USUBJID), .groups = "drop")
    
    summ_var <- data %>%
      dplyr::group_by(!!by, !!var) %>%
      dplyr::summarise(n = dplyr::n_distinct(USUBJID), .groups = "drop") %>%
      dplyr::left_join(denom, by = rlang::as_name(by)) %>%
      dplyr::mutate(
        pct   = (n / cnt) * 100,
        c_pct = formatC(pct, digits = 1, format = "f"),
        c     = paste0(c_pct, "% (", n, "/", cnt, ")")
      ) %>%
      dplyr::arrange(!!by, desc(pct)) %>%
      dplyr::rename(desc = !!var) %>%
      dplyr::select(!!by, desc, c)
  }
  
  ## Add header row
  firstrow <- data.frame(
    desc = var_label,
    c    = "",
    stringsAsFactors = FALSE
  )
  
  summ_var$desc <- paste0(" ", summ_var$desc)
  
  dplyr::bind_rows(firstrow, summ_var)
}

#---- Function sum_cat_ci: Generate summary statistics for character variables with CI for percentage

sum_cat_ci <- function(base, var, var_label, by = NULL, conf.level = 0.95) {
  
  var <- rlang::sym(var)
  by  <- rlang::enquo(by)
  
  ## Source data always = base
  data <- base %>%
    dplyr::filter(!is.na(!!var))
  
  ## Helper to compute CI
  get_ci <- function(x, n, conf.level) {
    if (n == 0) return(c(NA, NA))
    ci <- prop.test(x, n, conf.level = conf.level)$conf.int
    return(ci * 100)
  }
  
  if (rlang::quo_is_null(by)) {
    
    ## No BY variable
    cnt <- data %>%
      dplyr::summarise(cnt = dplyr::n_distinct(USUBJID)) %>%
      dplyr::pull(cnt)
    
    summ_var <- data %>%
      dplyr::group_by(!!var) %>%
      dplyr::summarise(n = dplyr::n_distinct(USUBJID), .groups = "drop") %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        pct   = (n / cnt) * 100,
        ci    = list(get_ci(n, cnt, conf.level)),
        lcl   = ci[[1]],
        ucl   = ci[[2]],
        c_pct = formatC(pct, digits = 1, format = "f"),
        c_ci  = paste0("(", formatC(lcl, 1, format = "f"), ", ",
                       formatC(ucl, 1, format = "f"), ")"),
        c     = paste0(c_pct, "% ", c_ci, " (", n, "/", cnt, ")")
      ) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(desc(pct)) %>%
      dplyr::rename(desc = !!var) %>%
      dplyr::select(desc, c)
    
  } else {
    
    ## With BY variable
    denom <- data %>%
      dplyr::group_by(!!by) %>%
      dplyr::summarise(cnt = dplyr::n_distinct(USUBJID), .groups = "drop")
    
    summ_var <- data %>%
      dplyr::group_by(!!by, !!var) %>%
      dplyr::summarise(n = dplyr::n_distinct(USUBJID), .groups = "drop") %>%
      dplyr::left_join(denom, by = rlang::as_name(by)) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        pct   = (n / cnt) * 100,
        ci    = list(get_ci(n, cnt, conf.level)),
        lcl   = ci[[1]],
        ucl   = ci[[2]],
        c_pct = formatC(pct, digits = 1, format = "f"),
        c_ci  = paste0("(", formatC(lcl, 1, format = "f"), ", ",
                       formatC(ucl, 1, format = "f"), ")"),
        c     = paste0(c_pct, "% ", c_ci, " (", n, "/", cnt, ")")
      ) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(!!by, desc(pct)) %>%
      dplyr::rename(desc = !!var) %>%
      dplyr::select(!!by, desc, c)
  }
  
  ## Add header row
  firstrow <- data.frame(
    desc = var_label,
    c    = "",
    stringsAsFactors = FALSE
  )
  
  summ_var$desc <- paste0(" ", summ_var$desc)
  
  dplyr::bind_rows(firstrow, summ_var)
}



# ----- Function sum_cont: Generate summary functions for continuous variables

sum_cont <- function(base, var, var_label, by = NULL, blank_row = FALSE) {
  
  var <- rlang::sym(var)
  by  <- rlang::enquo(by)
  
  ## Base data: keep non-missing analysis variable
  data_var <- base %>%
    dplyr::filter(!is.na(!!var))
  
  if (rlang::quo_is_null(by)) {
    
    ## ---- No BY variable (overall) ----
    res <- data_var %>%
      dplyr::summarise(
        c = paste(
          paste0(n_distinct(USUBJID)),
          paste0(
            formatC(mean(!!var), digits = 1, format = "f", flag = "0"),
            " ", plusminus, " ",
            formatC(sd(!!var), digits = 1, format = "f", flag = "0")
          ),
          paste0(
            formatC(median(!!var), digits = 1, format = "f", flag = "0"),
            " [",
            paste(range(!!var), collapse = ", "),
            "]"
          ),
          sep = "\n"
        )
      ) %>%
      dplyr::mutate(desc = var_label) %>%
      dplyr::select(desc, c)
    
  } else {
    
    ## ---- With BY variable (e.g. AVISIT) ----
    res <- data_var %>%
      dplyr::group_by(!!by) %>%
      dplyr::summarise(
        c = paste(
          paste0(n_distinct(USUBJID)),
          paste0(
            formatC(mean(!!var), digits = 1, format = "f", flag = "0"),
            " ", plusminus, " ",
            formatC(sd(!!var), digits = 1, format = "f", flag = "0")
          ),
          paste0(
            formatC(median(!!var), digits = 1, format = "f", flag = "0"),
            " [",
            paste(range(!!var), collapse = ", "),
            "]"
          ),
          sep = "\n"
        ),
        .groups = "drop"
      ) %>%
      dplyr::mutate(desc = var_label) %>%
      dplyr::select(!!by, desc, c)
  }
  
  res
}