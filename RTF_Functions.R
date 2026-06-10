# ---- Program Header ----
# Project: Utility Program
# Program Name: RTF_Functions
# Author: Grace Chen
# Created: 10/9/2025
# Purpose: RTF functions for Table and Listing reports 
# Revision History:
# Date        Author        Revision
# 

# ---- Initialize Libraries ----````
library(r2rtf)
library(dplyr)
library(tidyr)

# ---- Special character ---

#plus and minus: \'b1
#greater and equals to: \u8805?
#less and equals to: \u8804?
#superscript" {\\super 2}
#footnote wrap to next line: \line
#table header wrap to next line: \n


# plusminus <- "\\'b1"
# le <- '\\leq'
# ge <- '\\geq'


# ---- Define System parameters---
runtime <- Sys.time()
prg_path <- rstudioapi::getSourceEditorContext()$path
prg_name <- basename(prg_path)
# prg_name <- gsub("_", "", prg_name)
user <- toupper(Sys.info()[["user"]])

#footnote4 <- paste0("Source Program: ", prg_path)
#footnote4 <- paste0("Source Program: ", prg_name)
footnote5 <- paste0("Data Extract Date: ", extract_dt)
footnote6 <- paste0("Run Date: ", runtime)
footnote7 <- paste0("Programmer: ", user)

# source("02_Program/Batch/RTF_Functions.R")

# rtf_X_X(
#   data=final,
#   number="Listing XXX",
#   title="",
#   subtitle1="Treated Patients",
#   subtitle2= "US GPS",
#   colheader= "||", 
#   colheader2= '',
#   colwidth = c(4,3,3,3,4,6,3,3,3,3),
#   just = c("l","c","c","c","c","c","c","c","c","c"),
#   hjust = c("l","c","c","c","c","c","c","c","c","c"),
#   footnote1 = paste(""),
#   footnote2 = "",
#   footnote3 = "",
#   footnote4 = paste0("Source Program: ", prg_name),  
#   footnote5 = paste0("Data Extract Date :", extract_dt),
#   footnote6 = paste0("Run Date :", runtime),  
#   footnote7 = paste0("Programmer :", user), 
#   output = "04_Output/.rtf"
# )




# ---- Function to generate RTF Table
RTF_TABLE <- function(data, orientation, number, title,
                    subtitle1, subtitle2,
                    colheader, colheader2,
                    colwidth, hjust, just,
                    footnote1, footnote2, footnote3,
                    footnote4, footnote5, footnote6, footnote7,output) {
  
  comb_title <- paste0 (number," ",title,"-",subtitle1,"-",subtitle2)
  
  data %>%
    rtf_page(
      orientation = orientation,
      width = ifelse(orientation == "portrait", 8.5, 11),
      height = ifelse(orientation == "portrait", 11, 8.5),
      margin = c(1.25, 1.0, 1.0, 1.0, 1.75, 1.0),
      nrow = ifelse(orientation == "portrait", 50, 23),
      border_first = "single",
      border_last = "single",
      border_color_first = NULL,
      border_color_last = NULL,
      col_width = ifelse(orientation == "portrait", 8.5, 11) - ifelse(orientation == "portrait", 2.25, 2.5),
      use_color = FALSE
      # ,
      # use_i18n = FALSE
    ) %>%
    rtf_title(
      title = comb_title,
      text_justification = "l",
      # title = c(number, title),
      # subtitle = c(subtitle1, subtitle2),
      # text_justification = "c",
      text_format = "b",
      text_font_size = 10
    ) %>%
    rtf_colheader(
      colheader = colheader,
      col_rel_width = colwidth,
      text_justification = hjust,
      text_format = "b",
      border_left = rep("", length(colheader)),
      border_right = rep("", length(colheader))
    ) %>%
    rtf_colheader(
      colheader = colheader2,
      col_rel_width = colwidth,
      text_justification = hjust,
      border_top = "",
      text_format = "b",
      border_left = rep("", length(colheader2)),
      border_right = rep("", length(colheader2))
    ) %>%
    rtf_body(
      as_colheader = FALSE,
      col_rel_width = colwidth,
      border_bottom = "",
      text_justification = just,
      text_font_size = 9,
      cell_height = 0.15,
      border_left = "",
      border_right = "",
      border_first = "single",
      border_last = "single",
      last_row = FALSE
    ) %>%
    rtf_footnote(
      footnote = c(footnote1, footnote2, footnote3, 
                   footnote4, footnote5, footnote7, footnote6),
      border_left = "",
      border_right = "",
      border_bottom = "",
      text_font_size = 8,
      text_convert = FALSE
    ) %>%
    rtf_page_footer("Page \\chpgn", text_font_size = 7) %>%
    rtf_encode() %>%
    write_rtf(output)
}



# ---- Function to generate RTF listing
RTF_LIST <- function(data, orientation,number, title,
                    subtitle1, subtitle2,
                    colheader, colheader2,
                    colwidth, hjust, just,
                    footnote1, footnote2, footnote3, 
                    footnote4, footnote5, footnote6, footnote7,output) {
  
  comb_title <- paste0 (number," ",title,"-",subtitle1,"-",subtitle2)
  
  data %>%
    rtf_page(
      orientation = orientation,
      width = ifelse(orientation == "portrait", 8.5, 11),
      height = ifelse(orientation == "portrait", 11, 8.5),
      margin = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5),
      # nrow = ifelse(orientation == "portrait", 50, 33),
      nrow = ifelse(orientation == "portrait", 50, 50),
      border_first = "single",
      border_last = "single",
      border_color_first = NULL,
      border_color_last = NULL,
      # col_width = ifelse(orientation == "portrait", 8.5, 11) - ifelse(orientation == "portrait", 2.25, 2.5),
      col_width = ifelse(orientation == "portrait", 8.5, 11) - ifelse(orientation == "portrait", 0.5, 1),
      use_color = FALSE
      # ,
      # use_i18n = FALSE
    ) %>%
    rtf_title(
      title = comb_title,
      text_justification = "l",
      # title = c(number, title),
      # subtitle = c(subtitle1, subtitle2),
      # text_justification = "c",
      text_format = "b",
      text_font_size = 10
    ) %>%
    rtf_colheader(
      colheader = colheader,
      col_rel_width = colwidth,
      text_justification = hjust,
      text_format = "b",
      border_left = rep("", length(colheader)),
      border_right = rep("", length(colheader))
    ) %>%
    rtf_colheader(
      colheader = colheader2,
      col_rel_width = colwidth,
      text_justification = hjust,
      border_top = "",
      text_format = "b",
      border_left = rep("", length(colheader2)),
      border_right = rep("", length(colheader2))
    ) %>%
    rtf_body(
      as_colheader = FALSE,
      col_rel_width = colwidth,
      border_bottom = "",
      text_justification = just,
      text_font_size = 9,
      cell_height = 0.15,
      border_left = "",
      border_right = "",
      border_first = "single",
      border_last = "single",
      last_row = FALSE
    ) %>%
    rtf_footnote(
      footnote = c(footnote1, footnote2, footnote3,
                   footnote4, footnote5, footnote7, footnote6),
      border_left = "",
      border_right = "",
      border_bottom = "",
      text_font_size = 8,
      text_convert = FALSE
    ) %>%
    rtf_page_footer("Page \\chpgn", text_font_size = 7) %>%
    rtf_encode() %>%
    write_rtf(output)
}



RTF_TABLE_DoubleHeader <- function(data, orientation, number, title,
                      subtitle1, subtitle2,
                      colheader, colheader2,
                      colwidth, hjust, just,
                      colwidth2, hjust2,
                      footnote1, footnote2, footnote3, 
                      footnote4, footnote5, footnote6, footnote7,output) {
  
  comb_title <- paste0 (number," ",title,"-",subtitle1,"-",subtitle2)
  
  data %>%
    rtf_page(
      orientation = orientation,
      width = ifelse(orientation == "portrait", 8.5, 11),
      height = ifelse(orientation == "portrait", 11, 8.5),
      margin = c(1.25, 1.0, 1.0, 1.0, 1.75, 1.0),
      nrow = ifelse(orientation == "portrait", 50, 46),
      border_first = "single",
      border_last = "single",
      border_color_first = NULL,
      border_color_last = NULL,
      col_width = ifelse(orientation == "portrait", 8.5, 11) - ifelse(orientation == "portrait", 2.25, 2.5),
      use_color = FALSE
      # ,
      # use_i18n = FALSE
    ) %>%
    rtf_title(
      title = comb_title,
      text_justification = "l",
      # title = c(number, title),
      # subtitle = c(subtitle1, subtitle2),
      # text_justification = "c",
      text_format = "b",
      text_font_size = 10
    ) %>%
    rtf_colheader(
      colheader = colheader2,
      col_rel_width = colwidth2,
      text_justification = hjust2,
      text_format = "b",
      border_left = rep("", length(colheader2)),
      border_right = rep("", length(colheader2))
    ) %>%
    rtf_colheader(
      colheader = colheader,
      col_rel_width = colwidth,
      text_justification = hjust,
      border_top = "",
      text_format = "b",
      border_left = rep("", length(colheader)),
      border_right = rep("", length(colheader))
    ) %>%
    rtf_body(
      as_colheader = FALSE,
      col_rel_width = colwidth,
      border_bottom = "",
      text_justification = just,
      text_font_size = 9,
      cell_height = 0.15,
      border_left = "",
      border_right = "",
      border_first = "single",
      border_last = "single",
      last_row = FALSE
    ) %>%
    rtf_footnote(
      footnote = c(footnote1, footnote2, footnote3, 
                   footnote4, footnote5, footnote7, footnote6),
      border_left = "",
      border_right = "",
      border_bottom = "",
      text_font_size = 8,
      text_convert = FALSE
    ) %>%
    rtf_page_footer("Page \\chpgn", text_font_size = 7) %>%
    rtf_encode() %>%
    write_rtf(output)
}




