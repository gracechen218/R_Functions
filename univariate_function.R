# ---- Program Header ----
# Project: Utility Program
# Program Name: Univariate_Function
# Author: Grace Chen
# Created: 6/10/2025
# Purpose: Generate publish ready table for univariate analyses results 
# Revision History:
# Date        Author        Revision
# 

# ---- Initialize Libraries ----
library(r2rtf)
library(dplyr)
library(tidyr)

# ---- Example usage ---

# predictors <- c('AGE','BMI','BOSYSBP','BODIABP','BOHR','MEDN','PRTIME','DVTIME','PUPACAN','EMITIME',"Daytime_ASYSBPA","Nighttime_ASYSBPA",'SEX','ISHFL','HHHOSP','CVHHAFI','CVHHHFYN','CVHCADYN','MHAPN','MHDIAII','MHCKD','ANETYC','ANETYG')
# 
# #Note: dataset "base" include outcome variable and all predictors variables. One row per patient. 
# final0 <- univariate_glm_fmt(
#   data = base,
#   outcome = "CHG",
#   predictors = predictors
# )
# 
# label_map <- c(
#   AGE = "Age (years)",
#   SEX = "Sex",
#   BMI = "Body Mass Index (kg/m2)",
#   BOSYSBP = "Baseline Office SBP (mmHg)",
#   BODIABP = "Baseline Office DBP (mmHg)",
#   BOHR = "Baseline Heart Rate",
#   ISHFL = "Baseline Isolated Systolic Hypertension",
#   
#   HHHOSP = "History of Hospitalized for hypertensive crisis",
#   CVHHAFI = "History of Atrial Fibrillation",
#   CVHHHFYN = "History of Heart Failure",
#   CVHCADYN = "History of Coronary artery disease (CAD)",
#   MHAPN = "History of Sleep Apnea",
#   MHDIAII = "History of Type II Diabetes",
#   MHCKD = "History of Chronic Kidney Disease",
#   
#   MEDN = "Baseline Number of Medications",
#   ANETYC = "Conscious Sedation Use",
#   ANETYG = "General Anesthesia Use",
#   PRTIME = "Procedure Time (min)",
#   DVTIME = "Device Time (min)",
#   PUPACAN = "Number of Paradise catheters used",
#   EMITIME = "Emission Time (min)",
#   Daytime_ASYSBPA = "Baseline Daytime Ambulatory SBP (mmHg)",
#   Nighttime_ASYSBPA = "Baseline Nighttime Ambulatory SBP (mmHg)"
# )
# 
# Add lable to the output table
# final0$Predictor <- label_map[final0$Variable]
# 
# final <- final0 %>% 
#   select(Predictor, Level, N, Mean_CHG, Estimate_CI, P_Value)



# ---- Function ---

univariate_glm_fmt <- function(data, outcome, predictors) {
  
  results_list <- list()
  
  for (var in predictors) {
    
    tryCatch({
      
      if (!var %in% names(data)) next
      
      is_cat <- is.factor(data[[var]]) || is.character(data[[var]])
      
      # Compute total N
      total_n <- sum(!is.na(data[[var]]) & !is.na(data[[outcome]]))
      if (total_n == 0) next
      
      if (is_cat) {
        
        # Force factor
        data[[var]] <- as.factor(data[[var]])
        levs <- levels(data[[var]])
        
        # Fit model
        model <- glm(as.formula(paste(outcome, "~", var)), 
                     data = data, family = gaussian())
        
        coef_summary <- summary(model)$coefficients
        ci <- confint.default(model)
        
        # Subgroup stats
        counts <- table(data[[var]])
        means <- tapply(data[[outcome]], data[[var]], mean, na.rm = TRUE)
        
        # Reference row
        ref <- levs[1]
        
        ref_row <- data.frame(
          Variable = var,
          Level = ref,
          N = as.numeric(counts[ref]),
          Mean_CHG = as.numeric(means[ref]),
          Estimate_CI = "Ref",
          P_Value = NA,
          stringsAsFactors = FALSE
        )
        
        # Non-reference rows
        res_rows <- list()
        
        for (lvl in levs[-1]) {
          
          coef_name <- paste0(var, lvl)
          
          if (!coef_name %in% rownames(coef_summary)) next
          
          est <- coef_summary[coef_name, "Estimate"]
          pval <- coef_summary[coef_name, "Pr(>|t|)"]
          ci_low <- ci[coef_name, 1]
          ci_high <- ci[coef_name, 2]
          
          res_rows[[lvl]] <- data.frame(
            Variable = var,
            Level = lvl,
            N = as.numeric(counts[lvl]),
            Mean_CHG = as.numeric(means[lvl]),
            Estimate_CI = sprintf("%.2f (%.2f, %.2f)", est, ci_low, ci_high),
            P_Value = ifelse(pval < 0.001, "<0.001", sprintf("%.3f", pval)),
            stringsAsFactors = FALSE
          )
        }
        
        res <- do.call(rbind, c(list(ref_row), res_rows))
        res$Mean_CHG <- sprintf("%.2f", res$Mean_CHG)
        
      } else {
        
        model <- glm(as.formula(paste(outcome, "~", var)), 
                     data = data, family = gaussian())
        
        coef_summary <- summary(model)$coefficients
        ci <- confint.default(model)
        
        # ✅ Use slope (predictor coefficient), NOT intercept
        est <- coef_summary[var, "Estimate"]
        pval <- coef_summary[var, "Pr(>|t|)"]
        ci_low <- ci[var, 1]
        ci_high <- ci[var, 2]
        
        # ✅ Mean CHG = overall outcome mean
        mean_chg <- mean(data[[outcome]], na.rm = TRUE)
        
        res <- data.frame(
          Variable = var,
          Level = var,
          N = total_n,
          Mean_CHG = NA,
          Estimate_CI = sprintf("%.2f (%.2f, %.2f)", 
                                est, ci_low, ci_high),
          P_Value = ifelse(pval < 0.001, "<0.001", 
                           sprintf("%.3f", pval)),
          stringsAsFactors = FALSE
        )
        res$Mean_CHG <- sprintf("%.2f", res$Mean_CHG)
      }
      
      
      rownames(res) <- NULL
      results_list[[var]] <- res
      
    }, error = function(e) {
      message(paste("Skipping:", var))
    })
  }
  
  if (length(results_list) == 0) return(data.frame())
  
  final_results <- do.call(rbind, results_list)
  
  return(final_results)
}


