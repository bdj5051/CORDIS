library(forestploter)
library(dplyr)

yuhs_results <- readRDS("/Users/jennifer/Desktop/results/cohort_method_result_YUHS.rds")
amc_results <- readRDS("/Users/jennifer/Desktop/results/cohort_method_result_AMC.rds")
kumc_results <- readRDS("/Users/jennifer/Desktop/results/cohort_method_result_KUMC.rds")
meta_results <- readRDS("//Users/jennifer/Desktop/results/cohort_method_result_Meta-analysis.rds")
total_results <- rbind(amc_results, yuhs_results, kumc_results, meta_results)

subset <- total_results %>% filter(outcome_id %in% c(1029, 1112), analysis_id %in% c(1,2)) %>% 
  select(database_id, analysis_id, target_id, outcome_id, rr, ci_95_lb, ci_95_ub, p, calibrated_rr, calibrated_ci_95_lb, calibrated_ci_95_ub, calibrated_p,
         target_outcomes, comparator_outcomes, target_days, comparator_days) %>% as.data.frame() %>% 
  arrange(analysis_id, outcome_id, target_id)
openxlsx::write.xlsx(subset, "/Users/jennifer/Desktop/results/results.xlsx")
# MACE --------------------------------------------------------------------
mace_1y <- total_results %>% filter(target_id == 1365 & outcome_id == 1029 & analysis_id== 1 & database_id %in% c("YUHS", "AMC", "KUMC")) %>% 
  select(database_id, target_outcomes, target_subjects, comparator_outcomes, comparator_subjects, calibrated_rr, calibrated_ci_95_lb, calibrated_ci_95_ub, calibrated_log_rr, calibrated_se_log_rr, calibrated_p) %>% 
  arrange(desc(database_id)) %>% as.data.frame()
mace_meta_1y <- meta_results %>% filter(target_id == 1365 & outcome_id == 1029 & analysis_id== 1) %>% 
  select(database_id, target_outcomes, target_subjects, comparator_outcomes, comparator_subjects, calibrated_rr, calibrated_ci_95_lb, calibrated_ci_95_ub, calibrated_log_rr, calibrated_se_log_rr, calibrated_p) %>% 
  as.data.frame()

meta1 <- meta::metagen(data = mace_1y[mace_1y$database_id %in% c("YUHS", "AMC"),],
                       TE = calibrated_log_rr,
                       seTE = calibrated_se_log_rr,
                       sm = "HR")
meta::forest(meta1)

mace_3y <- total_results %>% filter(target_id == 1365 & outcome_id == 1029 & analysis_id== 2 & database_id %in% c("YUHS", "AMC", "KUMC")) %>% 
  select(database_id, target_outcomes, target_subjects, comparator_outcomes, comparator_subjects, calibrated_rr, calibrated_ci_95_lb, calibrated_ci_95_ub, calibrated_log_rr, calibrated_se_log_rr, calibrated_p) %>% 
  arrange(desc(database_id)) %>% as.data.frame()
mace_meta_3y <- meta_results %>% filter(target_id == 1365 & outcome_id == 1029 & analysis_id== 2) %>% 
  select(database_id, target_outcomes, target_subjects, comparator_outcomes, comparator_subjects, calibrated_rr, calibrated_ci_95_lb, calibrated_ci_95_ub, calibrated_log_rr, calibrated_se_log_rr, calibrated_p) %>% 
  arrange(desc(database_id)) %>% as.data.frame()

meta2 <- meta::metagen(data = mace_3y[mace_3y$database_id %in% c("YUHS", "AMC"),],
                       TE = calibrated_log_rr,
                       seTE = calibrated_se_log_rr,
                       sm = "HR")
meta::forest(meta2)

mace <- rbind(mace_1y, mace_meta_1y, mace_3y, mace_meta_3y)

data_mace <- data.frame(
  Study = c("Intention to treat (1Y)", "YUHS", "AMC", "KUMC", "Overall", "Intention to treat (3Y)", "YUHS", "AMC", "KUMC", "Overall"),
  Romosozumab = c("", "13/1,563", "4/885", "1/196", "17/2,448", "", "19/1,563", "5/885", "3/196", "242,448"),
  Denosumab = c("", "5/1,563", "7/885", "2/196", "16/2,448", "", "11/1,563", "5/885", "4/196", "16/2,448"),
  HR = c(NA, 1.1846458, 0.5391118, 0.5, 0.9137345, NA, 1.3717145, 0.7216359, 0.5, 1.1615856),
  CI_Lower = c(NA, 0.4647077, 0.1379681, 0.5, 0.4042056, NA, 0.6468487, 0.2006072,0.5, 0.6029928),
  CI_Upper = c(NA, 3.019932, 2.106585, 0.5, 2.065559, NA, 2.908873, 2.595911,0.5,2.237640),
  Weight = c(NA, 0.68, 0.32, 1, 0.8, NA, 0.68, 0.32, 1, 0.8)
)

data_mace$Group <- ifelse(is.na(data_mace$HR), 
                          data_mace$Study,
                          paste0("   ", data_mace$Study))
data_mace$` ` <- paste(rep(" ", 20), collapse = " ")
data_mace$`Calibrated HR (95% CI)` <-  ifelse(is.na(data_mace$HR), "",
                              sprintf("%.2f (%.2f - %.2f)",
                                      round(data_mace$HR,2), round(data_mace$CI_Lower,2), round(data_mace$CI_Upper,2)))
tm <- forest_theme(base_size = 10,
                   # Graphical parameters of confidence intervals
                   ci_pch = 15,
                   ci_col = "black",
                   ci_fill = "black",
                   ci_alpha = 1,
                   ci_lty = 1,
                   ci_lwd = 1.3,
                   ci_Theight = 0.1,
                   # Graphical parameters of reference line
                   refline_lwd = 1,
                   refline_lty = "dashed",
                   refline_col = "grey20",
                   # Graphical parameters of vertical line
                   vertline_lwd = 1,
                   vertline_lty = "dashed",
                   vertline_col = "grey20",
                   # Graphical parameters of diamond shaped summary CI
                   summary_fill = "#006400",
                   summary_col = "#006400",
                   core=list(bg_params=list(fill = "White"),
                             fg_params=list(hjust = 1, x = 0.9)),
                   colhead=list(fg_params=list(hjust=0.5, x=0.5))
                   )

# # Final data manipulation part 
data_mace$CI_Upper <- as.numeric(data_mace$CI_Upper)
data_mace$CI_Lower <- as.numeric(data_mace$CI_Lower)
data_mace$HR <- as.numeric(data_mace$HR)
data_mace$summary <- c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE)

# Forest plot
pt <- forest(data_mace[,c(1:3, 10,9)],
             est = data_mace$HR,
             lower = data_mace$CI_Lower, 
             upper = data_mace$CI_Upper,
             is_summary = data_mace$summary,
             sizes = data_mace$Weight,
             ci_column = 5,
             ref_line = 1,
             # arrow_lab = c("Favors Romosozumab", "Favors Denosumab"),
             ticks_at = c(0.1, 0.5, 1, 2.5),
             x_trans = "log",
             # xlab = "Hazard Ratio (95% CI)",
             # title = "MACE, 1-Year and 3-Year Outcomes",
             theme = tm)

plot(pt)


# CV event --------------------------------------------------------------------
cv_1y <- total_results %>% filter(target_id == 1365 & outcome_id == 1112 & analysis_id== 1 & database_id %in% c("YUHS", "AMC", "KUMC")) %>% 
  select(database_id, target_outcomes, target_subjects, comparator_outcomes, comparator_subjects, calibrated_rr, calibrated_ci_95_lb, calibrated_ci_95_ub, calibrated_log_rr, calibrated_se_log_rr, calibrated_p) %>% 
  arrange(desc(database_id)) %>% as.data.frame()
cv_meta_1y <- meta_results %>% filter(target_id == 1365 & outcome_id == 1112 & analysis_id== 1) %>% 
  select(database_id, target_outcomes, target_subjects, comparator_outcomes, comparator_subjects, calibrated_rr, calibrated_ci_95_lb, calibrated_ci_95_ub, calibrated_log_rr, calibrated_se_log_rr, calibrated_p) %>% 
  as.data.frame()

meta1 <- meta::metagen(data = cv_1y[cv_1y$database_id %in% c("YUHS", "AMC"),],
                       TE = calibrated_log_rr,
                       seTE = calibrated_se_log_rr,
                       sm = "HR")
meta::forest(meta1)

cv_3y <- total_results %>% filter(target_id == 1365 & outcome_id == 1112 & analysis_id== 2 & database_id %in% c("YUHS", "AMC", "KUMC")) %>% 
  select(database_id, target_outcomes, target_subjects, comparator_outcomes, comparator_subjects, calibrated_rr, calibrated_ci_95_lb, calibrated_ci_95_ub, calibrated_log_rr, calibrated_se_log_rr, calibrated_p) %>% 
  arrange(desc(database_id)) %>% as.data.frame()
cv_meta_3y <- meta_results %>% filter(target_id == 1365 & outcome_id == 1112 & analysis_id== 2) %>% 
  select(database_id, target_outcomes, target_subjects, comparator_outcomes, comparator_subjects, calibrated_rr, calibrated_ci_95_lb, calibrated_ci_95_ub, calibrated_log_rr, calibrated_se_log_rr, calibrated_p) %>% 
  arrange(desc(database_id)) %>% as.data.frame()

meta2 <- meta::metagen(data = cv_3y[cv_3y$database_id %in% c("YUHS", "AMC"),],
                       TE = calibrated_log_rr,
                       seTE = calibrated_se_log_rr,
                       sm = "HR")
meta::forest(meta2)

cv <- rbind(cv_1y, cv_meta_1y, cv_3y, cv_meta_3y)

data_cv <- data.frame(
  Study = c("Intention to treat (1Y)", "YUHS", "AMC", "KUMC", "Overall", "Intention to treat (3Y)", "YUHS", "AMC", "KUMC", "Overall"),
  Romosozumab = c("", "13/1,563", "4/885","1/190", "17/2,448", "", "19/1,563", "5/885", "4/190", "242,448"),
  Denosumab = c("", "5/1,563", "7/885","4/190", "16/2,448", "", "11/1,563", "5/885","5/190", "16/2,448"),
  HR = c(NA, 0.6870380, 0.5413595, 0.5, 0.6749568, NA, 0.8478685, 0.6365608, 0.5, 0.8033003),
  CI_Lower = c(NA, 0.3884682, 0.1385389, 0.5, 0.3931479, NA, 0.5428571, 0.2080681, 0.5, 0.5248112),
  CI_Upper = c(NA, 1.215083, 2.115436, 0.5, 1.158766, NA, 1.324255, 1.947486, 0.5, 1.229569),
  Weight = c(NA, 0.68, 0.32, 1, 0.8, NA, 0.68, 0.32, 1, 0.8)
)

data_cv$Group <- ifelse(is.na(data_cv$HR), 
                        data_cv$Study,
                        paste0("   ", data_cv$Study))
data_cv$` ` <- paste(rep(" ", 20), collapse = " ")
data_cv$`Calibrated HR (95% CI)` <-  ifelse(is.na(data_cv$HR), "",
                                            sprintf("%.2f (%.2f - %.2f)",
                                                    round(data_cv$HR,2), round(data_cv$CI_Lower,2), round(data_cv$CI_Upper,2)))
tm <- forest_theme(base_size = 10,
                   # Graphical parameters of confidence intervals
                   ci_pch = 15,
                   ci_col = "black",
                   ci_fill = "black",
                   ci_alpha = 1,
                   ci_lty = 1,
                   ci_lwd = 1.3,
                   ci_Theight = 0.1,
                   # Graphical parameters of reference line
                   refline_lwd = 1,
                   refline_lty = "dashed",
                   refline_col = "grey20",
                   # Graphical parameters of vertical line
                   vertline_lwd = 1,
                   vertline_lty = "dashed",
                   vertline_col = "grey20",
                   # Graphical parameters of diamond shaped summary CI
                   summary_fill = "#006400",
                   summary_col = "#006400",
                   core=list(bg_params=list(fill = "White"),
                             fg_params=list(hjust = 1, x = 0.9)),
                   colhead=list(fg_params=list(hjust=0.5, x=0.5))
)

# # Final data manipulation part 
data_cv$CI_Upper <- as.numeric(data_cv$CI_Upper)
data_cv$CI_Lower <- as.numeric(data_cv$CI_Lower)
data_cv$HR <- as.numeric(data_cv$HR)
data_cv$summary <- c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE)

# Forest plot
pt <- forest(data_cv[,c(1:3, 10,9)],
             est = data_cv$HR,
             lower = data_cv$CI_Lower, 
             upper = data_cv$CI_Upper,
             is_summary = data_cv$summary,
             sizes = data_cv$Weight,
             ci_column = 5,
             ref_line = 1,
             # arrow_lab = c("Favors Romosozumab", "Favors Denosumab"),
             ticks_at = c(0.1, 0.5, 1, 2.5),
             x_trans = "log",
             # xlab = "Hazard Ratio (95% CI)",
             # title = "cv, 1-Year and 3-Year Outcomes",
             theme = tm)

plot(pt)

# Sub Group (MACE) --------------------------------------------------------------------
cv_meta_subgroup <- meta_results %>% filter(target_id != 1365 & outcome_id == 1112 & analysis_id %in% c(1:2))%>% 
  select(analysis_id, target_id, target_outcomes, target_subjects, comparator_outcomes, comparator_subjects, calibrated_rr, calibrated_ci_95_lb, calibrated_ci_95_ub) %>% 
  arrange(analysis_id) %>% as.data.frame()

data_cv <- data.frame(
  Study = c("Intention to treat (1Y)", "Sex", "Male", "Female", "Age", "<65Y", ">= 65Y", "Fracture", "No History", "Has history", 
            "Intention to treat (3Y)", "Sex", "Male", "Female", "Age", "<65Y", ">= 65Y", "Fracture", "No History", "Has history"),
  Romosozumab = c("", "", "2/133", "10/2,140", "", "2/506", "15/1,703", "", "10/756", "5/1,391",
                  "", "", "7/284", "16/2,140", "", "2/506", "21/1,703", "", "17/1,703", "7/1,391"),
  Denosumab = c("", "", "1/133", "11/2,140", "", "2/506", "12/1,703", "", "4/756", "6/1,391",
                "", "", "3/284", "14/2,140", "", "3/506", "18/1,703", "", "7/1,703", "8/1,391"),
  HR = c(NA, NA, 1.4832896, 0.7150507, NA, 0.7613091, 0.9781425, NA, 1.0752873, 0.9417579,
         NA, NA, 1.9075652, 0.7364923, NA, 1.0318222, 0.8984266, NA, 1.2040023, 0.9916644),
  CI_Lower = c(NA, NA, 0.4258693, 0.3831948, NA, 0.1501471, 0.3964940, NA, 0.5222360, 0.3976585,
               NA, NA, 0.6100132, 0.4564879, NA, 0.3340253, 0.5023553, NA, 0.3579163, 0.5049808),
  CI_Upper = c(NA, NA, 5.166252, 1.334302, NA, 3.860158, 2.413058, NA, 2.214023, 2.230326,
               NA, NA, 5.965125, 1.188248, NA, 3.187355, 1.606772, NA, 4.050169, 1.947398)
)

data_cv$Group <- ifelse(is.na(data_cv$HR), 
                        data_cv$Study,
                        paste0("   ", data_cv$Study))
data_cv$` ` <- paste(rep(" ", 20), collapse = " ")
data_cv$`Calibrated HR (95% CI)` <-  ifelse(is.na(data_cv$HR), "",
                                              sprintf("%.2f (%.2f - %.2f)",
                                                      round(data_cv$HR,2), round(data_cv$CI_Lower,2), round(data_cv$CI_Upper,2)))

tm <- forest_theme(base_size = 10,
                   # Graphical parameters of confidence intervals
                   ci_pch = 15,
                   ci_col = "black",
                   ci_fill = "black",
                   ci_alpha = 1,
                   ci_lty = 1,
                   ci_lwd = 1.3,
                   ci_Theight = 0.1,
                   # Graphical parameters of reference line
                   refline_lwd = 1,
                   refline_lty = "dashed",
                   refline_col = "grey20",
                   # Graphical parameters of vertical line
                   vertline_lwd = 1,
                   vertline_lty = "dashed",
                   vertline_col = "grey20",
                   # Graphical parameters of diamond shaped summary CI
                   summary_fill = "#006400",
                   summary_col = "#006400",
                   core=list(bg_params=list(fill = "White"),
                             fg_params=list(hjust = 1, x = 0.9)),
                   colhead=list(fg_params=list(hjust=0.5, x=0.5))
)

# # Final data manipulation part 
data_cv$CI_Upper <- as.numeric(data_cv$CI_Upper)
data_cv$CI_Lower <- as.numeric(data_cv$CI_Lower)
data_cv$HR <- as.numeric(data_cv$HR)

# Forest plot
pt <- forest(data_cv[,c(1:3, 8, 7)],
             est = data_cv$HR,
             lower = data_cv$CI_Lower, 
             upper = data_cv$CI_Upper,
             sizes = 0.7,
             ci_column = 5,
             ref_line = 1,
             # arrow_lab = c("Favors Romosozumab", "Favors Denosumab"),
             ticks_at = c(0.5, 1, 2.5),
             x_trans = "log",
             # xlab = "Hazard Ratio (95% CI)",
             # title = "cv, 1-Year and 3-Year Outcomes",
             theme = tm)

plot(pt)

# Sub Group (MACE)  ----------------------------------------------------------------------
mace_meta_subgroup <- meta_results %>% filter(target_id != 1365 & outcome_id == 1029 & analysis_id %in% c(1:2))%>% 
  select(analysis_id, target_id, target_outcomes, target_subjects, comparator_outcomes, comparator_subjects, calibrated_rr, calibrated_ci_95_lb, calibrated_ci_95_ub) %>% 
  arrange(analysis_id) %>% as.data.frame()

data_mace <- data.frame(
  Study = c("Intention to treat (1Y)", "Sex", "Male", "Female", "Age", "<65Y", ">= 65Y", "Fracture", "No History", "Has history", 
            "Intention to treat (3Y)", "Sex", "Male", "Female", "Age", "<65Y", ">= 65Y", "Fracture", "No History", "Has history"),
  Romosozumab = c("", "", "7/271", "20/2,077", "", "4/497", "25/1,636", "", "18/1,083", "12/1,349",
                  "", "", "7/284", "16/2,140", "", "2/506", "21/1,703", "", "17/1,703", "7/1,391"),
  Denosumab = c("", "", "9/271", "33/2,077", "", "8/791", "38/1,703", "", "4/756", "6/1,391",
                "", "", "3/284", "14/2,140", "", "3/506", "18/1,703", "", "7/1,703", "8/1,391"),
  HR = c(NA, NA, 2.0001057, 0.8223766, NA, 0.5932835, 1.1147123, NA, 2.0837088, 0.7123419,
         NA, NA, 3.0817624, 0.9599110, NA, 0.5127866, 0.9808907, NA, 2.3385575, 0.7604230),
  CI_Lower = c(NA, NA, 0.13191962, 0.32924755, NA, 0.06835126, 0.49284240, NA, 0.63036472, 0.19997494,
               NA, NA, 0.68636743, 0.45208724, NA, 0.07490594, 0.51351429, NA, 0.93617608, 0.25683221),
  CI_Upper = c(NA, NA, 30.324701, 2.054088, NA, 5.149654, 2.521260, NA, 6.887826, 2.537473,
               NA, NA, 13.836990, 2.038167, NA, 3.510403, 1.873651, NA, 5.841691, 2.251444)
)

data_mace$Group <- ifelse(is.na(data_mace$HR), 
                          data_mace$Study,
                          paste0("   ", data_mace$Study))
data_mace$` ` <- paste(rep(" ", 20), collapse = " ")
data_mace$`Calibrated HR (95% CI)` <-  ifelse(is.na(data_mace$HR), "",
                                              sprintf("%.2f (%.2f - %.2f)",
                                                      round(data_mace$HR,2), round(data_mace$CI_Lower,2), round(data_mace$CI_Upper,2)))

tm <- forest_theme(base_size = 10,
                   # Graphical parameters of confidence intervals
                   ci_pch = 15,
                   ci_col = "black",
                   ci_fill = "black",
                   ci_alpha = 1,
                   ci_lty = 1,
                   ci_lwd = 1.3,
                   ci_Theight = 0.1,
                   # Graphical parameters of reference line
                   refline_lwd = 1,
                   refline_lty = "dashed",
                   refline_col = "grey20",
                   # Graphical parameters of vertical line
                   vertline_lwd = 1,
                   vertline_lty = "dashed",
                   vertline_col = "grey20",
                   # Graphical parameters of diamond shaped summary CI
                   summary_fill = "#006400",
                   summary_col = "#006400",
                   core=list(bg_params=list(fill = "White"),
                             fg_params=list(hjust = 1, x = 0.9)),
                   colhead=list(fg_params=list(hjust=0.5, x=0.5))
)

# # Final data manipulation part 
data_mace$CI_Upper <- as.numeric(data_mace$CI_Upper)
data_mace$CI_Lower <- as.numeric(data_mace$CI_Lower)
data_mace$HR <- as.numeric(data_mace$HR)

# Forest plot
pt <- forest(data_mace[,c(1:3, 9, 8)],
             est = data_mace$HR,
             lower = data_mace$CI_Lower, 
             upper = data_mace$CI_Upper,
             sizes = 0.7,
             ci_column = 5,
             ref_line = 1,
             # arrow_lab = c("Favors Romosozumab", "Favors Denosumab"),
             ticks_at = c(0.5, 1, 2.5),
             x_trans = "log",
             # xlab = "Hazard Ratio (95% CI)",
             # title = "cv, 1-Year and 3-Year Outcomes",
             theme = tm)

plot(pt)

