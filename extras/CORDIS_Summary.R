library(openxlsx)
library(dplyr)

# Table List --------------------------------------------------------------

covariates <- read.csv("/Users/jennifer/Desktop/results/export_amc/covariate.csv")
Demographics <- covariates %>% filter(covariate_analysis_id %in% c(1,3),
                      analysis_id == 2) %>% 
  mutate(name = sub(".*group: (.+)$", "\\1", covariate_name)) %>% 
  arrange(covariate_id) %>% 
  select(covariate_id, name)
colnames(Demographics) <- SqlRender::snakeCaseToCamelCase(colnames(Demographics))

General <- covariates %>% filter(covariate_analysis_id==210,
                                 analysis_id == 2,
                                 covariate_id %in% c(75053210, 4006969210,438409210,4212540210,255573210,201606210,4182210210,440383210,201820210,318800210,192671210,439727210,432867210,316866210,4104000210,433736210,80180210,255848210,140168210,4030518210,80809210,435783210,4279309210,81893210,81902210,197494210,4134440210)) %>% 
  mutate(name = sub(".*index: (.+)$", "\\1", covariate_name)) %>% 
  arrange(name) %>% 
  select(covariate_id, name)
colnames(General) <- SqlRender::snakeCaseToCamelCase(colnames(General))

CardiovascularDisease <- covariates %>% filter(covariate_analysis_id==210,
                                               analysis_id == 2,
                                               covariate_id %in% c(313217210,381591210,317576210,321588210,316139210,4185932210,321052210,440417210,444247210)) %>% 
  mutate(name = sub(".*index: (.+)$", "\\1", covariate_name)) %>% 
  arrange(name) %>% 
  select(covariate_id, name)
colnames(CardiovascularDisease) <- SqlRender::snakeCaseToCamelCase(colnames(CardiovascularDisease))

Neoplasms <- covariates %>% filter(covariate_analysis_id==210,
                                   analysis_id == 2,
                                   covariate_id %in% c(4044013210,432571210,40481902210,443392210,4112853210,4180790210,443388210,197508210,200962210)) %>% 
  mutate(name = sub(".*index: (.+)$", "\\1", covariate_name)) %>% 
  arrange(name) %>% 
  select(covariate_id, name)
colnames(Neoplasms) <- SqlRender::snakeCaseToCamelCase(colnames(Neoplasms))

Medication <- covariates %>% filter(covariate_analysis_id==410,
                                    analysis_id == 2,
                                    covariate_id %in% c(21601782410,21602796410,21604686410,21604389410,21603932410,21601387410,21602028410,21600960410,21601664410,21601744410,21601461410,21600046410,21603248410,21600712410,21603890410,21601853410,21604254410,21604489410,21604752410, 21600713410, 21600859410, 19009405410, 42709324410)) %>% 
  mutate(name = sub(".*index: (.+)$", "\\1", covariate_name)) %>% 
  arrange(name) %>% 
  select(covariate_id, name)
colnames(Medication) <- SqlRender::snakeCaseToCamelCase(colnames(Medication))

specifications <- rbind(Demographics, General, CardiovascularDisease, Neoplasms, Medication)

# Baseline Table (AMC) ---------------------------------------------------------
amc <- CohortMethod::loadCohortMethodData("/Users/jennifer/Desktop/results/Baseline/AMC_l1_t1365_c1366.zip")
beforePop <- readRDS("/Users/jennifer/Desktop/results/Baseline/AMC_l1_s1_p1_t1365_c1366_o1112.rds")
afterPop <- readRDS("/Users/jennifer/Desktop/results/Baseline/AMC_l1_s1_p1_t1365_c1366_s1_o1112.rds")
CohortMethod::plotPs(data = beforePop, targetLabel = "Romosozumab", comparatorLabel = "Denosumab", showCountsLabel = TRUE, showEquiposeLabel = TRUE)
beforeCovBal <- CohortMethod::computeCovariateBalance(population = beforePop, cohortMethodData = amc)
afterCovBal <- CohortMethod::computeCovariateBalance(population = afterPop, cohortMethodData = amc)

#covBalance
plotCovariateBalanceScatterPlotAMC(balance = afterCovBal, showCovariateCountLabel = TRUE, showMaxLabel = TRUE)+
  geom_hline(yintercept = 0.1, color = "red")

#table1
amc_before_table1 <- specifications %>% left_join(beforeCovBal, by = "covariateId") %>% 
  select(covariateId, name, afterMatchingSumTarget, afterMatchingSumComparator)
amc_after_table1 <- specifications %>% left_join(afterCovBal, by = "covariateId") %>% 
  select(covariateId, afterMatchingSumTarget, afterMatchingSumComparator)
amc_table1 <- inner_join(amc_before_table1, amc_after_table1, by = "covariateId")
amc_table1[is.na(amc_table1)] <- 0
colnames(amc_table1) <- c("covariateId", "name", "beforeMatchingTarget", "beforeMatchingComparator", "afterMatchingTarget", "afterMatchingComparator")
write.csv(amc_table1, "/Users/jennifer/Desktop/results/Baseline/AMC_Table1.csv")

#incidence
amc_results <- readRDS("/Users/jennifer/Desktop/results/cohort_method_result_AMC.rds")
amc_results %>% filter(outcome_id %in% c(1029, 1112), analysis_id %in% c(1:2)) %>% 
  mutate(taregtIR = target_outcomes/(target_days/365.25)*1000, 
         comparatorIR = comparator_outcomes/(comparator_days/365.25)*1000) %>% 
  select(analysis_id, target_id, outcome_id, taregtIR, comparatorIR) %>% arrange(outcome_id, analysis_id, target_id) %>% as.data.frame()

# Baseline Table (KUMC) ---------------------------------------------------------

kumc <- CohortMethod::loadCohortMethodData("/Users/jennifer/Desktop/results/Baseline/KUMC_l1_t1365_c1366.zip")
beforePop <- readRDS("/Users/jennifer/Desktop/results/Baseline/KUMC_l1_s1_p1_t1365_c1366_o1112.rds")
CohortMethod::plotPs(data = beforePop, targetLabel = "Romosozumab", comparatorLabel = "Denosumab", showCountsLabel = TRUE, showEquiposeLabel = TRUE)
afterPop <- readRDS("/Users/jennifer/Desktop/results/Baseline/KUMC_l1_s1_p1_t1365_c1366_s1_o1112.rds")

beforeCovBal <- CohortMethod::computeCovariateBalance(population = beforePop, cohortMethodData = kumc)
afterCovBal <- CohortMethod::computeCovariateBalance(population = afterPop, cohortMethodData = kumc)
plotCovariateBalanceScatterPlotAMC(balance = afterCovBal, showCovariateCountLabel = TRUE, showMaxLabel = TRUE)+
  geom_hline(yintercept = 0.1, color = "red")
CohortMethod::plotKaplanMeier(population = afterPop, targetLabel = "Romosozuamb", comparatorLabel = "Denosumab")
CohortMethod::drawAttritionDiagram(object = afterPop, targetLabel = "Romosozumab", comparatorLabel = "Denosumab")
kumc_before_table1 <- specifications %>% left_join(beforeCovBal, by = "covariateId") %>% 
  select(covariateId, name, afterMatchingSumTarget, afterMatchingSumComparator)
kumc_after_table1 <- specifications %>% left_join(afterCovBal, by = "covariateId") %>% 
  select(covariateId, afterMatchingSumTarget, afterMatchingSumComparator)
kumc_table1 <- inner_join(kumc_before_table1, kumc_after_table1, by = "covariateId")
kumc_table1[is.na(kumc_table1)] <- 0
colnames(kumc_table1) <- c("covariateId", "name", "beforeMatchingTarget", "beforeMatchingComparator", "afterMatchingTarget", "afterMatchingComparator")
write.csv(kumc_table1, "/Users/jennifer/Desktop/results/Baseline/kumc_Table1.csv")

#incidence
kumc_results <- readRDS("/Users/jennifer/Desktop/results/cohort_method_result_KUMC.rds")
kumc_results %>% filter(outcome_id %in% c(1029, 1112), analysis_id %in% c(1:2)) %>% 
  mutate(taregtIR = target_outcomes/(target_days/365.25)*1000, 
         comparatorIR = comparator_outcomes/(comparator_days/365.25)*1000) %>% 
  select(analysis_id, target_id, outcome_id, target_outcomes, comparator_outcomes, taregtIR, comparatorIR) %>% arrange(outcome_id, analysis_id, target_id) %>% as.data.frame()


# Baseline Table (YUHS) ---------------------------------------------------------
yuhs <- read.csv("/Users/jennifer/Desktop/results/Baseline/baseline_YUHS.csv")
yuhs <- yuhs[c("covariateId", "beforeMatchingSumTarget", "beforeMatchingSumComparator", "afterMatchingSumTarget", "afterMatchingSumComparator")]
yuhs_table1 <- specifications %>% left_join(yuhs, by = "covariateId")
yuhs_table1[is.na(yuhs_table1)] <- 0
colnames(yuhs_table1) <- c("covariateId", "name", "beforeMatchingTarget", "beforeMatchingComparator", "afterMatchingTarget", "afterMatchingComparator")
write.csv(yuhs_table1, "/Users/jennifer/Desktop/results/Baseline/yuhs_Table1.csv")

#incidence
yuhs_results <- readRDS("/Users/jennifer/Desktop/results/cohort_method_result_YUHS.rds")
yuhs_results %>% filter(outcome_id %in% c(1029, 1112), analysis_id %in% c(1:2)) %>% 
  mutate(taregtIR = target_outcomes/(target_days/365.25)*1000, 
         comparatorIR = comparator_outcomes/(comparator_days/365.25)*1000) %>% 
  select(analysis_id, target_id, outcome_id, target_outcomes, comparator_outcomes, taregtIR, comparatorIR) %>% arrange(outcome_id, analysis_id, target_id) %>% as.data.frame()



# overall -----------------------------------------------------------------
overall <- full_join(amc, yuhs, by = "covariateId", suffix = c("_AMC", "_YUHS"))

beforeMatchingTarget <- 1600+930
beforeMatchingComparator <- 10362+13009
afterMatchingTarget <- 1517+862
afterMatchingComparator <- 1517+862


overall_results <- overall %>% group_by(covariateId) %>% 
  summarise(beforeMatchingSumTarget = sum(beforeMatchingSumTarget_AMC, beforeMatchingSumTarget_YUHS, na.rm = TRUE),
            beforeMatchingSumComparator = sum(beforeMatchingSumComparator_AMC, beforeMatchingSumComparator_YUHS, na.rm = TRUE),
            afterMatchingSumTarget = sum(afterMatchingSumTarget_AMC, afterMatchingSumTarget_YUHS, na.rm = TRUE),
            afterMatchingSumComparator = sum(afterMatchingSumComparator_AMC, afterMatchingSumComparator_YUHS, na.rm = TRUE))

overall_results <- rbind(overall_results, colSums(overall_results[(overall_results$covariateId == 10003) | (overall_results$covariateId == 11003),])) # Age(21006): 50s
overall_results <- rbind(overall_results, colSums(overall_results[(overall_results$covariateId == 12003) | (overall_results$covariateId == 13003),])) # Age(25006): 60s
overall_results <- rbind(overall_results, colSums(overall_results[(overall_results$covariateId == 14003) | (overall_results$covariateId == 15003),])) # Age(29006): 70s
overall_results <- rbind(overall_results, colSums(overall_results[(overall_results$covariateId == 16003) | (overall_results$covariateId == 17003) | (overall_results$covariateId == 18003) | (overall_results$covariateId == 19003) | (overall_results$covariateId == 20003),])) # Age(90015): over 80s
overall_results <- overall_results %>% filter(!covariateId %in% c(10003:20003)) %>% arrange(covariateId)

overall_results <- overall_results %>% mutate(beforeMatchingMeanTarget = beforeMatchingSumTarget/beforeMatchingTarget,
                           beforeMatchingMeanComparator = beforeMatchingSumComparator/beforeMatchingComparator,
                           afterMatchingMeanTarget = afterMatchingSumTarget/afterMatchingTarget,
                           afterMatchingMeanComparator = afterMatchingSumComparator/afterMatchingComparator,
                           beforeMatchingSd = sqrt(((sqrt((beforeMatchingSumTarget-(beforeMatchingSumTarget^2/beforeMatchingTarget))/beforeMatchingTarget)^2+(sqrt((beforeMatchingSumComparator-(beforeMatchingSumComparator^2/beforeMatchingComparator))/beforeMatchingComparator)^2)/2))),
                           afterMatchingSd = sqrt(((sqrt((afterMatchingSumTarget-(afterMatchingSumTarget^2/afterMatchingTarget))/afterMatchingTarget)^2+(sqrt((afterMatchingSumComparator-(afterMatchingSumComparator^2/afterMatchingComparator))/afterMatchingComparator)^2)/2))),
                           beforeStdDiff = (beforeMatchingMeanTarget-beforeMatchingMeanComparator)/beforeMatchingSd,
                           afterStdDiff = (afterMatchingMeanTarget-afterMatchingMeanComparator)/afterMatchingSd
                           ) %>% as.data.frame()
overall_results <- overall_results %>% left_join(overall[c("covariateId", "covariateName")])
openxlsx::write.xlsx(x = overall_results, file = "/Users/jennifer/Desktop/results/Baseline/overall_baseline (YUHS, AMC).xlsx")

# before PS matching (YUHS): target (1600), comparator (10,362)
# before PS matching (AMC): target (930), comparator (13,009)
# after PS matching (YUHS): target (1,517), comparator (1,517)
# after PS matching (AMC): target (862), comparator (862)


as.data.frame(FeatureExtraction::getDefaultTable1Specifications())

#incidence
meta_results <- readRDS("//Users/jennifer/Desktop/results/cohort_method_result_Meta-analysis.rds")
meta_results %>% filter(outcome_id %in% c(1029, 1112), analysis_id %in% c(1:2)) %>% 
  mutate(taregtIR = target_outcomes/(target_days/365.25)*1000, 
         comparatorIR = comparator_outcomes/(comparator_days/365.25)*1000) %>% 
  select(analysis_id, target_id, outcome_id, target_outcomes, comparator_outcomes, taregtIR, comparatorIR) %>% arrange(outcome_id, analysis_id, target_id) %>% as.data.frame()

# Attrition ---------------------------------------------------------------
yuhs_attrition <- read.csv("/Users/jennifer/Desktop/results/241022_Export/export/attrition.csv")
yuhs_attrition_1112 <- yuhs_attrition %>% filter(target_id==1365, outcome_id==1112, analysis_id == 1)

amc_attrition <- read.csv("/Users/jennifer/Desktop/results/export/attrition.csv")
amc_attrition_1112 <- amc_attrition %>% filter(target_id==1365, outcome_id==1112, analysis_id == 1)

overall_attrition_1112 <- yuhs_attrition_1112[c("exposure_id", "target_id", "comparator_id", "outcome_id", "sequence_number", "description")]
overall_attrition_1112$subjects <- rowSums(cbind(yuhs_attrition_1112[c("subjects")], amc_attrition_1112[c("subjects")]))
openxlsx::write.xlsx(x = overall_attrition_1112, file = "/Users/jennifer/Desktop/results/Baseline/overall_attrition_1112 (YUHS, AMC).xlsx")


# negative control estimates ----------------------------------------------
plotControlsCordis <- function (logRr, seLogRr = NULL, ci95Lb = NULL, ci95Ub = NULL, 
          trueLogRr, estimateType = "relative risk", fileName = NULL, 
          title = NULL) 
{
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertNumeric(logRr, min.len = 1, add = errorMessages)
  checkmate::assertNumeric(seLogRr, len = length(logRr), null.ok = TRUE, 
                           add = errorMessages)
  checkmate::assertNumeric(ci95Lb, len = length(logRr), null.ok = TRUE, 
                           add = errorMessages)
  checkmate::assertNumeric(ci95Ub, len = length(logRr), null.ok = TRUE, 
                           add = errorMessages)
  checkmate::assertNumeric(trueLogRr, len = length(logRr), 
                           add = errorMessages)
  checkmate::assertCharacter(estimateType, len = 1, add = errorMessages)
  checkmate::assertCharacter(fileName, len = 1, null.ok = TRUE, 
                             add = errorMessages)
  checkmate::assertCharacter(title, null.ok = TRUE, len = 1, 
                             add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)
  if (is.null(seLogRr) && is.null(ci95Lb)) {
    stop("Must specify either standard error or confidence interval")
  }
  data <- data.frame(logRr = logRr, trueLogRr = trueLogRr)
  if (is.null(seLogRr)) {
    data$seLogRr <- (log(ci95Ub) - log(ci95Lb))/(2 * qnorm(0.975))
  }
  else {
    data$seLogRr <- seLogRr
  }
  if (is.null(ci95Lb)) {
    data$ci95Lb <- exp(data$logRr + qnorm(0.025) * data$seLogRr)
    data$ci95Ub <- exp(data$logRr + qnorm(0.975) * data$seLogRr)
  }
  else {
    data$ci95Lb <- ci95Lb
    data$ci95Ub <- ci95Ub
  }
  data <- data[!is.na(data$seLogRr), ]
  data$Significant <- data$ci95Lb > exp(data$trueLogRr) | 
    data$ci95Ub < exp(data$trueLogRr)
  data$Group <- as.factor(paste("True", estimateType, "=", 
                                exp(data$trueLogRr)))
  temp1 <- aggregate(Significant ~ Group, data = data, length)
  temp2 <- aggregate(Significant ~ Group, data = data, mean)
  temp1$nLabel <- paste0(formatC(temp1$Significant, big.mark = ","), 
                         " estimates")
  temp1$Significant <- NULL
  temp2$meanLabel <- paste0(formatC(100 * (1 - temp2$Significant), 
                                    digits = 1, format = "f"), "% of CIs includes ", substr(as.character(temp2$Group), 
                                                                                            start = 21, stop = nchar(as.character(temp2$Group))))
  temp2$Significant <- NULL
  dd <- merge(temp1, temp2)
  dd$tes <- as.numeric(substr(as.character(dd$Group), start = 21, 
                              stop = nchar(as.character(dd$Group))))
  breaks <- c(0.25, 0.5, 1, 2, 4, 6, 8)
  theme <- ggplot2::element_text(colour = "#000000", size = 14)
  themeRA <- ggplot2::element_text(colour = "#000000", size = 14, 
                                   hjust = 1)
  alpha <- 1 - min(0.95 * (nrow(data)/nrow(dd)/50000)^0.1, 
                   0.95)
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = logRr, y = seLogRr)) + 
    ggplot2::geom_vline(xintercept = log(breaks), colour = "#CCCCCC", 
                        lty = 1, size = 0.5) + ggplot2::geom_abline(ggplot2::aes(intercept = (-log(.data$tes))/qnorm(0.025), 
                                                                                 slope = 1/qnorm(0.025)), colour = rgb(0.8, 0, 0), linetype = "dashed", 
                                                                    size = 1, alpha = 0.5, data = dd) + ggplot2::geom_abline(ggplot2::aes(intercept = (-log(.data$tes))/qnorm(0.975), 
                                                                                                                                          slope = 1/qnorm(0.975)), colour = rgb(0.8, 0, 0), linetype = "dashed", 
                                                                                                                             size = 1, alpha = 0.5, data = dd) + ggplot2::geom_point(size = 2, 
                                                                                                                                                                                     color = rgb(0, 0, 0, alpha = 0.05), alpha = alpha, shape = 16) + 
    ggplot2::geom_hline(yintercept = 0) + ggplot2::geom_label(x = log(0.26), 
                                                              y = 0.96, alpha = 1, hjust = "left", ggplot2::aes(label = .data$nLabel), 
                                                              size = 5, data = dd) + ggplot2::geom_label(x = log(0.26), 
                                                                                                         y = 0.8, alpha = 1, hjust = "left", ggplot2::aes(label = .data$meanLabel), 
                                                                                                         size = 5, data = dd) + ggplot2::scale_x_continuous(paste("Estimated", estimateType), limits = log(c(0.1, 10)), breaks = log(breaks), 
                                                                                                                                                            labels = breaks) + ggplot2::scale_y_continuous("Standard Error", 
                                                                                                                                                                                                           limits = c(0, 1)) + ggplot2::facet_grid(. ~ Group) + 
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), 
                   panel.background = ggplot2::element_blank(), panel.grid.major = ggplot2::element_blank(), 
                   axis.ticks = ggplot2::element_blank(), axis.text.y = themeRA, 
                   axis.text.x = theme, axis.title = theme, legend.key = ggplot2::element_blank(), 
                   strip.text.x = theme, strip.text.y = theme, strip.background = ggplot2::element_blank(), 
                   legend.position = "none")
  if (!is.null(title)) {
    plot <- plot + ggplot2::ggtitle(title)
  }
  if (!is.null(fileName)) {
    ggplot2::ggsave(fileName, plot, width = 1.6 + 3 * nrow(dd), 
                    height = 2.8, dpi = 400)
  }
  return(plot)
}

yuhs_results <- readRDS("/Users/jennifer/Desktop/results/cohort_method_result_YUHS.rds")
amc_results <- readRDS("/Users/jennifer/Desktop/results/cohort_method_result_AMC.rds")
kumc_results <- readRDS("/Users/jennifer/Desktop/results/cohort_method_result_KUMC.rds")

meta_results <- readRDS("//Users/jennifer/Desktop/results/cohort_method_result_Meta-analysis.rds")

negative <- meta_results %>% filter(target_id == 1365, !outcome_id %in% c(1000:2000), analysis_id==2, !is.na(log_rr)) %>% 
  mutate(trueLogRr = 0) %>% 
  select(log_rr, se_log_rr, ci_95_lb, ci_95_ub, trueLogRr, calibrated_log_rr, calibrated_se_log_rr, calibrated_ci_95_lb, calibrated_ci_95_ub) %>% 
  as.data.frame()

negative <- yuhs_results %>% filter(target_id == 1365, !outcome_id %in% c(1000:2000), analysis_id==2, !is.na(log_rr)) %>% 
  mutate(trueLogRr = 0) %>% 
  select(log_rr, se_log_rr, ci_95_lb, ci_95_ub, trueLogRr, calibrated_log_rr, calibrated_se_log_rr, calibrated_ci_95_lb, calibrated_ci_95_ub) %>% 
  as.data.frame()

negative <- amc_results %>% filter(target_id == 1365, !outcome_id %in% c(1000:2000), analysis_id==2, !is.na(log_rr)) %>% 
  mutate(trueLogRr = 0) %>% 
  select(log_rr, se_log_rr, ci_95_lb, ci_95_ub, trueLogRr, calibrated_log_rr, calibrated_se_log_rr, calibrated_ci_95_lb, calibrated_ci_95_ub) %>% 
  as.data.frame()

negative <- kumc_results %>% filter(target_id == 1365, !outcome_id %in% c(1000:2000), analysis_id==2, !is.na(log_rr)) %>% 
  mutate(trueLogRr = 0) %>% 
  select(log_rr, se_log_rr, ci_95_lb, ci_95_ub, trueLogRr, calibrated_log_rr, calibrated_se_log_rr, calibrated_ci_95_lb, calibrated_ci_95_ub) %>% 
  as.data.frame()

plotControlsCordis(logRr = negative$log_rr,
                   seLogRr = negative$se_log_rr,
                   ci95Lb = negative$ci_95_lb, 
                   ci95Ub = negative$ci_95_ub,
                   trueLogRr = negative$trueLogRr,
                   estimateType = "Hazard ratio")

plotControlsCordis(logRr = negative$calibrated_log_rr,
                   seLogRr = negative$calibrated_se_log_rr,
                   ci95Lb = negative$calibrated_ci_95_lb, 
                   ci95Ub = negative$calibrated_ci_95_ub,
                   trueLogRr = negative$trueLogRr,
                   estimateType = "Hazard ratio")



plotCovariateBalanceScatterPlotAMC <- function (balance, absolute = TRUE, threshold = 0, title = "Standardized difference of mean", 
                                                fileName = NULL, beforeLabel = "Before matching", afterLabel = "After matching", 
                                                showCovariateCountLabel = FALSE, showMaxLabel = FALSE) 
{
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertDataFrame(balance, add = errorMessages)
  checkmate::assertLogical(absolute, len = 1, add = errorMessages)
  checkmate::assertNumber(threshold, lower = 0, add = errorMessages)
  checkmate::assertCharacter(title, len = 1, add = errorMessages)
  checkmate::assertCharacter(fileName, len = 1, null.ok = TRUE, 
                             add = errorMessages)
  checkmate::assertCharacter(beforeLabel, len = 1, add = errorMessages)
  checkmate::assertCharacter(afterLabel, len = 1, add = errorMessages)
  checkmate::assertLogical(showCovariateCountLabel, len = 1, 
                           add = errorMessages)
  checkmate::assertLogical(showMaxLabel, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)
  if (absolute) {
    balance$beforeMatchingStdDiff <- abs(balance$beforeMatchingStdDiff)
    balance$afterMatchingStdDiff <- abs(balance$afterMatchingStdDiff)
  }
  limits <- c(min(c(balance$beforeMatchingStdDiff, balance$afterMatchingStdDiff), 
                  na.rm = TRUE), max(c(balance$beforeMatchingStdDiff, 
                                       balance$afterMatchingStdDiff), na.rm = TRUE))
  plot <- ggplot2::ggplot(balance, ggplot2::aes(x = .data$beforeMatchingStdDiff, 
                                                y = .data$afterMatchingStdDiff)) + ggplot2::geom_point(color = rgb(0, 
                                                                                                                   0, 0.8, alpha = 0.3), shape = 16) + ggplot2::geom_abline(slope = 1, 
                                                                                                                                                                            intercept = 0, linetype = "dashed") + ggplot2::geom_hline(yintercept = 0) + 
    ggplot2::geom_vline(xintercept = 0) + ggplot2::ggtitle(title) + 
    ggplot2::scale_x_continuous(beforeLabel, limits = c(0, 0.4)) + 
    ggplot2::scale_y_continuous(afterLabel, limits = c(0, 0.4))
  if (threshold != 0) {
    plot <- plot + ggplot2::geom_hline(yintercept = c(threshold, 
                                                      -threshold), alpha = 0.5, linetype = "dotted")
  }
  if (showCovariateCountLabel || showMaxLabel) {
    labels <- c()
    if (showCovariateCountLabel) {
      labels <- c(labels, sprintf("Number of covariates: %s", 
                                  format(nrow(balance), big.mark = ",", scientific = FALSE)))
    }
    if (showMaxLabel) {
      labels <- c(labels, sprintf("%s max(absolute): %.2f", 
                                  afterLabel, max(abs(balance$afterMatchingStdDiff), 
                                                  na.rm = TRUE)))
    }
    dummy <- data.frame(text = paste(labels, collapse = "\n"))
    plot <- plot + ggplot2::geom_label(x = 0 + 0.01, 
                                       y = 0.4, hjust = "left", vjust = "top", alpha = 0.8, 
                                       ggplot2::aes(label = text), data = dummy, size = 3.5)
  }
  if (!is.null(fileName)) {
    ggplot2::ggsave(fileName, plot, width = 4, height = 4, 
                    dpi = 400)
  }
  return(plot)
}

