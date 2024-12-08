source("tasks/du01/analysis.r")
library(lawstat)
library(BSDA)

# -------
# b)

# Shapiro-Wilk test
shapiro_test(nvidia_3070_no_outliers$increase)
shapiro_test(amd_7700_no_outliers$increase)

# Data not normal, data bellow alpha, use Symmetry
symmetry.test(nvidia_3070_no_outliers$increase, boot = FALSE)
symmetry.test(amd_7700_no_outliers$increase, boot = FALSE)

# P value < 0.05 -> use alternative -> not symmetric -> SIGN
# P value > 0.05 -> use symmetric -> symmetric -> wilcox

BSDA::SIGN.test(nvidia_3070_no_outliers$increase, md = 0, alternative = "greater")
BSDA::SIGN.test(amd_7700_no_outliers$increase, md = 0, alternative = "greater")

# If above 0, medians are statistically significant

SIGN.test(nvidia_3070_no_outliers$increase, alternative = "greater", conf.level = 0.95, conf.int = TRUE)
SIGN.test(amd_7700_no_outliers$increase, alternative = "greater", conf.level = 0.95, conf.int = TRUE)

# -------
# c)

wilcox.test(nvidia_3070_no_outliers$increase, amd_7700_no_outliers$increase, alternative = "greater")
wilcox.test(nvidia_3070_no_outliers$increase, amd_7700_no_outliers$increase, alternative = "greater", conf.int = TRUE)

# Point estimate
nvidia_3070_char_no_outliers$median - amd_7700_char_no_outliers$median

