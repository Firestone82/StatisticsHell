library(readxl)
library(dplyr)
library(rstatix)

# Read data from excel file
data <- read_excel("ukol_166.xlsx")
print(data)

# Split data into separate dataframes for each GPU
# ==================================================
nvidia_2080 <- data[, c(1, 2, 3)]
colnames(nvidia_2080) <- c("Id", "fps_release", "fps_patched")

nvidia_3070 <- data[, c(1, 4, 5)]
colnames(nvidia_3070) <- c("Id", "fps_release", "fps_patched")

amd_6800 <- data[, c(1, 6, 7)]
colnames(amd_6800) <- c("Id", "fps_release", "fps_patched")

amd_7700 <- data[, c(1, 8, 9)]
colnames(amd_7700) <- c("Id", "fps_release", "fps_patched")

# Drop NA rows
drop_na <- function(data) {
	data <- data[complete.cases(data),]
	return(data)
}

nvidia_2080 <- drop_na(nvidia_2080)
nvidia_3070 <- drop_na(nvidia_3070)
amd_6800 <- drop_na(amd_6800)
amd_7700 <- drop_na(amd_7700)

# Calculate increase in FPS
# ==================================================
increase <- function(data) {
	data$increase <- data$fps_patched - data$fps_release
	return(data)
}

nvidia_2080 <- increase(nvidia_2080)
print(paste("Nvidia 2080 has", nrow(nvidia_2080), "samples"))

nvidia_3070 <- increase(nvidia_3070)
print(paste("Nvidia 3070 has", nrow(nvidia_3070), "samples"))

amd_6800 <- increase(amd_6800)
print(paste("AMD 6800 has", nrow(amd_6800), "samples"))

amd_7700 <- increase(amd_7700)
print(paste("AMD 7700 has", nrow(amd_7700), "samples"))

# Find outliers using rstatix
# ==================================================
outliers <- function(data) {
	d <- data %>% identify_outliers(increase)
	return(d)
}

nvidia_3070_outliers <- outliers(nvidia_3070)
nvidia_3070_no_outliers <- nvidia_3070[-nvidia_3070_outliers$Id,]
print(paste("Nvidia 3070 has", nrow(nvidia_3070_outliers), "outliers"))
print(nvidia_3070_outliers)

amd_7700_outliers <- outliers(amd_7700)
amd_7700_no_outliers <- amd_7700[-amd_7700_outliers$Id,]
print(paste("AMD 7700 has", nrow(amd_7700_outliers), "outliers"))
print(amd_7700_outliers)

# Table
# ==================================================
characteristics <- function(data) {
	data %>% summarise(
		range = length(increase),
		minimum = min(increase, na.rm = TRUE),
		Q1 = quantile(increase, 0.25, na.rm = TRUE),                  # Round to same places as 'sd'
		median = median(increase, na.rm = TRUE),                      # Round to same places as 'sd'
		mean = mean(increase, na.rm = TRUE),                          # Round to same places as 'sd'
		Q3 = quantile(increase, 0.75, na.rm = TRUE),                  # Round to same places as 'sd'
		maximum = max(increase, na.rm = TRUE),
		sd = sd(increase, na.rm = TRUE),
		cv = (sd(increase) / mean(increase)) * 100,                   # Round to 1 decimal place
		skewness = moments::skewness(increase, na.rm = TRUE),         # Round to 1 decimal place
		kurtosis = (moments::kurtosis(increase, na.rm = TRUE) - 3),   # Round to 1 decimal place
		lower_bound = Q1 - 1.5 * (Q3 - Q1),                           # Round to 4 decimal places
		upper_bound = Q3 + 1.5 * (Q3 - Q1)                            # Round to 4 decimal places
	)
}

nvidia_3070_char <- characteristics(nvidia_3070)
amd_7700_char <- characteristics(amd_7700)

nvidia_3070_char_no_outliers <- characteristics(nvidia_3070_no_outliers)
amd_7700_char_no_outliers <- characteristics(amd_7700_no_outliers)

characteristics_table <- as.data.frame(rbind(nvidia_3070_char, amd_7700_char, nvidia_3070_char_no_outliers, amd_7700_char_no_outliers))
rownames(characteristics_table) <- c("nvidia_3070_rel", "amd_7700_rel", "nvidia_3070_patch", "amd_7700_patch")

t(characteristics_table)

# Sigma 3 rule
# ==================================================

print(paste("Nvidia 3070 - 3 sigma rule: ", nvidia_3070_char$sd * 2))
print(paste("AMD 7700 - 3 sigma rule: ", amd_7700_char$sd * 2))

# Graphs
# ==================================================

# Boxplot & Histogram
{
	png("assets/hist_boxplot.png", width = 800, height = 600, res = 100)

	# Template
	pom <- layout(mat = matrix(1:4, 2, 2, byrow = FALSE), height = c(8, 3))
	par(bg = "white")
	par(oma = c(1, 1, 0, 1), mar = c(2, 2, 4, 1))

	hist(
		nvidia_3070$increase,
		main = "Histogram & Boxplot for Nvidia 3070",
		xlab = "increase",
		ylab = "frequency",
		breaks = 20
	)

	boxplot(
		nvidia_3070$increase,
		horizontal = TRUE,
		boxwex = 1.5
	)

	hist(
		amd_7700$increase,
		main = "Histogram & Boxplot for AMD 7700",
		xlab = "increase",
		ylab = "frequency",
		breaks = 20
	)

	boxplot(
		amd_7700$increase,
		horizontal = TRUE,
		boxwex = 1.5
	)

	dev.off()
}

# QQ plot
{
	png("assets/qq.png", width = 800, height = 500, res = 100)

	# Template
	pom <- layout(mat = matrix(1:2, 1, 2, byrow = FALSE), height = c(2.5, 1))
	par(bg = "white")
	par(oma = c(1, 1, 0, 1), mar = c(2, 2, 4, 1))

	qqnorm(
		nvidia_3070_no_outliers$increase,
		main = "QQ plot for Nvidia 3070",
		xlab = "Theoretical quantiles",
		ylab = "Sample quantiles"
	)
	qqline(nvidia_3070_no_outliers$increase)

	qqnorm(
		amd_7700_no_outliers$increase,
		main = "QQ plot for AMD 7700",
		xlab = "Theoretical quantiles",
		ylab = "Sample quantiles"
	)
	qqline(amd_7700_no_outliers$increase)

	dev.off()
}
