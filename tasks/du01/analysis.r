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
		minimum = min(increase),
		Q1 = quantile(increase, 0.25),                  # Round to same places as 'sd'
		median = median(increase),                      # Round to same places as 'sd'
		mean = mean(increase),                          # Round to same places as 'sd'
		Q3 = quantile(increase, 0.75),                  # Round to same places as 'sd'
		maximum = max(increase),
		sd = sd(increase),
		cv = (sd / mean) * 100,                         # Round to 1 decimal place
		skewness = moments::skewness(increase),         # Round to 1 decimal place
		kurtosis = (moments::kurtosis(increase) - 3),   # Round to 1 decimal place
		lower_bound = Q1 - 1.5 * (Q3 - Q1),             # Round to 4 decimal places
		upper_bound = Q3 + 1.5 * (Q3 - Q1)              # Round to 4 decimal places
	)
}

nvidia_3070_char <- characteristics(nvidia_3070)
amd_7700_char <- characteristics(amd_7700)

nvidia_3070_char_no_outliers <- characteristics(nvidia_3070_no_outliers)
amd_7700_char_no_outliers <- characteristics(amd_7700_no_outliers)

characteristics_table <- as.data.frame(rbind(nvidia_3070_char, amd_7700_char, nvidia_3070_char_no_outliers, amd_7700_char_no_outliers))
rownames(characteristics_table) <- c("nvidia_3070", "amd_7700", "nvidia_3070_no_outliers", "amd_7700_no_outliers")

t(characteristics_table)

# Sigma x rule
# ==================================================

sigma <- function(data, rule) {
	lower_bound <- data$mean - rule * data$sd
	upper_bound <- data$mean + rule * data$sd
	return(c(lower_bound, upper_bound))
}

print(paste("Nvidia 3070 - 3 sigma rule: ", sigma(nvidia_3070_char_no_outliers, 2)))
print(paste("AMD 7700 - 3 sigma rule: ", sigma(amd_7700_char_no_outliers, 2)))

# Graphs
# ==================================================

# Boxplot & Histogram
{
	png("assets/hist_boxplot.png", width = 800, height = 600, res = 100)

	# Template
	pom <- layout(mat = matrix(1:4, 2, 2, byrow = FALSE), height = c(8, 3))
	par(bg = "white")
	par(oma = c(0, 0, 0, 0))

	par(mar = c(2, 4, 2, 2))
	hist(
		nvidia_3070_no_outliers$increase,
		main = "Nvidia RTX 3070 Ti",
		ylab = "Frequency",
		xlim = c(-5, 7),
		ylim = c(0, 30),
		breaks = 5
	)

	par(mar = c(4, 2, 2, 2))
	boxplot(
		nvidia_3070$increase,
		horizontal = TRUE,
		ylim = c(-5, 7),
		xlab = "FPS increase",
		boxwex = 1.5
	)

	par(mar = c(2, 4, 2, 2))
	hist(
		amd_7700_no_outliers$increase,
		main = "AMD Radeon RX 7700 XT",
		ylab = "",
		xlim = c(3, 16),
		ylim = c(0, 30),
		breaks = 5
	)

	par(mar = c(4, 2, 2, 2))
	boxplot(
		amd_7700$increase,
		horizontal = TRUE,
		ylim = c(3, 16),
		xlab = "FPS increase",
		boxwex = 1.5
	)

	dev.off()
}

# QQ plot
{
	png("assets/qq.png", width = 800, height = 450, res = 100)

	# Template
	pom <- layout(mat = matrix(1:2, 1, 2, byrow = FALSE), height = c(2, 1))
	par(bg = "white")
	par(oma = c(0, 0, 0, 0))

	par(mar = c(4, 4, 3, 2))
	qqnorm(
	  nvidia_3070_no_outliers$increase,
	  main = "Nvidia RTX 3070 Ti"
	)
	qqline(nvidia_3070_no_outliers$increase)

	par(mar = c(4, 4, 3, 2))
	qqnorm(
	  amd_7700_no_outliers$increase,
	  ylab = "",
	  main = "AMD Radeon RX 7700 XT",
	)
	qqline(amd_7700_no_outliers$increase)

	dev.off()
}