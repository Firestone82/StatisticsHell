source("tasks/du02/analysis.r")

install.packages("pacman")
library(pacman)

pacman::p_load(
  readxl,             # potřebné k načtení xlsx souboru
  dplyr,              # pro efektivní úpravy data.frame
  ggplot2,            # pro "hezčí" grafiku
  rstatix,            # identify_outliers()
  moments,            # funkce skewness(), kurtosis()
  dlookr,             # funkce normality() pro výpočet Shapirova-Wilkova testu pomocí dplyr
  BSDA,               # funkce SIGN.test()
  lawstat,            # funkce symmetry.test() pro ověření symetrie
  EnvStats,           # funkce varTest() pro odhad a test rozptylu
  ggpubr,             # ggarrange() pro slučování obrázků
  tibble,             # umožňuje nastavení počtu des. míst ve výstupech balíčku dplyr
  scales,             # umožňuje nastavit formát popisku os do formátu s mezerou mezi tisíci
  dunn.test,          # pro provedení testu Dunn
)

# Graphs
# ============================

{
	png("assets/box_plot_all.png", width = 800, height = 600, res = 100)

	# Set the graphics parameters
	par(bg = "white", mar = c(4, 4, 4, 4))

	# Create a single boxplot with all data series
	boxplot(
	  nvidia_2080_no_outliers$increase,
	  nvidia_3070_no_outliers$increase,
	  amd_6800_no_outliers$increase,
	  amd_7700_no_outliers$increase,
	  horizontal = TRUE,
	  ylim = c(4, 7),
	  names = c("Nvidia 2080", "Nvidia 3070", "AMD 6800", "AMD 7700"),
	  main = "Boxplot graph for graphics cards fps increase comparison",
	  xlab = "FPS Increase",
	  ylab = "Graphics Cards",
	  boxwex = 0.7
	)

	# Save the plot
	dev.off()
}

{
	png("assets/histograms_all.png", width = 600, height = 800)

	# Define the layout matrix
	layout(matrix(c(1, 2, 3, 4), ncol = 1, nrow = 4, byrow = TRUE), widths = c(3, 1.2))

	# Adjust margins to give more space for each plot
	par(mar = c(5, 4, 3, 2))  # bottom, left, top, right

	xlim <- c(4, 7)
	ylim <- c(0, 18)
	xlab <- "FPS Increase"

	# Nvidia RTX 2080 Ti
	hist(nvidia_2080_no_outliers$increase, main = "Histogram of Nvidia RTX 2080 Ti", xlab = xlab, breaks = 15, xlim = xlim, ylim = ylim)

	# Nvidia RTX 3070 Ti
	hist(nvidia_3070_no_outliers$increase, main = "Histogram of Nvidia RTX 3070 Ti", xlab = xlab, breaks = 15, xlim = xlim, ylim = ylim)

	# AMD Radeon RX 6800 XT
	hist(amd_6800_no_outliers$increase, main = "Histogram of AMD Radeon RX 6800 XT", xlab = xlab, breaks = 15, xlim = xlim, ylim = ylim)

	# AMD Radeon RX 7700 XT
	hist(amd_7700_no_outliers$increase, main = "Histogram of AMD Radeon RX 7700 XT", xlab = xlab, breaks = 15, xlim = xlim, ylim = ylim)

	# Reset the plotting area to default
	layout(1)

	# Close the PNG device
	dev.off()
}

{
	png("assets/qq_all.png", width = 800, height = 220)

	# Define the layout matrix
	layout(matrix(c(1, 2, 3, 4), ncol = 4, nrow = 1, byrow = TRUE))

	# Adjust margins to give more space for each plot
	par(mar = c(5, 4, 3, 2))  # bottom, left, top, right

	xlim <- c(-3, 3)
	ylim <- c(3, 7)

	# Nvidia RTX 2080 Ti
	qqnorm(nvidia_2080_no_outliers$increase, main = "QQ Plot of Nvidia RTX 2080 Ti", xlim = xlim, ylim = ylim)
	qqline(nvidia_2080_no_outliers$increase)

	# Nvidia RTX 3070 Ti
	qqnorm(nvidia_3070_no_outliers$increase, main = "QQ Plot of Nvidia RTX 3070 Ti", xlim = xlim, ylim = ylim)
	qqline(nvidia_3070_no_outliers$increase)

	# AMD Radeon RX 6800 XT
	qqnorm(amd_6800_no_outliers$increase, main = "QQ Plot of AMD Radeon RX 6800 XT", xlim = xlim, ylim = ylim)
	qqline(amd_6800_no_outliers$increase)

	# AMD Radeon RX 7700 XT
	qqnorm(amd_7700_no_outliers$increase, main = "QQ Plot of AMD Radeon RX 7700 XT", xlim = xlim, ylim = ylim)
	qqline(amd_7700_no_outliers$increase)

	# Reset the plotting area to default
	layout(1)

	# Close the PNG device
	dev.off()
}

# Normality tests
# ============================

norm_char_fn <- function(data) {
	data %>%
	  summarise(
		skewness = moments::skewness(increase, na.rm = TRUE),
		kurtosis = moments::kurtosis(increase, na.rm = TRUE) - 3,
		shapiro = shapiro.test(increase)$p.value
	  )
}

all_norm_chars <- as.data.frame(rbind(
  norm_char_fn(nvidia_2080_no_outliers),
  norm_char_fn(nvidia_3070_no_outliers),
  norm_char_fn(amd_6800_no_outliers),
  norm_char_fn(amd_7700_no_outliers)
))

rownames(all_norm_chars) <- c("nvidia_2080", "nvidia_3070", "amd_6800", "amd_7700")
t(all_norm_chars)

# Symmetry tests
# ============================

symmetry.test(nvidia_2080_no_outliers$increase, boot = FALSE)
symmetry.test(nvidia_3070_no_outliers$increase, boot = FALSE)
symmetry.test(amd_6800_no_outliers$increase, boot = FALSE)
symmetry.test(amd_7700_no_outliers$increase, boot = FALSE)

BSDA::SIGN.test(nvidia_2080_no_outliers$increase, md = 0, alternative = "greater")
BSDA::SIGN.test(nvidia_3070_no_outliers$increase, md = 0, alternative = "greater")
BSDA::SIGN.test(amd_6800_no_outliers$increase, md = 0, alternative = "greater")
BSDA::SIGN.test(amd_7700_no_outliers$increase, md = 0, alternative = "greater")

# Homoskedasticity tests
# ============================

# Since not normal-> use Levene

var_char_fn <- function(data) {
	data %>%
	  summarise(
		rozptyl = var(increase, na.rm = TRUE),
		smerodatna_odchylka = sd(increase, na.rm = TRUE),
	  )
}

all_var_chars <- as.data.frame(rbind(
  var_char_fn(nvidia_2080_no_outliers),
  var_char_fn(nvidia_3070_no_outliers),
  var_char_fn(amd_6800_no_outliers),
  var_char_fn(amd_7700_no_outliers)
))

rownames(all_var_chars) <- c("nvidia_2080", "nvidia_3070", "amd_6800", "amd_7700")
t(all_var_chars)

biggest_var <- all_var_chars %>% filter(rozptyl == max(rozptyl))
smallest_var <- all_var_chars %>% filter(rozptyl == min(rozptyl))

frac <- biggest_var$rozptyl / smallest_var$rozptyl
frac

all_data <- rbind(nvidia_2080_no_outliers, nvidia_3070_no_outliers, amd_6800_no_outliers, amd_7700_no_outliers)
all_data$GPU <- c(
  rep("Nvidia 2080", nrow(nvidia_2080_no_outliers)),
  rep("Nvidia 3070", nrow(nvidia_3070_no_outliers)),
  rep("AMD 6800", nrow(amd_6800_no_outliers)),
  rep("AMD 7700", nrow(amd_7700_no_outliers))
)

levene.test(all_data$increase, all_data$GPU)

# Median increase
# ============================

med_char_fn <- function(data) {
	data %>%
	  summarise(
		median = median(increase, na.rm = TRUE),
		interval_from = wilcox.test(increase, alternative = "two.sided", conf.level = 1 - 0.05, conf.int = TRUE)$conf.int[1],
		interval_to = wilcox.test(increase, alternative = "two.sided", conf.level = 1 - 0.05, conf.int = TRUE)$conf.int[2]
	  )
}

all_med_chars <- as.data.frame(rbind(
  med_char_fn(nvidia_2080_no_outliers),
  med_char_fn(nvidia_3070_no_outliers),
  med_char_fn(amd_6800_no_outliers),
  med_char_fn(amd_7700_no_outliers)
))

rownames(all_med_chars) <- c("nvidia_2080", "nvidia_3070", "amd_6800", "amd_7700")
t(all_med_chars)

# Kruskal Wallis test
# ============================

kruskal.test(all_data$increase, all_data$GPU)

# post-hoc test for median increase for all 4 GPUs - dunn
dunn.test::dunn.test(
  all_data$increase,
  all_data$GPU,
  method = "bonferroni", altp = TRUE
)

# celkový průměr
median_vsech <- median(all_data$increase)

# průměry ve skupinách
efekty <- all_data %>%
  group_by(GPU) %>%
  summarize(median_skup = median(increase))

# efekty
efekty$efekt <- efekty$median_skup - median_vsech

# vypsat setřízené
efekty %>% arrange(desc(median_skup))