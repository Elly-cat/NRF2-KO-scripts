#load in data
install.packages("readxl")
library(readxl)
setwd("C:/Users/Hp/OneDrive - NHL Stenden/DNA Editing and CRISPR")
data <- read_xlsx( "PipeteerTabellen.xlsx", sheet = "WST1 data verwerkt", range = "B97:E115")

#or manually type in data
data <- data.frame(
  conc = rep(c(0, 1, 2.5, 5, 10, 25, 50, 100), 2),
  group = rep(c("WT", "Nrf2_KO"), each = 8),
  mean_viab = c(100, 95, 80, 60, 40, 25, 10, 5,   # WT
                100, 90, 70, 50, 30, 15, 8, 4),  # KO 
  sd = c(2, 3, 4, 5, 6, 4, 3, 2, 
         3, 4, 5, 4, 5, 4, 3, 2))

#load in
library(drc)
library(ggplot2)
library(dplyr)

#===============================dose response curve============================================
# Fit the data to a dose-response curve. You want to model cell viability (mean_viab) as a function of concentration (conc).
#curve id Creates separate curves for each group. fct Fits a four-parameter logistic curve.
fit <- drm(mean_viab ~ conc, data = data, curveid = group, fct = LL.4())

#using the log(0) gives an error. this bypassed by using making 0µM into 0.1µM to make log10() possible.
data$conc[data$conc == 0] <- 0.1

#creates a grid of values for plotting. Let me plot the curve for WT and KO at the same concentrations.
plot_data <- expand.grid(
  conc = exp(seq(log(min(data$conc[data$conc>0])), log(max(data$conc)), length.out = 30)),
  group = unique(data$group))

#Calculate the fitted curve values for each point in plot_data to plot a smooth dose-response curve.
plot_data$fit <- predict(fit, newdata = plot_data)

#plot the curve
ggplot(data, aes(x = conc, y = mean_viab, color = group)) +
  geom_point() +
  geom_line(data = plot_data, aes(x = conc, y = fit, color = group), size = 1) + 
  geom_errorbar(aes(ymin = mean_viab - sem, ymax = mean_viab + sem), width = 0.1) +
  scale_x_log10() +
  theme_classic() +
  labs(
    x = "Cisplatine (µM)",
    y = "Cell viability (%)",
    title = "Cisplatine Dose–Response Curves (WT vs Nrf2-KO clones)"
  ) +
  scale_color_manual(values = c("WT" = "#1874CD", "Nrf2_KO" = "#2E8B57"))


#=================================IC50===============================================
#calculate the Effective Dose: Calculates the 50% response point (IC50).
#delta gives a 95% confidence interval.
ED(fit, 50, interval = "delta")

#save as data frame
IC50_vals <- ED(fit, 50, interval = "delta")

#add column of names
IC50_df <- as.data.frame(IC50_vals)

#add group rowname
IC50_df$group <- rownames(IC50_df)

#remove first row
rownames(IC50_df) <- NULL

#remove the e and 50 from the group names
IC50_df$group <- gsub("^e:|:50$", "", IC50_df$group)

#plot the IC50
ggplot(IC50_df, aes(x = group, y = Estimate, fill = group)) +
  geom_col() +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.1) +
  theme_classic() +
  scale_fill_manual(values = c("WT" = "#1874CD", "Nrf2_KO" = "#2E8B57")) +
  labs(
    title = "IC50 values with 95% confidence intervals",
    x = "",
    y = "IC50 (µM)"
  )

#================================test for significance===========================================
#model comparison non linear
#shows the model for each group. Individual curve per group
fit <- drm(mean_viab ~ conc, data = data, curveid = group, fct = LL.4())

#This model assumes that all groups share the same curve. One curve for all data combined.
fit2 <- drm(mean_viab ~ conc, data = data, fct = LL.4())
#significance across the dose curve.
anova(fit, fit2)

#linear regression
# slope on the entire line with 95% confidence
# extracts all parameters (b, c, d, e) of the fit.
coef(fit)

#give confidence intervals and select only the slope (b) parameters.
confint(fit)[grep("^b:", rownames(confint(fit))), ]

#take data between 1µM and 10µM
data_1_10 <- subset(data, conc >= 1 & conc <= 10)

#make the regression for each group
lm_WT  <- lm(mean_viab ~ conc, data = subset(data_1_10, group == "WT"))
lm_KO  <- lm(mean_viab ~ conc, data = subset(data_1_10, group == "Nrf2_KO"))

#get the slope from each group
coef(lm_WT)["conc"]
coef(lm_KO)["conc"]

#parameter values + p-values.
lm_int <- lm(mean_viab ~ I(log(conc)) + group, data = data_1_10)
summary(lm_int)

#test whether the groups differ significantly in slope on a log scale.
anova(lm_int)


