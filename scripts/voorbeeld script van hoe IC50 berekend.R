
install.packages("drc")
library(drc)

# voorbeelddata
conc <- c(0, 1, 2.5, 5, 10, 25, 50, 100)
viab <- c(100, 95, 80, 60, 45, 25, 10, 5)
data <- data.frame(conc, viab)

# model fitten voor concentration/dose/time-effect/response data
#fct → bepaalt hoe de curve tussen dose en response wordt gemodelleerd
#LL.4 is een 4 parameter logistische functie, Meest gebruikt: Slope, Lower, Upper, en IC50 (volledige 4-parameter logistische curve)
#in het kort het is een soort curvende grafiek.
model <- drm(viab ~ conc, data = data, fct = LL.4())

# IC50 berekenen, 50 geeft aan dat je ic50 wil
#geeft de estimate, error, lower an upper bound
#interval geeft type betrouwbaarheid. "delta" → delta-methode (meest gebruikt, snel en betrouwbaar)
ED(model, 50, interval = "delta")

#plot model
plot(model,
     xlab = "Cisplatine (µM)",
     ylab = "Cell viability (%)",
     main = "Dose–response curve (HeLa)",
     type = "all",
     log = "x")
??plot
#voor 2 groepen
model2 <- drm(viab ~ conc, data = data, fct = LL.4(), curveid = groep)
ED(model2, 50, interval = "delta")






#voorbeeld met 2 groepen
library(ggplot2)
library(drc)
# Voorbeelddata:of gwn de cv inladen
data <- data.frame(
  conc = rep(c(0, 1, 2.5, 5, 10, 25, 50, 100), 2),
  group = rep(c("WT", "Nrf2_KO"), each = 8),
  mean_viab = c(100, 95, 80, 60, 40, 25, 10, 5,   # WT
                100, 90, 70, 50, 30, 15, 8, 4),  # KO
  sd = c(2, 3, 4, 5, 6, 4, 3, 2, 
         3, 4, 5, 4, 5, 4, 3, 2)
)

# ---- 3. Dose–response model fitten per groep ----
model <- drm(mean_viab ~ conc, curveid = group, data = data, fct = LL.4())

# ---- 4. IC50 berekenen ----
IC50_vals <- ED(model, 50, interval = "delta")
print(IC50_vals)

# ---- 5. Plot met error bars ----
# Data voorbereiden
pred_data <- expand.grid(
  conc = seq(min(data$conc), max(data$conc), length.out = 100),
  group = unique(data$group)
)
??expand.grid
# Voorspelde waarden uit het model
pred_data$pred <- predict(model, newdata = pred_data)

# Plot met ggplot2
p <- ggplot(data, aes(x = conc, y = mean_viab, color = group, group = group)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_viab - sd, ymax = mean_viab + sd), width = 0.1) +
  geom_line(data = pred_data, aes(y = pred), size = 1) +
  scale_x_log10(breaks = c(0.1, 1, 2.5, 5, 10, 25, 50, 100)) +
  labs(
    title = "Cisplatine Dose–Response Curves (WT vs Nrf2-KO)",
    x = "Cisplatine (µM, log schaal)",
    y = "Cell viability (%)"
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  )

print(p)
