
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






#voorbeeld met 3 groepen
library(ggplot2)
library(drc)
# Voorbeelddata:of gwn de cv inladen
data <- data.frame(
  conc = rep(c(0, 1, 2.5, 5, 10, 25, 50, 100), 3),
  group = rep(c("WT", "Nrf2_KO_clone_1", "Nrf2_KO_clone_2"), each = 8),
  mean_viab = c(100, 95, 80, 60, 40, 25, 10, 5,   # WT
                100, 90, 70, 50, 30, 15, 8, 4,    #KO 1
                100, 91, 67, 52, 28, 10, 7, 2),  # KO 2
  sd = c(2, 3, 4, 5, 6, 4, 3, 2, 
         3, 4, 5, 4, 5, 4, 3, 2,
         3, 4, 5, 4, 5, 4, 3, 2)
)

# ---- 3. Dose–response model fitten per groep ----
# ~ zegt hoe wilt modeleren y = viability en x= conc
#curveid geef aan dat je meerder groepen hebt
# welke model wil je dit geval LL.4
model <- drm(mean_viab ~ conc, curveid = group, data = data, fct = LL.4())
??drm
# ---- 4. IC50 berekenen ----
#delta geeft aan betrouwbaarheid methode via delta methode
IC50_vals <- ED(model, 50, interval = "delta")
print(IC50_vals)

# ---- 5. Plot met error bars ----
# Data voorbereiden
# hij gaat voor spellen alle combinatie concentratie per groep tussen 0 en 100µM
#dit geeft vloeiender lijnen
pred_data <- expand.grid(
  conc = seq(min(data$conc), max(data$conc), length.out = 100),
  group = unique(data$group)
)
??expand.grid
# Voorspelde waarden uit het model
#voorspelt nu de celviability voor elke concentratie die zit dan in kolom van pred.
pred_data$pred <- predict(model, newdata = pred_data)

# Plot met ggplot2
p <- ggplot(data, aes(x = conc, y = mean_viab, color = group, group = group)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_viab - sd, ymax = mean_viab + sd), width = 0.1) +
  geom_line(data = pred_data, aes(y = pred), linewidth = 1) +
  scale_x_log10(breaks = c(0.1, 1, 2.5, 5, 10, 25, 50, 100)) +
  labs(
    title = "Cisplatine Dose–Response Curves (WT vs Nrf2-KO clones)",
    x = "Cisplatine (µM, log schaal)",
    y = "Cell viability (%)"
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank()
  )

print(p)


#ic50 plot
library(dplyr)
#voeg kolom van namen toe
IC50_df <- as.data.frame(IC50_vals)
IC50_df$Group <- rownames(IC50_df)
rownames(IC50_df) <- NULL
# haal weg de e en 50 uit de groep namen
IC50_df$Group <- gsub("^e:|:50$", "", IC50_df$Group)

#plot de IC50
ggplot(IC50_df, aes(x = Group, y = Estimate, fill = Group)) +
  geom_col() +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.1) +
  theme_classic() +
  labs(
    title = "IC50 values with 95% confidence intervals",
    x = "",
    y = "IC50 (µM)"
  )


#anova op de dose curve
#Om te bepalen of de IC₅₀-waarden significant verschilden tussen de experimentele groepen,
#werden de volledige dosis–respons-curves gemodelleerd met een vierparameter log-logistische functie.

#geeft de model voor elke groep. Eigen curve per groep
model <- drm(mean_viab ~ conc, curveid = group, data = data, fct = LL.4())
#Dit model gaat ervan uit dat alle groepen dezelfde curve delen. Één curve voor alle data samen
model2 <- drm(mean_viab ~ conc, data = data, fct = LL.4())
#significantie over de dosis curve.
anova(model, model2)

#“Modelvergelijking toonde aan dat het toelaten van afzonderlijke curves per groep een 
#significant betere fit opleverde dan één gezamenlijke curve (F = 8.52, p = 0.012), 
#wat duidt op een significant verschil in IC₅₀ tussen de groepen.”

#of

#“De geschatte IC₅₀ voor de WT-cellen was 7.06 µM (95% CI: 5.49–8.63),
#terwijl voor de Nrf2_KO-clonen de IC₅₀ respectievelijk 4.74 µM (95% CI: 3.89–5.60) en
#4.79 µM (95% CI: 4.02–5.56) bedroeg.
#De extra sum-of-squares F-test toonde een significant verschil aan tussen de groepen (p < 0.05).”
