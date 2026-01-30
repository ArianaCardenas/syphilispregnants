#Joint regression for syphilis trends:

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("Trendy")

library(Trendy)

# # Ejecutar análisis Joinpoint automático
# jp_model <- joinpoint(data_syph$year, data_syph$rate_cong, family = "Poisson", n.trend = 3)
# 
# # Resumen general
# summary(jp_model)
# 
# # Graficar
# plot(jp_model, main = "Joinpoint regression - Sífilis congénita (Perú, 2015–2022)",
#      ylab = "Tasa por 1,000 NV", xlab = "Año")
# 
# # Resultados principales
# jp_model$apc   # APC por segmento con IC95%
# jp_model$aapc  # AAPC global con IC95%


install.packages("segmented")
library(segmented)

# Ejemplo básico
model_cong <- lm(year ~ rate, data = data_congenital)
seg_model_cong <- segmented(model_cong, seg.Z = ~rate, npsi = 1)  # npsi = número de puntos de cambio

summary(seg_model_cong)
plot(seg_model_cong)


model_mat <- lm(year ~ rate, data = data_maternal)
seg_model_mat<- segmented(model_mat, seg.Z = ~rate, npsi = 1)  # npsi = número de puntos de cambio

summary(seg_model_mat)
plot(seg_model_mat)

model_screen <- lm(year ~ sypprop, data = syphilis_df)
seg_model_screen<- segmented(model_screen, seg.Z = ~sypprop, npsi = 1)  # npsi = número de puntos de cambio

summary(seg_model_screen)
plot(seg_model_screen)
