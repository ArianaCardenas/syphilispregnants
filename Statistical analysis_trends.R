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

#Run Rmd 5_Tendencias_general


install.packages("segmented")
library(segmented)
library(ggplot2)

# Ejemplo básico
model_cong <- lm(year ~ rate, data = data_congenital)
seg_model_cong <- segmented(model_cong, seg.Z = ~rate, npsi = 1)  # npsi = número de puntos de cambio

summary(seg_model_cong)
plot(seg_model_cong, xlab ="Rate",)
title("Congenital Syphilis",
      cex.main = 1,   font.main= 1, col.main= "black",
      adj = 0, line = 0.5)


model_mat <- lm(year ~ rate, data = data_maternal)
seg_model_mat<- segmented(model_mat, seg.Z = ~rate, npsi = 1)  # npsi = número de puntos de cambio

summary(seg_model_mat)
plot(seg_model_mat, xlab ="Rate")
title("Maternal Syphilis",
      cex.main = 1,   font.main= 1, col.main= "black",
      adj = 0, line = 0.5)

model_screen <- lm(year ~ sypprop, data = syphilis_df)
seg_model_screen<- segmented(model_screen, seg.Z = ~sypprop, npsi = 1)  # npsi = número de puntos de cambio

summary(seg_model_screen)
plot(seg_model_screen, xlab ="Prop. %")
title("Syphilis screening",
      cex.main = 1,   font.main= 1, col.main= "black",
      adj = 0, line = 0.5)


install.packages("magick")
library(magick)
install.packages("ggplotify")
library(ggplotify)

# Replace "path/to/your/image.jpg" with your actual file path
a <- image_read("Figures/a.png")
a <- ggplotify::as.grob(a)
b <- image_read("Figures/b.png")
b <- ggplotify::as.grob(b)
c <- image_read("Figures/c.png")
c <- ggplotify::as.grob(c)
# Map of the three

library(gridExtra)
library(patchwork)

# Saving figure 1:

figure1<-cowplot::plot_grid(a, 
                            b, 
                            c, 
                            ncol = 1, labels = c("a","b", "c"), 
                            label_size = 18)

print(figure1)

ggsave("Figures/Figure_Jointpoint.png", plot = figure1, width = 3.3, height = 8, dpi = 300)


