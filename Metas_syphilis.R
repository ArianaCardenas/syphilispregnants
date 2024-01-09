library(tidyverse)
library(gtsummary)
library(survey)
library(RColorBrewer)
library(sf)
library(tmap)
library(ggsci)
#install.packages("ggupset")
library(ggupset)

data <- read_csv("Data_final/data_syphilis.csv")

 # Recodificar la variable s411g
 data$s411g <- ifelse(data$s411g == 8, 0, data$s411g)
 
 # Verificar los resultados
 table(data$s411g, useNA = "always")
#Meta 1 : 95% de las gestantes deben ser tamizadas
 
freq_screening_2014 <- data %>% 
  filter(year==2014) %>% 
  group_by(departament) %>% 
  summarise(syphilis_prop = mean(s411g, na.rm = TRUE)) 
print(frequency)
            
freq_screening_2018 <- data %>% 
  filter(year==2018) %>% 
  group_by(departament) %>% 
  summarise(syphilis_prop = mean(s411g, na.rm = TRUE)) 

freq_screening_2022 <- data %>% 
  filter(year==2022) %>% 
  group_by(departament) %>% 
  summarise(syphilis_prop = mean(s411g, na.rm = TRUE)) 

freq_screening_total <- data %>% 
  group_by(departament) %>% 
  summarise(syphilis_prop = mean(s411g, na.rm = TRUE)) 

#Meta 2 : Menos del 0.5 casos de sífilis congénita

congenital <- read_csv("Data_final/syphilis_congenital.csv")
#CORRER R script: Congenital_syphilis

congenital_2014 <- data_congenital %>% 
  filter(year==2014) %>% 
  group_by(NOMBDEP) %>% 
  summarise(congenital_prop = mean(rate, na.rm = TRUE)) 

congenital_2018 <- data_congenital %>% 
  filter(year==2018) %>% 
  group_by(NOMBDEP) %>% 
  summarise(congenital_prop = mean(rate, na.rm = TRUE)) 

pre_congenital_2022 <- data_congenital %>% 
  filter(year==2022) %>% 
  group_by(NOMBDEP) %>% 
  summarise(congenital_prop = mean(rate, na.rm = TRUE)) 

congenital_2022 <- data_congenital %>% 
  filter(year==2022) %>% 
  select(geometry) %>% 
  left_join(pre_congenital_2022, by = "NOMBDEP") 

congenital_total <- data_congenital %>% 
  group_by(NOMBDEP) %>% 
  summarise(congenital_prop = mean(rate, na.rm = TRUE)) 

#Meta 3: disminución en un 90% los casos de sífilis materna
#Correr el rmd de Mapas Syphilis INS.Rmd

maternal_2018<- dntest %>% 
  filter(year==2018) %>% 
  group_by(NOMBDEP) %>% 
  summarise(maternal_prop_18 = mean(rate, na.rm = TRUE))

maternal_2022<- dntest %>% 
  filter(year==2022) %>% 
  group_by(NOMBDEP) %>% 
  summarise(maternal_prop_22 = mean(rate, na.rm = TRUE))

combined_data_maternal <- maternal_2018 %>% 
  left_join(maternal_2022, by = c("NOMBDEP"))

combined_data_maternal <-  combined_data_maternal %>% 
  mutate(rest= maternal_prop_18 - maternal_prop_22) %>% 
  mutate(rest_porcent = (100 - ((maternal_prop_22/maternal_prop_18)*100)))
  
#Figura 2022

# Convertir a mayuscula en la tabla freq_screening_2022
freq_screening_2022 <- freq_screening_2022 %>%
  mutate(departament = toupper(departament))

# Luego, realizar la fusión
combined_data_2022 <- congenital_2022 %>%
  left_join(freq_screening_2022, by = c("NOMBDEP" = "departament")) %>% 
  left_join(combined_data_maternal, by = "NOMBDEP") 

GOALS_2022 <- combined_data_2022 %>% 
 
    mutate(
    goal1 = ifelse(syphilis_prop>=0.95, 1,0),
    goal2 = ifelse(congenital_prop<0.5,1,0),
    goal3 = ifelse(rest_porcent >=45,1,0),
    
    goals = ifelse(syphilis_prop>=0.95 & congenital_prop<0.50 & rest_porcent>=45, list(c ("Goal 1", "Goal 2", "Goal 3")),
                   ifelse(syphilis_prop<0.95 & congenital_prop>=0.50 & rest_porcent<45, list(c(NA)),
                          ifelse(syphilis_prop>=0.95 & congenital_prop>=0.50 & rest_porcent<45, list(c("Goal 1")),
                                 ifelse(syphilis_prop<0.95 & congenital_prop<0.50 & rest_porcent<45, list(c("Goal 2")),
                                        ifelse(syphilis_prop<0.95 & congenital_prop>=0.50 & rest_porcent>=45, list(c("Goal 3")),
                                               ifelse(syphilis_prop>=0.95 & congenital_prop<0.50 & rest_porcent<45, list(c("Goal 1", "Goal 2")),
                                                      ifelse(syphilis_prop>=0.95 & congenital_prop>=0.50 & rest_porcent>=45, list(c("Goal 1", "Goal 3")),
                                                             ifelse(syphilis_prop<0.95 & congenital_prop<0.50 & rest_porcent>=45, list(c("Goal 2", "Goal 3")), NA)
               ))))))))

figure_2a<-
  ggplot(GOALS_2022) +
  aes(x = goals)+
  geom_bar(fill = "#506D84")+
  geom_text(stat='count', aes(label=after_stat(count)), vjust=-1, size = 4) +
  theme_minimal()+
  theme(
    panel.grid = element_blank()
  )+
  labs(x = "Goals", y = "")+
  scale_x_upset(n_intersections= 8) +
  scale_y_continuous(breaks = NULL)
print(figure_2a)

#ggsave("Figure_2a.png",plot = figure_2a, width = 8, height = 4, dpi = 300)

#Goal 1

figure_2b1 <- ggplot() +
  geom_sf(data = GOALS_2022, aes(geometry = geometry, fill = syphilis_prop), col = "#b6cfde") +
  scale_fill_gradient(
    high = "#0C6291", 
    low = "#e5e5e5", 
    limits = c(0,1), 
    breaks = seq(0,1, by = 0.1),
    labels = c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%", "100%")
  )+
  labs(fill= "Goal 1") +
  theme_minimal()
print(figure_2b1)
#ggsave("Figure_2b1.png",plot = figure_2b1, width = 8, height = 4, dpi = 300)


##Goal 2

figure_2b2 <- ggplot() +
  geom_sf(data = GOALS_2022, aes(geometry = geometry, fill = congenital_prop), col = "#b6cfde") +
  scale_fill_gradient(
    high = "#0C6291", 
    low = "#e5e5e5", 
    limits = c(10.00, 0), 
    breaks = seq(10.00, 0, by = -1.0),
    labels = c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%", "100%"),
    trans = "reverse"
  )+
  labs(fill= "Goal 2") +
  theme_minimal()
print(figure_2b2)

#ggsave("Figure_2b2.png",plot = figure_2b2, width = 8, height = 4, dpi = 300)

#Goal 3

GOALS_2022 <- GOALS_2022 %>% 
  mutate(goal_maternal = ifelse(rest_porcent >= 45, "100",
                                ifelse(rest_porcent >=35 & rest_porcent<45, "90",
                                       ifelse(rest_porcent >=25 & rest_porcent<35, "80",
                                              ifelse(rest_porcent >=15 & rest_porcent <25, "70",
                                                     ifelse(rest_porcent >=5 & rest_porcent <15, "60",
                                                            ifelse(rest_porcent >= -5 & rest_porcent <5, "50",
                                                                   ifelse(rest_porcent >= -15 & rest_porcent < -5, "40",
                                                                          ifelse(rest_porcent >= -25 & rest_porcent < -15, "30",
                                                                                 ifelse(rest_porcent >= -35 & rest_porcent < -25, "20",
                                                                                        ifelse(rest_porcent>= -45 & rest_porcent< -35, "10",
                                                                                               ifelse(rest_porcent >= -55 & rest_porcent < -45, "0", NA))))))))))))


GOALS_2022$goal_maternal <- as.numeric(as.character(GOALS_2022$goal_maternal))

figure_2b3 <- ggplot() +
  geom_sf(data = GOALS_2022, aes(geometry = geometry, fill = goal_maternal), col = "#b6cfde") +
  scale_fill_gradient(
    high = "#0C6291", 
    low = "#e5e5e5", 
    limits = c(0,100), 
    breaks = seq(0,100, by = 10),
    labels = c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%", "100%")
  )+
  labs(fill= "Goal 3") +
  theme_minimal()

print(figure_2b3)

#ggsave("Figure_2b3.png",plot = figure_2b3, width = 8, height = 4, dpi = 300)

figure_2b<-cowplot::plot_grid(figure_2b1,figure_2b2, figure_2b3, ncol = 3, rel_heights = c(0.6,0.4))

figure2<-cowplot::plot_grid(figure_2a,figure_2b, ncol = 1, labels = c("a","b"), rel_heights = c(0.6,0.4))

print(figure2)

#ggsave("Figure2.png", plot = figure2, width = 8, height = 4, dpi = 300)







