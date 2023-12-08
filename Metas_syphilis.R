library(tidyverse)
library(gtsummary)
library(survey)
library(RColorBrewer)
library(sf)
library(tmap)
library(ggsci)
install.packages("ggupset")
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

congenital_2022 <- data_congenital %>% 
  filter(year==2022) %>% 
  group_by(NOMBDEP) %>% 
  summarise(congenital_prop = mean(rate, na.rm = TRUE)) 

congenital_total <- data_congenital %>% 
  group_by(NOMBDEP) %>% 
  summarise(congenital_prop = mean(rate, na.rm = TRUE)) 

#Meta 3: disminución en un 90% los casos de sífilis materna




#Figura 2022

# Convertir a mayuscula en la tabla freq_screening_2022
freq_screening_2022 <- freq_screening_2022 %>%
  mutate(departament = toupper(departament))

# Luego, realizar la fusión
combined_data_2022 <- congenital_2022 %>%
  left_join(freq_screening_2022, by = c("NOMBDEP" = "departament"))

METAS_2022 <- combined_data_2022 %>% 
  
  mutate(
    meta1 = ifelse(syphilis_prop>=0.95, 1,0),
    meta2 = ifelse(congenital_prop<0.5,1,0),
    
    metas = ifelse(syphilis_prop>=0.95 & congenital_prop<0.50, list(c ("Meta 1", "Meta 2")),
               ifelse(syphilis_prop<0.95 & congenital_prop>=0.50, list(c(NA)),
               ifelse(syphilis_prop>=0.95 & congenital_prop>=0.50, list(c("Meta 1")),
               ifelse(syphilis_prop<0.95 & congenital_prop<0.50, list(c("Meta 2")), NA)
               ))))

figura_metas_2022<-
  ggplot(METAS_2022) +
  aes(x = metas)+
  geom_bar(fill = "#506D84")+
  geom_text(stat='count', aes(label=after_stat(count)), vjust=-1, size = 4) +
  theme_minimal()+
  theme(
    panel.grid = element_blank()
  )+
  labs(x = "Goals", y = "")+
  scale_x_upset(n_intersections= 4) +
  scale_y_continuous(breaks = NULL)
print(figura_metas_2022)

#Figura 2018

# Convertir a mayuscula en la tabla freq_screening_2022
freq_screening_2018 <- freq_screening_2018 %>%
  mutate(departament = toupper(departament))

# Luego, realizar la fusión
combined_data_2018 <- congenital_2018 %>%
  left_join(freq_screening_2018, by = c("NOMBDEP" = "departament"))

METAS_2018 <- combined_data_2018 %>% 
  
  mutate(
    meta1 = ifelse(syphilis_prop>=0.95, 1,0),
    meta2 = ifelse(congenital_prop<0.5,1,0),
    
    metas = ifelse(syphilis_prop>=0.95 & congenital_prop<0.50, list(c ("Meta 1", "Meta 2")),
                   ifelse(syphilis_prop<0.95 & congenital_prop>=0.50, list(c(NA)),
                          ifelse(syphilis_prop>=0.95 & congenital_prop>=0.50, list(c("Meta 1")),
                                 ifelse(syphilis_prop<0.95 & congenital_prop<0.50, list(c("Meta 2")), NA)
                          ))))

figura_metas_2018<-
  ggplot(METAS_2018) +
  aes(x = metas)+
  geom_bar(fill = "#506D84")+
  geom_text(stat='count', aes(label=after_stat(count)), vjust=-1, size = 4) +
  theme_minimal()+
  theme(
    panel.grid = element_blank()
  )+
  labs(x = "Goals", y = "")+
  scale_x_upset(n_intersections= 4) +
  scale_y_continuous(breaks = NULL)
print(figura_metas_2018)

#Figura 2014

# Convertir a mayuscula en la tabla freq_screening_2022
freq_screening_2014 <- freq_screening_2014 %>%
  mutate(departament = toupper(departament))

# Luego, realizar la fusión
combined_data_2014 <- congenital_2014 %>%
  left_join(freq_screening_2014, by = c("NOMBDEP" = "departament"))

METAS_2014 <- combined_data_2014 %>% 
  
  mutate(
    meta1 = ifelse(syphilis_prop>=0.95, 1,0),
    meta2 = ifelse(congenital_prop<0.5,1,0),
    
    metas = ifelse(syphilis_prop>=0.95 & congenital_prop<0.50, list(c ("Meta 1", "Meta 2")),
                   ifelse(syphilis_prop<0.95 & congenital_prop>=0.50, list(c(NA)),
                          ifelse(syphilis_prop>=0.95 & congenital_prop>=0.50, list(c("Meta 1")),
                                 ifelse(syphilis_prop<0.95 & congenital_prop<0.50, list(c("Meta 2")), NA)
                          ))))

figura_metas_2014<-
  ggplot(METAS_2014) +
  aes(x = metas)+
  geom_bar(fill = "#506D84")+
  geom_text(stat='count', aes(label=after_stat(count)), vjust=-1, size = 4) +
  theme_minimal()+
  theme(
    panel.grid = element_blank()
  )+
  labs(x = "Goals", y = "")+
  scale_x_upset(n_intersections= 4) +
  scale_y_continuous(breaks = NULL)
print(figura_metas_2014)

#Figura total

# Convertir a mayuscula en la tabla 
freq_screening_total <- freq_screening_total %>%
  mutate(departament = toupper(departament))

# Luego, realizar la fusión
combined_data_total <- congenital_total %>%
  left_join(freq_screening_total, by = c("NOMBDEP" = "departament"))

METAS_total <- combined_data_total %>% 
  
  mutate(
    meta1 = ifelse(syphilis_prop>=0.95, 1,0),
    meta2 = ifelse(congenital_prop<0.5,1,0),
    
    metas = ifelse(syphilis_prop>=0.95 & congenital_prop<0.50, list(c ("Meta 1", "Meta 2")),
                   ifelse(syphilis_prop<0.95 & congenital_prop>=0.50, list(c(NA)),
                          ifelse(syphilis_prop>=0.95 & congenital_prop>=0.50, list(c("Meta 1")),
                                 ifelse(syphilis_prop<0.95 & congenital_prop<0.50, list(c("Meta 2")), NA)
                          ))))

figura_metas_total<-
  ggplot(METAS_total) +
  aes(x = metas)+
  geom_bar(fill = "#506D84")+
  geom_text(stat='count', aes(label=after_stat(count)), vjust=-1, size = 4) +
  theme_minimal()+
  theme(
    panel.grid = element_blank()
  )+
  labs(x = "Goals", y = "")+
  scale_x_upset(n_intersections= 4) +
  scale_y_continuous(breaks = NULL)
print(figura_metas_total)

