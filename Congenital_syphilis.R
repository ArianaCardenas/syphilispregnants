library(sf)
library(purrr)
install.packages("tidyverse", repos = "http://cran.us.r-project.org")
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(readr)
library(survey)
install.packages("ggspatial")
library(ggspatial)

data_ubigeo <- read_csv("Data_final/data_ubigeo_innovalab.csv")
data_ubigeo_2022_innova <- read_csv("Data_final/data_ubigeo_2022_innova.csv")
provincias <- read_sf("Límites - mapas/PROVINCIAS_inei_geogpsperu_suyopomalia.shp")
departamentos <- read_sf("Límites - mapas/DEPARTAMENTOS_inei_geogpsperu_suyopomalia.shp")
data_syphilis <- read_csv("Data_final/data_syphilis.csv")
congenital <- read_csv("Data_final/syphilis_congenital.csv")
newborn <- read_csv("Data_final/Nacidos_vivos_long.csv")

data_ubigeo_2022_innova = data_ubigeo_2022_innova[,c(2:6,1,7:9)]
data_ubigeo = data_ubigeo[,c(1:6,8,10,12)]

data_ubigeo_2022_innova = rename(data_ubigeo_2022_innova, c("NOMBDEP"="DEPARTAMEN","NOMBPROV"="PROVINCIA","NOMBDIST"="DISTRITO"))

data_ubigeo = rbind(data_ubigeo,data_ubigeo_2022_innova)

data_total <- data_syphilis%>% 
  left_join(data_ubigeo, by = c("caseid","year"))

data_total <- data_total %>%
  left_join(provincias, by = c("NOMBDEP", "NOMBPROV"))

#Data INS - Sifilis congénita

congenital <- congenital %>%
  select(c(1:16)) %>%
  select(!c(4:7))

congenital <- rename(
  congenital, c("NOMBDEP"="DEPARTAMENTO","NOMBPROV"="PROVINCIA","NOMBDIST"="DISTRITO"))

congenital_long <- pivot_longer(congenital, cols = c(4:12), 
                                names_to = "year", values_to = "congenital")

congenital_long <- congenital_long %>%
  group_by(NOMBDEP, NOMBPROV, year) %>%
  summarise(congenital = sum(congenital, na.rm = TRUE)) %>%
  unnest(congenital)

newborn <- newborn %>%
  rename(c("NOMBDEP"="departamento","NOMBPROV"="provincia", "year"="years")) %>%
  mutate(year = as.character(year)) 

data_congenital <- congenital_long %>%
  left_join(newborn, by = c("NOMBDEP","NOMBPROV","year")) %>%
  mutate(rate = (congenital/n)*1000) %>% 
  mutate_if(is.numeric, round, 2) %>%
  filter(rate < 100.0)  %>% #Se excluyó el valor de Barranca en 2014, ya que tenia 3 casos de 4 nacidos.
  left_join(provincias, by = c("NOMBDEP","NOMBPROV"))
  # summarise(meanrate = mean(rate, na.rm = TRUE)) %>% 
  # mutate_if(is.numeric, round, 2)

ggplot() +
  geom_sf(data = data_congenital,
          aes(geometry = geometry,fill = rate),
          color = "black", size = 0.2) +
  # geom_sf_text(data = data_grafico_know,
  #              aes(geometry = geometry,label = meanrate),
  #              size=2.0, color = "black") +
  scale_fill_gradient(low = "skyblue", high = "blue") +
  labs(title="Rate of congenital syphilis",
       fill = "Rate")

ggplot() +
  geom_sf(data = data_congenital,
          aes(geometry = geometry,fill = rate),
          color = "black", size = 0.2) +
  scale_fill_gradient(low = "skyblue", high = "blue") +
  facet_wrap(.~year) + 
  labs(title="Rate of congenital syphilis",
       fill = "Rate")

#dumbell plot

newborn2 <- newborn %>%
  group_by(NOMBDEP, year) %>%
  summarise(n = sum(n, na.rm = TRUE))

d14_d22 <- congenital_long %>%
  group_by(NOMBDEP, year) %>%
  summarise(congenital = sum(congenital, na.rm = TRUE)) %>%
  left_join(newborn2, by = c("NOMBDEP","year")) %>%
  mutate(rate = (congenital/n)*1000) %>% 
  mutate_if(is.numeric, round, 2) %>%
  filter(rate < 100.0) %>% #Se excluyó el valor de Barranca en 2014, ya que tenia 3 casos de 4 nacidos.
  filter(year==2014|year==2018|year==2022) 

ggplot(d14_d22, aes(x = rate, y = NOMBDEP)) +
  geom_line() +
  geom_point(aes(color = year), size = 3) +
  xlab("Rate of syphilis congenital")+
  ylab("Region")


# Casos de sifilis congenita con covariables de ENDES (pendiente)

data_filtered <- data_total %>%
  filter(!is.na(health_insurance))%>%
  mutate(
    insurance=ifelse(health_insurance=="Yes",1,0)
  ) %>%
  filter(year != "2010" & year != "2011")

data_filtered$NOMBDEP = as.factor(data_filtered$NOMBDEP)
data_filtered$NOMBPROV = as.factor(data_filtered$NOMBPROV)
data_filtered$year = as.factor(data_filtered$year)
data_filtered$health_insurance = as.factor(data_filtered$health_insurance)

# Crear el diseño de muestreo estratificado
design <- svydesign(id = ~ v001, strata = ~ v022, nest = TRUE, weights = ~ v005, data = data_filtered)

options(survey.lonely.psu="adjust")

syphilis_year<- svyby(~health_insurance,~NOMBDEP+year,svymean,design=design,deff=TRUE,na.rm=TRUE)
syphilis_total<- svyby(~health_insurance,~NOMBDEP+NOMBPROV,svymean,design=design,deff=TRUE,na.rm=TRUE)

syphilis_year = as.data.frame(syphilis_year)
syphilis_total = as.data.frame(syphilis_total)

data_grafico_total <- syphilis_total  %>%
  left_join(departamentos, by = c("NOMBDEP"))


