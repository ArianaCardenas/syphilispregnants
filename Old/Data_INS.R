#Data INS
#Recodificación: 
# 10a-11a:0, 12a-14a:1, 15a-17a:2, 18a-19a:3, 20a-29a:4, 30a-59a:5
library(tidyverse)
library(readr)
#install.packages("reshape2")
library(reshape2)
library(janitor)
library(readxl)
library(dplyr)

##2011###

Data_ins_unida <- read_excel("Data_ins_unida.xlsx",sheet = "2011")

Data_ins_2011 <- Data_ins_unida %>% 
  mutate(id = group_indices(Data_ins_unida, .dots=c("DISTRITO", "PROVINCIA", "DEPARTAMENTO"))) 
glimpse(Data_ins_2011)

#Sumando el total de pruebas rápidas y pruebas  RPR
Data_ins_2011 <- Data_ins_2011 %>% mutate(RAPID_TEST_NUMBER = P_RAPIDA_I + P_RAPIDA_II + P_RAPIDA_III,
                                          RPR_NUMBER = P_RPR_LESS24SS + P_RPR_MORE24SS,
                                          YEAR="2011") %>% clean_names()
Data_ins_2011 <- Data_ins_2011 %>% mutate(test_number = rapid_test_number + rpr_number,
                                          tamizaje_reactivo = p_rapida_reactivo + p_rpr_reactivo,
                                          rango_edad = case_when((grupo_edad==0) ~ "<12 años",
                                                                (grupo_edad==1 | grupo_edad==2)~ "12-17 años",
                                                                (grupo_edad==3 | grupo_edad==4)~ "18-29 años",
                                                                (grupo_edad==5)~ "30-59 años"))
INS_2011_long <- Data_ins_2011 %>% select(id,distrito, departamento, provincia, rango_edad, test_number, tamizaje_reactivo, year)

##2012###

Data_ins_unida <- read_excel("Data_ins_unida.xlsx",sheet = "2012")

Data_ins_2012 <- Data_ins_unida %>% 
  mutate(id = group_indices(Data_ins_unida, .dots=c("DISTRITO", "PROVINCIA", "DEPARTAMENTO"))) 
glimpse(Data_ins_2012)

#Sumando el total de pruebas rápidas y pruebas  RPR
Data_ins_2012 <- Data_ins_2012 %>% mutate(RAPID_TEST_NUMBER = P_RAPIDA_I + P_RAPIDA_II + P_RAPIDA_III,
                                          RPR_NUMBER = P_RPR_LESS24SS + P_RPR_MORE24SS,
                                          YEAR="2012") %>% clean_names()

Data_ins_2012 <- Data_ins_2012 %>% mutate(test_number = rapid_test_number + rpr_number,
                                          tamizaje_reactivo = p_rapida_reactivo + p_rpr_reactivo,
                                          rango_edad = case_when((grupo_edad==0) ~ "<12 años",
                                                                 (grupo_edad==1 | grupo_edad==2)~ "12-17 años",
                                                                 (grupo_edad==3 | grupo_edad==4)~ "18-29 años",
                                                                 (grupo_edad==5)~ "30-59 años"))

INS_2012_long <- Data_ins_2012 %>% select(id,distrito, departamento, provincia, rango_edad, test_number, tamizaje_reactivo, year)

##2013###

Data_ins_unida <- read_excel("Data_ins_unida.xlsx",sheet = "2013")

Data_ins_2013 <- Data_ins_unida %>% 
  mutate(id = group_indices(Data_ins_unida, .dots=c("DISTRITO", "PROVINCIA", "DEPARTAMENTO"))) 
glimpse(Data_ins_2013)

#Sumando el total de pruebas rápidas y pruebas  RPR
Data_ins_2013 <- Data_ins_2013 %>% mutate(RAPID_TEST_NUMBER = P_RAPIDA_I + P_RAPIDA_II + P_RAPIDA_III,
                                          RPR_NUMBER = P_RPR_LESS24SS + P_RPR_MORE24SS,
                                          YEAR="2013") %>% clean_names()

Data_ins_2013 <- Data_ins_2013 %>% mutate(test_number = rapid_test_number + rpr_number,
                                          tamizaje_reactivo = p_rapida_reactivo + p_rpr_reactivo,
                                          rango_edad = case_when((grupo_edad==0) ~ "<12 años",
                                                                 (grupo_edad==1 | grupo_edad==2)~ "12-17 años",
                                                                 (grupo_edad==3 | grupo_edad==4)~ "18-29 años",
                                                                 (grupo_edad==5)~ "30-59 años"))
INS_2013_long <- Data_ins_2013 %>% select(id,distrito, departamento, provincia, rango_edad, test_number, tamizaje_reactivo, year)

##2014###

Data_ins_unida <- read_excel("Data_ins_unida.xlsx",sheet = "2014")

Data_ins_2014 <- Data_ins_unida %>% 
  mutate(id = group_indices(Data_ins_unida, .dots=c("DISTRITO", "PROVINCIA", "DEPARTAMENTO"))) 
glimpse(Data_ins_2014)

#Sumando el total de pruebas rápidas y pruebas  RPR
Data_ins_2014 <- Data_ins_2014 %>% mutate(RAPID_TEST_NUMBER = P_RAPIDA_I + P_RAPIDA_II + P_RAPIDA_III,
                                          RPR_NUMBER = P_RPR_LESS24SS + P_RPR_MORE24SS,
                                          YEAR="2014") %>% clean_names()

Data_ins_2014 <- Data_ins_2014 %>% mutate(test_number = rapid_test_number + rpr_number,
                                          tamizaje_reactivo = p_rapida_reactivo + p_rpr_reactivo,
                                          rango_edad = case_when((grupo_edad==0) ~ "<12 años",
                                                                 (grupo_edad==1 | grupo_edad==2)~ "12-17 años",
                                                                 (grupo_edad==3 | grupo_edad==4)~ "18-29 años",
                                                                 (grupo_edad==5)~ "30-59 años"))
INS_2014_long <- Data_ins_2014 %>% select(id,distrito, departamento, provincia, rango_edad, test_number, tamizaje_reactivo, year)

##2015###

Data_ins_unida <- read_excel("Data_ins_unida.xlsx",sheet = "2015")

Data_ins_2015 <- Data_ins_unida %>% 
  mutate(id = group_indices(Data_ins_unida, .dots=c("DISTRITO", "PROVINCIA", "DEPARTAMENTO"))) 
glimpse(Data_ins_2015)

#Sumando el total de pruebas rápidas y pruebas  RPR
Data_ins_2015 <- Data_ins_2015 %>% mutate(RAPID_TEST_NUMBER = P_RAPIDA_I + P_RAPIDA_II + P_RAPIDA_III,
                                          RPR_NUMBER = P_RPR_LESS24SS + P_RPR_MORE24SS,
                                          YEAR="2015") %>% clean_names()

Data_ins_2015 <- Data_ins_2015 %>% mutate(test_number = rapid_test_number + rpr_number,
                                          tamizaje_reactivo = p_rapida_reactivo + p_rpr_reactivo,
                                          rango_edad = case_when((grupo_edad==0) ~ "<12 años",
                                                                 (grupo_edad==1 | grupo_edad==2)~ "12-17 años",
                                                                 (grupo_edad==3 | grupo_edad==4)~ "18-29 años",
                                                                 (grupo_edad==5)~ "30-59 años"))
INS_2015_long <- Data_ins_2015 %>% select(id,distrito, departamento, provincia, rango_edad, test_number, tamizaje_reactivo, year)

##2016###

Data_ins_unida <- read_excel("Data_ins_unida.xlsx",sheet = "2016")

Data_ins_2016 <- Data_ins_unida %>% 
  mutate(id = group_indices(Data_ins_unida, .dots=c("DISTRITO", "PROVINCIA", "DEPARTAMENTO"))) 
glimpse(Data_ins_2016)

#Sumando el total de pruebas rápidas y pruebas  RPR
Data_ins_2016 <- Data_ins_2016 %>% mutate(RAPID_TEST_NUMBER = P_RAPIDA_I + P_RAPIDA_II + P_RAPIDA_III,
                                          RPR_NUMBER = P_RPR_LESS24SS + P_RPR_MORE24SS,
                                          YEAR="2016") %>% clean_names()

Data_ins_2016 <- Data_ins_2016 %>% mutate(test_number = rapid_test_number + rpr_number,
                                          tamizaje_reactivo = p_rapida_reactivo + p_rpr_reactivo,
                                          rango_edad = case_when((grupo_edad==0) ~ "<12 años",
                                                                 (grupo_edad==1 | grupo_edad==2)~ "12-17 años",
                                                                 (grupo_edad==3 | grupo_edad==4)~ "18-29 años",
                                                                 (grupo_edad==5)~ "30-59 años"))
INS_2016_long <- Data_ins_2016 %>% select(id,distrito, departamento, provincia, rango_edad, test_number, tamizaje_reactivo, year)

##2017###

Data_ins_unida <- read_excel("Data_ins_unida.xlsx",sheet = "2017")

Data_ins_2017 <- Data_ins_unida %>% 
  mutate(id = group_indices(Data_ins_unida, .dots=c("DISTRITO", "PROVINCIA", "DEPARTAMENTO"))) 
glimpse(Data_ins_2017)

#Sumando el total de pruebas rápidas y pruebas  RPR
Data_ins_2017 <- Data_ins_2017 %>% mutate(RAPID_TEST_NUMBER = P_RAPIDA_I + P_RAPIDA_II + P_RAPIDA_III,
                                          RPR_NUMBER = P_RPR_LESS24SS + P_RPR_MORE24SS,
                                          YEAR="2017") %>% clean_names()

Data_ins_2017 <- Data_ins_2017 %>% mutate(test_number = rapid_test_number + rpr_number,
                                          tamizaje_reactivo = p_rapida_reactivo + p_rpr_reactivo,
                                          rango_edad = case_when((grupo_edad==0) ~ "<12 años",
                                                                 (grupo_edad==1 | grupo_edad==2)~ "12-17 años",
                                                                 (grupo_edad==3 | grupo_edad==4)~ "18-29 años",
                                                                 (grupo_edad==5)~ "30-59 años"))

INS_2017_long <- Data_ins_2017 %>% select(id,distrito, departamento, provincia, rango_edad, test_number, tamizaje_reactivo, year)

##2018###

Data_ins_unida <- read_excel("Data_ins_unida.xlsx",sheet = "2018")

Data_ins_2018 <- Data_ins_unida %>% 
  mutate(id = group_indices(Data_ins_unida, .dots=c("DISTRITO", "PROVINCIA", "DEPARTAMENTO"))) 
glimpse(Data_ins_2018)

#Sumando el total de pruebas rápidas y pruebas  RPR
Data_ins_2018 <- Data_ins_2018 %>% mutate(RAPID_TEST_NUMBER = P_RAPIDA_I + P_RAPIDA_II + P_RAPIDA_III,
                                          RPR_NUMBER = P_RPR_LESS24SS + P_RPR_MORE24SS,
                                          YEAR="2018") %>% clean_names()

Data_ins_2018 <- Data_ins_2018 %>% mutate(test_number = rapid_test_number + rpr_number,
                                          tamizaje_reactivo = p_rapida_reactivo + p_rpr_reactivo,
                                          rango_edad = case_when((grupo_edad==0) ~ "<12 años",
                                                                 (grupo_edad==1 | grupo_edad==2)~ "12-17 años",
                                                                 (grupo_edad==3 | grupo_edad==4)~ "18-29 años",
                                                                 (grupo_edad==5)~ "30-59 años"))

INS_2018_long <- Data_ins_2018 %>% select(id,distrito, departamento, provincia, rango_edad, test_number, tamizaje_reactivo, year)

##2019###

Data_ins_unida <- read_excel("Data_ins_unida.xlsx",sheet = "2019")

Data_ins_2019 <- Data_ins_unida %>% 
  mutate(id = group_indices(Data_ins_unida, .dots=c("DISTRITO", "PROVINCIA", "DEPARTAMENTO"))) 
glimpse(Data_ins_2019)

#Sumando el total de pruebas rápidas y pruebas  RPR
Data_ins_2019 <- Data_ins_2019 %>% mutate(TEST_NUMBER = `1_TAMIZAJE_I` + `1_TAMIZAJE_II` + `1_TAMIZAJE_III` + `2_TAMIZAJE_II` + `2_TAMIZAJE_III`,
                                          TAMIZAJE_REACTIVO = `1_TAMIZAJE_REACTIVO` + `2_TAMIZAJE_REACTIVO`,
                                          RANGO_EDAD = case_when((GRUPO_EDAD==0) ~ "<12 años",
                                                                 (GRUPO_EDAD==1 | GRUPO_EDAD==2)~ "12-17 años",
                                                                 (GRUPO_EDAD==3 | GRUPO_EDAD==4)~ "18-29 años",
                                                                 (GRUPO_EDAD==5)~ "30-59 años"),
                                          YEAR="2019") %>% clean_names()

INS_2019_long <- Data_ins_2019 %>% select(id,distrito, departamento, provincia, rango_edad, test_number, tamizaje_reactivo, year)

##2020###

Data_ins_unida <- read_excel("Data_ins_unida.xlsx",sheet = "2020")

Data_ins_2020 <- Data_ins_unida %>% 
  mutate(id = group_indices(Data_ins_unida, .dots=c("DISTRITO", "PROVINCIA", "DEPARTAMENTO"))) 
glimpse(Data_ins_2020)

#Sumando el total de pruebas rápidas y pruebas  RPR
Data_ins_2020 <- Data_ins_2020 %>% mutate(TEST_NUMBER = `1_TAMIZAJE_I` + `1_TAMIZAJE_II` + `1_TAMIZAJE_III` + `2_TAMIZAJE_II` + `2_TAMIZAJE_III`,
                                          TAMIZAJE_REACTIVO = `1_TAMIZAJE_REACTIVO` + `2_TAMIZAJE_REACTIVO`,
                                          RANGO_EDAD = case_when((GRUPO_EDAD==0) ~ "<12 años",
                                                                 (GRUPO_EDAD==1 | GRUPO_EDAD==2)~ "12-17 años",
                                                                 (GRUPO_EDAD==3 | GRUPO_EDAD==4)~ "18-29 años",
                                                                 (GRUPO_EDAD==5)~ "30-59 años"),
                                          YEAR="2020") %>% clean_names()

INS_2020_long <- Data_ins_2020 %>% select(id,distrito, departamento, provincia, rango_edad, test_number, tamizaje_reactivo, year)

##2021###

Data_ins_unida <- read_excel("Data_ins_unida.xlsx",sheet = "2021")

Data_ins_2021 <- Data_ins_unida %>% 
  mutate(id = group_indices(Data_ins_unida, .dots=c("DISTRITO", "PROVINCIA", "DEPARTAMENTO"))) 
glimpse(Data_ins_2021)

#Sumando el total de pruebas rápidas y pruebas  RPR
Data_ins_2021 <- Data_ins_2021 %>% mutate(TEST_NUMBER = `1_TAMIZAJE_I` + `1_TAMIZAJE_II` + `1_TAMIZAJE_III` + `2_TAMIZAJE_II` + `2_TAMIZAJE_III`,
                                          TAMIZAJE_REACTIVO = `1_TAMIZAJE_REACTIVO` + `2_TAMIZAJE_REACTIVO`,
                                          RANGO_EDAD = case_when((GRUPO_EDAD==0) ~ "<12 años",
                                                                 (GRUPO_EDAD==1 | GRUPO_EDAD==2)~ "12-17 años",
                                                                 (GRUPO_EDAD==3 | GRUPO_EDAD==4)~ "18-29 años",
                                                                 (GRUPO_EDAD==5)~ "30-59 años"),
                                          YEAR="2021") %>% clean_names()

INS_2021_long <- Data_ins_2021 %>% select(id,distrito, departamento, provincia, rango_edad, test_number, tamizaje_reactivo, year)

##2022###

Data_ins_unida <- read_excel("Data_ins_unida.xlsx",sheet = "2022")

Data_ins_2022 <- Data_ins_unida %>% 
  mutate(id = group_indices(Data_ins_unida, .dots=c("DISTRITO", "PROVINCIA", "DEPARTAMENTO"))) 
glimpse(Data_ins_2022)

#Sumando el total de pruebas rápidas y pruebas  RPR
Data_ins_2022 <- Data_ins_2022 %>% mutate(TEST_NUMBER = `1_TAMIZAJE` + `2_TAMIZAJE`,
                                          TAMIZAJE_REACTIVO = `1_TAMIZAJE_REACTIVO` + `2_TAMIZAJE_REACTIVO`,
                                          RANGO_EDAD = case_when((GRUPO_EDAD==0) ~ "<12 años",
                                                                 (GRUPO_EDAD==1 | GRUPO_EDAD==2)~ "12-17 años",
                                                                 (GRUPO_EDAD==3 | GRUPO_EDAD==4)~ "18-29 años",
                                                                 (GRUPO_EDAD==5)~ "30-59 años"),
                                          YEAR="2022") %>% clean_names()

INS_2022_long <- Data_ins_2022 %>% select(id,distrito, departamento, provincia, rango_edad, test_number, tamizaje_reactivo, year)


#JOINT#
INS_final_long<- rbind(INS_2011_long, INS_2012_long,INS_2013_long, INS_2014_long, INS_2015_long, INS_2016_long,INS_2017_long,INS_2018_long,INS_2019_long,INS_2020_long,INS_2021_long,INS_2022_long)

INS_final_long <- INS_final_long %>%
  group_by(departamento, provincia, distrito,year, rango_edad) %>%
  summarize(
    test_number = sum(test_number),
    tamizaje_reactivo = sum(tamizaje_reactivo)
  )

##Prueba##

INS_final_long <- INS_final_long %>% select(
                                      distrito, departamento, provincia, rango_edad, test_number, tamizaje_reactivo, year
                                    )
INS_final_wide <- INS_final_long %>% pivot_wider(names_from = rango_edad, values_from = c (test_number, tamizaje_reactivo))

write.csv(INS_final_long,"./data_final/ins_final_long.csv", row.names = F)
write.csv(INS_final_wide,"./data_final/ins_final_wide.csv", row.names = F)
