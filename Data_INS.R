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

Data_ins_2011 <- transform(Data_ins_unida, id = as.numeric(factor(DISTRITO)))
glimpse(Data_ins_2011)

#Sumando el total de pruebas rápidas y pruebas  RPR
Data_ins_2011 <- Data_ins_2011 %>% mutate(RAPID_TEST_NUMBER = P_RAPIDA._I + P_RAPIDA._II + P_RAPIDA._III,
                                          RPR_NUMBER = P_RPR_LESS24SS + P_RPR_MORE24SS,
                                          YEAR="2011") %>% clean_names()

                       
INS_2011_long <- Data_ins_2011 %>% select(id,distrito, grupo_edad, rapid_test_number, p_rapida_reactivo,  rpr_number, p_rpr_reactivo, year)

##2012###

Data_ins_unida <- read_excel("Data_ins_unida.xlsx",sheet = "2012")

Data_ins_2012 <- transform(Data_ins_unida, id = as.numeric(factor(DISTRITO)))
glimpse(Data_ins_2012)

#Sumando el total de pruebas rápidas y pruebas  RPR
Data_ins_2012 <- Data_ins_2012 %>% mutate(RAPID_TEST_NUMBER = P_RAPIDA._I + P_RAPIDA._II + P_RAPIDA._III,
                                          RPR_NUMBER = P_RPR_LESS24SS + P_RPR_MORE24SS,
                                          YEAR="2012") %>% clean_names()

INS_2012_long <- Data_ins_2012 %>% select(id,distrito, grupo_edad, rapid_test_number, p_rapida_reactivo,  rpr_number, p_rpr_reactivo, year)

##2013###

Data_ins_unida <- read_excel("Data_ins_unida.xlsx",sheet = "2013")

Data_ins_2013 <- transform(Data_ins_unida, id = as.numeric(factor(DISTRITO)))
glimpse(Data_ins_2013)

#Sumando el total de pruebas rápidas y pruebas  RPR
Data_ins_2013 <- Data_ins_2013 %>% mutate(RAPID_TEST_NUMBER = P_RAPIDA._I + P_RAPIDA._II + P_RAPIDA._III,
                                          RPR_NUMBER = P_RPR_LESS24SS + P_RPR_MORE24SS,
                                          YEAR="2013") %>% clean_names()

INS_2013_long <- Data_ins_2013 %>% select(id,distrito, grupo_edad, rapid_test_number, p_rapida_reactivo,  rpr_number, p_rpr_reactivo, year)

##2014###

Data_ins_unida <- read_excel("Data_ins_unida.xlsx",sheet = "2014")

Data_ins_2014 <- transform(Data_ins_unida, id = as.numeric(factor(DISTRITO)))
glimpse(Data_ins_2014)

#Sumando el total de pruebas rápidas y pruebas  RPR
Data_ins_2014 <- Data_ins_2014 %>% mutate(RAPID_TEST_NUMBER = P_RAPIDA._I + P_RAPIDA._II + P_RAPIDA._III,
                                          RPR_NUMBER = P_RPR_LESS24SS + P_RPR_MORE24SS,
                                          YEAR="2014") %>% clean_names()

INS_2014_long <- Data_ins_2014 %>% select(id,distrito, grupo_edad, rapid_test_number, p_rapida_reactivo,  rpr_number, p_rpr_reactivo, year)

##2015###

Data_ins_unida <- read_excel("Data_ins_unida.xlsx",sheet = "2015")

Data_ins_2015 <- transform(Data_ins_unida, id = as.numeric(factor(DISTRITO)))
glimpse(Data_ins_2015)

#Sumando el total de pruebas rápidas y pruebas  RPR
Data_ins_2015 <- Data_ins_2015 %>% mutate(RAPID_TEST_NUMBER = P_RAPIDA._I + P_RAPIDA._II + P_RAPIDA._III,
                                          RPR_NUMBER = P_RPR_LESS24SS + P_RPR_MORE24SS,
                                          YEAR="2015") %>% clean_names()

INS_2015_long <- Data_ins_2015 %>% select(id,distrito, grupo_edad, rapid_test_number, p_rapida_reactivo,  rpr_number, p_rpr_reactivo, year)

##2016###

Data_ins_unida <- read_excel("Data_ins_unida.xlsx",sheet = "2016")

Data_ins_2016 <- transform(Data_ins_unida, id = as.numeric(factor(DISTRITO)))
glimpse(Data_ins_2016)

#Sumando el total de pruebas rápidas y pruebas  RPR
Data_ins_2016 <- Data_ins_2016 %>% mutate(RAPID_TEST_NUMBER = P_RAPIDA._I + P_RAPIDA._II + P_RAPIDA._III,
                                          RPR_NUMBER = P_RPR_LESS24SS + P_RPR_MORE24SS,
                                          YEAR="2016") %>% clean_names()

INS_2016_long <- Data_ins_2016 %>% select(id,distrito, grupo_edad, rapid_test_number, p_rapida_reactivo,  rpr_number, p_rpr_reactivo, year)

#JOINT#
Data_unida<- rbind(INS_2011_long, INS_2012_long,INS_2013_long, INS_2014_long, INS_2015_long, INS_2016_long)


