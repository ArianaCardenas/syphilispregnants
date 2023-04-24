#Data INS
#Recodificación: 
# 10a-11a:0, 12a-14a:1, 15a-17a:2, 18a-19a:3, 20a-29a:4, 30a-59a:5
library(tidyverse)
library(readr)
#install.packages("reshape2")
library(reshape2)

##2011###

Data_ins_unida <- read_excel("Data_ins_unida.xlsx",sheet = "2011")

Data_ins_2011 <- transform(Data_ins_unida, id = as.numeric(factor(DISTRITO)))
glimpse(Data_ins_2011)

#Sumando el total de pruebas rápidas y pruebas  RPR
Data_ins_2011 <- Data_ins_2011 %>% mutate(RAPID_TEST_NUMBER = P_RAPIDA._I + P_RAPIDA._II + P_RAPIDA._III,
                                          RPR_NUMBER = P_RPR_LESS24SS + P_RPR_MORE24SS,
                                          YEAR="2011") 
                       
INS_2011_long <- Data_ins_2011 %>% select(id, DISTRITO, GRUPO_EDAD, RAPID_TEST_NUMBER, P_RAPIDA._REACTIVO,  RPR_NUMBER, P_RPR_REACTIVO, YEAR)

##2012###

Data_ins_unida <- read_excel("Data_ins_unida.xlsx",sheet = "2012")

Data_ins_2012 <- transform(Data_ins_unida, id = as.numeric(factor(DISTRITO)))
glimpse(Data_ins_2012)

#Sumando el total de pruebas rápidas y pruebas  RPR
Data_ins_2012 <- Data_ins_2012 %>% mutate(RAPID_TEST_NUMBER = P_RAPIDA._I + P_RAPIDA._II + P_RAPIDA._III,
                                          RPR_NUMBER = P_RPR_LESS24SS + P_RPR_MORE24SS,
                                          YEAR="2012") 

INS_2012_long <- Data_ins_2012 %>% select(id, DISTRITO, GRUPO_EDAD, RAPID_TEST_NUMBER, P_RAPIDA._REACTIVO,  RPR_NUMBER, P_RPR_REACTIVO, YEAR)

#JOINT#
Data_unida<- rbind(INS_2011_long, INS_2012_long)

