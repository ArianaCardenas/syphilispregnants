#title: "Gestantes_Sifilis"

library(tidyverse)
library(haven)
library(gtsummary)
library(survey)
library(stringr)
library(dplyr)
library(janitor)
library(purrr)

## Preparacion de datos

# Bases por anio
# He unido los archivos de cada modulo para tener una sola base por anio. Algunas encuestas son dirigidas a la mujer en edad fertil/madre, otras nivel hogar. Como veran, solo la base de programas sociales es a nivel hogar (de las que he usado), por eso es la unica que el join es por HHID. Las demas, son dirigidas a la madre/MEF. 
getwd()

##Gestantes 2021

salud <- read_sav("./Data_endes/2021/CSALUD01.sav") %>% unite("CASEID", HHID, QSNUMERO, sep = "  ",remove = FALSE) #HHID

salud <- salud %>% select(ID1,
                          HHID,
                          CASEID,
                          QS25BB,
                          QS29A,
                          QS601A,
                          QS603) %>%
  clean_names()
progsociales <- read_sav("./Data_endes/2021/programassociales.sav") #No identificador individual
progsociales <- progsociales %>% select(ID1,
                          HHID,
                          QH95,
                          QH106) %>%
  clean_names()
gestacion1 <- read_sav("./Data_endes/2021/RE223132.sav")
gestacion1 <- gestacion1 %>% select(ID1, CASEID, V201, V206, V207, V208, V213
                                    ,V218, V219, V225, V228, V233, V238, V302, V313) %>%
  clean_names()
pareja <- read_sav("./Data_endes/2021/RE516171.sav")
pareja <- pareja %>% select(ID1,
                            CASEID,
                            V531,
                            V532,
                            V701,
                            V730) %>%
  clean_names()
sexualidad <- read_sav("./Data_endes/2021/RE758081.sav")
sexualidad <- sexualidad %>% select(ID1, CASEID, V750, V761, V761B, V763A,
                            V763B, V763C, V766B, V768A, V769, V770, V785,
                            V830, V835A, V836) %>%
  clean_names()
hogar<- read_sav("./Data_endes/2021/REC0111.sav")
hogar <- hogar %>% select(ID1, V012, V131, CASEID, V002, V003, V007, V040, V025, V024,
                          V101, V102, V103, V136, V150, AWFACTW, HHID, UBIGEO,
                          V001, V005, V022, V190) %>%
  clean_names()
nino <- read_sav("./Data_endes/2021/REC21.sav")
nino <- nino %>% group_by(CASEID) %>% filter(BORD == max(BORD)) %>%
  select(ID1, CASEID, BIDX, B0, BD, B1, B2, B4, B11, B16, Q220A) %>%
  clean_names()
prenatal <- read_sav("./Data_endes/2021/REC41.sav") %>% rename(IDX94 = MIDX) %>%
  clean_names()
salud2 <- read_sav("./Data_endes/2021/REC42.sav")
salud2 <- salud2 %>% select(ID1, CASEID, V437, V438, V445, V453, V454, V456,
                            V457, V463A, V464, V481)%>%
  clean_names()
violence <- read_sav("./Data_endes/2021/REC84DV.sav")
violence <- violence %>% select(ID1, CASEID, D104, D106, D107, D108, D115Y,
                                D116, D118A, D118Y, D119Y) %>%
  clean_names()
ITS <- read_sav("./Data_endes/2021/REC91.sav")
ITS <- ITS %>% select(ID1, CASEID, S108N, SREGION, SPROVIN, SDISTRI, S119D, 
                      S815AA, S815AB, S815AC, S815AD, S815AE, S815AX, S815AZ,
                      S816AA, S816AB, S816AC, S816AD, S816AE, S816AF, S816AG,
                      S816AH, S816AI, S816AJ, S816AK, S816AL, S816AW) %>%
  clean_names() 
prenatal2 <- read_sav("./Data_endes/2021/REC94.sav")
prenatal2 <- prenatal2 %>% select(ID1, CASEID, IDX94, S410B, S411B, S411G, S411H, S411BA, 
                                S411CA, S411DA, S411EA, S426FA,QI411_M,
                                QI411F) %>%
  clean_names() #VD
sociodemo <- read_sav("./Data_endes/2021/REC0111.sav")
sociodemo <- sociodemo %>% select(ID1, CASEID, V190) %>%
  clean_names()
estadocivil <- read_sav("./Data_endes/2021/RECH1.sav") %>% unite("CASEID", HHID:HVIDX, sep = "  ",remove = FALSE) #HHID
estadocivil <- estadocivil %>% select (ID1, HV115, CASEID) %>%
  clean_names()
peso <- read_sav("./Data_endes/2021/RECH5.sav")%>% unite("CASEID", HHID, HA0, sep = "  ",remove = FALSE) #HHID
peso <- peso %>% select(ID1, HHID,CASEID, HA2, HA3, HA40) %>%
  clean_names()
hogar2 <- read_sav("./Data_endes/2021/RECH0.sav")%>% unite("CASEID", HHID, HV003, sep = "  ",remove = FALSE) #HHID

hogar2 <- hogar2 %>% select(ID1, HHID,CASEID, HV001, HV007, UBIGEO, longitudx, 
                            latitudy, HV022, HV005) %>%
  clean_names()

#Se une SALUD y PROGSOCIALES por HHID
dfprogsociales <- salud %>% left_join(progsociales, by = "hhid")

df_gestantes2021<-
  prenatal2 %>% 
  left_join(dfprogsociales, by = "caseid") %>% 
  left_join(gestacion1, by = "caseid") %>% 
  left_join(pareja, by = "caseid") %>%
  left_join(sexualidad, by = "caseid") %>% 
  left_join(hogar, by = "caseid") %>%
  left_join(salud2, by = "caseid") %>% 
  left_join(prenatal, by = "caseid") %>%
  left_join(violence, by = "caseid") %>% 
  left_join(ITS, by = "caseid") %>%
  left_join(sociodemo, by = "caseid") %>%
  left_join(estadocivil, by = "caseid") %>%
  left_join(peso, by = "caseid") %>%
  left_join(hogar2, by = "caseid") %>%
  inner_join(nino, by = "caseid") %>%
  mutate(year = "2021",
         v131 = case_when((v131==3 | v131==4 | v131==5 | v131==6 | v131==7 | v131==8 | v131==9) ~ 4,
                          (v131==2) ~ 3,
                          (v131==1) ~ 2,
                          (v131==10) ~ 1,
                          (v131==11 | v131==12) ~ 5))

table(df_gestantes2021$s411g, exclude = NULL)


## Gestantes 2020

salud <- read_sav("./Data_endes/2020/CSALUD01.sav") %>% unite("CASEID", HHID, QSNUMERO, sep = "  ",remove = FALSE) #HHID
salud <- salud %>% select(ID1,
                          HHID,
                          CASEID,
                          QS25BB,
                          QS29A,
                          QS601A,
                          QS603) %>%
  clean_names()
progsociales <- read_sav("./Data_endes/2020/programassociales.sav") #No identificador individual
progsociales <- progsociales %>% select(ID1,
                                        HHID,
                                        QH95,
                                        QH106) %>%
  clean_names()
gestacion1 <- read_sav("./Data_endes/2020/RE223132.sav")
gestacion1 <- gestacion1 %>% select(ID1, CASEID, V201, V206, V207, V208, V213
                                    ,V218, V219, V225, V228, V233, V238, V302, V313) %>%
  clean_names()
pareja <- read_sav("./Data_endes/2020/RE516171.sav")
pareja <- pareja %>% select(ID1,
                            CASEID,
                            V531,
                            V532,
                            V701,
                            V730) %>%
  clean_names()
sexualidad <- read_sav("./Data_endes/2020/RE758081.sav")
sexualidad <- sexualidad %>% select(ID1, CASEID, V750, V761, V761B, V763A,
                                    V763B, V763C, V766B, V768A, V769, V770, V785,
                                    V830, V835A, V836) %>%
  clean_names()
hogar<- read_sav("./Data_endes/2020/REC0111.sav")
hogar <- hogar %>% select(ID1, V012, V131, CASEID, V002, V003, V007, V040, V025, V024,
                          V101, V102, V103, V136, V150, AWFACTW, HHID, UBIGEO,
                          V001, V005, V022, V190) %>%
  clean_names()
nino <- read_sav("./Data_endes/2020/REC21.sav")
nino <- nino %>% group_by(CASEID) %>% filter(BORD == max(BORD)) %>% 
  select(ID1, CASEID, BIDX, B0, BD, B1, B2, B4, B11, B16, Q220A)%>%
  clean_names()
prenatal <- read_sav("./Data_endes/2020/REC41.sav") %>% rename(IDX94 = MIDX) %>%
  clean_names()
salud2 <- read_sav("./Data_endes/2020/REC42.sav")
salud2 <- salud2 %>% select(ID1, CASEID, V437, V438, V445, V453, V454, V456,
                            V457, V463A, V464, V481)%>%
  clean_names()
violence <- read_sav("./Data_endes/2020/REC84DV.sav")
violence <- violence %>% select(ID1, CASEID, D104, D106, D107, D108, D115Y,
                                D116, D118A, D118Y, D119Y) %>%
  clean_names()
ITS <- read_sav("./Data_endes/2020/REC91.sav")
ITS <- ITS %>% select(ID1, CASEID, S108N, SREGION, SPROVIN, SDISTRI, S119D, 
                      S815AA, S815AB, S815AC, S815AD, S815AE, S815AX, S815AZ,
                      S816AA, S816AB, S816AC, S816AD, S816AE, S816AF, S816AG,
                      S816AH, S816AI, S816AJ, S816AK, S816AL, S816AW) %>%
  clean_names() 
prenatal2 <- read_sav("./Data_endes/2020/REC94.sav")
prenatal2 <- prenatal2 %>% select(ID1, CASEID, IDX94, S410B, S411B, S411G, S411H, S411BA, 
                                  S411CA, S411DA, S411EA, S426FA,QI411_M,
                                  QI411F) %>%
  clean_names() #VD
sociodemo <- read_sav("./Data_endes/2020/REC0111.sav")
sociodemo <- sociodemo %>% select(ID1, CASEID, V190) %>%
  clean_names()
estadocivil <- read_sav("./Data_endes/2020/RECH1.sav") %>% unite("CASEID", HHID:HVIDX, sep = "  ",remove = FALSE) #HHID
estadocivil <- estadocivil %>% select (ID1, HV115, CASEID) %>%
  clean_names()
peso <- read_sav("./Data_endes/2020/RECH5.sav")%>% unite("CASEID", HHID, HA0, sep = "  ",remove = FALSE) #HHID
peso <- peso %>% select(ID1, HHID,CASEID, HA2, HA3, HA40) %>%
  clean_names()
hogar2 <- read_sav("./Data_endes/2020/RECH0.sav")%>% unite("CASEID", HHID, HV003, sep = "  ",remove = FALSE) #HHID
hogar2 <- hogar2 %>% select(ID1, HHID,CASEID, HV001, HV007, UBIGEO, longitudx, 
                            latitudy, HV022, HV005) %>%
  clean_names()

#Se une SALUD y PROGSOCIALES por HHID
dfprogsociales <- salud %>% left_join(progsociales, by = "hhid")

df_gestantes2020<-
  prenatal2 %>% 
  left_join(dfprogsociales, by = "caseid") %>% 
  left_join(gestacion1, by = "caseid") %>% 
  left_join(pareja, by = "caseid") %>%
  left_join(sexualidad, by = "caseid") %>% 
  left_join(hogar, by = "caseid") %>%
  inner_join(nino, by = "caseid") %>%
  left_join(salud2, by = "caseid") %>% 
  left_join(prenatal, by = "caseid") %>%
  left_join(violence, by = "caseid") %>% 
  left_join(ITS, by = "caseid") %>%
  left_join(sociodemo, by = "caseid") %>%
  left_join(estadocivil, by = "caseid") %>%
  left_join(peso, by = "caseid") %>%
  left_join(hogar2, by = "caseid") %>%
  mutate(year = "2020",
         v131 = case_when((v131==3 | v131==4 | v131==5 | v131==6 | v131==7 | v131==8 | v131==9) ~ 4,
                          (v131==2) ~ 3,
                          (v131==1) ~ 2,
                          (v131==10) ~ 1,
                          (v131==11 | v131==12) ~ 5))

table(df_gestantes2020$s411g, exclude = NULL)
dim(df_gestantes2020)  


## Gestantes 2019
salud <- read_sav("./Data_endes/2019/CSALUD01.sav") %>% unite("CASEID", HHID, QSNUMERO, sep = "  ",remove = FALSE) #HHID
salud <- salud %>% select(ID1,
                          HHID,
                          CASEID,
                          QS25BB,
                          QS29A,
                          QS601A,
                          QS603) %>%
  clean_names()
progsociales <- read_sav("./Data_endes/2019/programassociales.sav") #No identificador individual
progsociales <- progsociales %>% select(ID1,
                                        HHID,
                                        QH95,
                                        QH106) %>%
  clean_names()
gestacion1 <- read_sav("./Data_endes/2019/RE223132.sav")
gestacion1 <- gestacion1 %>% select(ID1, CASEID, V201, V206, V207, V208, V213
                                    ,V218, V219, V225, V228, V233, V238, V302, V313) %>%
  clean_names()
pareja <- read_sav("./Data_endes/2019/RE516171.sav")
pareja <- pareja %>% select(ID1,
                            CASEID,
                            V531,
                            V532,
                            V701,
                            V730) %>%
  clean_names()
sexualidad <- read_sav("./Data_endes/2019/RE758081.sav")
sexualidad <- sexualidad %>% select(ID1, CASEID, V750, V761, V761B, V763A,
                                    V763B, V763C, V766B, V768A, V769, V770, V785,
                                    V830, V835A, V836) %>%
  clean_names()
hogar<- read_sav("./Data_endes/2019/REC0111.sav")
hogar <- hogar %>% select(ID1, V012, V131, CASEID, V002, V003, V007, V040, V025, V024,
                          V101, V102, V103, V136, V150, AWFACTW, HHID, UBIGEO,
                          V001, V005, V022, V190) %>%
  clean_names()
nino <- read_sav("./Data_endes/2019/REC21.sav")
nino <- nino  %>% group_by(CASEID) %>% filter(BORD == max(BORD)) %>% 
  select(ID1, CASEID, BIDX, B0, BD, B1, B2, B4, B11, B16, Q220A)%>%
  clean_names()
prenatal <- read_sav("./Data_endes/2019/REC41.sav") %>% rename(IDX94 = MIDX) %>%
  clean_names()
salud2 <- read_sav("./Data_endes/2019/REC42.sav")
salud2 <- salud2 %>% select(ID1, CASEID, V437, V438, V445, V453, V454, V456,
                            V457, V463A, V464, V481)%>%
  clean_names()
violence <- read_sav("./Data_endes/2019/REC84DV.sav")
violence <- violence %>% select(ID1, CASEID, D104, D106, D107, D108, D115Y,
                                D116, D118A, D118Y, D119Y) %>%
  clean_names()
ITS <- read_sav("./Data_endes/2019/REC91.sav")
ITS <- ITS %>% select(ID1, CASEID, S108N, SREGION, SPROVIN, SDISTRI, S119D, 
                      S815AA, S815AB, S815AC, S815AD, S815AE, S815AX, S815AZ,
                      S816AA, S816AB, S816AC, S816AD, S816AE, S816AF, S816AG,
                      S816AH, S816AI, S816AJ, S816AK, S816AL, S816AW) %>%
  clean_names() 
prenatal2 <- read_sav("./Data_endes/2019/REC94.sav")
prenatal2 <- prenatal2 %>% select(ID1, CASEID, IDX94, S410B, S411B, S411G, S411H, S411BA, 
                                  S411CA, S411DA, S411EA, S426FA,QI411_M,
                                  QI411F) %>%
  clean_names() #VD
sociodemo <- read_sav("./Data_endes/2019/REC0111.sav")
sociodemo <- sociodemo %>% select(ID1, CASEID, V190) %>%
  clean_names()
estadocivil <- read_sav("./Data_endes/2019/RECH1.sav") %>% unite("CASEID", HHID:HVIDX, sep = "  ",remove = FALSE) #HHID
estadocivil <- estadocivil %>% select (ID1, HV115, CASEID) %>%
  clean_names()
peso <- read_sav("./Data_endes/2019/RECH5.sav")%>% unite("CASEID", HHID, HA0, sep = "  ",remove = FALSE) #HHID
peso <- peso %>% select(ID1, HHID,CASEID, HA2, HA3, HA40) %>%
  clean_names()
hogar2 <- read_sav("./Data_endes/2019/RECH0.sav")%>% unite("CASEID", HHID, HV003, sep = "  ",remove = FALSE) #HHID
hogar2 <- hogar2 %>% select(ID1, HHID,CASEID, HV001, HV007, UBIGEO, longitudx, 
                            latitudy, HV022, HV005) %>%
  clean_names()

#Se une SALUD y PROGSOCIALES por HHID
dfprogsociales <- salud %>% left_join(progsociales, by = "hhid")

df_gestantes2019<-
  prenatal2 %>% 
  left_join(dfprogsociales, by = "caseid") %>% 
  left_join(gestacion1, by = "caseid") %>% 
  left_join(pareja, by = "caseid") %>%
  left_join(sexualidad, by = "caseid") %>% 
  left_join(hogar, by = "caseid") %>%
  inner_join(nino, by = "caseid") %>%
  left_join(salud2, by = "caseid") %>% 
  left_join(prenatal, by = "caseid") %>%
  left_join(violence, by = "caseid") %>% 
  left_join(ITS, by = "caseid") %>%
  left_join(sociodemo, by = "caseid") %>%
  left_join(estadocivil, by = "caseid") %>%
  left_join(peso, by = "caseid") %>%
  left_join(hogar2, by = "caseid") %>%
  mutate(year = "2019",
         v131 = case_when((v131==3 | v131==4 | v131==5 | v131==6 | v131==7 | v131==8 | v131==9) ~ 4,
                          (v131==2) ~ 3,
                          (v131==1) ~ 2,
                          (v131==10) ~ 1,
                          (v131==11 | v131==12) ~ 5))

table(df_gestantes2019$s411g, exclude = NULL)
dim(df_gestantes2019) 

## Gestantes 2018
salud <- read_sav("./Data_endes/2018/CSALUD01.sav") %>% unite("CASEID", HHID, QSNUMERO, sep = "  ",remove = FALSE) #HHID
salud <- salud %>% select(HHID,
                          CASEID,
                          QS25BB,
                          QS29A,
                          QS601A,
                          QS603) %>%
  clean_names()
progsociales <- read_sav("./Data_endes/2018/programassociales.sav") #No identificador individual
progsociales <- progsociales %>% select(HHID,
                                        QH95,
                                        QH106) %>%
  clean_names()
gestacion1 <- read_sav("./Data_endes/2018/RE223132.sav")
gestacion1 <- gestacion1 %>% select(CASEID, V201, V206, V207, V208, V213
                                    ,V218, V219, V225, V228, V233, V238, V302, V313) %>%
  clean_names()
pareja <- read_sav("./Data_endes/2018/RE516171.sav")
pareja <- pareja %>% select(CASEID,
                            V531,
                            V532,
                            V701,
                            V730) %>%
  clean_names()
sexualidad <- read_sav("./Data_endes/2018/RE758081.sav")
sexualidad <- sexualidad %>% select(CASEID, V750, V761, V761B, V763A,
                                    V763B, V763C, V766B, V768A, V769, V770, V785,
                                    V830, V835A, V836) %>%
  clean_names()
hogar<- read_sav("./Data_endes/2018/REC0111.sav")
hogar <- hogar %>% select(V012, V131, CASEID, V002, V003, V040, V025, V024,
                          V101, V102, V103, V136, V150, AWFACTW, HHID,
                          V001, V005, V022, V190) %>%
  clean_names()
nino <- read_sav("./Data_endes/2018/REC21.sav")
nino <- nino  %>% group_by(CASEID) %>% filter(BORD == max(BORD)) %>% 
  select(CASEID, BIDX, B0, BD, B1, B2, B4, B11, B16, Q220A)%>%
  clean_names()
prenatal <- read_sav("./Data_endes/2018/REC41.sav") %>% rename(IDX94 = MIDX) %>%
  clean_names()
salud2 <- read_sav("./Data_endes/2018/REC42.sav")
salud2 <- salud2 %>% select(CASEID, V437, V438, V445, V453, V454, V456,
                            V457, V463A, V464, V481)%>%
  clean_names()
violence <- read_sav("./Data_endes/2018/REC84DV.sav")
violence <- violence %>% select(CASEID, D104, D106, D107, D108, D115Y,
                                D116, D118A, D118Y, D119Y) %>%
  clean_names()
ITS <- read_sav("./Data_endes/2018/REC91.sav")
ITS <- ITS %>% select(CASEID, S108N, SREGION, SPROVIN, SDISTRI, S119D, 
                      S815AA, S815AB, S815AC, S815AD, S815AE, S815AX, S815AZ,
                      S816AA, S816AB, S816AC, S816AD, S816AE, S816AF, S816AG,
                      S816AH, S816AI, S816AJ, S816AK, S816AL, S816AW) %>%
  clean_names() 
prenatal2 <- read_sav("./Data_endes/2018/REC94.sav")
prenatal2 <- prenatal2 %>% select(CASEID, IDX94, S410B, S411B, S411G, S411H, S411BA, 
                                  S411CA, S411DA, S411EA, S426FA,QI411_M,
                                  QI411F) %>%
  clean_names() #VD
sociodemo <- read_sav("./Data_endes/2018/REC0111.sav")
sociodemo <- sociodemo %>% select(CASEID, V190) %>%
  clean_names()
estadocivil <- read_sav("./Data_endes/2018/RECH1.sav") %>% unite("CASEID", HHID:HVIDX, sep = "  ",remove = FALSE) #HHID
estadocivil <- estadocivil %>% select (HV115, CASEID) %>%
  clean_names()
peso <- read_sav("./Data_endes/2018/RECH5.sav")%>% unite("CASEID", HHID, HA0, sep = "  ",remove = FALSE) #HHID
peso <- peso %>% select(HHID,CASEID, HA2, HA3, HA40) %>%
  clean_names()
hogar2 <- read_sav("./Data_endes/2018/RECH0.sav")%>% unite("CASEID", HHID, HV003, sep = "  ",remove = FALSE) #HHID
hogar2 <- hogar2 %>% select(HHID,CASEID, HV001, longitudx, 
                            latitudy, HV022, HV005) %>%
  clean_names()

#Se une SALUD y PROGSOCIALES por HHID
dfprogsociales <- salud %>% left_join(progsociales, by = "hhid")

df_gestantes2018<-
  prenatal2 %>% 
  left_join(dfprogsociales, by = "caseid") %>% 
  left_join(gestacion1, by = "caseid") %>% 
  left_join(pareja, by = "caseid") %>%
  left_join(sexualidad, by = "caseid") %>% 
  left_join(hogar, by = "caseid") %>%
  inner_join(nino, by = "caseid") %>%
  left_join(salud2, by = "caseid") %>% 
  left_join(prenatal, by = "caseid") %>%
  left_join(violence, by = "caseid") %>% 
  left_join(ITS, by = "caseid") %>%
  left_join(sociodemo, by = "caseid") %>%
  left_join(estadocivil, by = "caseid") %>%
  left_join(peso, by = "caseid") %>%
  left_join(hogar2, by = "caseid") %>%
  mutate(year = "2018",
         v131 = case_when((v131==3 | v131==4 | v131==5 | v131==6 | v131==7 | v131==8 | v131==9) ~ 4,
                          (v131==2) ~ 3,
                          (v131==1) ~ 2,
                          (v131==10) ~ 1,
                          (v131==11 | v131==12) ~ 5))

table(df_gestantes2018$s411g, exclude = NULL)
dim(df_gestantes2018) 

## Gestantes 2017
salud <- read_sav("./Data_endes/2017/CSALUD01.sav") %>% unite("CASEID", HHID, QSNUMERO, sep = "  ",remove = FALSE) #HHID
salud <- salud %>% select(HHID,
                          CASEID,
                          QS25BB,
                          QS29A,
                          QS601A,
                          QS603) %>%
  clean_names()
progsociales <- read_sav("./Data_endes/2017/programassociales.sav") #No identificador individual
progsociales <- progsociales %>% select(HHID,
                                        QH95,
                                        QH106) %>%
  clean_names()
gestacion1 <- read_sav("./Data_endes/2017/RE223132.sav")
gestacion1 <- gestacion1 %>% select(CASEID, V201, V206, V207, V208, V213
                                    ,V218, V219, V225, V228, V233, V238, V302, V313) %>%
  clean_names()
pareja <- read_sav("./Data_endes/2017/RE516171.sav")
pareja <- pareja %>% select(CASEID,
                            V531,
                            V532,
                            V701,
                            V730) %>%
  clean_names()
sexualidad <- read_sav("./Data_endes/2017/RE758081.sav")
sexualidad <- sexualidad %>% select(CASEID, V750, V761, V761B, V763A,
                                    V763B, V763C, V766B, V768A, V769, V770, V785,
                                    V830, V835A, V836) %>%
  clean_names()
hogar<- read_sav("./Data_endes/2017/REC0111.sav")
hogar <- hogar %>% select(V012, V131, CASEID, V002, V003, V007, V040, V025, V024,
                          V101, V102, V103, V136, V150, AWFACTW, 
                          V001, V005, V022, V190) %>%
  clean_names()
nino <- read_sav("./Data_endes/2017/REC21.sav")
nino <- nino  %>% group_by(CASEID) %>% filter(BORD == max(BORD)) %>% 
  select(CASEID, BIDX, B0, BD, B1, B2, B4, B11, B16)%>%
  clean_names()
prenatal <- read_sav("./Data_endes/2017/REC41.sav") %>% rename(IDX94 = MIDX) %>%
  clean_names()
salud2 <- read_sav("./Data_endes/2017/REC42.sav")
salud2 <- salud2 %>% select(CASEID, V437, V438, V445, V453, V454, V456,
                            V457, V463A, V464, V481)%>%
  clean_names()
violence <- read_sav("./Data_endes/2017/REC84DV.sav")
violence <- violence %>% select(CASEID, D104, D106, D107, D108, D115Y,
                                D116, D118A, D118Y, D119Y) %>%
  clean_names()
ITS <- read_sav("./Data_endes/2017/REC91.sav")
ITS <- ITS %>% select(CASEID, S108N, SREGION, SPROVIN, SDISTRI, S119D, 
                      S815AA, S815AB, S815AC, S815AD, S815AE, S815AX, S815AZ,
                      S816AA, S816AB, S816AC, S816AD, S816AE, S816AF, S816AG,
                      S816AH, S816AI, S816AJ, S816AK, S816AL, S816AW) %>%
  clean_names() 
prenatal2 <- read_sav("./Data_endes/2017/REC94.sav")
prenatal2 <- prenatal2 %>% select(CASEID, IDX94, S410B, S411B, S411G, S411H, S411BA, 
                                  S411CA, S411DA, S411EA, S426FA) %>%
  clean_names() #VD
sociodemo <- read_sav("./Data_endes/2017/REC0111.sav")
sociodemo <- sociodemo %>% select(CASEID, V190) %>%
  clean_names()
estadocivil <- read_sav("./Data_endes/2017/RECH1.sav") %>% unite("CASEID", HHID:HVIDX, sep = "  ",remove = FALSE) #HHID
estadocivil <- estadocivil %>% select (HV115, CASEID) %>%
  clean_names()
peso <- read_sav("./Data_endes/2017/RECH5.sav")%>% unite("CASEID", HHID, HA0, sep = "  ",remove = FALSE) #HHID
peso <- peso %>% select(HHID,CASEID, HA2, HA3, HA40) %>%
  clean_names()
hogar2 <- read_sav("./Data_endes/2017/RECH0.sav")%>% unite("CASEID", HHID, HV003, sep = "  ",remove = FALSE) #HHID
hogar2 <- hogar2 %>% select(HHID,CASEID, HV001, HV007, HV022, HV005, long_ccpp, lat_ccpp) %>%
  clean_names() %>% rename(longitudx = long_ccpp, latitudy = lat_ccpp)

#Se une SALUD y PROGSOCIALES por HHID
dfprogsociales <- salud %>% left_join(progsociales, by = "hhid")

df_gestantes2017<-
  prenatal2 %>% 
  left_join(dfprogsociales, by = "caseid") %>% 
  left_join(gestacion1, by = "caseid") %>% 
  left_join(pareja, by = "caseid") %>%
  left_join(sexualidad, by = "caseid") %>% 
  left_join(hogar, by = "caseid") %>%
  inner_join(nino, by = "caseid") %>%
  left_join(salud2, by = "caseid") %>% 
  left_join(prenatal, by = "caseid") %>%
  left_join(violence, by = "caseid") %>% 
  left_join(ITS, by = "caseid") %>%
  left_join(sociodemo, by = "caseid") %>%
  left_join(estadocivil, by = "caseid") %>%
  left_join(peso, by = "caseid") %>%
  left_join(hogar2, by = "caseid") %>%
  mutate(year = "2017",
         v131 = case_when((v131==3 | v131==4 | v131==5 | v131==6 | v131==7 | v131==8 | v131==9) ~ 4,
                          (v131==2) ~ 3,
                          (v131==1) ~ 2,
                          (v131==10) ~ 1,
                          (v131==11 | v131==12) ~ 5))

table(df_gestantes2017$s411g, exclude = NULL)
dim(df_gestantes2017) 

## Gestantes 2016
salud <- read_sav("./Data_endes/2016/CSALUD01.sav") %>% unite("CASEID", HHID, QSNUMERO, sep = "  ",remove = FALSE) #HHID
salud <- salud %>% select(HHID,
                          CASEID,
                          QS29A,
                          QS601A,
                          QS603) %>%
  clean_names()
progsociales <- read_sav("./Data_endes/2016/programassociales.sav") #No identificador individual
progsociales <- progsociales %>% select(HHID,
                                        QH95,
                                        QH106) %>%
  clean_names()
gestacion1 <- read_sav("./Data_endes/2016/RE223132.sav")
gestacion1 <- gestacion1 %>% select(CASEID, V201, V206, V207, V208, V213
                                    ,V218, V219, V225, V228, V233, V238, V302, V313) %>%
  clean_names()
pareja <- read_sav("./Data_endes/2016/RE516171.sav")
pareja <- pareja %>% select(CASEID,
                            V531,
                            V532,
                            V701,
                            V730) %>%
  clean_names()
sexualidad <- read_sav("./Data_endes/2016/RE758081.sav")
sexualidad <- sexualidad %>% select(CASEID, V750, V761, V761B, V763A,
                                    V763B, V763C, V766B, V768A, V769, V770, V785,
                                    V830, V835A, V836) %>%
  clean_names()
hogar<- read_sav("./Data_endes/2016/REC0111.sav")
hogar <- hogar %>% select(V012, V131, CASEID, V002, V003, V007, V040, V025, V024,
                          V101, V102, V103, V136, V150, AWFACTW,
                          V001,  V022, V190) %>%
  clean_names()
nino <- read_sav("./Data_endes/2016/REC21.sav")
nino <- nino  %>% group_by(CASEID) %>% filter(BORD == max(BORD)) %>% 
  select(CASEID, BIDX, B0, BD, B1, B2, B4, B11, B16)%>%
  clean_names()
prenatal <- read_sav("./Data_endes/2016/REC41.sav") %>% rename(IDX94 = MIDX) %>%
  clean_names()
salud2 <- read_sav("./Data_endes/2016/REC42.sav")
salud2 <- salud2 %>% select(CASEID, V437, V438, V445, V453, V454, V456,
                            V457, V463A, V464, V481)%>%
  clean_names()
violence <- read_sav("./Data_endes/2016/REC84DV.sav")
violence <- violence %>% select(CASEID, D104, D106, D107, D108, D115Y,
                                D116, D118A, D118Y, D119Y) %>%
  clean_names()
ITS <- read_sav("./Data_endes/2016/REC91.sav")
ITS <- ITS %>% select(CASEID, S108N, SREGION, SPROVIN, SDISTRI, 
                      S815AA, S815AB, S815AC, S815AD, S815AE, S815AX, S815AZ,
                      S816AA, S816AB, S816AC, S816AD, S816AE, S816AF, S816AG,
                      S816AH, S816AI, S816AJ, S816AK, S816AL, S816AW) %>%
  clean_names() 
prenatal2 <- read_sav("./Data_endes/2016/REC94.sav")
prenatal2 <- prenatal2 %>% select(CASEID, IDX94, S410B, S411B, S411G, S411H, S411BA, 
                                  S411CA, S411DA, S411EA, S426FA) %>%
  clean_names() #VD
sociodemo <- read_sav("./Data_endes/2016/REC0111.sav")
sociodemo <- sociodemo %>% select(CASEID, V190) %>%
  clean_names()
estadocivil <- read_sav("./Data_endes/2016/RECH1.sav") %>% unite("CASEID", HHID:HVIDX, sep = "  ",remove = FALSE) #HHID
estadocivil <- estadocivil %>% select (HV115, CASEID) %>%
  clean_names()
peso <- read_sav("./Data_endes/2016/RECH5.sav")%>% unite("CASEID", HHID, HA0, sep = "  ",remove = FALSE) #HHID
peso <- peso %>% select(HHID,CASEID, HA2, HA3, HA40) %>%
  clean_names()
hogar2 <- read_sav("./Data_endes/2016/RECH0.sav")%>% unite("CASEID", HHID, HV003, sep = "  ",remove = FALSE) #HHID
hogar2 <- hogar2 %>% select(HHID,CASEID, HV001, HV007,  longitudx, 
                            latitudy) %>%
  clean_names()

#Se une SALUD y PROGSOCIALES por HHID
dfprogsociales <- salud %>% left_join(progsociales, by = "hhid")

df_gestantes2016<-
  prenatal2 %>% 
  left_join(dfprogsociales, by = "caseid") %>% 
  left_join(gestacion1, by = "caseid") %>% 
  left_join(pareja, by = "caseid") %>%
  left_join(sexualidad, by = "caseid") %>% 
  left_join(hogar, by = "caseid") %>%
  inner_join(nino, by = "caseid") %>%
  left_join(salud2, by = "caseid") %>% 
  left_join(prenatal, by = "caseid") %>%
  left_join(violence, by = "caseid") %>% 
  left_join(ITS, by = "caseid") %>%
  left_join(sociodemo, by = "caseid") %>%
  left_join(estadocivil, by = "caseid") %>%
  left_join(peso, by = "caseid") %>%
  left_join(hogar2, by = "caseid") %>%
  mutate(year = "2016")

table(df_gestantes2016$s411g, exclude = NULL)
dim(df_gestantes2016) 

## Gestantes 2015
salud <- read_sav("./Data_endes/2015/CSALUD01.sav") %>% unite("CASEID", HHID, QSNUMERO, sep = "  ",remove = FALSE) #HHID
salud <- salud %>% select(HHID,
                          CASEID,QS29A,
                          QS601A,
                          QS603) %>%
  clean_names()
progsociales <- read_sav("./Data_endes/2015/programassociales.sav") #No identificador individual
progsociales <- progsociales %>% select(HHID,
                                        QH95,
                                        QH106) %>%
  clean_names()
gestacion1 <- read_sav("./Data_endes/2015/RE223132.sav")
gestacion1 <- gestacion1 %>% select(CASEID, V201, V206, V207, V208, V213
                                    ,V218, V219, V225, V228, V233, V238, V302, V313) %>%
  clean_names()
pareja <- read_sav("./Data_endes/2015/RE516171.sav")
pareja <- pareja %>% select(CASEID,
                            V531,
                            V532,
                            V701,
                            V730) %>%
  clean_names()
sexualidad <- read_sav("./Data_endes/2015/RE758081.sav")
sexualidad <- sexualidad %>% select(CASEID, V750, V761, V761B, V763A,
                                    V763B, V763C, V766B, V768A, V769, V770, V785,
                                    V830, V835A, V836) %>%
  clean_names()
hogar<- read_sav("./Data_endes/2015/REC0111.sav")
hogar <- hogar %>% select(V012, V131, CASEID, V002, V003, V007, V040, V025, V024,
                          V101, V102, V103, V136, V150, AWFACTW,
                          V001, V005, V022, V190) %>%
  clean_names()
nino <- read_sav("./Data_endes/2015/REC21.sav")
nino <- nino  %>% group_by(CASEID) %>% filter(BORD == max(BORD)) %>% 
  select(CASEID, BIDX, B0, BD, B1, B2, B4, B11, B16)%>%
  clean_names()
prenatal <- read_sav("./Data_endes/2015/REC41.sav") %>% rename(IDX94 = MIDX) %>%
  clean_names()
salud2 <- read_sav("./Data_endes/2015/REC42.sav")
salud2 <- salud2 %>% select(CASEID, V437, V438, V445, V453, V454, V456,
                            V457, V463A, V464, V481)%>%
  clean_names()
violence <- read_sav("./Data_endes/2015/REC84DV.sav")
violence <- violence %>% select(CASEID, D104, D106, D107, D108, D115Y,
                                D116, D118A, D118Y, D119Y) %>%
  clean_names()
ITS <- read_sav("./Data_endes/2015/REC91.sav")
ITS <- ITS %>% select(CASEID, S108N, SREGION, SPROVIN, SDISTRI, 
                      S815AA, S815AB, S815AC, S815AD, S815AE, S815AX, S815AZ,
                      S816AA, S816AB, S816AC, S816AD, S816AE, S816AF, S816AG,
                      S816AH, S816AI, S816AJ, S816AK, S816AL, S816AW) %>%
  clean_names() 
prenatal2 <- read_sav("./Data_endes/2015/REC94.sav")
prenatal2 <- prenatal2 %>% select(CASEID, IDX94, S410B, S411B, S411G, S411H, S411BA, 
                                  S411CA, S411DA, S411EA, S426FA) %>%
  clean_names() #VD
sociodemo <- read_sav("./Data_endes/2015/REC0111.sav")
sociodemo <- sociodemo %>% select(CASEID, V190) %>%
  clean_names()
estadocivil <- read_sav("./Data_endes/2015/RECH1.sav") %>% unite("CASEID", HHID:HVIDX, sep = "  ",remove = FALSE) #HHID
estadocivil <- estadocivil %>% select (HV115, CASEID) %>%
  clean_names()
peso <- read_sav("./Data_endes/2015/RECH5.sav")%>% unite("CASEID", HHID, HA0, sep = "  ",remove = FALSE) #HHID
peso <- peso %>% select(HHID,CASEID, HA2, HA3, HA40) %>%
  clean_names()
hogar2 <- read_sav("./Data_endes/2015/RECH0.sav")%>% unite("CASEID", HHID, HV003, sep = "  ",remove = FALSE) #HHID
hogar2 <- hogar2 %>% select(HHID,CASEID, HV001, HV007, longitudx, 
                            latitudy, HV022, HV005) %>%
  clean_names()

#Se une SALUD y PROGSOCIALES por HHID
dfprogsociales <- salud %>% left_join(progsociales, by = "hhid")

df_gestantes2015<-
  prenatal2 %>% 
  left_join(dfprogsociales, by = "caseid") %>% 
  left_join(gestacion1, by = "caseid") %>% 
  left_join(pareja, by = "caseid") %>%
  left_join(sexualidad, by = "caseid") %>% 
  left_join(hogar, by = "caseid") %>%
  inner_join(nino, by = "caseid") %>%
  left_join(salud2, by = "caseid") %>% 
  left_join(prenatal, by = "caseid") %>%
  left_join(violence, by = "caseid") %>% 
  left_join(ITS, by = "caseid") %>%
  left_join(sociodemo, by = "caseid") %>%
  left_join(estadocivil, by = "caseid") %>%
  left_join(peso, by = "caseid") %>%
  left_join(hogar2, by = "caseid") %>%
  mutate(year = "2015")

table(df_gestantes2015$s411g, exclude = NULL)
dim(df_gestantes2015) 

## Gestantes 2014
salud <- read_sav("./Data_endes/2014/CSALUD01.sav") %>% unite("CASEID", HHID, QSNUMERO, sep = "  ",remove = FALSE) #HHID
salud <- salud %>% select(ID1,
                          HHID,
                          CASEID,
                          QS25BB,
                          QS29A,
                          QS601A,
                          QS603) %>%
  clean_names()
progsociales <- read_sav("./Data_endes/2014/programassociales.sav") #No identificador individual
progsociales <- progsociales %>% select(ID1,
                                        HHID,
                                        QH95,
                                        QH106) %>%
  clean_names()
gestacion1 <- read_sav("./Data_endes/2014/RE223132.sav")
gestacion1 <- gestacion1 %>% select(CASEID, V201, V206, V207, V208, V213
                                    ,V218, V219, V225, V228, V233, V238, V302, V313) %>%
  clean_names()
pareja <- read_sav("./Data_endes/2014/RE516171.sav")
pareja <- pareja %>% select(CASEID,
                            V531,
                            V532,
                            V701,
                            V730) %>%
  clean_names()
sexualidad <- read_sav("./Data_endes/2014/RE758081.sav")
sexualidad <- sexualidad %>% select(CASEID, V750, V761, V761B, V763A,
                                    V763B, V763C, V766B, V768A, V769, V770, V785,
                                    V830, V835A, V836) %>%
  clean_names()
hogar<- read_sav("./Data_endes/2014/REC0111.sav")
hogar <- hogar %>% select(V012, V131, CASEID, V002, V003, V007, V040, V025, V024,
                          V101, V102, V103, V136, V150, AWFACTW, V001, V022, V190) %>%
  clean_names()
nino <- read_sav("./Data_endes/2014/REC21.sav")
nino <- nino  %>% group_by(CASEID) %>% filter(BORD == max(BORD)) %>% 
  select(CASEID, BIDX, B0, B1, B2, B4, B11, B16)%>%
  clean_names()
prenatal <- read_sav("./Data_endes/2014/REC41.sav") %>% rename(IDX94 = MIDX) %>%
  clean_names()
salud2 <- read_sav("./Data_endes/2014/REC42.sav")
salud2 <- salud2 %>% select(CASEID, V437, V438, V445, V453, V454, V456,
                            V457, V463A, V464, V481)%>%
  clean_names()
violence <- read_sav("./Data_endes/2014/REC84DV.sav")
violence <- violence %>% select(CASEID, D104, D106, D107, D108, D115Y,
                                D116, D118A, D118Y, D119Y) %>%
  clean_names()
ITS <- read_sav("./Data_endes/2014/REC91.sav")
ITS <- ITS %>% select(CASEID, S108N, SPROVIN, SDISTRI,  
                      S815AA, S815AB, S815AC, S815AD, S815AE, S815AX, S815AZ,
                      S816AA, S816AB, S816AC, S816AD, S816AE, S816AF, S816AG,
                      S816AH, S816AI, S816AJ, S816AK, S816AL, S816AW) %>%
  clean_names() 
prenatal2 <- read_sav("./Data_endes/2014/REC94.sav")
prenatal2 <- prenatal2 %>% select(CASEID, IDX94, S410B, S411B, S411G, S411H, S411BA, 
                                  S411CA, S411DA, S411EA) %>%
  clean_names() #VD
sociodemo <- read_sav("./Data_endes/2014/REC0111.sav")
sociodemo <- sociodemo %>% select(CASEID, V190) %>%
  clean_names()
estadocivil <- read_sav("./Data_endes/2014/RECH1.sav") %>% unite("CASEID", HHID:HVIDX, sep = "  ",remove = FALSE) #HHID
estadocivil <- estadocivil %>% select (HV115, CASEID) %>%
  clean_names()
peso <- read_sav("./Data_endes/2014/RECH5.sav")%>% unite("CASEID", HHID, HA0, sep = "  ",remove = FALSE) #HHID
peso <- peso %>% select(HHID,CASEID, HA2, HA3, HA40) %>%
  clean_names()
hogar2 <- read_sav("./Data_endes/2014/RECH0.sav")%>% unite("CASEID", HHID, HV003, sep = "  ",remove = FALSE) #HHID
hogar2 <- hogar2 %>% select(ID1, HHID,CASEID, HV001, HV007, UBIGEO, longitudx, 
                            latitudy, HV022, HV005) %>%
  clean_names()

#Se une SALUD y PROGSOCIALES por HHID
dfprogsociales <- salud %>% left_join(progsociales, by = "hhid")

df_gestantes2014<-
  prenatal2 %>% 
  left_join(dfprogsociales, by = "caseid") %>% 
  left_join(gestacion1, by = "caseid") %>% 
  left_join(pareja, by = "caseid") %>%
  left_join(sexualidad, by = "caseid") %>% 
  left_join(hogar, by = "caseid") %>%
  inner_join(nino, by = "caseid") %>%
  left_join(salud2, by = "caseid") %>% 
  left_join(prenatal, by = "caseid") %>%
  left_join(violence, by = "caseid") %>% 
  left_join(ITS, by = "caseid") %>%
  left_join(sociodemo, by = "caseid") %>%
  left_join(estadocivil, by = "caseid") %>%
  left_join(peso, by = "caseid") %>%
  left_join(hogar2, by = "caseid") %>%
  mutate(year = "2014")

table(df_gestantes2014$s411g, exclude = NULL)
dim(df_gestantes2014) 

## Gestantes 2013
salud <- read_sav("./Data_endes/2013/CSALUD01.sav") %>% unite("CASEID", HHID, QSNUMERO, sep = "  ",remove = FALSE) #HHID
salud <- salud %>% select(HHID,
                          CASEID,
                          QS29A,
                          QS601A,
                          QS603) %>%
  clean_names()
#progsociales <- read_sav("./Data_endes/2013/programassociales.sav") #No identificador individual
#progsociales <- progsociales %>% select(HHID, QH95, QH106) %>%
 # clean_names()
gestacion1 <- read_sav("./Data_endes/2013/RE223132.sav")
gestacion1 <- gestacion1 %>% select(CASEID, V201, V206, V207, V208, V213
                                    ,V218, V219, V225, V228, V233, V238, V302, V313) %>%
  clean_names()
pareja <- read_sav("./Data_endes/2013/RE516171.sav")
pareja <- pareja %>% select(CASEID,
                            V531,
                            V532,
                            V701,
                            V730) %>%
  clean_names()
sexualidad <- read_sav("./Data_endes/2013/RE758081.sav")
sexualidad <- sexualidad %>% select(CASEID, V750, V761, V761B, V763A,
                                    V763B, V763C, V766B, V768A, V769, V770, V785,
                                    V830, V835A, V836) %>%
  clean_names()
hogar<- read_sav("./Data_endes/2013/REC0111.sav")
hogar <- hogar %>% select(V131, CASEID, V002, V003, V007, V040, V025, V024,
                          V101, V102, V103, V136, V150, AWFACTW, 
                          V001, V005, V022, V190) %>%
  clean_names()

nino <- read_sav("./Data_endes/2013/REC21.sav")
nino <- nino  %>% group_by(CASEID) %>% filter(BORD == max(BORD)) %>% 
  select(CASEID, BIDX, B0, B1, B2, B4, B11, B16)%>%
  clean_names()
prenatal <- read_sav("./Data_endes/2013/REC41.sav") %>% rename(IDX94 = MIDX) %>%
  clean_names()
salud2 <- read_sav("./Data_endes/2013/REC42.sav")
salud2 <- salud2 %>% select(CASEID, V437, V438, V445, V453, V454, V456,
                            V457, V463A, V464, V481)%>%
  clean_names()
violence <- read_sav("./Data_endes/2013/REC84DV.sav")
violence <- violence %>% select(CASEID, D104, D106, D107, D108, D115Y,
                                D116, D118A, D118Y, D119Y) %>%
  clean_names()
ITS <- read_sav("./Data_endes/2013/REC91.sav")
ITS <- ITS %>% select(CASEID, S108N, SREGION, SPROVIN, SDISTRI,  
                      S815AA, S815AB, S815AC, S815AD, S815AE, S815AX, S815AZ,
                      S816AA, S816AB, S816AC, S816AD, S816AE, S816AF, S816AG,
                      S816AH, S816AI, S816AJ, S816AK, S816AL, S816AW) %>%
  clean_names() 
prenatal2 <- read_sav("./Data_endes/2013/REC94.sav")
prenatal2 <- prenatal2 %>% select(CASEID, IDX94, S410B, S411B, S411G, S411H, S411BA, 
                                  S411CA, S411DA, S411EA) %>%
  clean_names() #VD
sociodemo <- read_sav("./Data_endes/2013/REC0111.sav")
sociodemo <- sociodemo %>% select(CASEID, V190) %>%
  clean_names()
estadocivil <- read_sav("./Data_endes/2013/RECH1.sav") %>% unite("CASEID", HHID:HVIDX, sep = "  ",remove = FALSE) #HHID
estadocivil <- estadocivil %>% select (HV115, CASEID) %>%
  clean_names()
peso <- read_sav("./Data_endes/2013/RECH5.sav")%>% unite("CASEID", HHID, HA0, sep = "  ",remove = FALSE) #HHID
peso <- peso %>% select(HHID,CASEID, HA2, HA3, HA40) %>%
  clean_names()
hogar2 <- read_sav("./Data_endes/2013/RECH0.sav")%>% unite("CASEID", HHID, HV003, sep = "  ",remove = FALSE) #HHID
hogar2 <- hogar2 %>% select(HHID,CASEID, HV001, HV007, longitudx, 
                            latitudy, HV022, HV005) %>%
  clean_names()

df_gestantes2013<-
  prenatal2 %>% 
  left_join(salud, by = "caseid") %>% 
  left_join(gestacion1, by = "caseid") %>% 
  left_join(pareja, by = "caseid") %>%
  left_join(sexualidad, by = "caseid") %>% 
  left_join(hogar, by = "caseid") %>%
  inner_join(nino, by = "caseid") %>%
  left_join(salud2, by = "caseid") %>% 
  left_join(prenatal, by = "caseid") %>%
  left_join(violence, by = "caseid") %>% 
  left_join(ITS, by = "caseid") %>%
  left_join(sociodemo, by = "caseid") %>%
  left_join(estadocivil, by = "caseid") %>%
  left_join(peso, by = "caseid") %>%
  left_join(hogar2, by = "caseid") %>%
  mutate(year = "2013")

table(df_gestantes2013$s411g, exclude = NULL)
dim(df_gestantes2013) 

## Gestantes 2012

# salud <- read_sav("./Data_endes/2012/CSALUD01.sav") %>% unite("CASEID", HHID, QSNUMERO, sep = "  ",remove = FALSE) #HHID
# salud <- salud %>% select(HHID,
#                           CASEID,
#                           QS29A,
#                           QS601A,
#                           QS603) %>%
#   clean_names()
#progsociales <- read_sav("./Data_endes/2013/programassociales.sav") #No identificador individual
#progsociales <- progsociales %>% select(HHID, QH95, QH106) %>%
# clean_names()
gestacion1 <- read_sav("./Data_endes/2012/RE212232.sav")
gestacion1 <- gestacion1 %>% select(CASEID, V201, V206, V207, V208, V213
                                    ,V218, V219, V225, V228, V233, V238, V302, V313) %>%
  clean_names()
pareja <- read_sav("./Data_endes/2012/RE516171.sav")
pareja <- pareja %>% select(CASEID,
                            V531,
                            V532,
                            V701,
                            V730) %>%
  clean_names()
sexualidad <- read_sav("./Data_endes/2012/RE758081.sav")
sexualidad <- sexualidad %>% select( CASEID, V750, V761, V761B, V763A,
                                    V763B, V763C, V766B, V768A, V769, V770, V785,
                                    V830, V835A, V836) %>%
  clean_names()
hogar<- read_sav("./Data_endes/2012/REC0111.sav")
hogar <- hogar %>% select(V012, V131, CASEID, V002, V003, V007, V040, V025, V024,
                          V101, V102, V103, V136, V150, AWFACTW,
                          V001, V005, V022, V190) %>%
  clean_names()
nino <- read_sav("./Data_endes/2012/REC21.sav")
nino <- nino  %>% group_by(CASEID) %>% filter(BORD == max(BORD)) %>% 
  select(CASEID, BIDX, B0, B1, B2, B4, B11, B16)%>%
  clean_names()
prenatal <- read_sav("./Data_endes/2012/REC41.sav") %>% rename(IDX94 = MIDX) %>%
  clean_names()
salud2 <- read_sav("./Data_endes/2012/REC42.sav")
salud2 <- salud2 %>% select(CASEID, V437, V438, V445, V453, V454, V456,
                            V457, V463A, V464, V481)%>%
  clean_names()
violence <- read_sav("./Data_endes/2012/REC84DV.sav")
violence <- violence %>% select(CASEID, D104, D106, D107, D108, D115Y,
                                D116, D118A, D118Y, D119Y) %>%
  clean_names()
ITS <- read_sav("./Data_endes/2012/REC91.sav")
ITS <- ITS %>% select(CASEID, S108N, SREGION, SPROVIN, SDISTRI, 
                      S815AA, S815AB, S815AC, S815AD, S815AE, S815AX, S815AZ,
                      S816AA, S816AB, S816AC, S816AD, S816AE, S816AF, S816AG,
                      S816AH, S816AI, S816AJ, S816AK, S816AL, S816AW) %>%
  clean_names() 
prenatal2 <- read_sav("./Data_endes/2012/REC94.sav")
prenatal2 <- prenatal2 %>% select(CASEID, IDX94, S410B, S411B, S411G, S411H, S411BA, 
                                  S411CA, S411DA, S411EA) %>%
  clean_names() #VD
sociodemo <- read_sav("./Data_endes/2012/REC0111.sav")
sociodemo <- sociodemo %>% select(CASEID, V190) %>%
  clean_names()
estadocivil <- read_sav("./Data_endes/2012/RECH1.sav") %>% unite("CASEID", HHID:HVIDX, sep = "  ",remove = FALSE) #HHID
estadocivil <- estadocivil %>% select (HV115, CASEID) %>%
  clean_names()
peso <- read_sav("./Data_endes/2012/RECH5.sav")%>% unite("CASEID", HHID, HA0, sep = "  ",remove = FALSE) #HHID
peso <- peso %>% select(HHID,CASEID, HA2, HA3, HA40) %>%
  clean_names()
hogar2 <- read_sav("./Data_endes/2012/RECH0.sav")%>% unite("CASEID", HHID, HV003, sep = "  ",remove = FALSE) #HHID
hogar2 <- hogar2 %>% select(HHID,CASEID, HV001, HV007,  longitudx, 
                            latitudy, HV022, HV005) %>%
  clean_names()


df_gestantes2012<-
  prenatal2 %>% 
  left_join(gestacion1, by = "caseid") %>% 
  left_join(pareja, by = "caseid") %>%
  left_join(sexualidad, by = "caseid") %>% 
  left_join(hogar, by = "caseid") %>%
  inner_join(nino, by = "caseid") %>%
  left_join(salud2, by = "caseid") %>% 
  left_join(prenatal, by = "caseid") %>%
  left_join(violence, by = "caseid") %>% 
  left_join(ITS, by = "caseid") %>%
  left_join(sociodemo, by = "caseid") %>%
  left_join(estadocivil, by = "caseid") %>%
  left_join(peso, by = "caseid") %>%
  left_join(hogar2, by = "caseid") %>%
  mutate(year = "2012")

table(df_gestantes2012$s411g, exclude = NULL)
dim(df_gestantes2012) 

## Gestantes 2011

# salud <- read_sav("./Data_endes/2011/CSALUD01.sav") %>% unite("CASEID", HHID, QSNUMERO, sep = "  ",remove = FALSE) #HHID
# salud <- salud %>% select(ID1,
#                           HHID,
#                           CASEID,
#                           QS25BB,
#                           QS29A,
#                           QS601A,
#                           QS603) %>%
#   clean_names()
# progsociales <- read_sav("./Data_endes/2020/programassociales.sav") #No identificador individual
# progsociales <- progsociales %>% select(ID1,
#                                         HHID,
#                                         QH95,
#                                         QH106) %>%
#   clean_names()

gestacion1 <- read_sav("./Data_endes/2011/RE223132.sav")
gestacion1 <- gestacion1 %>% select(CASEID, V201, V206, V207, V208, V213
                                    ,V218, V219, V225, V228, V233, V238, V302, V313) %>%
  clean_names()
pareja <- read_sav("./Data_endes/2011/RE516171.sav")
pareja <- pareja %>% select(CASEID,
                            V531,
                            V532,
                            V701,
                            V730) %>%
  clean_names()
sexualidad <- read_sav("./Data_endes/2011/RE758081.sav")
sexualidad <- sexualidad %>% select(CASEID, V750, V761, V761B, V763A,
                                    V763B, V763C, V766B, V768A, V769, V770, V785,
                                    V830, V835A, V836) %>%
  clean_names()
hogar<- read_sav("./Data_endes/2011/REC0111.sav")
hogar <- hogar %>% select(V012, V131, CASEID, V002, V003, V007, V040, V025, V024,
                          V101, V102, V103, V136, V150, AWFACTW,
                          V001, V005, V022, V190) %>%
  clean_names()
nino <- read_sav("./Data_endes/2011/REC21.sav")
nino <- nino  %>% group_by(CASEID) %>% filter(BORD == max(BORD)) %>% 
  select(CASEID, BIDX, B0, B1, B2, B4, B11, B16)%>%
  clean_names()
prenatal <- read_sav("./Data_endes/2011/REC41.sav") %>% rename(IDX94 = MIDX) %>%
  clean_names()
salud2 <- read_sav("./Data_endes/2011/REC42.sav")
salud2 <- salud2 %>% select(CASEID, V437, V438, V445, V453, V454, V456,
                            V457, V463A, V464, V481)%>%
  clean_names()
violence <- read_sav("./Data_endes/2011/REC84DV.sav")
violence <- violence %>% select(CASEID, D104, D106, D107, D108, D115Y,
                                D116, D118A, D118Y, D119Y) %>%
  clean_names()
ITS <- read_sav("./Data_endes/2011/REC91.sav")
ITS <- ITS %>% select(CASEID, S108N, SREGION, SPROVIN, SDISTRI, 
                      S815AA, S815AB, S815AC, S815AD, S815AE, S815AX, S815AZ,
                      S816AA, S816AB, S816AC, S816AD, S816AE, S816AF, S816AG,
                      S816AH, S816AI, S816AJ, S816AK, S816AL, S816AW) %>%
  clean_names() 
prenatal2 <- read_sav("./Data_endes/2011/REC94.sav")
prenatal2 <- prenatal2 %>% select(CASEID, IDX94, S410B, S411B, S411G, S411H, S411BA, 
                                  S411CA, S411DA, S411EA) %>%
  clean_names() #VD
sociodemo <- read_sav("./Data_endes/2011/REC0111.sav")
sociodemo <- sociodemo %>% select(CASEID, V190) %>%
  clean_names()
estadocivil <- read_sav("./Data_endes/2011/RECH1.sav") %>% unite("CASEID", HHID:HVIDX, sep = "  ",remove = FALSE) #HHID
estadocivil <- estadocivil %>% select (HV115, CASEID) %>%
  clean_names()
peso <- read_sav("./Data_endes/2011/RECH5.sav")%>% unite("CASEID", HHID, HA0, sep = "  ",remove = FALSE) #HHID
peso <- peso %>% select(HHID,CASEID, HA2, HA3, HA40) %>%
  clean_names()
hogar2 <- read_sav("./Data_endes/2011/RECH0.sav")%>% unite("CASEID", HHID, HV003, sep = "  ",remove = FALSE) #HHID
hogar2 <- hogar2 %>% select(HHID,CASEID, HV001, HV007, longitudx, 
                            latitudy, HV022, HV005) %>%
  clean_names()

df_gestantes2011<-
  prenatal2 %>% 
  left_join(gestacion1, by = "caseid") %>% 
  left_join(pareja, by = "caseid") %>%
  left_join(sexualidad, by = "caseid") %>% 
  left_join(hogar, by = "caseid") %>%
  inner_join(nino, by = "caseid") %>%
  left_join(salud2, by = "caseid") %>% 
  left_join(prenatal, by = "caseid") %>%
  left_join(violence, by = "caseid") %>% 
  left_join(ITS, by = "caseid") %>%
  left_join(sociodemo, by = "caseid") %>%
  left_join(estadocivil, by = "caseid") %>%
  left_join(peso, by = "caseid") %>%
  left_join(hogar2, by = "caseid") %>%
  mutate(year = "2011")

table(df_gestantes2011$s411g, exclude = NULL)
dim(df_gestantes2011)  

## Gestantes 2010
# salud <- read_sav("./Data_endes/2010/CSALUD01.sav") %>% unite("CASEID", HHID, QSNUMERO, sep = "  ",remove = FALSE) #HHID
# salud <- salud %>% select(ID1,
#                           HHID,
#                           CASEID,
#                           QS25BB,
#                           QS29A,
#                           QS601A,
#                           QS603) %>%
#   clean_names()
# progsociales <- read_sav("./Data_endes/2010/programassociales.sav") #No identificador individual
# progsociales <- progsociales %>% select(ID1,
#                                         HHID,
#                                         QH95,
#                                         QH106) %>%
#   clean_names()
gestacion1 <- read_sav("./Data_endes/2010/RE223132.sav")
gestacion1 <- gestacion1 %>% select(CASEID, V201, V206, V207, V208, V213
                                    ,V218, V219, V225, V228, V233, V238, V302, V313) %>%
  clean_names()
pareja <- read_sav("./Data_endes/2010/REC516171.sav")
pareja <- pareja %>% select(CASEID,
                            V531,
                            V532,
                            V701,
                            V730) %>%
  clean_names()
sexualidad <- read_sav("./Data_endes/2010/RE758081.sav")
sexualidad <- sexualidad %>% select(CASEID, V750, V761, V761B, V763A,
                                    V763B, V763C, V766B, V768A, V769, V770, V785,
                                    V830, V835A, V836) %>%
  clean_names()
hogar<- read_sav("./Data_endes/2010/REC0111.sav")
hogar <- hogar %>% select(V012, V131, CASEID, V002, V003, V007, V040, V025, V024,
                          V101, V102, V103, V136, V150, AWFACTW,
                          V001, V005, V022, V190) %>%
  clean_names()
nino <- read_sav("./Data_endes/2010/REC21.sav")
nino <- nino  %>% group_by(CASEID) %>% filter(BORD == max(BORD)) %>% 
  select(CASEID, BIDX, B0,B1, B2, B4, B11, B16)%>%
  clean_names()
prenatal <- read_sav("./Data_endes/2010/REC41.sav") %>% rename(IDX94 = MIDX) %>%
  clean_names()
salud2 <- read_sav("./Data_endes/2010/REC42.sav")
salud2 <- salud2 %>% select(CASEID, V437, V438, V445, V453, V454, V456,
                            V457, V463A, V464, V481)%>%
  clean_names()
violence <- read_sav("./Data_endes/2010/REC84DV.sav")
violence <- violence %>% select(CASEID, D104, D106, D107, D108, D115Y,
                                D116, D118A, D118Y, D119Y) %>%
  clean_names()
ITS <- read_sav("./Data_endes/2010/REC91.sav")
ITS <- ITS %>% select(CASEID, S108N, SREGION, SPROVIN, SDISTRI,  
                      S815AA, S815AB, S815AC, S815AD, S815AE, S815AX, S815AZ,
                      S816AA, S816AB, S816AC, S816AD, S816AE, S816AF, S816AG,
                      S816AH, S816AI, S816AJ, S816AK, S816AL, S816AW) %>%
  clean_names() 
prenatal2 <- read_sav("./Data_endes/2010/REC94.sav")
prenatal2 <- prenatal2 %>% select(CASEID, IDX94, S410B, S411B, S411G, S411H, S411BA, 
                                  S411CA, S411DA, S411EA) %>%
  clean_names() #VD
sociodemo <- read_sav("./Data_endes/2010/REC0111.sav")
sociodemo <- sociodemo %>% select(CASEID, V190) %>%
  clean_names()
estadocivil <- read_sav("./Data_endes/2010/RECH1.sav") %>% unite("CASEID", HHID:HVIDX, sep = "  ",remove = FALSE) #HHID
estadocivil <- estadocivil %>% select (HV115, CASEID) %>%
  clean_names()
peso <- read_sav("./Data_endes/2010/RECH5.sav")%>% unite("CASEID", HHID, HA0, sep = "  ",remove = FALSE) #HHID
peso <- peso %>% select(HHID,CASEID, HA2, HA3, HA40) %>%
  clean_names()
hogar2 <- read_sav("./Data_endes/2010/RECH0.sav")%>% unite("CASEID", HHID, HV003, sep = "  ",remove = FALSE) #HHID
hogar2 <- hogar2 %>% select(HHID,CASEID, HV001, HV007, longitudx, 
                            latitudy, HV022, HV005) %>%
  clean_names()

df_gestantes2010<-
  prenatal2 %>% 
  left_join(gestacion1, by = "caseid") %>% 
  left_join(pareja, by = "caseid") %>%
  left_join(sexualidad, by = "caseid") %>% 
  left_join(hogar, by = "caseid") %>%
  inner_join(nino, by = "caseid") %>%
  left_join(salud2, by = "caseid") %>% 
  left_join(prenatal, by = "caseid") %>%
  left_join(violence, by = "caseid") %>% 
  left_join(ITS, by = "caseid") %>%
  left_join(sociodemo, by = "caseid") %>%
  left_join(estadocivil, by = "caseid") %>%
  left_join(peso, by = "caseid") %>%
  left_join(hogar2, by = "caseid") %>%
  mutate(year = "2010")

table(df_gestantes2010$s411g, exclude = NULL)
dim(df_gestantes2010)  


## Union de bases
#Se unieron las bases con bind_rows para no perder infromación de las encuestas que no se encuentran en algunos años, usando la función List, se perdían variables de la encuesta progsociales

list <- bind_rows(df_gestantes2010,df_gestantes2011,df_gestantes2012,
            df_gestantes2013,df_gestantes2014,df_gestantes2015,
            df_gestantes2016,df_gestantes2017,df_gestantes2018,
            df_gestantes2019,df_gestantes2020, df_gestantes2021)

sifilis_gestantes<- map_dfr(.x = list,
                 .f = ~ data_syph_funct(.x)) # funcion de prueba creada

(list=ls()) # Elimina todos los objetos del enviroment


## Transformar los datos en objetos survey

df_syphilis<- datafinal %>% 
    mutate(
    year = as.numeric(year),
    filtro = year - b2
  ) %>% 
  
  filter(filtro == 1) %>% 
  
  
  group_by(caseid) %>% 
  slice(1) %>% 
  
  ungroup()

write.csv(df_syphilis,"./Data_final/data_syphilis.csv", row.names = F)
