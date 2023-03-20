dataprep<-function(data){
  data %>% 
    select(caseid,
           qh95,
           qh106,
           v025,
           v024,
           v101,
           v102,
           v103,
           v136,
           v150,
           v001,
           v005,
           v022,
           v190,
           sregion,
           sprovincia,
           sdistri,
           longitudx,
           latitudy,
      CASEID,
           year,
           B2,
           B3,
           V012,
           V190,
           V150,
           V025,
           V103,
           V136,
           V131,
           V024,
           V155,
           V001,
           V005,
           V022,
           HV115,
           S108N,
           SREGION,
           S621,
           S704N,
           S802,
           S815AA,
           S815AB,
           S815AC,
           S815AD,
           S815AE,
           S815AX,
           S815AZ,
           S816AA,
           S816AB,
           S816AC,
           S816AD,
           S816AE,
           S816AF,
           S816AG,
           S816AH,
           S816AI,
           S816AJ,
           S816AK,
           S816AL,
           S816AW,
           S816AZ,
           #S413,
           S411G,
           S411H,
           M2A,
           M2B,
           M2C,
           M2D,
           M2E,
           M2F,
           M2G,
           M2H,
           M2I,
           M2J,
           M2K,
           M2L,
           M2M,
           M2N,
           M10,
           M13,
           M14,
           M57E,
           M57F,
           M57G,
           M57H,
           M57I,
           M57J,
           M57K,
           M57L,
           M57M,
           M57N,
           M57O,
           M57P,
           M57Q,
           M57R,
           M57S,
           M57T,
           M57U,
           M57V,
           M57X,
           D118Y,
           V763A,
           V763B,
           V763C,
           V774A,
           V774B,
           V774C,
           V201,
           V208,
           V211,
           V481,
           V481A,
           V481B,
           V481C,
           V481D,
           V481E,
           V481F,
           V481G,
           V481H,
           V481X,
           V209,
           V213,
           V214,
           IDX94,
           #MIDX,
    ) %>% 
    
    mutate(
      v005 = v005/1000000,
      
      juntos = case_when((qh95==1)~ "Yes",
                         (qh95==2)~ "No",
                         (qh95==3)~ "Don't know"),
      
      wawa_wasi = case_when((qh106==1)~ "Yes",
                            (qh106==2)~ "No",
                            (qh106==3)~ "Don't know"),
                               
      type_residence = ifelse(v025==1,"Urban","Rural"),
      
      departament = factor(v024, levels = c(1:25), labels = c("Amazonas","Ancash","Apurimac","Arequipa","Ayacucho",   
                                                             "Cajamarca","Callao","Cusco","Huancavelica","Huanuco",      
                                                             "Ica","Junin","La Libertad","Lambayeque","Lima",         
                                                             "Loreto","Madre De Dios","Moquegua","Pasco","Piura",     
                                                             "Puno","San Martin","Tacna","Tumbes","Ucayali")),
      
      childhood_place_residence = case_when((v103==0)~ "Capital",
                                            (v103==1)~ "City",
                                            (v103==2)~ "Town",
                                            (v103==3)~ "Countryside",
                                            (v103==4)~ "Abroad",
                                            TRUE     ~ "Don´t know"),
      
      members_household = case_when(is.na(v136)              ~ "NA",
                                         (v136 < 5)          ~ "1-4",
                                         (v136 >=5 & v136 <7)~ "5-6",
                                          TRUE               ~ "MORE THAN 7"),
      
      relationship_household_head = case_when(is.na(v150)~ "NA",
                                              (v150==1)~ "Head",
                                              (v150==2)~ "Wife",
                                              (v150==3)~ "Daugther",
                                              TRUE     ~ "Other"),
      
      wealth_index = factor(v190, levels = c(1:5), labels = c("Poorest","Poor","Middle","Rich","Richest")),
      
      natural_region = factor(sregion, levels = c(1:4), labels = c("Lima Metropolitan","Rest of Coast","Highland","Jungle")),
      
      
      AGE_MOTHER = ifelse(V012<25,"15-24a",
                          ifelse(V012>=25&V012<30,"25-30a",
                                 ifelse(V012>=30&V012<35,"30-35a",
                                        ifelse(V012>=35,"35 a mas",NA)))),
      
      
      
      RELATIONSHIP_HOUSEHOLD_HEAD = ifelse(V150==1,"HEAD", 
                                           ifelse(V150==2, "WIFE", 
                                                  ifelse(V150==3, "DAUGTHER/SON", 
                                                         ifelse(V150>=4, "OTHER", NA)))),
      
      TYPE_PLACE_RESIDENCE = ifelse(V025==1,"URBAN","RURAL"),
      
      ETHNICITY = ifelse(V131==1,"SPANISH",
                         ifelse(V131==2, "QUECHUA",
                                ifelse(V131==3, "AIMARA",
                                       ifelse(V131==4, "OTHER INDIGENOUS",
                                              ifelse(V131==5,"FOREIGNER",NA))))),
      
      DEPARTAMEN = factor(V024, levels = c(1:25), labels = c("AMAZONAS","ANCASH","APURIMAC","AREQUIPA","AYACUCHO",
                                                             "CAJAMARCA","CALLAO","CUSCO","HUANCAVELICA","HUANUCO",
                                                             "ICA","JUNIN","LA LIBERTAD","LAMBAYEQUE","LIMA",
                                                             "LORETO","MADRE DE DIOS","MOQUEGUA","PASCO","PIURA",
                                                             "PUNO","SAN MARTIN","TACNA","TUMBES","UCAYALI")),
      
      LITERACY = factor(V155, levels = c(0:4), labels = c("CANNOT_READ_AT_ALL","ABLE_TO_READ_ONLY_PARTS_OF_SENTENCE",
                                                          "ABLE_TO_READ_WHOLE_SENTENCE","NO_CARD_WITH_REQUIRED_LANGUAGE","BLIND/VISUALLY_IMPAIRED")),
      
      CURRENT_MARITAL_STATUS = ifelse(HV115==0,"SINGLE", 
                                      ifelse(HV115>=1&HV115<=2,"MARRIED/LIVING_TOGETHER", 
                                             ifelse(HV115>=3,"WIDOWED/DIVORCED/SEPARATED",NA))),
      
      EDU_LEVEL = ifelse(S108N==0,"NONE/PRESCHOOL",
                         ifelse(S108N==1,"PRIMARY",
                                ifelse(S108N==2,"SECONDARY",
                                       ifelse(S108N>=3&S108N<6,"HIGHER",NA)))),
      
      NATURAL_REGION = factor(SREGION, levels = c(1:4), labels = c("LIMA_METROPOLITAN","REST_OF_COAST","HIGHLAND","JUNGLE")),
      
      PARTNER_APPROVE_DISAPPROVE_FAMILY_PLANNING = factor(S621, levels = c(1,2,8), labels = c("APPROVE","DISAPPROVE","DONT_KNOW")),
      
      PARTNER_EDU_LEVEL = ifelse(S704N==0,"NONE/PRESCHOOL",
                                 ifelse(S704N==1,"PRIMARY",
                                        ifelse(S704N==2,"SECONDARY",
                                               ifelse(S704N>=3&S704N<6,"HIGHER",NA)))),
      
      CAN_SOMETHING_BE_DONE_PREVENT_AIDS = factor(S802, levels = c(0,1,8), labels = c("NO","YES","DONT_KNOW")),
      
      
      KNOW_ETS = (S815AA+S815AB+S815AC+S815AD+S815AE+S815AX), KNOW_ETS=case_when(KNOW_ETS==0~"NONE",
                                                                                 KNOW_ETS==1~"AT LEAST 1",
                                                                                 KNOW_ETS>=2~"MORE THAN 1"),
      
      KNOW_SYMPTON_ETS = (S816AA+S816AB+S816AC+S816AD+S816AE+S816AF+S816AG+S816AH+S816AI+S816AJ+S816AK+S816AL+S816AW) , KNOW_SYMPTON_ETS=case_when(KNOW_SYMPTON_ETS==0~"NO",
                                                                                                                                                   KNOW_SYMPTON_ETS==1~"AT LEAST 1",
                                                                                                                                                   KNOW_SYMPTON_ETS>=2~"MORE THAN 1"),
      
      
      CHECKUP_RULE_OUT_SYPHILIS = as.factor(ifelse(S411G == 1,"YES","NO")),
      
      CHECKUP_RULE_OUT_HIV = as.factor(ifelse(S411H == 1,"YES","NO")),
      
      #CHECKUP_RULE_OUT_HEPATITIS = as.factor(ifelse(QI411_M == 1,"YES","NO")),
      
      PRENATAL_CARE_ATTENTION = case_when((M2A==1 | M2B==1 | M2C==1 | M2D==1) ~ "PROFESSIONAL_OR_TECHNICAL",
                                          (M2E==1 | M2F==1 | M2G==1 | M2H==1 | M2I==1 | M2J==1 | M2K==1 | M2L==1 | M2M==1) ~ "OTHER",
                                          (M2N==1) ~ "NOBODY"),#misma respuesta en 2 var. Hay respuestas en el 1er grupo y 2do grupo, y lo agrupa en el 1ero
      
      INTENDED_PREGNANCY = factor(M10, levels = c(1,2,3), labels = c("ENTONCES","ESPERAR_MAS","NO_QUERIA_MAS")),
      
      PHYSICAL_VIOLENCE= factor(D118Y, levels = c(0,1), labels = c("YES","NO")),
      
      DIAGNOSTED_STD_LAST_12_MONTHS = factor(V763A, levels = c(0,1,8), labels = c("NO","YES","DONT_KNOW")),
      
      KNOW_HIV_TRANSMISSION_MOTHER_TO_CHILD = (V774A+V774B+V774C) , 
      KNOW_HIV_TRANSMISSION_MOTHER_TO_CHILD = case_when(KNOW_HIV_TRANSMISSION_MOTHER_TO_CHILD==0~"NO",
                                                        KNOW_HIV_TRANSMISSION_MOTHER_TO_CHILD>=1~"YES"),
      
      HOUSEHOLD_MEMBERS = ifelse(V136<5,"1-4",
                                 ifelse(V136>=5&V136<7,"5-6",
                                        ifelse(V136>=7,"MORE THAN 7",NA))),
      
      FIRST_PRENATAL_VISIT = factor(M13, levels = c(0,1,2,3,4,5,6,7,8,9)),
      
      NUMBER_PRENATAL_VISITS = factor(M14, levels = c (0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)),
      
      PRENATAL_ATTENTION_PLACE = case_when((M57E==1 | M57F==1 | M57G==1) ~ "MINSA",
                                           (M57I==1 | M57K==1) ~ "ESSALUD",
                                           (M57J==1) ~ "FF.AA.",
                                           (M57H==1 | M57M==1 | M57N==1 | M57O==1 | M57R==1) ~ "PRIVATE",
                                           (M57L==1 | M57P==1 | M57Q==1 | M57S==1 | M57T==1 | M57U==1 | M57V==1 | M57X==1) ~ "OTHERS"),#misma resp en 2 variables. ejm:%>% filter(M57F==1 & M57I==1 & M57L==1)
      
      COMPLEXITY_OF_PRENATAL_ATTENTION_PLACE = case_when((M57G==1 | M57K==1 | M57O==1 | M57P==1 | M57R==1 | M57S==1 | M57T==1 | M57U==1 | M57V==1 | M57X==1) ~ "LEVEL 1",
                                                         (M57F==1 | M57H==1 | M57L==1 | M57M==1 | M57N==1 | M57Q==1) ~ "LEVEL 2",
                                                         (M57E==1 | M57I==1 | M57J==1) ~ "LEVEL 3"),#misma resp en 2 variables. ejm: %>% filter(M57E==1 & M57F==1 & M57G==1)
      
      HAVE_ITS_SYMPTOMS=case_when((V763B==1 & V763C==0)~"ONLY SORE/ULCER",
                                  (V763B==1 & V763C==8)~"ONLY SORE/ULCER",
                                  (V763B==0 & V763C==1)~"ONLY FLOW",
                                  (V763B==8 & V763C==1)~"ONLY FLOW",
                                  (V763B==1 & V763C==1)~"BOTH",
                                  ((V763B==0 & V763C==0) | (V763B==8 & V763C==8) | (V763B==8 & V763C==0) | (V763B==0 & V763C==8))~"NONE"),
      
      TOTAL_CHILDREN = factor(V201, levels = c (0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)),
      
      UNDER_SIXYEARS_CHILDREN =factor(V208, levels = c(1,2,3,4)),
      
      #HEALTH_INSURANCE=case_when(SH11A==1 ~ "ESSALUD",
      # SH11C==1 ~ "SIS",
      #(SH11B==1 | SH11D==1 | SH11E==1 | SH11Y==1) ~ "OTHERS",
      #SH11Z==1 ~ "NONE"),#misma respuesta en 2 variables,ejm: %>% filter(SH11A==1 & SH11B==1)
      HEALTH_INSURANCE=case_when((V481==0)~"NO",
                                 (V481==1)~"YES"),
      
      LAST_BIRTH = ifelse(V209==0,"MORE THAN 12 MONTHS",
                          ifelse(V209>=1&V209<4,"LESS THAN 12 MONTHS", NA)),
      
    )
}