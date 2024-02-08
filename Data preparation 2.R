datafinal <- list %>% 
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
         #sprovincia,
         sdistri,
         longitudx,
         latitudy,
         m2a,
         m2b,
         m2c,
         m2d,
         m2e,
         m2f,
         m2g,
         m2h,
         m2i,
         m2j,
         m2k,
         m2l,
         m2m,
         m2n,
         m3a,
         m3b,
         m3c,
         m3d,
         m3e,
         m3f,
         m3g,
         m3h,
         m3i,
         m3j,
         m3k,
         m3l,
         m3m,
         m3n,
         m13,
         m14,
         m15,
         m17,
         m42a,
         m42b,
         m42c,
         m42d,
         m42e,
         m43,
         m44,
         m45,
         m46,
         m51a,
         m57e,
         m57f,
         m57g,
         m57h,
         m57i,
         m57j,
         m57k,
         m57l,
         m57m,
         m57n,
         m57o,
         m57p,
         m57q,
         m57r,
         m57x,
         m71,
         m72,
         v481,
         s410b,
         s411g,
         s411h,
         s411b,
         s411f,
         s411ba,
         s411ca,
         s411da,
         s411ea,
         s426fa,
         qi411_m,
         qi411f,
         bidx,
         b0,
         bd,
         b1,
         b2,
         b4,
         b11,
         b16,
         m10,
         m18,
         m19,
         m19a,
         m70,
         m73,
         qs25bb,
         qs29a,
         qs601a,
         qs603,
         v201,
         v206,
         v207,
         v208,
         v213,
         v218,
         v219,
         v225,
         v228,
         v233,
         v238,
         v302,
         v313,
         v531,
         v532,
         v701,
         v730,
         v750,
         v761,
         v761b,
         v763a,
         v763b,
         v763c,
         v766b,
         v768a,
         v769,
         v770,
         v785,
         v830,
         v835a,
         v836,
         v012,
         v131,
         q220a,
         v437,
         v438,
         v445,
         v453,
         v454,
         v456,
         v457,
         v463a,
         v464,
         d104,
         d106,
         d107,
         d108,
         d115y,
         d116,
         d118a,
         d118y,
         d119y,
         s108n,
         s119d,
         s815aa,
         s815ab,
         s815ac,
         s815ad,
         s815ae,
         s815ax,
         s815az,
         s816aa,
         s816ab,
         s816ac,
         s816ad,
         s816ae,
         s816af,
         s816ag,
         s816ah,
         s816ai,
         s816aj,
         s816ak,
         s816al,
         s816aw,
         hv115,
         ha2,
         ha3,
         ha40,
         #idx94,
         #midx,
         hhid,
         year
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
    
    members_household = case_when(is.na(v136)              ~ NA,
                                  (v136 < 5)          ~ "1-4",
                                  (v136 >=5 & v136 <7)~ "5-6",
                                  TRUE               ~ "MORE THAN 7"),
    
    relationship_household_head = case_when(is.na(v150)~ NA,
                                            (v150==1)~ "Head",
                                            (v150==2)~ "Wife",
                                            (v150==3)~ "Daugther",
                                            TRUE     ~ "Other"),
    
    wealth_index = factor(v190, levels = c(1:5), labels = c("Poorest","Poor","Middle","Rich","Richest")),
    
    natural_region = factor(sregion, levels = c(1:4), labels = c("Lima Metropolitan","Rest of Coast","Highland","Jungle")),
    
    prenatal_care_provider_1 = case_when((m2a==1) ~ "Doctor",
                                         (m2c==1) ~ "Obstetrician",
                                         (m2b==1) ~ "Nurse",
                                         (m2d==1 | m2e==1 | m2f==1) ~ "technitian/other health provider",
                                         (m2g==1 | m2h==1 | m2k==1) ~ "Midwife/other",
                                         (m2n==1)~ "None"),
    
    prenatal_care_provider_2 = case_when ((m2a==1 | m2c==1 | m2b==1) ~ "Skilled",
                                          (m2d==1 | m2e==1 | m2f==1 | m2g==1 | m2h==1 | m2k==1 | m2n==1) ~ "Unskilled"),
                                       
    provider_childbirth_1 = case_when((m3a==1) ~ "Doctor",
                                                  (m3c==1) ~ "Obstetrician",
                                                  (m3b==1) ~ "Nurse",
                                                  (m3d==1 | m3e==1 | m3f==1) ~ "technitian/other health provider",
                                                  (m3g==1 | m3h==1 | m3k==1) ~ "Midwife/other",
                                                  (m3n==1)~ "None"),
    
    provider_childbirth_2 = case_when((m3a==1 | m3c==1 | m3b==1) ~ "Skilled",
                                      (m3d==1 | m3e==1 | m3f==1 | m3g==1 | m3h==1 | m3k==1 | m3n==1) ~ "Unskilled"),
                                                    
    
    first_prenatal_visit_1 = factor(m13,levels = c(0,1,2,3,4,5,6,7,8,9)),
    
    first_prenatal_visit_2 = case_when(is.na(v150)~ NA,
                                       (m13==0) ~ "0 month",
                                       (m13==1) ~ "Primer",
                                       (m13==2) ~ "Second",
                                       (m13==3) ~ "Third",
                                       TRUE ~ "More than third"),
    
    number_prenatal_visits_1 = factor(m14, levels = c (0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)),
    
    number_prenatal_visits_2 = case_when(is.na(m14) ~ NA,
                                         (m14<6) ~ "0-5",
                                         (m14>=6 & m14<=9) ~ "6-9",
                                         TRUE ~ "More than 9"),
    
    place_childbirth = case_when(is.na(m15) ~ NA,
                                 (m15==21 | m15==22 | m15==23) ~ "Hospital",
                                 (m15==24 | m15==25 | m15==26) ~ "Health center/post",
                                 (m15==31 | m15==32) ~ "Private",
                                 (m15==11 | m15==12) ~ "Midwife/home",
                                 TRUE ~ "Other"),
    
    caesarean = case_when((m17==0) ~ "No",
                          (m17==1) ~ "Yes"),
    
    basic_prenatal_care = (m42a+s411b+m42c+m42d+m42e+m43),
    basic_prenatal_care = case_when((basic_prenatal_care==0) ~ "None",
                                    (basic_prenatal_care==6) ~ "Complete",
                                    (basic_prenatal_care>6 & basic_prenatal_care<=47) ~ "Incomplete", 
                                    (basic_prenatal_care>=48) ~ "Don´t know"),
    
    pc_screening = (m42a+s411b+m42c+m42d+m42e+s411f),
    prenatal_care_screening= case_when((m14>=6 & pc_screening==6) ~"Yes",
                                       ((m14<6 & pc_screening!=6) |(m14<6 & pc_screening==6) | (m14>=6 & pc_screening!=6)) ~ "No"),
    
    prenatal_care_education= case_when((m14>=6 & m43==1) ~"Yes",
                                       ((m14<6 & m43!=1)|(m14<6 & m43==1)|(m14>=6 & m43!=1)) ~ "No"),
    
    iron_supplement = case_when(is.na(m45) ~ NA,
                                (m45==0) ~ "No",
                                (m45==1) ~ "Yes",
                                (m45==8) ~ "Don´t know"),
    
    place_prenatal_care = case_when((m57e==1 | m57i==1 | m57j==1) ~ "Hospital",
                                    (m57f==1 | m57g==1 | m57k==1 | m57h==1) ~ "Health center/post/policlinic",
                                    (m57m==1 | m57n==1 | m57o==1 | m57r==1) ~ "Private",
                                    (m57l==1 | m57p==1 | m57q==1 | m57x==1) ~ "Other"),
    
    health_insurance = case_when((v481==0)~"No",
                                 (v481==1)~"Yes"),
    
    last_prenatal_visit = case_when((s410b==98) ~ NA),
    last_prenatal_visit = factor(s410b, levels = c(0,1,2,3,4,5,6,7,8,9)),
    
    syphilis_screening = case_when(is.na(s411g) ~ NA,
                                   ((s411g==0) |  (s411g==8)) ~ "No",
                                   (s411g==1) ~ "Yes"),
                                   
    syphilis_screening_completo = case_when(is.na(s411g) ~ NA,
                                  (s411g==0) ~ "No",
                                  (s411g==1) ~ "Yes",                              
                                  (s411g==8) ~ "Don´t know"),
                                  
    hiv_screening = case_when(is.na(s411h) ~ NA,
                              (s411h==0) ~ "No",
                              (s411h==1) ~ "Yes",
                              (s411h==8) ~ "Don't know"),
    
    syphilis_screening_month = case_when((s411da==98) ~ NA),
    syphilis_screening_month = factor(s411da, levels = c(0,1,2,3,4,5,6,7,8,9)),
    
    hiv_screening_month = case_when((s411ea==98) ~ NA),
    hiv_screening_month = factor(s411ea, levels = c(0,1,2,3,4,5,6,7,8,9)),
    
    hepatitis_screening = case_when(is.na(qi411_m) ~ NA,
                                    (qi411_m==0) ~ "No",
                                    (qi411_m==1) ~ "Yes",
                                    (qi411_m==8) ~ "Don't know"),
    
    hepatitis_screening_month = case_when((qi411f==98) ~ NA),
    hepatitis_screening_month = factor(qi411f, levels = c(0,1,2,3,4,5,6,7,8,9)),
    
    single_multiple_delivery = case_when(is.na(qi411_m) ~ NA,
                                         (b0==0) ~ "Single",
                                         TRUE ~ "Multiple"),
    
    sex_child = as.factor(ifelse(b4 == 1,"Masculino","Femenino")),
    
    size_birth = case_when ((m18==1) ~ "very large",
                            (m18==2) ~ "large",
                            (m18==3) ~ "normal",
                            (m18==4) ~ "small",
                            (m18==5) ~ "very_small",
                            (m18==8) ~ NA),
    
    weight_birth = case_when((m19>=2500) ~ "normal",
                             (m19<2500) ~ "low",
                             (m19==9996 | m19==9998) ~ NA),
    
    age_mother = ifelse(v012<18,"15-17",
                        ifelse(v012>=18 & v012<30, "18-29",
                               ifelse(v012>=30,"30-49", NA))),
    
    age_mother_2 = ifelse(v012<=19,"15-19",
                          ifelse(v012>19 & v012<25, "20-24",
                                 ifelse(v012>=25&v012<30,"25-29",
                                        ifelse(v012>=30&v012<35,"30-34",
                                               ifelse(v012>=35,"35 more",NA))))),
    
    ethnicity = case_when(is.na(v131) ~ NA,
                          (v131==1) ~ "Spanish",
                          (v131==2) ~ "Quechua",
                          (v131==3) ~ "Aimara",
                          (v131==4) ~ "Other indigenous",
                          TRUE ~ "Foreigner"),
    
    
    marital_status = ifelse(hv115==0,"Single", 
                            ifelse(hv115==1|hv115==2,"Married/living_together", 
                                   ifelse(hv115>=3,"Widowed/Divorced/Separated",NA))),
    
    education_level = ifelse(s108n==0,"None/Preschool",
                             ifelse(s108n==1,"Primary",
                                    ifelse(s108n==2,"Secondary",
                                           ifelse(s108n>=3&s108n<6,"Higher",NA)))),
    
    education_level_partner = case_when(is.na(v701) ~ NA,
                                        (v701==0) ~ "None",
                                        (v701==1) ~ "Primary",
                                        (v701==2) ~ "Secondary",
                                        (v701==8) ~ "Don´t know"),
    
    know_syphilis = case_when((s815aa==0) ~ "No",
                              (s815aa==1) ~ "Yes"),
    
    know_other_sti = (s815ab+s815ac+s815ad+s815ae+s815ax), know_other_sti=case_when(know_other_sti==0~"None",
                                                                                    know_other_sti==1~"At least one",
                                                                                    know_other_sti>=2~"More than one"),
    
    know_sti_symptoms = (s816aa+s816ab+s816ab+s816ad+s816ae+s816af+s816ag+s816ah+s816ai+s816aj+s816ak+s816al+s816aw), 
    know_sti_symptoms = case_when(know_sti_symptoms==0~"NO",
                                  know_sti_symptoms==1~"At least one",
                                  know_sti_symptoms>=2~"More than one"),
    
    emotional_violence = factor(d104, levels = c(0,1), labels = c("No", "Yes")),
    
    minor_physical_violence = factor(d106, levels = c(0,1), labels = c("No", "Yes")),
    
    severe_physical_violence = factor(d107, levels = c(0,1), labels = c("No", "Yes")),
    
    sexual_physical_violence = factor(d108, levels = c(0,1), labels = c("No", "Yes")), 
    
    physical_violence_pregnancy_partner = factor(d118a, levels = c(0,1), labels = c("No", "Yes")),
    
    physical_violence_pregnancy = factor(d118y, levels = c(0,1), labels = c("Yes","No")),
    
    assistance_violence_sought = factor(d119y, levels = c(0,1), labels = c("Yes","No")),
    
    diagnosted_sti_12months = factor(v763a, levels = c(0,1,8), labels = c("No","Yes","Dont Know")),
    
    hiv_screening_last12months = factor(qs603, levels = c(0,1,8), labels = c("No","Yes","Dont Know")),
    
    have_sti_symptoms = case_when((v763b==1 & v763c==0)~"Only sore/ulcer",
                                  (v763b==1 & v763c==8)~"Only sore/ulcer",
                                  (v763b==0 & v763c==1)~"Only flow",
                                  (v763b==8 & v763c==1)~"Only flow",
                                  (v763b==1 & v763c==1)~"Both",
                                  ((v763b==0 & v763c==0) | (v763b==8 & v763c==8) | (v763b==8 & v763c==0) | (v763b==0 & v763c==8))~"None"),
    
    identity_1 = case_when(is.na(qs25bb) ~ NA,
                           qs25bb==7 ~ "Mixed race",
                           qs25bb==5 ~ "Afroperuvian",
                           qs25bb==6 ~ "White",
                           qs25bb==8 ~ "Other",
                           TRUE ~ "Indigenous"),
    
    identity_2 = case_when(is.na(s119d) ~ NA,
                           s119d==7 ~ "Mixed race",
                           s119d==5 ~ "Afroperuvian",
                           s119d==6 ~ "White",
                           s119d==8 ~ "Other",
                           TRUE ~ "Indigenous"),
    
    know_hiv = factor(v750, levels = c(0,1), labels = c("No", "Yes")),
    
    know_hiv_1 = factor(qs29a, levels = c(0,1), labels = c("No", "Yes")),
    
    know_hiv_2 = factor(qs601a, levels = c(0,1), labels = c("No", "Yes")),
    
    total_children_born = case_when(is.na(v201) ~ NA,
                                    v201==0 ~ "O",
                                    v201==1 ~ "1",
                                    v201==2 ~ "2",
                                    v201==3 ~ "3",
                                    v201==4 ~ "4",
                                    TRUE ~ "More than 4"),
    
    dead_children = case_when((v206==0 & v207==0) ~ "None",
                              (v206==1 | v207==1) ~ "1",
                              (v206>=2 | v207>=2) ~ "More than 1"),
    
    living_children = case_when(is.na(v218) ~ NA,
                                v218==0 ~ "O",
                                v218==1 ~ "1",
                                v218==2 ~ "2",
                                v218==3 ~ "3",
                                v218==4 ~ "4",
                                TRUE ~ "More than 4"),
    
    total_children = case_when(is.na(v219) ~ NA,
                               v219==0 ~ "O",
                               v219==1 ~ "1",
                               v219==2 ~ "2",
                               v219==3 ~ "3",
                               v219==4 ~ "4",
                               TRUE ~ "More than 4"),
    
    under_sixyears_children = case_when(is.na(v208) ~ NA,
                                        v208==0 ~ "O",
                                        v208==1 ~ "1",
                                        v208==2 ~ "2",
                                        v208>=2 ~ "More than 2"),
    
    under_fouryears_children = case_when(is.na(v238) ~ NA,
                                         v238==0 ~ "O",
                                         v238==1 ~ "1",
                                         v238==2 ~ "2",
                                         v238>=2 ~ "More than 2"),
    
    pregnant = factor(v213, levels = c(0,1), labels = c("No/not sure", "Yes")), 
    
    intended_pregnancy = case_when(is.na(v225) ~ NA,
                                   v225==0 ~ "Yes, at the moment",
                                   v225==1 ~ "No, wanted to wait more",
                                   v225==2 ~ "Did not want more children"),
   
    intended_pregnancy_2 = factor(m10, levels = c(1,2,3), labels = c("Yes, at the moment","No, wanted to wait more","Did not want more children")),
    
    abortion_stillbirth = factor(v228, levels = c(0,1), labels = c("No", "Yes")),
    
    abortion_stillbirth_months = factor(v233, levels = c (0,1,2,3,4,5,6,7,8,9)),
    
    contraceptive_method_ever = case_when(is.na(v302) ~ NA,
                                          v302==0 ~ "Never",
                                          v302==1 ~ "Other/folkloric",
                                          v302==2 ~ "Traditional",
                                          TRUE ~ "Modern"),
    
    contraceptive_method_currently = case_when(is.na(v313) ~ NA,
                                          v313==0 ~ "Never",
                                          v313==1 ~ "Folkloric",
                                          v313==2 ~ "Traditional",
                                          TRUE ~ "Modern"),
    
    age_first_sexual_intercourse = case_when((v531==98 | v531==97) ~ NA), 
    age_first_sexual_intercourse = factor(v531, levels = c(0:49)),
    
    age_partner = ifelse(v730<18,"15-17",
                         ifelse(v730>=18 & v730<30, "18-29",
                                ifelse(v730>=30,"30-49", NA))),
    
    age_partner_2 = ifelse(v730<=19,"15-19",
                           ifelse(v730>19 & v730<25, "20-24",
                                  ifelse(v730>=25&v730<30,"25-29",
                                         ifelse(v730>=30&v730<35,"30-34",
                                                ifelse(v730>=35,"35 more",NA))))),
    
    condon_use_last_partner = factor(v761, levels = c(0,1), labels = c("No", "Yes")),
    
    condon_use_penultimate_partner = factor(v761b, levels = c(0,1), labels = c("No", "Yes")),
    
    sexual_partner_1 = case_when(v766b==0 ~ "None",
                                 v766b==1 ~ "1",
                                 v766b>=2 ~ "More than 1",
                                 v766b==98~ NA),
    
    sexual_partner_2 = case_when(v836==0 ~ "None",
                                 v836==1 ~ "1",
                                 v836>=2 ~ "More than 1",
                                 v836==98~ NA),
    
    time_knowing_partner = factor(v768a, levels = c(101:399)),
    
    can_get_condoms = case_when(v769==0 ~ "No",
                                v769==1 ~ "Yes",
                                v769==2 ~ "Don't know"),
    
    treatment_advice_sti = factor(v770, levels = c(0,1), labels = c("No", "Yes")),
    
    treatment_advice_sti = factor(v785, levels = c(0,1), labels = c("No", "Yes")),
    
    age_first_sexual_partner = case_when((v830==98) ~ NA), #aquí el 98 significa "no sabe", para dejarlo numérico se consideró como NA
    age_first_sexual_partner = factor(v830, levels = c(0:97)),
    
    alcohol_prior_sexual_intercourse = case_when(is.na(v835a) ~ NA,
                                                 (v835a==0 | v835a==4) ~ "No",
                                                 (v835a==1 | v835a==2) ~ "Only one of them",
                                                 TRUE ~ "Both"),
    
    pregnancy_duration = factor(q220a, levels = c (0,1,2,3,4,5,6,7,8,9)),
    
    weight_1 = v437/10,
    
    weight_2 = ha2/10,
    
    size_1 = v438/1000,
    
    size_2 = ha3/1000,
    
    body_mass_index_1 = v445/100,
    
    body_mass_index_2 = ha40/100,
    
    hb = v453/10,
    
    hb_height_adjusted = v456/10,
    
    anemia_level = factor(v457, levels = c(4,3,2,1), labels = c("No", "Leve", "Mode", "Grave")),
    
    smoke = factor(v463a, levels = c(0,1), labels = c("No", "Yes")),
    
    number_cigarettes_lastday = case_when(is.na(v464) ~ NA,
                                          v464==0 ~ "No",
                                          v464==1 ~ "1",
                                          TRUE ~ "More than 1"),
    
    year_birth = factor(b2, levels = c(1970:2022))
    
  )

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

#write.csv(df_syphilis,"./data_final/data_syphilis.csv", row.names = F)

## Data de longitud y latitud
df_ubigeo <- df_syphilis %>% 
  select(caseid, year, departament, longitudx, latitudy)

#write.csv(df_ubigeo,"./data_final/data_ubigeo.csv", row.names = F)

#### Data de longitud y latitud (solo año 2022)

df_ubigeo_2022 <- df_syphilis %>% 
  filter(year==2022) %>% 
  select(caseid, year, departament, longitudx, latitudy)

#write.csv(df_ubigeo_2022,"./data_final/data_ubigeo_2022.csv", row.names = F)

### Uniendo con los datos de ubicación*
data_ubigeo_innovalab_2021 <- read_csv("./data_final/data_ubigeo_innovalab.csv")
data_ubigeo_2021 <- data_ubigeo_innovalab_2021 %>% 
  rename(
    DEPARTAMEN = NOMBDEP,
    PROVINCIA = NOMBPROV,
    DISTRITO = NOMBDIST
  ) %>% 
  select(caseid, year, DEPARTAMEN, PROVINCIA, DISTRITO)
    
data_ubigeo_2022 <- read_csv("./data_final/data_ubigeo_2022_innova.csv") %>% 
  select(caseid, year, DEPARTAMEN, PROVINCIA, DISTRITO)
  
data_ubigeos_final <- rbind(data_ubigeo_2021, data_ubigeo_2022) %>% 
  clean_names() %>% 
  rename(department_ubigeo = departamen)
 
### Uniendo con los datos de ubicación para obtener la ultima versión:

data_syphilis_final <- data_syphilis %>% 
  left_join(data_ubigeos_final, by = c("caseid", "year"))

#write.csv(data_syphilis_final,"./data_final/data_syphilis.csv", row.names = F)





