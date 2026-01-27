#Creating variables for GeoDa

#Run 6_Bivariate_maps_enaho.Rmd

departamentos2 = departamentos %>%
  select(c("NOMBDEP", "geometry"))

#POVERTY 

collapse_syph_pov = syphilis_poverty %>%
  select(-c("geometry")) %>%
  group_by(NOMBDEP) %>%
  summarise(across(everything(), mean), .groups = 'drop')

collapse_syph_pov = collapse_syph_pov %>%
  left_join(departamentos2, by = "NOMBDEP")

st_write(collapse_syph_pov, "geoda/poverty_cong.gpkg")


#EDUCATION

collapse_syph_edu = syphilis_education %>%
  select(-c("geometry")) %>%
  group_by(NOMBDEP) %>%
  summarise(across(everything(), mean), .groups = 'drop')

collapse_syph_edu = collapse_syph_edu %>%
  left_join(departamentos2, by = "NOMBDEP")

st_write(collapse_syph_edu, "geoda/education_cong.gpkg")


#PRENATAL CARE

collapse_syph_ANC = syphilis_precare %>%
  select(-c("geometry", 
            "DEff.prenatal_visitno", 
            "DEff.prenatal_visityes")) %>% #excluded bc the name was long (not accepted as shp file)
  group_by(NOMBDEP) %>%
  summarise(across(everything(), mean), .groups = 'drop')

collapse_syph_ANC = collapse_syph_ANC %>%
  left_join(departamentos2, by = "NOMBDEP")


st_write(collapse_syph_ANC, "geoda/precare_cong.gpkg")
