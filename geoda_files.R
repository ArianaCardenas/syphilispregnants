#Creating variables for GeoDa

#Run 6_Bivariate_maps_enaho.Rmd

### CONGENITAL SYPHILIS

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


############ MATERNAL SYPHILIS

#EDUCATION

collapse_syph_edu_mat = syphilis_education %>%
  select(-c("geometry")) %>%
  group_by(NOMBDEP) %>%
  summarise(across(everything(), mean), .groups = 'drop')

collapse_syph_edu_mat = collapse_syph_edu_mat %>%
  left_join(departamentos2, by = "NOMBDEP")

st_write(collapse_syph_edu_mat, "geoda/education_mat.gpkg")

#POVERTY

collapse_syph_pov_mat = syphilis_poverty %>%
  select(-c("geometry")) %>%
  group_by(NOMBDEP) %>%
  summarise(across(everything(), mean), .groups = 'drop')

collapse_syph_pov_mat = collapse_syph_pov_mat %>%
  left_join(departamentos2, by = "NOMBDEP")

st_write(collapse_syph_pov_mat, "geoda/poverty_mat.gpkg")

#PRENATAL CARE

collapse_syph_anc_mat = syphilis_precare %>%
  select(-c("geometry")) %>%
  group_by(NOMBDEP) %>%
  summarise(across(everything(), mean), .groups = 'drop')

collapse_syph_anc_mat = collapse_syph_anc_mat %>%
  left_join(departamentos2, by = "NOMBDEP")

st_write(collapse_syph_anc_mat, "geoda/precare_mat.gpkg")


##### After creating the figures in GeoDa

library(magick) # To import maps
library(ggplotify) #To change the ong files to graphic objects


###MATERNAL SYPHILIS

# Replace "path/to/your/image.jpg" with your actual file path
education_mat_plot <- image_read("Figures/GeoDa/education_mat.png")
education_mat_plot <- ggplotify::as.ggplot(education_mat_plot)
poverty_mat_plot <- image_read("Figures/GeoDa/poverty_mat.png")
poverty_mat_plot <- ggplotify::as.ggplot(poverty_mat_plot)
precare_mat_plot <- image_read("Figures/GeoDa/precare_mat.png")
precare_mat_plot <- ggplotify::as.ggplot(precare_mat_plot)

# Map of the three

library(gridExtra)
library(patchwork)

# Saving figure 1:


LISA_maternal <- (poverty_mat_plot | legend_poverty | 
                         education_mat_plot | legend_education | 
                         precare_mat_plot | legend_precare) + 
  plot_layout(widths = c(3, 0.6, 3, 0.6, 3, 0.6)) + 
  plot_annotation(
    title = 'b. Maternal Syphilis') 


LISA_maternal

ggsave("Figures/LISA_maternal.png", 
       plot = LISA_maternal, width = 10, height = 3, dpi = 300)


###CONGENITAL SYPHILIS

# Replace "path/to/your/image.jpg" with your actual file path
education_cong_plot <- image_read("Figures/GeoDa/education_cong.png")
education_cong_plot <- ggplotify::as.ggplot(education_cong_plot)
poverty_cong_plot <- image_read("Figures/GeoDa/poverty_cong.png")
poverty_cong_plot <- ggplotify::as.ggplot(poverty_cong_plot)
precare_cong_plot <- image_read("Figures/GeoDa/precare_cong.png")
precare_cong_plot <- ggplotify::as.ggplot(precare_cong_plot)

# Map of the three
# Saving figure:


LISA_congenital <- (poverty_cong_plot | legend_poverty | 
                    education_cong_plot | legend_education | 
                    precare_cong_plot | legend_precare) + 
  plot_layout(widths = c(3, 0.6, 3, 0.6, 3, 0.6)) + 
  plot_annotation(
    title = 'a. Congenital Syphilis') 


LISA_congenital

ggsave("Figures/LISA_congenital.png", 
       plot = LISA_congenital, width = 10, height = 3, dpi = 300)




