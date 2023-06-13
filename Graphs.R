#Gráficos*

library(tidyverse)
library(survey)
library(gtsummary)
library(ggsci)
library(DT)
library(patchwork)
library(cowplot)
library(sf)
library(rcartocolor)

df_syphilis <- read_csv("Data_final/data_syphilis.csv")

#Gráfico 1*

df<-
  df_syphilis %>% 
  mutate(
    syphilis_screening2= ifelse(is.na(syphilis_screening),"Missing",
                                   ifelse(syphilis_screening == 1,"Yes",
                                          ifelse(syphilis_screening == 0 | syphilis_screening == 8 , "No",syphilis_screening))),
    
    syphilis_screening2 = as.factor(syphilis_screening2)
  ) %>% 
  group_by(year) %>% 
  nest() %>% 
  mutate(
    datasvy = map(.x = data,
                  .f = ~svydesign(id =~ v001, strata =~ v022, weights=~ v005, data=.x))
  )
options(survey.lonely.psu="remove")

syphilis_df<-
  df %>% 
  mutate(
    
    syphilis = map(.x = datasvy,
              .f = ~svymean(~as.factor(syphilis_screening2), design = .x, na.rm = T)),
    
    syphilisprop = map(.x = syphilis,
                  .f = ~as.numeric(.x)),
    
    syphilisci = map(.x= syphilis,
                .f = ~confint(.x) %>% 
                  as.data.frame() %>%
                  rownames_to_column("var"))
    
  ) %>% 
  
  unnest(c(syphilisci,syphilisprop)) %>% 
  
  mutate(
    var = case_when(var == "as.factor(syphilis_screening2)Missing" ~ "Missing",
                    var == "as.factor(syphilis_screening2)Yes" ~ "Yes",
                    var == "as.factor(syphilis_screening2)No" ~ "No")
  )  

syphilis_df %>% 
  select(year,var,syphilisprop,`2.5 %`,`97.5 %`) %>% 
  mutate(
    across(.cols = syphilisprop:`97.5 %`, .fns = ~round(.*100,1))
  ) %>% 
  DT::datatable()

### grafico HIV 1

syphilis<-
  ggplot(syphilis_df %>%
           filter(var != "Missing"), aes(x = year, y = syphilisprop*100, ymin = `2.5 %`*100, ymax = `97.5 %`*100, group = var)) + 
  geom_line(linewidth = 1.2, aes(col = var))+
  geom_ribbon(aes(fill = var), alpha = 0.1)+
  #geom_errorbar(aes(color=var), width = 0.2, alpha = 0.35) +
  xlab("Years") +
  ylab("Prop. %")+
  scale_x_continuous(expand = c(0, 0), breaks = c(2010:2021), limits = c(2010,2021.1))+
  ggsci::scale_fill_futurama(alpha = 0.8)+
  ggsci::scale_color_futurama(alpha = 0.8)+
  
  theme_bw()+
  
  theme(
    axis.title = element_text(face ="bold", size = 11),
    legend.text = element_text(size = 8, face = "bold"),
    axis.text = element_text(face = "bold", family = "Arial"),
    axis.text.x = element_text( size = 9),
    legend.position = "top",
    legend.title = element_blank(),
    axis.line = element_line(colour = "black", linewidth = 1),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    panel.grid = element_blank()
  )

ggsave("grafico1a.png", dpi = 300, width = 10)



