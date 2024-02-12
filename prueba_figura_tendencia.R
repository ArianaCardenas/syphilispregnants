syphilis <- read_csv("Data_final/data_syphilis.csv")

df<-
  syphilis %>% 
  mutate(
    syphilis_screening_2 = ifelse(is.na(syphilis_screening),"Missing",
                                   ifelse(syphilis_screening == 1,"Yes",
                                          ifelse(syphilis_screening == 0 , "No",syphilis_screening))),
    
    syphilis_screening_2 = as.factor(syphilis_screening_2)
  ) %>% 
  group_by(year) %>% 
  nest() %>% 
  mutate(
    datasvy = map(.x = data,
                  .f = ~svydesign(id =~ v001, strata =~ v022, weights=~v005, data=.x))
  )
options(survey.lonely.psu="remove")
```


```{r}
syphilis_df<-
  df %>% 
  mutate(
    
    syphilis = map(.x = datasvy,
              .f = ~svymean(~as.factor(syphilis_screening_2), design = .x, na.rm = T)),
    
    sypprop = map(.x = syphilis,
                  .f = ~as.numeric(.x)),
    
    sypci = map(.x= syphilis,
                .f = ~confint(.x) %>% 
                  as.data.frame() %>%
                  rownames_to_column("var"))
    
  ) %>% 
  
  unnest(c(sypci,sypprop)) %>% 
  
  mutate(
    var = case_when(var == "as.factor(syphilis_screening_2)Missing" ~ "Missing",
                    var == "as.factor(syphilis_screening_2)YES" ~ "Yes",
                    var == "as.factor(syphilis_screening_2)NO" ~ "No")
  )  



syphilis_df %>% 
  select(year,var,sypprop,`2.5 %`,`97.5 %`) %>% 
  mutate(
    across(.cols = sypprop:`97.5 %`, .fns = ~round(.*100,1))
  ) %>% 
  DT::datatable()
```

### grafico HIV 1
```{r}
syphi<-
  ggplot(syphilis_df %>%
           filter(var != "Missing"), aes(x = year, y = sypprop*100, ymin = `2.5 %`*100, ymax = `97.5 %`*100, group = var)) + 
  geom_line(size = 1.2, aes(col = var))+
  geom_ribbon(aes(fill = var), alpha = 0.1)+
  #geom_errorbar(aes(color=var), width = 0.2, alpha = 0.35) +
  xlab("Years") +
  ylab("Prop. %")+
  scale_x_continuous(expand = c(0, 0), breaks = c(2014:2022), limits = c(2014,2022.1))+
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

print(syphi)

ggsave("figure_curva_syp_screening.png", dpi = 300, width = 10)