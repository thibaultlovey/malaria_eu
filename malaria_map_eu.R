library(tidyverse)
library(here)
library(lubridate)
library(janitor)
library(eurostat)
library(sf)
library(ggtext)

#load map europe
SHP_0 <- get_eurostat_geospatial(resolution = 10, 
                                 nuts_level = 0, 
                                 year = 2016)
EU28 <- eu_countries %>% 
  select(geo = code, name)
read.csv(here("data","malaria_eu.csv")) %>% 
  clean_names() %>% 
  mutate(year=factor(year(years(year))),
         mosquitos=factor(mosquitos),
         across(c(greece:netherlands),~as.numeric(.))) %>% 
  mutate(across(c(greece:netherlands),~replace_na(.,0))) %>% 
  pivot_longer(-c("year","mosquitos"),names_to = "name", values_to = "case") %>% 
  mutate(name=factor(name)) %>% 
  group_by(name) %>% 
  summarise(case=sum(case)) ->map_total
#prepare data total
map_total <- SHP_0 %>% 
  select(geo = NUTS_ID, geometry) %>% 
  inner_join(EU28, by = "geo") %>% 
  arrange(geo) %>% 
  st_as_sf() %>% 
  mutate(name=tolower(name)) %>% 
  left_join(map_total,by="name") %>% 
  mutate(case=ifelse(is.na(case)==T,0,case)) %>%
  mutate(case=factor(case)) %>%
  mutate(name=factor(name),
         geo=factor(geo)) 
#map total
map_total %>%
  ggplot() +
  geom_sf(data = map_total,lwd = 0) +
  aes(fill=case) +
  scale_x_continuous(limits = c(-10, 35)) +
  scale_y_continuous(limits = c(35, 65)) +
  theme_void() + 
  scale_fill_viridis_d(option="magma",limits=c(21,11,8,7,6,5,3,2,1,0),name = "Nombre Absolu de Cas [n]")+
  theme_bw()->figure1_a
#prepare data msoquitos
read.csv(here("data","malaria_eu.csv")) %>% 
  clean_names() %>% 
  mutate(year=factor(year(years(year))),
         mosquitos=factor(mosquitos),
         across(c(greece:netherlands),~as.numeric(.))) %>% 
  mutate(across(c(greece:netherlands),~replace_na(.,0))) %>% 
  pivot_longer(-c("year","mosquitos"),names_to = "name", values_to = "case") %>% 
  mutate(name=factor(name)) %>% 
  group_by(mosquitos,name) %>% 
  summarise(case=sum(case)) %>% 
  mutate(mosquitos=fct_recode(mosquitos,"P. vivax"="Plasmodium vivax")) ->map_mosquitos  
map_mosquitos <- SHP_0 %>% 
  select(geo = NUTS_ID, geometry) %>% 
  inner_join(EU28, by = "geo") %>% 
  arrange(geo) %>% 
  st_as_sf() %>% 
  mutate(name=tolower(name)) %>% 
  left_join(map_mosquitos,by="name") %>% 
  mutate(case=ifelse(is.na(case)==T,0,case)) %>%
  mutate(case=as.factor(case)) %>%
  mutate(name=factor(name),
         geo=factor(geo)) %>%
  filter(mosquitos %in% c("P. falciparum","P. ovale","P. vivax"))
#map mosquitos
map_mosquitos %>% 
  ggplot() +
  geom_sf(data = na.omit(map_mosquitos),lwd = 0) +
  aes(fill=case) +
  scale_x_continuous(limits = c(-10, 35)) +
  scale_y_continuous(limits = c(35, 65)) +
  scale_fill_viridis_d(option="magma",limits=c(21,11,8,7,6,5,3,2,1,0),name = "Nombre Absolu de Cas (n)")+
  facet_wrap(~mosquitos) +
  labs(caption="*Les données sur le paludisme proviennent des rapports épidémiologiques annuels de 2017 à 2020 du Centre européen de prévention\n et de contrôle des maladies (ECDC).")+
  theme_bw()+
  theme(plot.caption = element_text(hjust = 0))->figure1_b
#combine and export pdf
library(ggpubr)
figure2=ggarrange(figure1_a,figure1_b,ncol = 1, nrow = 2,common.legend = T,legend="right",labels = c("A", "B"))
ggsave(plot=figure2,here("figure1.pdf"),dpi=300)