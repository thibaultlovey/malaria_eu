library(tidyverse)
library(here)
library(lubridate)
library(janitor)
library(eurostat)
library(sf)
library(ggtext)

read.csv(here("data","malaria_eu.csv")) %>% 
  clean_names() %>% 
  mutate(year=factor(year(years(year))),
         mosquitos=factor(mosquitos),
         across(c(greece:netherlands),~as.numeric(.))) %>% 
  mutate(across(c(greece:netherlands),~replace_na(.,0))) %>% 
  pivot_longer(-c("year","mosquitos"),names_to = "name", values_to = "case") %>% 
  mutate(name=factor(name)) %>% 
  group_by(year,mosquitos,name) %>% 
  summarise(case=sum(case)) %>% 
  mutate(mosquitos=fct_recode(mosquitos,"P. vivax"="Plasmodium vivax")) ->data_eu

SHP_0 <- get_eurostat_geospatial(resolution = 10, 
                                 nuts_level = 0, 
                             year = 2016)
EU28 <- eu_countries %>% 
  select(geo = code, name)

SHP_28 <- SHP_0 %>% 
  select(geo = NUTS_ID, geometry) %>% 
  inner_join(EU28, by = "geo") %>% 
  arrange(geo) %>% 
  st_as_sf() %>% 
  mutate(name=tolower(name)) %>% 
  left_join(data_eu,by="name") %>% 
  mutate(case=ifelse(is.na(case)==T,0,case)) %>%
  mutate(case=factor(case))
 
 
SHP_28 %>%   
ggplot() +
  geom_sf(data = SHP_28,lwd = 0) +
  aes(fill=factor(case)) +
  scale_x_continuous(limits = c(-10, 35)) +
  scale_y_continuous(limits = c(35, 65)) +
  theme_void() + 
  scale_fill_viridis_d(option="magma",direction=-1,breaks=c(10,6,3,2,1,0))+
  guides(fill=guide_legend(title="Nombre Absolu de Cas (n)")) ->figure1_a



SHP_28 %>%
  filter(mosquitos %in% c("P. falciparum","P. ovale","P. vivax"))->new_data
new_data %>% 
  ggplot() +
  geom_sf(data = na.omit(new_data),lwd = 0) +
  aes(fill=factor(case)) +
  scale_x_continuous(limits = c(-10, 35)) +
  scale_y_continuous(limits = c(35, 65)) +
  theme_void() + 
  scale_fill_viridis_d(option="magma",direction=-1,breaks=c(10,6,3,2,1,0))+
  guides(fill=guide_legend(title="Nombre Absolu de Cas (n)")) +
  facet_wrap(~mosquitos) +
  labs(caption="*Les données sur le paludisme proviennent des rapports épidémiologiques annuels de 2017 à 2020 du Centre européen de prévention et de contrôle des maladies (ECDC).")+
  theme(plot.caption = element_text(hjust = 0)) ->figure1_b
  
  
  


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

map_total <- SHP_0 %>% 
  select(geo = NUTS_ID, geometry) %>% 
  inner_join(EU28, by = "geo") %>% 
  arrange(geo) %>% 
  st_as_sf() %>% 
  mutate(name=tolower(name)) %>% 
  left_join(map_total,by="name") %>% 
  mutate(case=ifelse(is.na(case)==T,0,case)) %>%
  mutate(case=as.numeric(case)) %>%
  mutate(name=factor(name),
         geo=factor(geo)) 

map_total %>%
  ggplot() +
  geom_sf(data = map_total,lwd = 0) +
  aes(fill=case) +
  scale_x_continuous(limits = c(-10, 35)) +
  scale_y_continuous(limits = c(35, 65)) +
  theme_void() + 
  scale_fill_viridis_c(option="magma",direction=-1,limits=c(0,25),name = "Nombre Absolu de Cas (n)")->figure1_a
  

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
  mutate(case=as.numeric(case)) %>%
  mutate(name=factor(name),
         geo=factor(geo)) %>%
  filter(mosquitos %in% c("P. falciparum","P. ovale","P. vivax"))+
  labs()

map_mosquitos %>% 
  ggplot() +
  geom_sf(data = na.omit(map_mosquitos),lwd = 0) +
  aes(fill=case) +
  scale_x_continuous(limits = c(-10, 35)) +
  scale_y_continuous(limits = c(35, 65)) +
  theme_void() + 
  scale_fill_viridis_b(option="magma",direction=-1,limits=c(0,25),breaks=c(0,1,2,3,4,5,10,15,20,25),name = "Nombre Absolu de Cas (n)")+
  facet_wrap(~mosquitos) +
  labs(caption="*Les données sur le paludisme proviennent des rapports épidémiologiques annuels de 2017 à 2020 du Centre européen de prévention et de contrôle des maladies (ECDC).")+
  theme(plot.caption = element_text(hjust = 0))->figure1_b

library(ggpubr)
figure2=ggarrange(figure1_a,figure1_b,ncol = 1, nrow = 2,common.legend = T,legend="right",labels = c("A", "B"))
ggsave(plot=figure2,here("figure1.pdf"),dpi=300)
######################################################
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

map_total %>%
  ggplot() +
  geom_sf(data = map_total,lwd = 0) +
  aes(fill=case) +
  scale_x_continuous(limits = c(-10, 35)) +
  scale_y_continuous(limits = c(35, 65)) +
  theme_void() + 
  scale_fill_viridis_d(option="magma",limits=c(21,11,8,7,6,5,3,2,1,0),name = "Nombre Absolu de Cas [n]")+
  theme_bw()->figure1_a


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

library(ggpubr)
figure2=ggarrange(figure1_a,figure1_b,ncol = 1, nrow = 2,common.legend = T,legend="right",labels = c("A", "B"))
ggsave(plot=figure2,here("figure1.pdf"),dpi=300)