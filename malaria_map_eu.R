library(tidyverse)
library(here)
library(lubridate)
library(janitor)
library(eurostat)
library(sf)
here()
read.csv(here("data","malaria_eu.csv")) %>% 
  clean_names() %>% 
  mutate(year=factor(year(years(year))),
         mosquitos=factor(mosquitos),
         across(c(greece:netherlands),~as.numeric(.))) %>% 
  mutate(across(c(greece:netherlands),~replace_na(.,0))) %>% 
  pivot_longer(-c("year","mosquitos"),names_to = "name", values_to = "case") %>% 
  mutate(name=factor(name)) %>% 
  group_by(year,mosquitos,name) %>% 
  summarise(case=sum(case)) -> data_eu

  

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
  guides(fill=guide_legend(title="Number of Cases"))
  
  








facet_wrap(~year, nrow = 2)+
  scale_fill_viridis_c(option="A")
  
?scale_fill_viridis_c

ggplot(data = SHP_28) +  
  ggplot2::geom_sf(data = SHP_28,lwd = 0) +
  aes(fill=case) +
  ggplot2::coord_sf(expand = FALSE)+ 
  scale_fill_viridis_c(trans="log10") +
  labs(title = "Symptoms by Unsubregion (n)",  
       subtitle = "2000-2022",  
       fill = "Number of cases reportes (n)") +  
  theme_bw() +  
  theme(plot.title =  
          element_text(hjust = 0.5)) +  
  theme(plot.subtitle =
          element_text(hjust = 0.5))  








world <- map_data("world")
europe <- subset(world, region %in% c("Albania", "Andorra", "Armenia", "Austria", "Azerbaijan",
                                      "Belarus", "Belgium", "Bosnia and Herzegovina", "Bulgaria",
                                      "Croatia", "Cyprus", "Czechia","Denmark","Estonia","Finland", 
                                      "France","Georgia", "Germany", "Greece","Hungary","Iceland", 
                                      "Ireland", "Italy","Kazakhstan", "Kosovo", "Latvia","Liechtenstein", 
                                      "Lithuania", "Luxembourg","Malta","Moldova","Monaco","Montenegro",
                                      "Macedonia", "Netherlands","Norway","Poland","Portugal","Romania",
                                      "Russia","San Marino","Serbia","Slovakia","Slovenia","Spain",
                                      "Sweden","Switzerland","Turkey","Ukraine","UK","Vatican"))

ggplot(data = europe, aes(x = long, y = lat, group = group)) + 
              geom_polygon(fill = "white", color = "black") +
              theme_void() +
              coord_fixed(ratio=1.6, xlim = c(-50, 80))          
            
            
            un_names<-read.csv(here("raw_data/un_name.csv"),sep=";") %>% #un_subregion
              clean_names() %>% 
              select(sub_region_name) %>% distinct() %>%
              mutate(sub_region_name=make_clean_names(sub_region_name)) %>%
              filter(!row_number() %in% c(5)) 
            
            data=respi_clean[,9:74] %>% 
              select_if(colnames(.) %in% c(un_names$sub_region_name)) %>% 
              summarize_all(funs(sum)) %>%
              pivot_longer(everything(),names_to = "un.regionsub.name", values_to = "case")
            
            library(giscoR)
            world<-gisco_get_countries(year = "2020") %>% view()
            un_name<-gisco_countrycode[,c(1,12)]
            world<-merge(world,un_name,by="ISO3_CODE")
            
            world %>% select(6,7) %>% mutate(un.regionsub.name=make_clean_names(un.regionsub.name)) %>%
              mutate(un.regionsub.name=str_replace_all(un.regionsub.name,"[:digit:]","")) %>% 
              mutate(un.regionsub.name=gsub("_$","",un.regionsub.name)) %>% 
              left_join(data,by="un.regionsub.name") %>% 
              mutate(case=replace_na(case,0)) -> world_subregion
            
            ggplot(data = world_subregion) +  
              ggplot2::geom_sf(data = world_subregion,lwd = 0) +
              aes(fill=case) +
              ggplot2::coord_sf(expand = FALSE)+ 
              scale_fill_viridis_c(trans="log10") +
              labs(title = "Symptoms by Unsubregion (n)",  
                   subtitle = "2000-2022",  
                   fill = "Number of cases reportes (n)") +  
              theme_bw() +  
              theme(plot.title =  
                      element_text(hjust = 0.5)) +  
              theme(plot.subtitle =
                      element_text(hjust = 0.5))          
            
            