#basic libraries

library(curl)
library(httr)
library(janitor)
library(viridis)
library(scales)
library(XML)
library(lubridate)
library(readxl)
library(cansim)
library(tidyverse)


#use NIR_prelim
get_new_nir<-function() {
  #download.file("http://data.ec.gc.ca/data/substances/monitor/canada-s-official-greenhouse-gas-inventory/GHG_Econ_Can_Prov_Terr.csv","canada_ghg.csv")
  #download.file("http://donnees.ec.gc.ca/data/substances/monitor/canada-s-official-greenhouse-gas-inventory/GHG_Econ_Can_Prov_Terr.csv","canada_ghg.csv",mode="wb")
  #download.file("http://data.ec.gc.ca/data/substances/monitor/canada-s-official-greenhouse-gas-inventory/GHG_Econ_Can_Prov_Terr.csv","canada_ghg_2020.csv",mode="wb")
  #nir_2021<-read.csv("canada_ghg_2021_orig.csv",stringsAsFactors = F)
  
  download.file("http://data.ec.gc.ca/data/substances/monitor/canada-s-official-greenhouse-gas-inventory/GHG_Econ_Can_Prov_Terr.csv","canada_ghg_2021_dl.csv",mode="wb")
  nir_2021<-read.csv("canada_ghg_2021_dl.csv",stringsAsFactors = F)
  main_sectors<-c(
    "Oil and Gas",        
    "Electricity",                                         
    "Transportation",
    "Heavy Industry",
    "Buildings",
    "Agriculture",
    "Waste",
    "Coal Production",
    "Light Manufacturing",
    "Construction",
    "Forest Resources")
  
  new_nir<-nir_2021 %>% 
    rename(Sector=Category,Sub.sector=Sub.category,Sub.sub.sector=Sub.sub.category)%>%
    mutate(Prov=as.factor(Region),
                               Prov=fct_recode(Prov,"AB"="Alberta",
                                               "BC"="British Columbia",
                                               "NL"="Newfoundland and Labrador",
                                               "MB"="Manitoba",
                                               "SK"="Saskatchewan",
                                               "NS"="Nova Scotia",
                                               "ON"="Ontario",
                                               "NT"="Northwest Territories",
                                               "QC"="Quebec",
                                               "NU"="Nunavut",
                                               "NB"="New Brunswick",
                                               "YT"="Yukon",
                                               "PE"="Prince Edward Island",
                                               "NT & NU"="Northwest Territories and Nunavut"),
                               Prov=fct_collapse(Prov,
                                                 "TERR" = c("NT", "NU","YT","NT & NU"),
                                                "ATL" = c("NL", "NB","NS","PE")),
                               #need to collapse new subsector structure
                               sector=Sub.sub.sector,
                               sector=ifelse(sector=="",Sub.sector,sector),
                               sector=ifelse(sector=="",Sector,sector),
                               sector=ifelse(sector=="",Source,sector),
                               #fix territories inventory
                               NULL)%>%
    mutate(sector=factor(sector),
          sector=fct_collapse(sector,"Inventory Total"=
                c("Territorial Inventory Total","National Inventory Total","Territories Inventory Total","Territory Inventory Total","Provincial Inventory Total"))
           )%>%
    group_by(Year, Prov,sector) %>% summarize(GHGs=sum(as.numeric(CO2eq),na.rm = T)) %>% ungroup()%>%
    select(sector,Prov,Year,GHGs)%>%#filter(!is.na(GHGs))%>%
      mutate(sector=fct_collapse(sector,
                                 "Transportation" = c("Transport","Transportation"),
                                 "Oil Sands" = c("Oil Sands","Oil Sands (Mining, In-situ, Upgrading)"),
            "Oil Sands In Situ" = c("In-situ","Bitumen In Situ","In-situ Bitumen"),
            "Oil Sands Mining" = c("Mining and Extraction","Bitumen Mining","Oil Sands Mining and Extraction"),
              "Oil Sands Upgrading" = c("Upgrading","Bitumen Upgrading")))

  
    
  #re-order east to west
  new_nir$Prov<-factor(new_nir$Prov, levels=c("Canada" ,"BC","AB" ,"SK","MB", "ON",     "QC" ,  "ATL" ,   "TERR"  ))
  

  
  
  new_nir
  }

#here, we want to use the new inventory data
new_nir<-get_new_nir()
save(new_nir,file = "data/nir_data.Rdata")




