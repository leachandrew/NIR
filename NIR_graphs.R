#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#basic libraries
options(scipen = 999)
library(ggthemes)
library(zoo)
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
library(cowplot)
library(ggrepel)
library(wesanderson)
library(ggsci)


#NIR data
get_new_nir<-function() {
  #download.file("http://data.ec.gc.ca/data/substances/monitor/canada-s-official-greenhouse-gas-inventory/GHG_Econ_Can_Prov_Terr.csv","canada_ghg.csv")
  #download.file("http://donnees.ec.gc.ca/data/substances/monitor/canada-s-official-greenhouse-gas-inventory/GHG_Econ_Can_Prov_Terr.csv","canada_ghg.csv",mode="wb")
  #download.file("http://data.ec.gc.ca/data/substances/monitor/canada-s-official-greenhouse-gas-inventory/GHG_Econ_Can_Prov_Terr.csv","canada_ghg_2020.csv",mode="wb")
  #nir_2021<-read.csv("canada_ghg_2021_orig.csv",stringsAsFactors = F)
  
  #download.file("http://data.ec.gc.ca/data/substances/monitor/canada-s-official-greenhouse-gas-inventory/GHG_Econ_Can_Prov_Terr.csv","canada_ghg_2021_dl.csv",mode="wb")
  #nir_2021<-read.csv("canada_ghg_2021_dl.csv",stringsAsFactors = F)
  
  #download.file("https://data.ec.gc.ca/data/substances/monitor/canada-s-official-greenhouse-gas-inventory/B-Economic-Sector/GHG_Econ_Can_Prov_Terr.csv","canada_ghg_2023_dl.csv",mode="wb")
  #nir_2023<-read.csv("canada_ghg_2023_dl.csv",stringsAsFactors = F)
  
  #download.file("https://data-donnees.ec.gc.ca/data/substances/monitor/canada-s-official-greenhouse-gas-inventory/B-Economic-Sector/EN_GHG_Econ_Can_Prov_Terr.csv","canada_ghg_2022_dl.csv",mode="wb")
  #nir_2022<-read.csv("canada_ghg_2022_dl.csv",stringsAsFactors = F)
  
  
  #download.file("https://data-donnees.ec.gc.ca/data/substances/monitor/canada-s-official-greenhouse-gas-inventory/B-Economic-Sector/EN_GHG_Econ_Can_Prov_Terr.csv","canada_ghg_2023_dl.csv",mode="wb")
  #nir_2023<-read.csv("canada_ghg_2023_dl.csv",stringsAsFactors = F)
  
  #download.file(
  #  "https://data-donnees.az.ec.gc.ca/api/file?path=/substances%2Fmonitor%2Fcanada-s-official-greenhouse-gas-inventory%2FB-Economic-Sector%2FEN_GHG_Econ_Can_Prov_Terr.csv",
  #  destfile = "canada_ghg_2023_dl2.csv",mode="wb")
  
  #download.file(
  #  "https://data-donnees.az.ec.gc.ca/api/file?path=/substances%2Fmonitor%2Fcanada-s-official-greenhouse-gas-inventory%2FB-Economic-Sector%2FEN_GHG_Econ_Can_Prov_Terr.csv",
  #  destfile = "canada_ghg_2024.csv",mode="wb")
  
  
    nir_full<-read.csv("canada_ghg_2024.csv",stringsAsFactors = F)
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

    nir_full<-nir_full %>% 
    #rename(Sector=Category,Sub.sector=Sub.category,Sub.sub.sector=Sub.sub.category)%>%
    mutate(prov=as.factor(Region),
           prov=fct_recode(prov,"AB"="Alberta",
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
           prov=fct_collapse(prov,
                             "TERR" = c("NT", "NU","YT","NT & NU"),
                             "ATL" = c("NL", "NB","NS","PE")),
           prov=fct_relevel(prov,c("Canada" ,"BC","AB" ,"SK","MB", "ON",     "QC" ,  "ATL" ,   "TERR"  )),
           CO2eq=as.numeric(CO2eq)/1000 #convert to Mt
           )%>%clean_names()
           
           save(nir_full,file="nir_full.RData")
           
           
           
           new_nir<-nir_full %>% mutate(
             #need to collapse new subsector structure
            sector=case_when(
             (sector!="")&(sub_sector=="")&(sub_sub_sector=="") ~ sector, #both subs are blank
             (sub_sector!="")&(sub_sub_sector=="") ~ sub_sector, #just sub_sub is blank
             (sub_sub_sector!="") ~ sub_sub_sector, #sub_sub exists
             (sector=="") ~ source
           ),
           #fix territories inventory
           NULL)%>%
    mutate(sector=factor(sector),
           sector=fct_collapse(sector,"Inventory Total"=
                                 c("Territories Inventory Total","Territory Inventory Total","Territorial Inventory Total","Provincial Inventory Total"))
    )%>%
    group_by(year, prov,sector) %>% summarize(GHGs=sum(as.numeric(co2eq),na.rm = T)) %>% ungroup()%>%
    select(sector,prov,year,GHGs)%>%#filter(!is.na(GHGs))%>%
    mutate(sector=fct_collapse(sector,
                               "Transportation" = c("Transport","Transportation"),
                               "Oil Sands" = c("Oil Sands","Oil Sands (Mining, In-situ, Upgrading)"),
                               "Oil Sands In Situ" = c("In-situ","Bitumen In Situ","In-situ Bitumen"),
                               "Oil Sands Mining" = c("Mining and Extraction","Bitumen Mining","Oil Sands Mining and Extraction"),
                               "Oil Sands Upgrading" = c("Upgrading","Bitumen Upgrading"),
                               "Total, Canada" = c("Total","GHG TOTAL","National Inventory Total")))
  
  
  
  #re-order east to west
  new_nir$prov<-factor(new_nir$prov, levels=c("Canada" ,"BC","AB" ,"SK","MB", "ON",     "QC" ,  "ATL" ,   "TERR"  ))
  
  
  
  
  new_nir
}




#here, we want to use the new inventory data
#new_nir<-get_new_nir()
#save(new_nir,file = "data/nir_data.Rdata")

load(file = "data/nir_data.Rdata")

#get population data
get_pop_data<-function(){
  #Cansim 17-10-0005-01
  pop_data<-get_cansim(1710000501)%>% filter(Gender=="Total - gender",`Age group`=="All ages")%>%
    clean_names()%>%
    mutate(prov=as.factor(geo),
           code=fct_recode(prov,"AB"="Alberta",
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
                           "NT & NU"="Northwest Territories and Nunavut",
                           "NT & NU"="Northwest Territories including Nunavut"),
           prov_name=geo,
           prov_pop=value,year=ref_date,
           code=fct_collapse(code,
                             "TERR" = c("NT", "NU","YT","NT & NU"),
                             "ATL" = c("NL", "NB","NS","PE")))%>%
    select(year,prov_name,code,prov_pop)%>% group_by(year,code)%>%summarize(prov_pop=sum(prov_pop))%>%
    ungroup()
  
  save(file="data/pop_data.Rdata",pop_data)

  #population projections
  
#  pop_proj<-get_cansim("1710011501")

}

#get_pop_data()
load(file = "data/pop_data.Rdata")




#get gdp data
get_gdp_data<-function(){
  #Cansim 36-10-0402-02
  gdp_data<-get_cansim("36-10-0222-01")%>% filter(Estimates=="Final consumption expenditure",
                                                  Prices=="Chained (2017) dollars",
                                                  GEO!="Outside Canada")%>%
    mutate(prov=as.factor(GEO),
           code=fct_recode(prov,"AB"="Alberta",
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
                           "NT & NU"="Northwest Territories and Nunavut",
                           "NT & NU"="Northwest Territories including Nunavut"),
           prov_name=GEO,
           code=fct_collapse(code,
                             "TERR" = c("NT", "NU","YT","NT & NU"),
                             "ATL" = c("NL", "NB","NS","PE")))%>%
    select(year=REF_DATE,prov_name,code,prov_gdp=VALUE)%>% group_by(year,code)%>%summarize(prov_gdp=sum(prov_gdp,na.rm=T))
  
  save(file="data/gdp_data.Rdata",gdp_data)
}


get_gdp_data()
load(file = "data/gdp_data.Rdata")


#create provincial data file
NIR_data<-filter(new_nir,prov!="Canada")%>% mutate(year=as.numeric(year))

#section out a sample of provincial totals (used later)
prov_samp<-filter(NIR_data,grepl("otal",sector))%>%select(-1) %>% rename(Prov_GHGs=GHGs)


#create 2030 comps
NIR_data<-NIR_data %>% group_by(prov) %>% mutate(GHGs_2005_net_30=.7*sum(GHGs*(sector=="Inventory Total")*(year==2005)),
                                                 GHGs_2005_net_40=.7*sum(GHGs*(sector=="Inventory Total")*(year==2005)),
                                                 GHGs_2005_net_45=.7*sum(GHGs*(sector=="Inventory Total")*(year==2005)))

                                                                       




#top level sectors - other sector levels are subsectors or sub-sub-sectors
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

attn_sectors<-c(
  "Oil and Gas",        
  "Electricity",                                         
  "Transportation",
  "Heavy Industry",
  "Buildings",
  "Agriculture",
  "Waste"
  #"Coal Production",
  #"Light Manufacturing",
  #"Construction",
  #"Forest Resources"
)


#prov_ghgs <- NIR_data%>% filter(sector %in% main_sectors)%>%group_by(prov,year)%>% summarize(GHGs=sum(GHGs,na.rm = T))
#save(prov_ghgs,file = "data/prov_ghgs.Rdata")
load(file = "data/prov_ghgs.Rdata")


NIR_natl<-filter(new_nir,prov=="Canada")
NIR_natl$year<-as.numeric(NIR_natl$year)
NIR_natl<-merge(filter(NIR_natl,sector %in% main_sectors),pop_data,by.x=c("prov","year"),by.y=c("code","year"))
NIR_natl$per_cap<-NIR_natl$GHGs/NIR_natl$prov_pop

NIR_natl<-NIR_natl %>% mutate(
  sector=as.factor(sector),
  sector=fct_other(sector,keep = attn_sectors)
)%>%group_by(sector,year,prov)%>%
  summarize(prov_pop=first(prov_pop),
            GHGs=sum(GHGs),
            per_cap=GHGs/prov_pop)

#save(NIR_natl,file = "data/nir_natl.Rdata")




#create totals for indexing
totals<-new_nir %>% filter(sector%in% c("Total, Canada","Inventory Total"))%>% select(-sector)%>% group_by(prov) %>% mutate(
  GHGs_2005_net_30=.7*sum(((year==2005))*GHGs),
  GHGs_2005_net_40=.6*sum(((year==2005))*GHGs),
  GHGs_2005_net_45=.55*sum(((year==2005))*GHGs),
  GHGs_2005=sum(((year==2005))*GHGs)
) %>% ungroup() %>%
  left_join(gdp_data%>%mutate(year=as.integer(year)),by=c("year","prov"="code"))%>%
  left_join(pop_data%>%mutate(year=as.integer(year)),by=c("year","prov"="code"))%>%
  mutate(ghg_per_cap=GHGs*10^6/prov_pop,
         ghg_per_gdp=GHGs/prov_gdp,
         gdp_per_cap=prov_gdp/prov_pop
  )%>%
  group_by(prov)%>%
  mutate(index_gdp=prov_gdp/first(prov_gdp)*100,
         index_pop=prov_pop/first(prov_pop)*100,
         index_ghg=GHGs/first(GHGs)*100,
         index_ghg_cap=ghg_per_cap/first(ghg_per_cap)*100,
         index_ghg_gdp=ghg_per_gdp/first(ghg_per_gdp)*100,
  ) %>% ungroup()%>%
  select(prov,year,index_gdp,index_pop,index_ghg,index_ghg_cap,index_ghg_gdp)%>%
  pivot_longer(-c(prov,year),names_to = "index",values_to = "value")%>%
  mutate(index=gsub("index_","",index),index=toupper(index),index=as_factor(index),
         index=fct_recode(index,"GHG emissions per capita"="GHG_CAP"),
         index=fct_recode(index,"GHG emissions per unit GDP"="GHG_GDP"),
         index=fct_recode(index,"Population"="POP"),
         index=fct_recode(index,"Inventory GHG emissions"="GHG"),
         index=fct_recode(index,"GDP (chained 2012 dollars)"="GDP")
  )




ggplot(totals%>% filter(prov%in% c("BC","AB","ON","QC","Canada")))+
  geom_line(aes(year,value,colour=index,group=index),linetype=1,size=1)+
  facet_wrap( ~ prov,nrow = 1)+
  #scale_color_viridis("",discrete=TRUE,guide_legend(NULL),option="E")+
  #scale_fill_viridis("",discrete=TRUE,option="E")+
  #scale_fill_manual("",values = my_palette,guide = "legend")+
  #scale_fill_grey("",guide = "legend",start = 0.9,end=0.05)+
  scale_x_continuous(breaks=pretty_breaks())+
  scale_colour_manual("",values=c("black",colors_ua10()[-c(3,5)]),guide = "legend")+
  guides(color=guide_legend(nrow =1,byrow=FALSE))+
  blake_theme()+theme(
    panel.spacing = unit(.5,"lines"),
    legend.position = "bottom",
    legend.margin=margin(c(.05,0,.05,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
    plot.caption = element_text(size = 12, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 20, face = "italic"),
    #panel.grid.minor = element_blank(),
    text = element_text(size = 20,face = "bold"),
    axis.text.y = element_text(size = 14,face = "bold", colour="black"),
    #axis.text.x = element_blank(),
    axis.text.x = element_text(size = 14, colour = "black", angle = 90,hjust = -1),
    strip.text.x = element_text(size = 14, colour = "black", angle = 0),
    axis.title.y = element_text(size = 16,face = "bold", colour="black"),
  )+
  labs(x=NULL,y="Indexed value (1990=100)",
       title="1990-2022 indexed provincial GHG emissions, GDP, population, and GHG emissions per unit GDP and per capita",
       #subtitle="Excluding Electricity",
       caption="Sources: Canada's National Inventory Report (2024), and Tables 17-10-0005-01 and 36-10-0402-02 (chained $2017) via Statistics Canada. Graph by @andrew_leach.",
       NULL
  )
ggsave("images/index_ghgs.png",width=15,height=8,bg="white",dpi=300)



theme_nir<-function(){
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(.05,0,.05,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
    plot.caption = element_text(size = 10, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 20, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 20,face = "bold"),
    axis.text.y = element_text(size = 14,face = "bold", colour="black"),
    #axis.text.x = element_blank(),
    axis.text.x = element_text(size = 14, colour = "black", angle = 90,vjust=0.5),
    strip.text.x = element_text(size = 14, colour = "black", angle = 0),
    axis.title.y = element_text(size = 14,face = "bold", colour="black"),
  )
}
  

labs_nir<-function(graph_title="GHG Emissions Data",graph_years="1990-2022"){
  labs(caption="Source: Environment Canada National Inventory Report (2024), graph by @andrew_leach",
       title=paste(graph_title,graph_years,sep=", "))

}

ggplot(filter(NIR_natl,sector!="National Inventory Total"))+
  geom_area(aes(year,GHGs,group=sector,fill=sector),color="black",size=0.5)+
  scale_fill_manual("",values = plot_palette,guide = "legend")+
  scale_colour_manual("",values="black",guide = "legend")+
  scale_x_continuous(breaks=pretty_breaks(n=16), expand = c(0,0))+
  guides(fill=guide_legend(nrow =1,byrow=FALSE),color=guide_legend(nrow =1,byrow=FALSE))+
  theme_nir()+
  labs_nir("Canadian GHG Emissions","1990-2023")+
  labs(x=NULL,y=expression('Annual GHG Emissions  '*'(Mt CO'[2]*'e)'),
      )
ggsave("images/nir_natl.png",width=16,height=9,bg="white",dpi=300)


ggplot(filter(NIR_natl,sector!="National Inventory Total"))+
  geom_area(aes(year,per_cap*10^6,group=sector,fill=sector),color="black",size=0.5)+
  scale_fill_manual("",values = plot_palette,guide = "legend")+
  scale_colour_manual("",values="black",guide = "legend")+
  scale_x_continuous(breaks=pretty_breaks(n=16), expand = c(0,0))+
  guides(fill=guide_legend(nrow =1,byrow=FALSE),color=guide_legend(nrow =1,byrow=FALSE))+
  theme_nir()+
  labs_nir("Canadian GHG Emissions Per Capita","1990-2023")+
  labs(x=NULL,y=expression('Annual Per-Capita Emissions  '*'(tonnes CO'[2]*'e per person)'))
       
ggsave("images/natl_per_cap.png",height = 9,width = 16,bg="white",dpi=300)



oil_sectors<-c("Conventional Heavy Oil Production","Conventional Light Oil Production","Frontier Oil Production",
               "Oil Sands Mining","Oil Sands In Situ","In-Situ","Oil Sands Upgrading")

oil_sands_sectors<-c("Oil Sands Mining","Oil Sands In Situ","Oil Sands Upgrading","In-Situ")

nir_oil<-ggplot(new_nir %>% filter(prov=="Canada",sector%in%oil_sectors)%>%
                  mutate(sector=as_factor(sector),
                         sector=fct_recode(sector,"In-Situ Oil Sands"="In-Situ"),
                         sector=fct_relevel(sector,"Conventional Light Oil Production")))+
  geom_area(aes(year,GHGs,group=sector,fill=sector),position="stack",color="black",size=0.25)+
  scale_fill_manual("",values = plot_palette,guide = "legend")+
  scale_colour_manual("",values="black",guide = "legend")+
  guides(fill=guide_legend(nrow =1,byrow=FALSE),color=guide_legend(nrow =1,byrow=FALSE))+
  scale_x_continuous(breaks=pretty_breaks(n=16), expand = c(0,0))+
  theme_nir()+
  labs_nir("Canadian GHG Emissions from Crude Oil and Oil Sands Production","1990-2023")+
  labs(x=NULL,y=expression('Annual GHG Emissions  '*'(Mt CO'[2]*'e)'))
nir_oil
ggsave("images/nir_oil.png",width=16,height=9,bg="white",dpi=300)


process_nir <- function(data, 
                        prov_values, 
                        other_val="OTHER") {
  data %>%
    # Handle provinces
    mutate(prov = as_factor(prov),
           prov = fct_other(prov, keep = prov_values, other_level = other_val)) %>%
    # Group and summarize
    group_by(prov, year, sector) %>%
    summarize(GHGs = sum(GHGs, na.rm = TRUE), .groups = "drop")
}



nir_oil_prov<-
#test<-
  new_nir %>% filter(prov!="Canada",sector%in%c(oil_sectors,"Natural Gas Production and Processing"))%>%
                  mutate(sector=as_factor(sector),
                         sector=fct_relevel(sector,"Conventional Light Oil Production"),
                         sector=fct_relevel(sector,"Natural Gas Production and Processing"),
                         sector=fct_recode(sector,"Natural Gas"="Natural Gas Production and Processing"),
                         sector=fct_recode(sector,"In-Situ Oil Sands"="In-Situ"),
                         sector=fct_recode(sector,"Conventional Light Oil"="Conventional Light Oil Production"),
                         sector=fct_recode(sector,"Conventional Heavy Oil"="Conventional Heavy Oil Production"),
                         sector=fct_recode(sector,"Frontier Oil"="Frontier Oil Production"),
                                              )%>%
                process_nir(prov_values = c("BC","AB","SK","ATL"),other_val = "ALL OTHERS")%>%
   ggplot()+
  geom_area(aes(year,GHGs,group=sector,fill=sector),position="stack",color="black",size=0.25)+
  #scale_fill_manual("",values = c(colors_tableau10(),colors_tableau10_medium()),guide = "legend")+
  scale_fill_manual("",values=plot_palette)+
  facet_grid( ~ prov)+
  scale_x_continuous(breaks=pretty_breaks(), expand = c(0,0))+
  scale_y_continuous(breaks=pretty_breaks(), expand = c(0,0))+
  expand_limits(y=140)+
  guides(fill=guide_legend(nrow =1,byrow=TRUE))+
  theme_nir()+
  labs_nir(graph_title = "Emissions from Oil Production by Province")+
    labs(x=NULL,y=expression('Annual GHG Emissions  '*'(Mt CO'[2]*'e)'))
nir_oil_prov
ggsave("images/nir_oil_prov.png",width=16,height=9,bg="white",dpi=300)




nir_oil_ab<-ggplot(new_nir %>% filter(prov=="AB",sector%in%oil_sectors)%>%
                  mutate(sector=as_factor(sector),
                         sector=fct_relevel(sector,"Conventional Light Oil Production")))+
  geom_area(aes(year,GHGs,group=sector,fill=sector),position="stack",color="black",size=0.25)+
  scale_fill_manual("",values=plot_palette)+
  scale_x_continuous(breaks=pretty_breaks(n=16), expand = c(0,0))+
  scale_y_continuous(breaks=pretty_breaks(n=5), expand = c(0,0))+
  guides(fill=guide_legend(nrow =2,byrow=TRUE))+
  theme_nir()+
  labs_nir(graph_title = "Emissions from Oil Production by Province")+
  labs(x=NULL,y=expression('Annual GHG Emissions  '*'(Mt CO'[2]*'e)'))
nir_oil_ab
ggsave("images/nir_AB_oil.png",width=12,height=5,bg="white",dpi=300)

nir_oil_prov<-
  #test<-
  new_nir %>% filter(prov!="Canada",sector%in%c(oil_sectors,"Natural Gas Production and Processing","Petroleum Refining"))%>%
  mutate(sector=as_factor(sector),
         sector=fct_relevel(sector,"Conventional Light Oil Production"),
         sector=fct_relevel(sector,"Natural Gas Production and Processing"),
         sector=fct_relevel(sector,"Petroleum Refining",after=Inf),
         sector=fct_recode(sector,"In-Situ Oil Sands"="In-Situ"),
         sector=fct_recode(sector,"Conventional Light Oil"="Conventional Light Oil Production"),
         sector=fct_recode(sector,"Conventional Heavy Oil"="Conventional Heavy Oil Production"),
         sector=fct_recode(sector,"Frontier Oil"="Frontier Oil Production"),
  )%>%
  process_nir(prov_values = c("BC","AB","SK","ATL"),other_val = "ALL OTHERS")%>%
  ggplot()+
  geom_area(aes(year,GHGs,group=sector,fill=sector),position="stack",color="black",size=0.25)+
  scale_fill_manual("",values=plot_palette)+
  facet_grid( ~ prov)+
  scale_x_continuous(breaks=pretty_breaks(), expand = c(0,0))+
  scale_y_continuous(breaks=pretty_breaks(), expand = c(0,0))+
  expand_limits(y=140)+
  guides(fill=guide_legend(nrow =1,byrow=TRUE))+
  theme_nir()+
  labs_nir(graph_title = "Emissions from Oil and Gas Production and Processing by Province")+
  labs(x=NULL,y=expression('Annual GHG Emissions  '*'(Mt CO'[2]*'e)'))
nir_oil_prov
ggsave("images/nir_oil_gas_prov.png",width=18,height=10,bg="white",dpi=300)



nir_oil_ab<-
  new_nir %>% filter(prov=="AB",sector%in%c(oil_sectors,"Natural Gas Production and Processing","Petroleum Refining"))%>%
  mutate(sector=as_factor(sector),
         sector=fct_relevel(sector,"Conventional Light Oil Production"),
         sector=fct_relevel(sector,"Natural Gas Production and Processing"),
         sector=fct_relevel(sector,"Petroleum Refining",after=Inf),
         sector=fct_recode(sector,"In-Situ Oil Sands"="In-Situ"),
         sector=fct_recode(sector,"Conventional Light Oil"="Conventional Light Oil Production"),
         sector=fct_recode(sector,"Conventional Heavy Oil"="Conventional Heavy Oil Production"),
         sector=fct_recode(sector,"Frontier Oil"="Frontier Oil Production"),
  )%>%
  ggplot()+
  geom_area(aes(year,GHGs,group=sector,fill=sector),position="stack",color="black",size=0.25)+
  scale_fill_manual("",values=plot_palette)+
  #facet_grid( ~ prov)+
  scale_x_continuous(breaks=pretty_breaks(n=16), expand = c(0,0))+
  scale_y_continuous(breaks=pretty_breaks(), expand = c(0,0))+
  expand_limits(y=140)+
  guides(fill=guide_legend(nrow =1,byrow=TRUE))+
  theme_nir()+
  labs_nir(graph_title = "Emissions from Oil and Gas Production and Processing in Alberta")+
  labs(x=NULL,y=expression('Annual GHG Emissions  '*'(Mt CO'[2]*'e)'))
nir_oil_ab
ggsave("images/nir_AB_oil_gas.png",width=18,height=10,bg="white",dpi=300)


#find the largest sectors

major_sectors<-NIR_natl %>% group_by(sector) %>% summarize(max_ghg=max(GHGs)) %>% arrange(-max_ghg) %>% head(6) %>% select(sector)
#use forcats to pick 5 largest sectors

large_sec<-NIR_data %>% filter(sector %in% main_sectors,sector!="National Inventory Total") %>% 
  mutate(sector=fct_other(as.factor(sector),keep = as.character(major_sectors$sector))) %>%
  group_by(sector,prov,year) %>% summarize(GHGs=sum(GHGs),GHGs_2005_net_30=sum(GHGs_2005_net_30))

#Environment Canada Emissions Projection Data

build_proj_data<-function(){

#file_loc<-"http://data.ec.gc.ca/data/substances/monitor/canada-s-greenhouse-gas-emissions-projections/Current-Projections-Actuelles/GHG-GES/detailed_GHG_emissions_GES_detaillees.csv"
#download.file(file_loc,destfile="data/ec_projections_2023.csv",mode = "wb")


#file_loc<-"https://data-donnees.az.ec.gc.ca/api/file?path=/substances%2Fmonitor%2Fcanada-s-greenhouse-gas-emissions-projections%2FCurrent-Projections-Actuelles%2FGHG-GES%2Fdetailed_GHG_emissions_GES_detaillees.csv"
#download.file(file_loc,destfile="data/ec_projections_2023_new.csv",mode = "wb")

#file_loc<-"https://data-donnees.az.ec.gc.ca/api/file?path=%2Fsubstances%2Fmonitor%2Fcanada-s-greenhouse-gas-emissions-projections%2FCurrent-Projections-Actuelles%2FGHG-GES%2Fdetailed_GHG_emissions_GES_detaillees.csv"
#download.file(file_loc,destfile="data/ec_projections_2024.csv",mode = "wb")
  
#End 2023 Projections Data
proj_data_2024<-read.csv("data/ec_projections_2024.csv",skip = 0,na = "-",fileEncoding = "Latin1", check.names = F) %>% 
  clean_names()%>%select(-c(2,4,6,8,10,12,14,28)) %>%
  pivot_longer(-c(1:7),names_to = "year",values_to = "emissions")%>%
  mutate(year=gsub("x","",year),emissions=gsub("-","0",emissions),year=as.numeric(year),emissions=as.numeric(emissions))
names(proj_data_2024)<-c("region","scenario","sector","subsector_level_1","subsector_level_2","subsector_level_3","unit","year","emissions")
  
  
  
#End 2023 Projections Data
proj_data_2023_new<-read.csv("data/ec_projections_2023_new.csv",skip = 0,na = "-",fileEncoding = "Latin1", check.names = F) %>% 
  clean_names()%>%select(-c(2,4,6,8,10,12,14,28)) %>%
  pivot_longer(-c(1:7),names_to = "year",values_to = "emissions")%>%
  mutate(year=gsub("x","",year),emissions=gsub("-","0",emissions),year=as.numeric(year),emissions=as.numeric(emissions))
names(proj_data_2023_new)<-c("region","scenario","sector","subsector_level_1","subsector_level_2","subsector_level_3","unit","year","emissions")


#2023 Projections Data
proj_data_2023<-read.csv("data/ec_projections_2023.csv",skip = 0,na = "-",fileEncoding = "Latin1", check.names = F) %>% 
  clean_names()%>%select(-c(2,4,6,8,10,12,14,28)) %>%
  pivot_longer(-c(1:7),names_to = "year",values_to = "emissions")%>%
  mutate(year=gsub("x","",year),emissions=gsub("-","0",emissions),year=as.numeric(year),emissions=as.numeric(emissions))
names(proj_data_2023)<-c("region","scenario","sector","subsector_level_1","subsector_level_2","subsector_level_3","unit","year","emissions")


#2022 Projections Data
proj_data_2022<-read.csv("data/ec_projections_2022.csv",skip = 0,na = "-",fileEncoding = "Latin1", check.names = F) %>% 
  clean_names()%>%select(-c(2,4,6,8,10,12,14,28)) %>%
  pivot_longer(-c(1:7),names_to = "year",values_to = "emissions")%>%
  mutate(year=gsub("x","",year),emissions=gsub("-","0",emissions),year=as.numeric(year),emissions=as.numeric(emissions))
names(proj_data_2022)<-c("region","scenario","sector","subsector_level_1","subsector_level_2","subsector_level_3","unit","year","emissions")

#2020 projections data
#file_loc<-"https://data-donnees.ec.gc.ca/data/substances/monitor/canada-s-greenhouse-gas-emissions-projections/Previous-Projections-Precedentes/2020/GHG-GES/detailed_GHG_emissions_GES_detaillees.csv"
#download.file(file_loc,destfile="data/ec_projections_2020.csv",mode = "wb")

proj_data_2020<-read.csv("data/ec_projections_2020.csv",skip = 0,na = "-",fileEncoding = "Latin1") %>% 
  clean_names()%>%select(-c(2,4,6,8,10,12,14,28)) %>%
  pivot_longer(-c(1:7),names_to = "year",values_to = "emissions")%>%
  mutate(year=gsub("x","",year),emissions=gsub("-","0",emissions),year=as.numeric(year),emissions=as.numeric(emissions))
names(proj_data_2020)<-c("region","scenario","sector","subsector_level_1","subsector_level_2","subsector_level_3","unit","year","emissions")


#load 2018 detailed projections data

#file_loc<-"http://data.ec.gc.ca/data/substances/monitor/canada-s-greenhouse-gas-emissions-projections/Previous-Projections-Precedentes/2018/GHG-GES/detailed_GHG_emissions_GES_detaillees.csv"
#download.file(file_loc,destfile="data/ec_projections_2018.csv",mode = "wb")
proj_data_2018<-read.csv("data/ec_projections_2018.csv",skip = 0,na = "-",fileEncoding = "Latin1") %>% 
  clean_names()%>%select(-c(2,4,6,8,10,12,14,28)) %>%
  pivot_longer(-c(1:7),names_to = "year",values_to = "emissions")%>%
  mutate(year=gsub("x","",year),emissions=gsub("-","0",emissions),year=as.numeric(year),emissions=as.numeric(emissions))

names(proj_data_2018)<-c("region","scenario","sector","subsector_level_1","subsector_level_2","subsector_level_3","unit","year","emissions")


#load 2019 detailed projections data
#file_loc<-"http://data.ec.gc.ca/data/substances/monitor/canada-s-greenhouse-gas-emissions-projections/Previous-Projections-Precedentes/2019/GHG-GES/detailed_GHG_emissions_GES_detaillees.csv"
#download.file(file_loc,destfile="data/ec_projections_2019.csv",mode = "wb")

proj_data_2019<-
  read_csv("data/ec_projections_2019.csv",skip = 0,na = "-",col_types = cols(.default = "c")) %>%
  clean_names()%>%select(-c(2,4,6,8,10,12,14,28)) %>%
  mutate_if(is.double,as.character()) %>%
  pivot_longer(-c(1:7),names_to = "year",values_to = "emissions")%>%
  mutate(year=gsub("x","",year),emissions=gsub("-","0",emissions),year=as.numeric(year),emissions=as.numeric(emissions))
names(proj_data_2019)<-c("region","scenario","sector","subsector_level_1","subsector_level_2","subsector_level_3","unit","year","emissions")


proj_data<-proj_data_2024 %>% bind_rows(proj_data_2023_new,proj_data_2023,proj_data_2022,proj_data_2020,proj_data_2019,proj_data_2018)%>%#bind_rows(proj_data_2022,proj_data_2020,proj_data_2019,proj_data_2018)%>%
  filter(scenario!="NIR 2018",scenario!="NIR 2019",scenario!="NIR 2020",scenario!="NIR 2021",sector!="Total")

proj_data<-proj_data %>% filter(!sector%in%c("International Emissions","n/a","WCI Credits"))%>%
  mutate(prov=as.factor(region),
         prov=fct_recode(prov,"AB"="Alberta",
                         "BC"="British Columbia",
                         "NL"="Newfoundland and Labrador",
                         "MB"="Manitoba",
                         "SK"="Saskatchewan",
                         "NS"="Nova Scotia",
                         "ON"="Ontario",
                         "NT"="Northwest Territory",
                         "NT"="Northwest Territories",
                         "QC"="Quebec",
                         "NU"="Nunavut",
                         "NB"="New Brunswick",
                         "YT"="Yukon Territory",
                         "YT"="Yukon",
                         "PE"="Prince Edward Island",
                         "NU"="Nunavut"
         ),
         prov=fct_collapse(prov,
                           "TERR" = c("NT", "NU","YT","NT & NU")
                           ,"ATL" = c("NL", "NB","NS","PE")
                           #,"OTHER ATL" = c("NL", "NB","PE")
         ))%>%
  select(year,prov,region,scenario,emissions,sector)%>% group_by(year,prov,sector,scenario)%>%summarize(emissions=sum(emissions,na.rm = T)) %>%ungroup()


#strip out nir from this csv, build new NIR

proj_data<-proj_data%>% 
  filter(!grepl("NIR 2024",scenario))%>%
  filter(!grepl("NIR 2023",scenario))%>%
  filter(!grepl("NIR 2022",scenario))%>%
  bind_rows(test=NIR_data %>%
    filter(sector %in% main_sectors)%>%
    mutate(sector=fct_other(sector,
    keep=c("Agriculture","Buildings","Electricity","Heavy Industry","Oil and Gas","Transportation"),                        
    other_level = "Waste and Others"
    ))%>%
  select(sector,year=year,prov=prov,emissions=GHGs)%>%
  group_by(year,prov,sector) %>% summarise(emissions=sum(emissions))%>% ungroup()%>%
  mutate(scenario="NIR 2024")
  )



#set terr agriculture equal to zero in years it doesn't appear


terr_ag_fix<-as_tibble(x=seq(1990,2021,1))%>% rename(year=value) %>% left_join(proj_data %>% filter(prov=="TERR",sector=="Agriculture",scenario=="NIR 2023")%>%
                                                                                 select(year,prov,sector,emissions))%>%
  mutate(emissions=na.fill(emissions,0),prov="TERR",sector="Agriculture",scenario="NIR 2024")


proj_data<-proj_data %>% filter(!((prov=="TERR")&(sector=="Agriculture") &(scenario=="NIR 2024"))) %>% bind_rows(terr_ag_fix)


#terr<-proj_data %>% filter(prov=="TERR", scenario %in% c(inventory,project_case),sector=="Transportation")

#figure out allocation rules from LLM Thesis.
get_pop_proj<-function(){
  #load population projections
  get_pop_proj<-get_cansim("17-10-0057-01")
  pop_proj<-get_pop_proj%>%clean_names()%>%
    filter(projection_scenario=="Projection scenario M1: medium-growth",
           gender=="Total - gender",
           age_group=="All ages"
           )%>%
    select(year=ref_date,prov=geo,pop=value)
    pop_proj<-pop_proj%>%
           mutate(prov=as_factor(prov),pop=as.numeric(gsub(",","",pop)),
           prov=fct_recode(prov,"AB"="Alberta",
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
                           "NU"="Nunavut"
           ),
           prov=fct_collapse(prov,
                             "TERR" = c("NT", "NU","YT","NT & NU")
                             ,"ATL" = c("NL", "NB","NS","PE")
                             #,"OTHER ATL" = c("NL", "NB","PE")
           ))%>%
    select(year,prov,pop)%>% group_by(year,prov)%>%
    summarize(pop=sum(pop)*1000, .groups = "drop")
    save(pop_proj,file="pop_proj.Rdata")
}  
 #get_pop_proj()
 load(file="pop_proj.Rdata")
 pop_merge<-pop_data%>%select(year=year,prov=code,pop=prov_pop) %>%
 bind_rows(pop_proj%>%filter(year<=2035,year>2024))%>%mutate(year=as.numeric(year))

alloc<-proj_data%>% left_join(pop_merge)%>%
  filter(scenario %in% c("NIR 2024","2024 Reference Case"))%>%
  filter(((scenario=="NIR 2024")& (year<2022))|((scenario=="2024 Reference Case")& (year>2022)))



per_cap_2030<-pop_merge %>% filter(year==2030,prov!="Canada")%>%
  mutate(per_cap_2030=pop/sum(pop)*425)%>%
  ungroup()%>%
  select(prov,per_cap_2030)

last_5 <- proj_data %>% filter(prov!="Canada")%>%
  group_by(prov) %>% 
  filter(scenario %in% c("NIR 2024"),
         year %in%seq(2022-5,2022))%>%
  group_by(prov,year)%>% summarize(ghgs=sum(emissions))%>%
  summarize(ghgs=mean(ghgs))%>%
  ungroup()%>%
  mutate(last_5=ghgs/sum(ghgs)*425)%>%
  select(prov,last_5)


basis_2005 <- proj_data %>% filter(prov!="Canada")%>%
  group_by(prov) %>% 
  filter(scenario %in% c("NIR 2024"),
         year %in%c(2005))%>%
  group_by(prov,year)%>% summarize(ghgs=sum(emissions))%>%
  ungroup()%>%
  mutate(basis_2005=ghgs/sum(ghgs)*425)%>%
  select(prov,basis_2005)


#proj_data<-proj_data%>% filter(sector!="Total")


#for the plot data, we want NIR pre-2020 and projections post-2020
#proj_data$prov<-factor(proj_data$prov,levels = (c("Canada","BC","AB","SK","MB","ON","QC","NB", "NS", "PE", "NL","ATL","OTHER ATL","TERR")))

proj_data<-
  proj_data %>% left_join(basis_2005)%>% left_join(last_5)%>% left_join(per_cap_2030)

#proj_data<-proj_data %>% filter(!((scenario=="2021 Reference Case") & (year<=2020)))

proj_data<-
  #testing<-
  proj_data %>% left_join(pop_merge,by=c("prov","year"))


proj_data<-proj_data %>%#filter(prov!="TERR")%>%
  mutate(prov=factor(prov, levels=c("Canada" ,"BC","AB" ,"SK","MB", "ON",     "QC" ,  "ATL" ,   "TERR"  )))

proj_data
}

proj_data<-build_proj_data()
save(proj_data,file = "data/proj_data.Rdata")
load(file = "data/proj_data.Rdata")


proj_graph<-function(){
  theme_minimal()+
  #theme_fivethirtyeight()+
  theme(
  legend.direction = "vertical", legend.box = "vertical", 
  legend.position = "right",
  legend.margin=margin(c(.05,0,.05,0),unit="cm"),
  legend.text = element_text(colour="black", size = 12),
  plot.caption = element_text(size = 10, face = "italic",hjust=0),
  plot.title = element_text(size=16,face = "bold"),
  plot.subtitle = element_text(size = 10),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  text = element_text(size = 20,face = "bold"),
  axis.text.y = element_text(size = 12,face = "bold", colour="black"),
  #axis.text.x = element_blank(),
  axis.text.x = element_text(size = 12, colour = "black", angle = 90,hjust=0.5,vjust=0.5),
  strip.text.x = element_text(size = 12, colour = "black", angle = 0),
  axis.title.y = element_text(size = 14,face = "bold", colour="black"),
)
  
}

inv_subtitle<-paste("2024 National Inventory (1990-2022)",sep="")
proj_subtitle<-paste(inv_subtitle," emissions and 2024 Additional Measures Scenario emissions projections (2023-2035, lighter fill)",sep="")
proj_caption<-"Source: Environment and Climate Change Canada. Graph by @andrew_leach."
proj_plain<- labs(x=NULL,y=expression('Annual Emissions  '*'(MtCO'[2]*'e)'),
                 #title="Canadian GHG Emissions by Province",
                 #subtitle=proj_subtitle,
                 #caption=proj_caption,
                 NULL)

proj_labs<-function(title="Canadian GHG Emissions by Province",subtitle=proj_subtitle,caption=proj_caption){
  labs(x=NULL,y=expression('Annual Emissions  '*'(MtCO'[2]*'e)'),
       title=title,
       subtitle=subtitle,
       caption=caption,
       NULL)
}



inventory="NIR 2024"
project_case<-"2024 Additional Measures Scenario"
nir_year<-2022

library(wesanderson)

plot_palette<-wes_palette("BottleRocket2", 7, type = "continuous")

#proj_data<-proj_data %>% filter(year<=2030)
prov_plot<-  
  ggplot(data = proj_data %>% filter(scenario%in% c(inventory,project_case) & prov !="Canada"))+
  geom_area(data=filter(proj_data,emissions>0 & scenario%in% c(inventory,project_case) & prov !="Canada" & year<=nir_year),
            aes(year,emissions,fill=sector),color="black",position = "stack",size=0.5,alpha=.8)+
  facet_wrap( ~ prov,nrow = 1)+
  scale_x_continuous(breaks=pretty_breaks())+
  scale_fill_manual("",values=plot_palette)+
  scale_colour_manual("",values="black",guide = "legend")+
  proj_graph()+
  proj_labs(title=NULL,subtitle = NULL)+
  theme(legend.position = c(.9,.8))+
  NULL
prov_plot
ggsave(filename = "images/inventory_prov_plain.png",plot = prov_plot, dpi = 300,width=14, height=7,bg="white")


prov_plot+
  scale_fill_grey("",start = 1, end = 0)
ggsave(filename = "images/inventory_prov_bw.png", dpi = 300,width=14, height=7,bg="white")

prov_plot+
  proj_labs(subtitle = inv_subtitle,title="Canadian GHG Emissions by Province and Sector")
  ggsave("images/inventory_prov.png",dpi = 300,width=14, height=7,bg="white")


prov_plot+
  geom_area(data = proj_data %>% filter(scenario%in% c(inventory,project_case) & prov !="Canada" )%>%
                        filter((scenario==project_case & year>nir_year)|(scenario==inventory & year<=nir_year)),
                      aes(year,emissions,fill=sector),color="black",position = "stack",size=0.1,alpha=.4)+
  proj_labs(title="Canadian GHG Emissions and Projections by Province")
ggsave("images/inventory_proj.png",dpi = 300,width=14, height=7,bg="white")


prov_plot+geom_area(data = proj_data %>% filter(scenario%in% c(inventory,project_case) & prov !="Canada" )%>%
                      filter((scenario==project_case & year>nir_year)|(scenario==inventory & year<=nir_year)),
                    aes(year,emissions,fill=sector),color="black",position = "stack",size=0.1,alpha=.4)+
  scale_fill_grey("",guide = "legend",start = 0.85,end=0)+
  proj_labs(title="Canadian GHG Emissions and Projections by Province")
  ggsave("images/inventory_proj_bw.png",dpi = 300,width=14, height=7,bg="white")


  prov_plot+geom_area(data = proj_data %>% filter(scenario%in% c(inventory,project_case) & prov !="Canada" )%>%
                        filter((scenario==project_case & year>nir_year)|(scenario==inventory & year<=nir_year)),
                      aes(year,emissions,fill=sector),color="black",position = "stack",size=0.1,alpha=.4)+
  geom_line(aes(year,basis_2005,lty=str_wrap("Pro-rated share of 2030 target based on 2005 GHGs\n",width = 20)),color="black",size=1.05)+
  geom_line(aes(year,per_cap_2030,lty=str_wrap("Equal per capita share of 2030 target",width = 20)),color="black",size=1.05)+
  geom_line(aes(year,last_5,lty=str_wrap("Pro-rated share of 2030 target based on 2018-22 GHGs",width = 20)),color="black",size=1.05)+
  scale_linetype_manual("",values=c("11","31","22"))+
  scale_fill_grey("",guide = "legend",start = 0.85,end=0)+
  theme(legend.position = c(.9,.65))+ 
  guides(linetype = guide_legend(
    keywidth = 2,  # Adjust the key width for linetype legend
    keyheight=3
  ),fill = guide_legend(
    keywidth = 1,  # Adjust the key width for linetype legend
    keyheight=1
  ))+
    proj_labs(title="Canadian GHG Emissions and Projections by Province",
              subtitle=paste(proj_subtitle,"\nDashed/dotted horizontal lines show three hypothetical emissions target allocation rules, with Canada's 2030 target assumed to be 425 Mt.",sep="")
              )+
  
    NULL
ggsave("images/inventory_proj_targets.png",dpi = 300,width=14, height=7,bg="white")


# per capita

pc_prov_plot<-  
  ggplot(data = proj_data %>% filter(scenario%in% c(inventory,project_case) & prov !="Canada")%>%
                         filter(prov!="TERR"))+
  geom_area(data=filter(proj_data,emissions>0 & scenario%in% c(inventory,project_case) & prov !="Canada" & year<=2022,prov!="TERR"),
            aes(year,emissions/pop*10^6,fill=sector),color="black",position = "stack",size=0.5,alpha=.8)+
  facet_wrap( ~ prov,nrow = 1)+
  scale_x_continuous(breaks=pretty_breaks())+
  scale_fill_manual("",values=plot_palette)+
  #scale_fill_manual("",values=colors_ua10())+
  scale_colour_manual("",values="black",guide = "legend")+
  proj_graph()+
  proj_labs(title=NULL,subtitle = NULL)+
  theme(legend.position = c(.9,.8))+
  NULL

pc_prov_plot+
  scale_fill_brewer("",palette = "Greys")
ggsave(filename = "images/inventory_pc_plain.png", dpi = 300,width=14, height=7,bg="white")

pc_prov_plot+
  
proj_labs(subtitle=paste(inv_subtitle," emissions per capita based on Statisitics Canada population estimates", sep=""),
       title="Canadian GHG Emissions Per Capita by Province",
       NULL)+
  labs(y=expression('Annual Emissions  '*'(tCO'[2]*'e per capita)'),)
ggsave("images/inventory_prov_pc.png",dpi = 220,width=14, height=7,bg="white")


pc_prov_plot+geom_area(data = proj_data %>% filter(scenario%in% c(inventory,project_case) & prov !="Canada" & prov!= "TERR")%>%
                      filter((scenario==project_case & year>nir_year)|(scenario==inventory & year<=nir_year)),
                    aes(year,emissions/pop*10^6,fill=sector),color="black",position = "stack",size=0.1,alpha=.4)+
  proj_labs(subtitle=paste(proj_subtitle," per capita (StatCan population estimates)", sep=""),
            title="Canadian GHG Emissions Per Capita by Province",
            NULL)+
  labs(y=expression('Annual Emissions  '*'(tCO'[2]*'e per capita)'),)+
NULL

ggsave("images/inventory_proj_pc.png",dpi = 300,width=14, height=7,bg="white")


#sector plots

plot_palette<-wes_palette("BottleRocket2", 8, type = "continuous")

sector_plot<-  
  ggplot()+
  geom_area(data=filter(proj_data,emissions>0 & scenario%in% c(inventory,project_case) & prov !="Canada" & year<=nir_year),
            aes(year,emissions,fill=prov),color="black",position = "stack",size=0.5,alpha=.8)+
  facet_wrap( ~ sector,nrow = 1)+
  scale_x_continuous(breaks=pretty_breaks())+
  scale_fill_manual("",values=plot_palette)+
  scale_colour_manual("",values="black",guide = "legend")+
  proj_graph()+
  proj_labs(title=NULL,subtitle = NULL)+
  theme(legend.position = c(.94,.75))+
  NULL
sector_plot
ggsave(filename = "images/inventory_sector_plain.png",plot = sector_plot, dpi = 300,width=14, height=7,bg="white")


sector_plot+
  scale_fill_brewer("",palette = "Greys")
ggsave(filename = "images/inventory_sector_bw.png", dpi = 300,width=14, height=7,bg="white")


sector_plot+
labs(title="Canadian GHG Emissions by Sector",
     subtitle=inv_subtitle)
ggsave("images/inventory_sector.png",dpi = 300,width=14, height=7,bg="white")

sector_proj<-sector_plot+geom_area(data = proj_data %>% filter(scenario%in% c(inventory,project_case) & prov !="Canada" )%>%
            filter((scenario==project_case & year>nir_year)|(scenario==inventory & year<=nir_year)),
          aes(year,emissions,fill=prov),color="black",position = "stack",size=0.1,alpha=.4)+
  proj_labs(title = "Canadian GHG Emissions and Projections By Sector and Province")
sector_proj
ggsave("images/sector_proj.png",dpi = 300,width=14, height=7,bg="white")


sector_proj+
  scale_fill_grey("",guide = "legend",start = 0.85,end=0)
ggsave("images/sector_proj_bw.png",dpi = 300,width=14, height=7,bg="white")


sector_proj_graph<-function(data_sent,inventory_sent=inventory,project_sent=project_case,cut_year=nir_year,
                            sector_sent="Oil and Gas",file_sent="images/oil_proj.png",dpi=300,
                            palette=plot_palette){
#data_sent<-proj_data
cases<-c(inventory_sent,project_sent)
data_sent%>% filter(scenario %in% cases & prov !="Canada" & sector==sector_sent)%>%
             filter((scenario==project_case & year>cut_year)|(scenario==inventory & year<=cut_year))%>%
  
ggplot()+
  geom_area(aes(year,ifelse(scenario==inventory,emissions,NA),fill=prov),color="black",position = "stack",linewidth=0.1)+
  geom_area(aes(year,emissions,fill=prov),color="black",position = "stack",linewidth=0.1,alpha=0.4)+
  scale_x_continuous(breaks=pretty_breaks(n=15))+
  #scale_fill_viridis("",discrete=TRUE,option="mako")+
  #scale_fill_viridis("",discrete=TRUE,option="turbo")+
  scale_fill_manual("",values=palette)+
  
  scale_colour_manual("",values="black",guide = "legend")+
  proj_graph()+
  proj_labs()+
  theme(axis.text.x = element_text(size = 12, colour = "black",angle = 0, hjust=0.5,vjust=0.5))+
  labs(title=paste("Canadian GHG Emissions from ",sector_sent,sep=""))
ggsave(file_sent,dpi = 300,width=14, height=7,bg="white")

}

#sector_proj_graph(proj_data,palette = grey.colors(8))
sector_proj_graph(proj_data,sector_sent = "Oil and Gas",file_sent = "images/oil_proj.png")
sector_proj_graph(proj_data,sector_sent = "Transportation",file_sent = "images/transp_proj.png")
sector_proj_graph(proj_data,sector_sent = "Electricity",file_sent = "images/power_proj.png")
sector_proj_graph(proj_data,sector_sent = "Buildings",file_sent = "images/buildings_proj.png")
sector_proj_graph(proj_data,sector_sent = "Agriculture",file_sent = "images/agriculture_proj.png")


prov_proj_graph<-function(data_sent,inventory_sent=inventory,project_sent=project_case,cut_year=nir_year,
                            prov_name="Alberta",prov_sent="AB",file_sent="images/AB_proj.png",dpi=300){
  #data_sent<-proj_data
  #project_sent<-ref_case
  cases<-c(inventory_sent,project_sent)
  data_sent%>% filter(scenario %in% cases & sector %in% main_sectors & prov==prov_sent)%>%
    filter((scenario==project_case & year>cut_year)|(scenario==inventory & year<=cut_year))%>%
    
    ggplot()+
    geom_area(aes(year,ifelse(scenario==inventory,emissions,NA),fill=sector),color="black",position = "stack",linewidth=0.1)+
    geom_area(aes(year,emissions,fill=sector),color="black",position = "stack",linewidth=0.1,alpha=0.4)+
    scale_x_continuous(breaks=pretty_breaks(n=15))+
    #scale_fill_viridis("",discrete=TRUE,option="mako")+
    scale_fill_manual("",values=plot_palette)+
    scale_colour_manual("",values="black",guide = "legend")+
    proj_graph()+
    proj_labs()+
    theme(axis.text.x = element_text(size = 12, colour = "black",angle = 0, hjust=0.5,vjust=0.5))+
    labs(title=paste(prov_name," Provincial GHG Emissions and Projections by Sector",sep=""))
  ggsave(file_sent,dpi = 300,width=14, height=7,bg="white")
}

prov_proj_graph(proj_data,prov_name="Alberta",prov_sent = "AB",file_sent = "images/AB_proj.png")
#prov_proj_graph(proj_data,prov_name="Alberta",prov_sent = "AB",file_sent = "images/AB_proj.png",project_sent=ref_case)

prov_proj_graph(proj_data,prov_name="Ontario",prov_sent = "ON",file_sent = "images/ON_proj.png")

prov_proj_graph(proj_data,prov_name="Quebec",prov_sent = "QC",file_sent = "images/QC_proj.png")

prov_proj_graph(proj_data,prov_name="British Columbia",prov_sent = "BC",file_sent = "images/BC_proj.png")


ggplot(filter(proj_data,emissions>0 & scenario%in% c("NIR 2024", "2024 Reference Case","2024 Additional Measures Scenario") & prov !="Canada" & sector=="Oil and Gas")%>%
       mutate(scenario=as_factor(scenario),
              scenario=fct_rev(scenario))
       )+
  geom_line(aes(year,emissions,group=scenario,color=scenario,lty=scenario),linewidth=1.05)+
  #geom_line(data=filter(proj_data,emissions>0 & scenario%in% c("NIR 2023", "2023 Reference Case") & prov !="Canada" & year<=2018 & sector=="Oil and Gas"),
  #          aes(year,emissions,fill=prov),color="black",position = "stack",size=0.1,alpha=.8)+
  #geom_line(data=filter(proj_data,emissions>0 & scenario%in% c("NIR 2023", "2023 Additional Measures Scenario") & prov !="Canada" & year>2021 & sector=="Oil and Gas"),
  #          aes(year,emissions,fill=prov),color="black",position = "stack",size=0.1,alpha=.6)+
    #geom_line(aes(year,net_30_2005,colour=str_wrap("30% below 2005 sector GHGs",width = 20)),linetype=1,size=1.05)+
  facet_wrap( ~ prov,nrow = 1)+
  scale_x_continuous(breaks=pretty_breaks())+
  scale_colour_manual("",values=c("black","black","darkgreen"),guide = "legend")+
  scale_linetype_manual("",values=c("solid","21","21"),guide = "legend")+
  #guides(fill=guide_legend(nrow =1,byrow=FALSE),color=guide_legend(nrow =1,byrow=FALSE))+
  proj_graph()+
  theme(legend.position = "bottom",legend.direction = "horizontal", legend.box = "horizontal", 
        legend.key.width = unit(.89, 'cm'), #change legend key width
        )+
  labs(x=NULL,y=expression('Annual Emissions  '*'(MtCO'[2]*'e)'),
       title="Canadian GHG Emissions from Oil and Gas",
       #subtitle=paste("National Inventory (1990-2021) levels and 2023 projections (2021-2030)",sep=""),
       caption=str_wrap("Source: Environment and Climate Change Canada. Graph by @andrew_leach.",width = 180))
ggsave("images/oil_gas_proj_prov.png",dpi = 300,width=14, height=7,bg="white")



ggplot()+
  geom_line(data=filter(proj_data,year>2010 & emissions>0 & scenario%in% c("2024 Reference Case","NIR 2024") & prov !="Canada" & sector=="Electricity"),
            aes(year,emissions,colour="B",lty="B"),size=1.1)+
  geom_line(data=filter(proj_data,year>2010 &emissions>0 & scenario%in% c("NIR 2024","2024 Additional Measures Scenario") & prov !="Canada" & sector=="Electricity"),
            aes(year,emissions,colour="C",lty="C"),size=1.1)+
  geom_line(data=proj_data%>%filter(year>2010 & emissions>0 & scenario%in% c("NIR 2024") & prov !="Canada" & sector=="Electricity"),
        aes(year,emissions,color="A",lty="A"),position = "stack",size=1.1)+
  #geom_line(aes(year,net_30_2005,colour=str_wrap("30% below 2005 sector GHGs",width = 20)),linetype=1,size=1.05)+
  facet_wrap( ~ prov,nrow = 1)+
  scale_colour_manual("",values=c("black",blakes_blue,colors_ua10()[1]),
                      labels=c("National Inventory (2024)","Reference Case (2024)","Additional Measures Scenario (2024)"),
                      guide = "legend")+
  scale_linetype_manual("",values=c("solid","21","21"),
                      labels=c("National Inventory (2024)","Reference Case (2024)","Additional Measures Scenario (2024)"),
                      guide = "legend")+
  
  scale_x_continuous(breaks=pretty_breaks())+
  blake_theme()+theme(
    panel.spacing = unit(2.5,"lines"),
    legend.position = "bottom",
    legend.margin=margin(c(.05,0,.05,0),unit="cm"),
    legend.text = element_text(colour="black", size = 10),
    plot.caption = element_text(size = 12, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 20, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 20,face = "bold"),
    axis.text.y = element_text(size = 14,face = "bold", colour="black"),
    #axis.text.x = element_blank(),
    axis.text.x = element_text(size = 14, colour = "black", angle = 90,hjust = -1),
    strip.text.x = element_text(size = 14, colour = "black", angle = 0),
    axis.title.y = element_text(size = 16,face = "bold", colour="black"),
  )+
  labs(x=NULL,y=expression('Annual Emissions  '*'(MtCO'[2]*'e)'),
       title="Canadian GHG Emissions from Electricity",
       subtitle=paste("National Inventory (1990-2022) levels and 2023 Reference Case projections (2020-2030, lighter fill)",sep=""),
       caption=str_wrap("Source: Environment and Climate Change Canada. Graph by @andrew_leach.",width = 180))
ggsave("images/power_proj_prov.png",dpi = 300,width=14, height=7,bg="white")


#emissions and targets graphs

nat_proj_data<-function(){ #national projections by scenario

  #get older natcom and trends information
  
  # #getting national communication info
  # library(tabulizer)
  # f <- "data/canada_nc7.pdf"
  # 
  # # extract tables from only second page
  # oil_sands_table<-data.frame(extract_tables(f, pages = 143)[1],stringsAsFactors = F)
  # oil_sands_table<-oil_sands_table[-1,] #drop the first row
  # oil_sands_table<-oil_sands_table[-14,] #drop the total row
  # oil_sands_table<-oil_sands_table[,-7] #drop the growth column
  # oil_sands_table[1,1]<-"sector" #add variable labels
  # colnames(oil_sands_table)<-oil_sands_table[1,]
  # oil_sands_table<-oil_sands_table[-1,] #drop the first row
  # 
  # 
  # sector_table<-data.frame(extract_tables(f, pages = 142)[1],stringsAsFactors = F)
  # sector_table<-sector_table[-1,] #drop the first row
  # sector_table<-sector_table[,-7] #drop the growth column
  # sector_table[1,1]<-"sector" #add variable labels
  # colnames(sector_table)<-sector_table[1,]
  # sector_table<-sector_table[-1,] #drop the first row
  # 
  # nat_com<-rbind(sector_table,oil_sands_table)
  # nat_com<-nat_com %>% pivot_longer(cols=-sector,values_to = "GHGs",names_to = "Year") %>% mutate(GHGs=as.numeric(GHGs))
  # nat_com$sector[nat_com$sector=="Oil Sandsf"]<-"Oil Sands"
  # nat_com$Year<-as.numeric(as.character(nat_com$Year))
  # #nat_com$sector[nat_com$sector=="Oil Sands"]<-"Oil Sands (Mining, In-situ, Upgrading)"
  # #nat_com$sector[nat_com$sector=="Bitumen In Situ"]<-"In Situ"
  # #nat_com$sector[nat_com$sector=="Bitumen Mining"]<-"Mining and Extraction"
  # #nat_com$sector[nat_com$sector=="Bitumen Upgrading"]<-"Mining and Extraction"
  # 
  # 
  # #ECCC 2018 December projections
  # 
  # ec_2018_file <- "data/ECCC_DEC_2018.pdf"
  # # extract tables from only second page
  # econ_table<-data.frame(extract_tables(ec_2018_file, pages = 9),stringsAsFactors = F)
  # 
  # 
  # ec_2019_file<-"data/progress-towards-ghg-emissions-target-en.pdf"
  # progress_annex_table<-data.frame(extract_tables(ec_2019_file, pages = 12),stringsAsFactors = F)
  # progress_annex_table<-progress_annex_table[-c(1:7),]
  # progress_annex_table[,1:6] <- sapply(progress_annex_table[,1:6], as.numeric)
  # names(progress_annex_table)<-c("Year",
  #                                "Second_Biennial",
  #                                "2017_Ref",
  #                                "2018_Ref",
  #                                "Adds_Case",
  #                                "Target Case")
  # progress_annex_table$`Target Case`<-NULL
  # progress_annex_table$sector<-"Total, Canada"
  # progress_annex_table$Adds_Case[progress_annex_table$Year==2030] <-592
  # 
  # 
  # sectors<-unique(nat_com$sector)
  # 
  # NIR_CAN<-NIR_data %>%group_by(sector,Year) %>% summarize(GHGs=sum(GHGs)) %>% ungroup()%>%
  #   mutate(sector=fct_collapse(sector,
  #                              "Total, Canada" = c("Total","GHG TOTAL","National Inventory Total","Inventory Total"))
  #   )
  # 
  # NIR_CAN<-bind_rows(NIR_CAN,nat_com)
  # 
  # 
  # 
  # 
  # cdn_data<-full_join(NIR_CAN,targets,by=c("Year","sector"))
  # #cdn_data<-targets
  # cdn_data$Year<-as.numeric(cdn_data$Year)
  # cdn_data$projection[cdn_data$Year==2017]<-715.759256
  # cdn_data$bau[cdn_data$Year==2017]<-715.759256
  # cdn_data$target[cdn_data$Year==2017]<-715.759256
  # cdn_data$adds[cdn_data$Year==2017]<-715.759256
  # palette<-rev(viridis(7,option = "C"))[-1]#[-c(1,2,3)]
  # cdn_data$kyoto[(cdn_data$Year>=2008 &cdn_data$Year<=2012)]<-565
  # cdn_data<-left_join(cdn_data,progress_annex_table,by=c("Year","sector"))
  
  
  #ECCC Targets data
  #targets_csv<-"https://www.canada.ca/content/dam/eccc/documents/csv/cesindicators/progress-canada-ghg-emissions-reduction-target/2019/progress-towards-canada-ghg-emissions-target-en.csv"
  #download.file(targets_csv,"data/targets.csv",mode="wb")
  target_data<-read_csv("data/targets.csv",skip = 2,na = "n/a")[-c(27:30),]
  
  target_data$`2018 Additional Measures[C] (megatonnes of carbon dioxide equivalent)`<-
    gsub("\\[D\\]","",target_data$`2018 Additional Measures[C] (megatonnes of carbon dioxide equivalent)`)
  
  target_data$`2018 Additional Measures[C] (megatonnes of carbon dioxide equivalent)`<-as.numeric(target_data$`2018 Additional Measures[C] (megatonnes of carbon dioxide equivalent)`)
  
  
  target_data<-target_data %>% pivot_longer(cols=-Year,names_to = "projection") %>% mutate(value=as.numeric(gsub("\\[.*\\]", "", value)),
                                                                                           projection=gsub("\\[.*\\]", "", projection),
                                                                                           projection=gsub(" \\(megatonnes of carbon dioxide equivalent\\)","",projection),
                                                                                           Year=as.numeric(Year))%>%
    rename(scenario=projection, year=Year,emissions=value)%>%
    mutate(scenario=gsub("Second Biennial Report Reference Case","2016 Reference Case",scenario))%>%
    filter(scenario %in% c("2016 Reference Case","2017 Reference Case"))%>%
    mutate(scen_year=as.numeric(str_sub(scenario,start=1,end=4))-2)%>%
    filter(year>scen_year)%>%select(-scen_year)
  
 #year scenario emissions for 2016 and 2017 ref case
  
  
  LULU<-proj_data%>%filter(prov=="Canada",grepl("ing LULUCF",sector),emissions>0)%>%
    mutate(scenario=ifelse(grepl("including",sector),paste(scenario,"(including LULUCF)"),paste(scenario,"(excluding LULUCF)")))%>%
    group_by(scenario,year)%>%
    summarize(emissions=sum(emissions,na.rm=T))%>%
    filter(emissions>0)
  

  
  
  
    
cdn_data<-proj_data %>% filter(prov!="Canada",!grepl("LULUCF",scenario),scenario!="n/a")%>%
    group_by(scenario,year)%>%
    summarize(emissions=sum(emissions,na.rm=T))%>%
    filter(emissions>0)%>%
  bind_rows(target_data)%>%
  bind_rows(
    proj_data%>%filter(prov=="Canada",sector=="Total (excluding LULUCF accounting contribution)",!grepl("Reference",scenario),!grepl("Additional",scenario))%>%
      group_by(scenario,year)%>%
      summarize(emissions=sum(emissions,na.rm=T))%>%
      filter(emissions>0)
  )%>%
  bind_rows(LULU)

  cdn_data
}

cdn_data<-nat_proj_data()



ab_proj<-proj_data %>% filter(prov=="AB")%>%
  group_by(scenario,year)%>%
  summarize(emissions=sum(emissions,na.rm=T))%>%
  filter(emissions>0)

ex_ab_proj<-proj_data %>% filter(prov!="AB",prov!="Canada")%>%
  group_by(scenario,year)%>%
  summarize(emissions=sum(emissions,na.rm=T))%>%
  filter(emissions>0)%>%
  filter(scenario %in% c("NIR 2024","NIR 2023","2023 Reference Case","2023 Additional Measures Scenario"))


# globe_data<- cdn_data %>% mutate(prov="Canada")%>%
#   bind_rows(ab_proj %>% mutate(prov="Alberta"))%>%
#   filter(scenario %in% c("NIR 2023","2023 Reference Case","2023 Additional Measures Scenario"))
#   
# write_csv(cdn_data,file="data/globe_data.csv")

#write_csv(globe_data,file="data/globe_data.csv")

#cdn_data<-read_csv(file="data/globe_data.csv")


#ggplot(canadian_data)+geom_line(aes(year,emissions,group=scenario,color=scenario))


palette<-c(colors_tableau10()[1:4],"dodgerblue",colors_tableau10()[5:10])


targets<-data.frame(year=c(2020,2030,2050),target=732*c(1-.17,1-.30,0),projection=c(728,722,NA),bau=c(768,815,NA),adds=c(690,583,NA))
targets$year<-as.numeric(as.character(targets$year))
targets$sector<-"Total, Canada"



targets_graph<-
  ggplot()+
  geom_line(data=filter(cdn_data,scenario %in% c("NIR 2024","2024 Reference Case")),aes(year,emissions),color="black",lty="11",size=1.45)+
  geom_line(data=filter(cdn_data,grepl('NIR 2024', scenario)),aes(year,emissions),color="black",size=1.45)+
  #scale_linetype_manual("",values=c("solid","31"))+
  geom_point(data=targets,aes(year,target),size=5,colour=palette[9])+
  geom_point(aes(2000,588.6),size=5,colour=palette[9])+ #rio target
  scale_color_manual("",values=palette[-1])+
  annotate("text",x=1990+(2019-1990)/2,y=790,label="National Inventory Emissions (1990-2022)",
           color="black",fontface="bold",hjust=0.5)+
  #Rio 
  annotate("text",x=2000,y=630,label="Rio Target\n(return to 1990 levels by 2000)",
           colour=palette[9],fontface="bold",hjust=0.5)+
  #kyoto
  #geom_point(aes(2010,565),size=5,colour=palette[9])+ #kyoto target
  geom_errorbarh(aes(xmin=2008,y=588.6*.94,xmax=2012),height=40,size=2,colour=palette[9])+
  annotate("text",x=2007,y=588.6*.94,label="Kyoto Target (6% below 1990 levels, 2008-12)",
           colour=palette[9],fontface="bold",hjust=1)+
  #copenhagen
  annotate("text",x=2014.5,y=732.218*.83,label="Copenhagen Target\n(17% below 2005 levels by 2020)",
           colour=palette[9],fontface="bold",hjust=.5)+
  #paris
  annotate("text",x=2030,y=732.218*.7,label="Paris Target (30% below 2005 levels by 2030)",
           colour=palette[9],fontface="bold",hjust=1.05)+
  #glasgow
  #geom_point(aes(2030,423),size=5,colour=palette[9])+ #glasgow target
  geom_errorbar(aes(x=2030,ymax=732.218*.6,ymin=732.218*.55),width=1.4,size=2,colour=palette[9])+
  annotate("text",x=2029,y=732.218*.575,label="Glasgow Target (40-45% below 2005 levels by 2030)",
           colour=palette[9],fontface="bold",hjust=1)+
  #2050
  annotate("text",x=2049.5,y=-12,label="2050 Net Zero Goal",
           colour=palette[9],fontface="bold",hjust=1)+
  
  geom_hline(yintercept=0,size=1)+
  #scale_y_continuous(limit=c(-10,825),expand = c(0,0))+
  scale_x_continuous(limit=c(1990,2052),breaks=seq(1990,2055,10),expand = c(0,0))+
  tombe_theme()+theme(legend.position = "none")+
  labs(x="",y=expression('Annual Emissions '*'(MtCO'[2]*'e)'),
       )+
  coord_cartesian(clip = 'off')

targets_graph+ 
  labs(title="Canada's GHG Emissions, Projections and Future Targets",
       subtitle="Source: Environment and Climate Change Canada Emissions Inventory (2024) and Projections (2023).")+
  annotate("text",x=2035.2,y=600,label="2024 ECCC Reference Case Projection (2023-2040)",color="black",fontface="bold",hjust=0)
ggsave("images/emissions_and_targets_simple.png",bg="white",dpi=300,width=15,height=7)




palette<-viridis(8,option="magma",direction = -1,begin = 0.05,end = .85)

targets_graph+ 
  labs(title="Canada's GHG Emissions, Projections and Future Targets",
  subtitle="Source: Environment and Climate Change Canada National Emissions Inventory Report (2024) and Projections (2016-2023). Graph by Andrew Leach.")+
  annotate("text",x=2040.2,y=588.6,label="2024 ECCC Reference Case",color="black",fontface="bold",hjust=0)+
  
  geom_line(data=filter(cdn_data,scenario %in% c("2024 Additional Measures Scenario")),aes(year,emissions),color="darkgreen",lty="22",size=1.45)+
  annotate("text",x=2040.2,y=472,label="2024 Additional Measures Scenario",color="darkgreen",fontface="bold",hjust=0,vjust=0.5)+
  
  geom_line(data=filter(cdn_data,scenario %in% c("2024 Additional Measures Scenario (including LULUCF)")),aes(year,emissions),color="darkgreen",lty="11",size=1.45)+
  annotate("text",x=2040.2,y=431.1,label="2024 Additional Measures Scenario\n(incl LULUCF)",lineheight = 0.9,color="darkgreen",fontface="bold",hjust=0,vjust=.8)+

  
  annotate("text",x=2035.2,y=573,label="2023 Reference Case",color=palette[6],fontface="bold",hjust=0,vjust=0.5)+
  geom_line(data=filter(cdn_data,scenario %in% c("2023 Reference Case")),aes(year,emissions),color=palette[6],lty="22",size=1.45)+
  
  geom_line(data=filter(cdn_data,scenario %in% c("2022 Reference Case")),aes(year,emissions),color=palette[5],lty="22",size=1.45)+
  annotate("text",x=2035.2,y=621,label="2022 Reference Case",color=palette[5],fontface="bold",hjust=0,vjust=0.5)+
  
  geom_line(data=filter(cdn_data,scenario %in% c("2021 Reference Case")),aes(year,emissions),color=palette[4],lty="22",size=1.45)+
  annotate("text",x=2030.2,y=658,label="2021 Reference Case",color=palette[4],fontface="bold",hjust=0,vjust=0.5)+
  
  geom_line(data=filter(cdn_data,scenario %in% c("2018 Reference Case")),aes(year,emissions),color=palette[3],lty="22",size=1.45)+
  annotate("text",x=2030.2,y=697,label="2018 Reference Case",color=palette[3],fontface="bold",hjust=0,vjust=0.5)+
  
  
  geom_line(data=filter(cdn_data,scenario %in% c("2017 Reference Case")),aes(year,emissions),color=palette[2],lty="22",size=1.45)+
  annotate("text",x=2030.2,y=722,label="2017 Reference Case",color=palette[2],fontface="bold",hjust=0,vjust=0.5)+
  
  geom_line(data=filter(cdn_data,scenario %in% c("2016 Reference Case")),aes(year,emissions),color=palette[1],lty="22",size=1.45)+
  annotate("text",x=2030.2,y=815,label="2016 Reference Case",color=palette[1],fontface="bold",hjust=0,vjust=0.5)+
  
  NULL
ggsave("images/emissions_and_targets_both.png",bg="white",dpi=300,width=15,height=7)



