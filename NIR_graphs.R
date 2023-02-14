#basic libraries

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



#use NIR_prelim
get_new_nir<-function() {
  #download.file("http://data.ec.gc.ca/data/substances/monitor/canada-s-official-greenhouse-gas-inventory/GHG_Econ_Can_Prov_Terr.csv","canada_ghg.csv")
  #download.file("http://donnees.ec.gc.ca/data/substances/monitor/canada-s-official-greenhouse-gas-inventory/GHG_Econ_Can_Prov_Terr.csv","canada_ghg.csv",mode="wb")
  #download.file("http://data.ec.gc.ca/data/substances/monitor/canada-s-official-greenhouse-gas-inventory/GHG_Econ_Can_Prov_Terr.csv","canada_ghg_2020.csv",mode="wb")
  #nir_2021<-read.csv("canada_ghg_2021_orig.csv",stringsAsFactors = F)
  
  #download.file("http://data.ec.gc.ca/data/substances/monitor/canada-s-official-greenhouse-gas-inventory/GHG_Econ_Can_Prov_Terr.csv","canada_ghg_2021_dl.csv",mode="wb")
  #nir_2021<-read.csv("canada_ghg_2021_dl.csv",stringsAsFactors = F)
  
  download.file("https://data.ec.gc.ca/data/substances/monitor/canada-s-official-greenhouse-gas-inventory/B-Economic-Sector/GHG_Econ_Can_Prov_Terr.csv","canada_ghg_2022_dl.csv",mode="wb")
  nir_2022<-read.csv("canada_ghg_2022_dl.csv",stringsAsFactors = F)
  
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
  
  new_nir<-nir_2022 %>% 
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
           sector=case_when(
             (Sector!="")&(Sub.sector=="")&(Sub.sub.sector=="") ~ Sector, #both subs are blank
             (Sub.sector!="")&(Sub.sub.sector=="") ~ Sub.sector, #just sub.sub is blank
             (Sub.sub.sector!="") ~ Sub.sub.sector, #sub.sub exists
             (Sector=="") ~ Source
           ),
           #fix territories inventory
           NULL)%>%
    mutate(sector=factor(sector),
           sector=fct_collapse(sector,"Inventory Total"=
                                 c("Territories Inventory Total","Territory Inventory Total","Territorial Inventory Total","Provincial Inventory Total"))
    )%>%
    group_by(Year, Prov,sector) %>% summarize(GHGs=sum(as.numeric(CO2eq),na.rm = T)) %>% ungroup()%>%
    select(sector,Prov,Year,GHGs)%>%#filter(!is.na(GHGs))%>%
    mutate(sector=fct_collapse(sector,
                               "Transportation" = c("Transport","Transportation"),
                               "Oil Sands" = c("Oil Sands","Oil Sands (Mining, In-situ, Upgrading)"),
                               "Oil Sands In Situ" = c("In-situ","Bitumen In Situ","In-situ Bitumen"),
                               "Oil Sands Mining" = c("Mining and Extraction","Bitumen Mining","Oil Sands Mining and Extraction"),
                               "Oil Sands Upgrading" = c("Upgrading","Bitumen Upgrading"),
                               "Total, Canada" = c("Total","GHG TOTAL","National Inventory Total")))
  
  
  
  #re-order east to west
  new_nir$Prov<-factor(new_nir$Prov, levels=c("Canada" ,"BC","AB" ,"SK","MB", "ON",     "QC" ,  "ATL" ,   "TERR"  ))
  
  
  
  
  new_nir
}

#here, we want to use the new inventory data
#new_nir<-get_new_nir()
#save(new_nir,file = "data/nir_data.Rdata")

load(file = "data/nir_data.Rdata")

#get population data
get_pop_data<-function(){
  #Cansim 17-10-0005-01
  pop_data<-get_cansim(1710000501)%>% filter(Sex=="Both sexes",`Age group`=="All ages")%>%
    mutate(Prov=as.factor(GEO),
           Code=fct_recode(Prov,"AB"="Alberta",
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
           Prov_name=GEO,
           Prov_pop=VALUE,Year=REF_DATE,
           Code=fct_collapse(Code,
                             "TERR" = c("NT", "NU","YT","NT & NU"),
                             "ATL" = c("NL", "NB","NS","PE")))%>%
    select(Year,Prov_name,Code,Prov_pop)%>% group_by(Year,Code)%>%summarize(Prov_pop=sum(Prov_pop))
  
  save(file="data/pop_data.Rdata",pop_data)
}

get_pop_data()
load(file = "data/pop_data.Rdata")

#get gdp data
get_gdp_data<-function(){
  #Cansim 36-10-0402-02
  gdp_data<-get_cansim("36-10-0222-01")%>% filter(Estimates=="Final consumption expenditure",
                                                  Prices=="Chained (2012) dollars",
                                                  GEO!="Outside Canada")%>%
    mutate(Prov=as.factor(GEO),
           Code=fct_recode(Prov,"AB"="Alberta",
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
           Prov_name=GEO,
           Prov_pop=VALUE,Year=REF_DATE,
           Code=fct_collapse(Code,
                             "TERR" = c("NT", "NU","YT","NT & NU"),
                             "ATL" = c("NL", "NB","NS","PE")))%>%
    select(Year=REF_DATE,Prov_name,Code,prov_gdp=VALUE)%>% group_by(Year,Code)%>%summarize(prov_gdp=sum(prov_gdp,na.rm=T))
  
  save(file="data/gdp_data.Rdata",gdp_data)
}


get_gdp_data()
load(file = "data/gdp_data.Rdata")


#create provincial data file
NIR_data<-filter(new_nir,Prov!="Canada")%>% mutate(Year=as.numeric(Year))

#section out a sample of provincial totals (used later)
prov_samp<-filter(NIR_data,grepl("otal",sector))%>%select(-1) %>% rename(Prov_GHGs=GHGs)


#create 2030 comps
NIR_data<-NIR_data %>% group_by(Prov) %>% mutate(GHGs_2005_net_30=.7*sum(GHGs*(sector=="Inventory Total")*(Year==2005)),
                                                 GHGs_2005_net_40=.7*sum(GHGs*(sector=="Inventory Total")*(Year==2005)),
                                                 GHGs_2005_net_45=.7*sum(GHGs*(sector=="Inventory Total")*(Year==2005)))

                                                                       
                                                                         
#create totals for indexing
totals<-new_nir %>% filter(sector=="Inventory Total")%>% select(-sector)%>% group_by(Prov) %>% mutate(
  GHGs_2005_net_30=.7*sum(((Year==2005))*GHGs),
  GHGs_2005=sum(((Year==2005))*GHGs)
) %>% ungroup() %>%
  left_join(gdp_data%>%mutate(Year=as.integer(Year)),by=c("Year","Prov"="Code"))%>%
  left_join(pop_data%>%mutate(Year=as.integer(Year)),by=c("Year","Prov"="Code"))%>%
  mutate(ghg_per_cap=GHGs*10^6/Prov_pop,
         ghg_per_gdp=GHGs/prov_gdp,
         gdp_per_cap=prov_gdp/Prov_pop
  )%>%
  group_by(Prov)%>%
  mutate(index_gdp=prov_gdp/first(prov_gdp)*100,
         index_pop=Prov_pop/first(Prov_pop)*100,
         index_ghg=GHGs/first(GHGs)*100,
         index_ghg_cap=ghg_per_cap/first(ghg_per_cap)*100,
         index_ghg_gdp=ghg_per_gdp/first(ghg_per_gdp)*100,
  ) %>% ungroup()%>%
  select(Prov,Year,index_gdp,index_pop,index_ghg,index_ghg_cap,index_ghg_gdp)%>%
  pivot_longer(-c(Prov,Year),names_to = "index",values_to = "value")%>%
  mutate(index=gsub("index_","",index),index=toupper(index),index=as_factor(index),
         index=fct_recode(index,"GHG emissions per capita"="GHG_CAP"),
         index=fct_recode(index,"GHG emissions per unit GDP"="GHG_GDP"),
         index=fct_recode(index,"Population"="POP"),
         index=fct_recode(index,"Inventory GHG emissions"="GHG"),
         index=fct_recode(index,"GDP (chained 2012 dollars)"="GDP")
  )


ggplot(totals)+
  geom_line(aes(Year,value,colour=index,group=index),linetype=1,size=1)+
  facet_wrap( ~ Prov,nrow = 2)+
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
       title="1990-2019 Provincial GHG Emissions, GDP, Population, and GHG Emissions per unit GDP and per Capita",
       #subtitle="Excluding Electricity",
       caption="Sources: Emissions via Environment and Climate Change Canada National Inventory Report (2021), population (Table 17-10-0005-01) and GDP (Table 36-10-0402-02) via Statistics Canada.\nGraph by @andrew_leach",
       NULL
  )
ggsave("images/index_ghgs.png",width=16,height=16,bg="white",dpi=300)

ggplot(filter(totals,Prov%in% c("AB","Canada")))+
  geom_line(aes(Year,value,colour=index,group=index),linetype=1,size=1)+
  facet_wrap( ~ Prov,nrow = 3)+
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
       title="1990-2019 Provincial GHG Emissions, GDP, Population, and GHG Emissions per unit GDP and per Capita",
       #subtitle="Excluding Electricity",
       caption="Sources: Emissions via Environment and Climate Change Canada National Inventory Report (2021), population (Table 17-10-0005-01) and GDP (Table 36-10-0402-02) via Statistics Canada.\nGraph by @andrew_leach",
       NULL
  )
ggsave("images/index_ghgs_AB.png",width=16,height=16,bg="white",dpi=300)

top<-ggplot(filter(totals,Prov%in% c("Canada")))+
  geom_line(aes(Year,value,colour=index,group=index),linetype=1,size=1)+
  facet_wrap( ~ Prov,nrow = 1)+
  #scale_color_viridis("",discrete=TRUE,guide_legend(NULL),option="E")+
  #scale_fill_viridis("",discrete=TRUE,option="E")+
  #scale_fill_manual("",values = my_palette,guide = "legend")+
  #scale_fill_grey("",guide = "legend",start = 0.9,end=0.05)+
  scale_x_continuous(breaks=pretty_breaks())+
  scale_y_continuous(breaks=seq(50,250,50))+
  expand_limits(y=c(50,250))+
  scale_colour_manual("",values=c("black",colors_ua10()[-c(3,5)]),guide = "legend")+
  guides(color=guide_legend(nrow =1,byrow=FALSE))+
  blake_theme()+theme(
    panel.spacing = unit(.5,"lines"),
    legend.position = "none",
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
       title="1990-2019 GHG Emissions, GDP, Population, and GHG Emissions per unit GDP and per Capita",
       #subtitle="Excluding Electricity",
       #caption="Sources: Emissions via Environment and Climate Change Canada National Inventory Report (2021), population (Table 17-10-0005-01) and GDP (Table 36-10-0402-02) via Statistics Canada.\nGraph by @andrew_leach",
       NULL
  )

bottom<-ggplot(filter(totals,!Prov%in% c("Canada")))+
  geom_line(aes(Year,value,colour=index,group=index),linetype=1,size=1)+
  facet_wrap( ~ Prov,nrow = 1)+
  #scale_color_viridis("",discrete=TRUE,guide_legend(NULL),option="E")+
  #scale_fill_viridis("",discrete=TRUE,option="E")+
  #scale_fill_manual("",values = my_palette,guide = "legend")+
  #scale_fill_grey("",guide = "legend",start = 0.9,end=0.05)+
  scale_x_continuous(breaks=pretty_breaks())+
  scale_y_continuous(breaks=seq(50,250,50))+
  expand_limits(y=c(50,250))+
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
       #title="1990-2019 Provincial GHG Emissions, GDP, Population, and GHG Emissions per unit GDP and per Capita",
       #subtitle="Excluding Electricity",
       caption="Sources: Emissions via Environment and Climate Change Canada National Inventory Report (2021), population (Table 17-10-0005-01) and GDP (Table 36-10-0402-02) via Statistics Canada.\nGraph by @andrew_leach",
       NULL
  )

library(cowplot)
plot_grid(top,bottom,nrow = 2)
ggsave("images/index_provs.png",width=16,height=16,bg="white",dpi=300)




ggplot(totals)+
  geom_line(aes(Year,value,colour=Prov,group=Prov),linetype=1,size=1)+
  facet_wrap( ~ index,nrow = 1,scales="free_y")+
  #scale_color_viridis("",discrete=TRUE,guide_legend(NULL),option="E")+
  #scale_fill_viridis("",discrete=TRUE,option="E")+
  #scale_fill_manual("",values = my_palette,guide = "legend")+
  #scale_fill_grey("",guide = "legend",start = 0.9,end=0.05)+
  scale_x_continuous(breaks=pretty_breaks())+
  scale_colour_manual("",values=c("black",colors_tableau10()),guide = "legend")+
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
    strip.text.x = element_text(size = 12, colour = "black", angle = 0),
    axis.title.y = element_text(size = 16,face = "bold", colour="black"),
  )+
  labs(x=NULL,y="Indexed value (1990=100)",
       title="1990-2018 provincial GHG emissions, GDP, population, and GHG emissions per unit GDP and per capita",
       #subtitle="Excluding Electricity",
       caption="Sources: Emissions via Environment and Climate Change Canada National Inventory Report (2020), population (Table 17-10-0005-01) and GDP (Table 36-10-0402-02) via Statistics Canada.\nGraph by @andrew_leach",
       NULL
  )
ggsave("images/GHG_per_GDP.png",width=16,height=9,bg="white",dpi=400)



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


prov_ghgs <- NIR_data%>% filter(sector %in% main_sectors)%>%group_by(Prov,Year)%>% summarize(GHGs=sum(GHGs,na.rm = T))
save(prov_ghgs,file = "data/prov_ghgs.Rdata")

NIR_natl<-filter(new_nir,Prov=="Canada")
NIR_natl$Year<-as.numeric(NIR_natl$Year)
NIR_natl<-merge(filter(NIR_natl,sector %in% main_sectors),pop_data,by.x=c("Prov","Year"),by.y=c("Code","Year"))
NIR_natl$per_cap<-NIR_natl$GHGs/NIR_natl$Prov_pop

NIR_natl<-NIR_natl %>% mutate(
  sector=as.factor(sector),
  sector=fct_other(sector,keep = attn_sectors)
)%>%group_by(sector,Year,Prov)%>%
  summarize(Prov_pop=first(Prov_pop),
            GHGs=sum(GHGs),
            per_cap=GHGs/Prov_pop)

ggplot(filter(NIR_natl,sector!="National Inventory Total"))+
  geom_area(aes(Year,GHGs,group=sector,fill=sector),color="black",size=0.5)+
  #scale_fill_manual("",values = c(colors_tableau10(),colors_tableau10_medium()),guide = "legend")+
  scale_fill_viridis("",discrete=TRUE,option="B",direction = -1)+
  scale_colour_manual("",values="black",guide = "legend")+
  scale_x_continuous(breaks=pretty_breaks(n=16), expand = c(0,0))+
  guides(fill=guide_legend(nrow =2,byrow=FALSE),color=guide_legend(nrow =1,byrow=FALSE))+
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
    axis.text.x = element_text(size = 14, colour = "black", angle = 90),
    strip.text.x = element_text(size = 14, colour = "black", angle = 0),
    axis.title.y = element_text(size = 14,face = "bold", colour="black"),
  )+
  labs(x=NULL,y=expression('Annual GHG Emissions  '*'(Mt CO'[2]*'e)'),
       title="Canadian GHG Emissions, 1990-2020",
       caption="Source: Environment Canada National Inventory Data, graph by @andrew_leach")
ggsave("images/nir_natl.png",width=16,height=9,bg="white",dpi=300)


ggplot(filter(NIR_natl,sector!="National Inventory Total"))+
  geom_area(aes(Year,per_cap*10^6,group=sector,fill=sector),color="black",size=0.5)+
  #scale_fill_manual("",values = c(colors_tableau10(),colors_tableau10_medium()),guide = "legend")+
  scale_fill_viridis("",discrete=TRUE,option="B",direction = -1)+
  #annotate("rect", fill = "black", 
  #         xmin = 2008+3/12, xmax =2008+4/12,
  #         ymin = -Inf, ymax = 23) +
  #annotate("text", x = 2008+3.5/12, y = 24.5, label = "Turning the Corner Plan\nIntroduced",size=4)+
  #scale_fill_manual("",values = c(colors_tableau10(),colors_tableau10_medium()),guide = "legend")+
  
  scale_colour_manual("",values="black",guide = "legend")+
  scale_x_continuous()+
  guides(fill=guide_legend(nrow =2,byrow=FALSE),color=guide_legend(nrow =1,byrow=FALSE))+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(.05,0,.05,0),unit="cm"),
    legend.text = element_text(colour="black", size = 8),
    plot.caption = element_text(size = 8, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 20, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 20,face = "bold"),
    axis.text.y = element_text(size = 14,face = "bold", colour="black"),
    #axis.text.x = element_blank(),
    axis.text.x = element_text(size = 10, colour = "black", angle = 90),
    strip.text.x = element_text(size = 10, colour = "black", angle = 0),
    axis.title.y = element_text(size = 10,face = "bold", colour="black"),
  )+
  labs(x=NULL,y=expression('Annual Per-Capita Emissions  '*'(tonnes CO'[2]*'e per person)'),
       title="Canadian Per Capita GHG Emissions by Sector, 1990-2020",
       #subtitle="According to Ian Brodie, they first declined after March, 2008.",
       caption="Source: Environment Canada National Inventory Data, graph by @andrew_leach")
ggsave("images/natl_per_cap.png",height = 9,width = 16,bg="white",dpi=300)




oil_sectors<-c("Conventional Heavy Oil Production","Conventional Light Oil Production","Frontier Oil Production",
               "Oil Sands Mining","Oil Sands In Situ","Oil Sands Upgrading")

oil_sands_sectors<-c("Oil Sands Mining","Oil Sands In Situ","Oil Sands Upgrading")

nir_oil<-ggplot(new_nir %>% filter(Prov=="Canada",sector%in%oil_sectors)%>%
                  mutate(sector=as_factor(sector),
                         sector=fct_relevel(sector,"Conventional Light Oil Production")))+
  geom_area(aes(Year,GHGs,group=sector,fill=sector),position="stack",color="black",size=0.25)+
  #scale_fill_manual("",values = c(colors_tableau10(),colors_tableau10_medium()),guide = "legend")+
  scale_fill_viridis("",discrete=TRUE,option="B",direction = -1)+
  #scale_color_viridis("",discrete=TRUE,guide_legend(NULL),option="D")+
  #scale_fill_viridis("",discrete=TRUE,option="D")+
  scale_x_continuous(breaks=pretty_breaks(n=15), expand = c(0,0))+
  scale_y_continuous(breaks=pretty_breaks(n=5), expand = c(0,0))+
  guides(fill=guide_legend(nrow =2,byrow=TRUE))+
  theme_minimal()+slide_theme()+
  labs(x=NULL,y=expression('Annual GHG Emissions  '*'(Mt CO'[2]*'e)'))
nir_oil
ggsave("images/nir_oil_raw.png",width=16,height=9,bg="white",dpi=300)



nir_oil+labs(title="Canadian GHG Emissions from Crude Oil and Oil Sands Production, 1990-2020",
       #subtitle="According to Ian Brodie, they first declined after March, 2008.",
       caption="Source: Environment Canada National Inventory Data, graph by @andrew_leach")
ggsave("images/nir_oil.png",width=16,height=9,bg="white",dpi=300)


nir_oil_prov<-ggplot(new_nir %>% filter(Prov!="Canada",sector%in%c(oil_sectors,"Natural Gas Production and Processing"))%>%
                  mutate(sector=as_factor(sector),
                         sector=fct_relevel(sector,"Conventional Light Oil Production"),
                         sector=fct_relevel(sector,"Natural Gas Production and Processing"),
                         sector=fct_recode(sector,"Natural Gas"="Natural Gas Production and Processing"),
                         sector=fct_recode(sector,"Conventional Light Oil"="Conventional Light Oil Production"),
                         sector=fct_recode(sector,"Conventional Heavy Oil"="Conventional Heavy Oil Production"),
                         sector=fct_recode(sector,"Frontier Oil"="Frontier Oil Production"),
                                              ))+
  geom_area(aes(Year,GHGs,group=sector,fill=sector),position="stack",color="black",size=0.25)+
  #scale_fill_manual("",values = c(colors_tableau10(),colors_tableau10_medium()),guide = "legend")+
  scale_fill_viridis("",discrete=TRUE,option="B",direction = -1)+
  facet_grid( ~ Prov)+
  #scale_color_viridis("",discrete=TRUE,guide_legend(NULL),option="D")+
  #scale_fill_viridis("",discrete=TRUE,option="D")+
  scale_x_continuous(breaks=pretty_breaks(), expand = c(0,0))+
  scale_y_continuous(breaks=pretty_breaks(), expand = c(0,0))+
  expand_limits(y=140)+
  guides(fill=guide_legend(nrow =1,byrow=TRUE))+
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
  labs(x=NULL,y=expression('Annual GHG Emissions  '*'(Mt CO'[2]*'e)'))
nir_oil_prov
ggsave("images/nir_oil_prov.png",width=16,height=9,bg="white",dpi=300)




nir_oil_ab<-ggplot(new_nir %>% filter(Prov=="AB",sector%in%oil_sectors)%>%
                  mutate(sector=as_factor(sector),
                         sector=fct_relevel(sector,"Conventional Light Oil Production")))+
  geom_area(aes(Year,GHGs,group=sector,fill=sector),position="stack",color="black",size=0.25)+
  #scale_fill_manual("",values = c(colors_tableau10(),colors_tableau10_medium()),guide = "legend")+
  scale_fill_viridis("",discrete=TRUE,option="B",direction = -1)+
  #scale_color_viridis("",discrete=TRUE,guide_legend(NULL),option="D")+
  #scale_fill_viridis("",discrete=TRUE,option="D")+
  scale_x_continuous(breaks=pretty_breaks(n=15), expand = c(0,0))+
  scale_y_continuous(breaks=pretty_breaks(n=5), expand = c(0,0))+
  guides(fill=guide_legend(nrow =2,byrow=TRUE))+
  theme_minimal()+slide_theme()+
  labs(x=NULL,y=expression('Annual GHG Emissions  '*'(Mt CO'[2]*'e)'))
nir_oil_ab
ggsave("images/nir_oil_ab_raw.png",width=16,height=9,bg="white",dpi=300)

nir_oil_ab+labs(title="GHG Emissions from Crude Oil and Oil Sands Production in Alberta, 1990-2020",
             #subtitle="According to Ian Brodie, they first declined after March, 2008.",
             caption="Source: Environment Canada National Inventory Data, graph by @andrew_leach")
ggsave("images/nir_AB_oil_gas.png",width=12,height=5,bg="white",dpi=300)




#find the largest sectors

major_sectors<-NIR_natl %>% group_by(sector) %>% summarize(max_ghg=max(GHGs)) %>% arrange(-max_ghg) %>% head(6) %>% select(sector)
#use forcats to pick 5 largest sectors

large_sec<-NIR_data %>% filter(sector %in% main_sectors,sector!="National Inventory Total") %>% 
  mutate(sector=fct_other(as.factor(sector),keep = major_sectors$sector)) %>%
  group_by(sector,Prov,Year) %>% summarize(GHGs=sum(GHGs),GHGs_2005_net_30=sum(GHGs_2005_net_30))

#put provincial emissions into the sample
large_sec<-large_sec%>%left_join(prov_samp,by=c("Prov","Year"))%>% left_join(pop_data%>%mutate(Year=as.numeric(Year)),by=c("Prov"="Code","Year"))

#sub_samp$Prov<-fct_rev(sub_samp$Prov)

#sub_samp<-merge(sub_samp,power_data_exp,by.x=c("Prov","Year"),by.y=c("Code","Year"))

ggplot(large_sec)+
  geom_area(aes(as.numeric(as.character(Year)),GHGs,fill=sector),color="black",position = "stack",size=0.1)+
  facet_grid( ~ Prov)+
  scale_fill_grey("",guide = "legend",start = 0.9,end=0.05)+
  blake_theme()+theme(
    panel.spacing = unit(.5,"lines"),
    legend.position = "right",
    legend.margin=margin(c(.05,0,.05,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
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
       title=paste(min(large_sec$Year),"-",max(large_sec$Year)," Regional GHG Emissions",sep=""),
       #subtitle="Excluding Electricity",
       #caption="Source: Environment Canada National Inventory Data, graph by @andrew_leach",
       NULL
  )


ggsave("images/inventory_ghgs_bw.png",width=16,height=9,bg="white",dpi=600)

my_palette<-c("#313695",colors_tableau10(),"Black")


ggplot(filter(large_sec,Prov!="TERR"),group=as.numeric(as.character(Year)))+
  geom_area(aes(as.numeric(as.character(Year)),GHGs/Prov_pop*10^6,colour=sector,fill=sector),position = "stack")+
  facet_grid( ~ Prov)+
  #scale_color_viridis("",discrete=TRUE,guide_legend(NULL),option="E")+
  #scale_fill_viridis("",discrete=TRUE,option="E")+
  scale_fill_manual("",values = my_palette,guide = "legend")+
  scale_colour_manual("",values=my_palette,guide = "legend")+
  guides(fill=guide_legend(nrow =2,byrow=FALSE))+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(.10,0,.10,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
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
  labs(x=NULL,y=expression('Annual Emissions  '*'(tCO'[2]*'e per capita)'),
       title="1990-2018 Provincial Per Capita GHG Emissions",
       #subtitle="Excluding Electricity",
       caption="Source: Environment Canada National Inventory Data, graph by @andrew_leach")


  ggplot(filter(large_sec,Prov!="TERR"),group=as.numeric(as.character(Year)))+
  geom_area(aes(as.numeric(as.character(Year)),GHGs/Prov_GHGs,colour=sector,fill=sector),position = "stack")+
  facet_grid( ~ Prov)+
  scale_y_continuous(labels = scales::percent)+
  #scale_color_viridis("",discrete=TRUE,guide_legend(NULL),option="E")+
  #scale_fill_viridis("",discrete=TRUE,option="E")+
  scale_fill_manual("",values = my_palette,guide = "legend")+
  scale_colour_manual("",values=my_palette,guide = "legend")+
  guides(fill=guide_legend(nrow =2,byrow=FALSE))+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(.10,0,.10,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
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
  labs(x=NULL,y=expression('Share of Annual Emissions  '*'(tCO'[2]*'e)'),
       title="1990-2019 Provincial GHG Emissions Shares",
       #subtitle="Excluding Electricity",
       caption="Source: Environment Canada National Inventory Data, graph by @andrew_leach")
ggsave("images/inventory_shares.png",width=16,height=9,bg="white",dpi=600)


#Environment Canada Emissions Projection Data
#file_loc<-"http://data.ec.gc.ca/data/substances/monitor/canada-s-greenhouse-gas-emissions-projections/Current-Projections-Actuelles/GHG-GES/detailed_GHG_emissions_GES_detaillees.csv"
#download.file(file_loc,destfile="data/ec_projections_2023.csv",mode = "wb")

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
file_loc<-"https://data-donnees.ec.gc.ca/data/substances/monitor/canada-s-greenhouse-gas-emissions-projections/Previous-Projections-Precedentes/2020/GHG-GES/detailed_GHG_emissions_GES_detaillees.csv"
download.file(file_loc,destfile="data/ec_projections_2020.csv",mode = "wb")

proj_data_2020<-read.csv("data/ec_projections_2020.csv",skip = 0,na = "-",fileEncoding = "Latin1") %>% 
  clean_names()%>%select(-c(2,4,6,8,10,12,14,28)) %>%
  pivot_longer(-c(1:7),names_to = "year",values_to = "emissions")%>%
  mutate(year=gsub("x","",year),emissions=gsub("-","0",emissions),year=as.numeric(year),emissions=as.numeric(emissions))
names(proj_data_2020)<-c("region","scenario","sector","subsector_level_1","subsector_level_2","subsector_level_3","unit","year","emissions")


#load 2018 detailed projections data

file_loc<-"http://data.ec.gc.ca/data/substances/monitor/canada-s-greenhouse-gas-emissions-projections/Previous-Projections-Precedentes/2018/GHG-GES/detailed_GHG_emissions_GES_detaillees.csv"
download.file(file_loc,destfile="data/ec_projections_2018.csv",mode = "wb")
proj_data_2018<-read.csv("data/ec_projections_2018.csv",skip = 0,na = "-",fileEncoding = "Latin1") %>% 
  clean_names()%>%select(-c(2,4,6,8,10,12,14,28)) %>%
  pivot_longer(-c(1:7),names_to = "year",values_to = "emissions")%>%
  mutate(year=gsub("x","",year),emissions=gsub("-","0",emissions),year=as.numeric(year),emissions=as.numeric(emissions))

names(proj_data_2018)<-c("region","scenario","sector","subsector_level_1","subsector_level_2","subsector_level_3","unit","year","emissions")


#load 2019 detailed projections data
file_loc<-"http://data.ec.gc.ca/data/substances/monitor/canada-s-greenhouse-gas-emissions-projections/Previous-Projections-Precedentes/2019/GHG-GES/detailed_GHG_emissions_GES_detaillees.csv"
download.file(file_loc,destfile="data/ec_projections_2019.csv",mode = "wb")


proj_data_2019<-
  read_csv("data/ec_projections_2019.csv",skip = 0,na = "-",col_types = cols(.default = "c")) %>%
  clean_names()%>%select(-c(2,4,6,8,10,12,14,28)) %>%
  mutate_if(is.double,as.character()) %>%
  pivot_longer(-c(1:7),names_to = "year",values_to = "emissions")%>%
  mutate(year=gsub("x","",year),emissions=gsub("-","0",emissions),year=as.numeric(year),emissions=as.numeric(emissions))
names(proj_data_2019)<-c("region","scenario","sector","subsector_level_1","subsector_level_2","subsector_level_3","unit","year","emissions")


proj_data<-proj_data_2019 %>% bind_rows(proj_data_2023,proj_data_2022,proj_data_2020,proj_data_2018)%>%
  filter(scenario!="NIR 2018",scenario!="NIR 2019",scenario!="NIR 2021",sector!="Total")

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
                         "QC"="Quebec",
                         "NU"="Nunavut",
                         "NB"="New Brunswick",
                         "YT"="Yukon Territory",
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
  filter(scenario!="NIR 2018",scenario!="NIR 2019",scenario!="NIR 2020",scenario!="NIR 2021",scenario!="NIR 2022")%>%
  bind_rows(test=NIR_data %>%
    filter(sector %in% main_sectors)%>%
    mutate(sector=fct_other(sector,
    keep=c("Agriculture","Buildings","Electricity","Heavy Industry","Oil and Gas","Transportation"),                        
    other_level = "Waste and Others"
    ))%>%
  select(sector,year=Year,prov=Prov,emissions=GHGs)%>%
  group_by(year,prov,sector) %>% summarise(emissions=sum(emissions))%>% ungroup()%>%
  mutate(scenario="NIR 2022")
  )



#set terr agriculture equal to zero in years it doesn't appear


terr_ag_fix<-as_tibble(x=seq(1990,2020,1))%>% rename(year=value) %>% left_join(proj_data %>% filter(prov=="TERR",sector=="Agriculture",scenario=="NIR 2022")%>%
                                                                                 select(year,prov,sector,emissions))%>%
  mutate(emissions=na.fill(emissions,0),prov="TERR",sector="Agriculture",scenario="NIR 2022")


proj_data<-proj_data %>% filter(!((prov=="TERR")&(sector=="Agriculture") &(scenario=="NIR 2022"))) %>% bind_rows(terr_ag_fix)


#figure out allocation rules from LLM Thesis.
#load population projections
#need to format projections to Year Code Prop_pop in raw numbers, not thousands
pop_proj<-read_csv("pop_proj.csv",col_types = cols(.default = "c")) %>%
  pivot_longer(-c(Year),names_to = "region",values_to = "pop")%>%
  mutate(prov=as.factor(region),pop=as.numeric(gsub(",","",pop)),
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
  select(year=Year,prov,pop)%>% group_by(year,prov)%>%
  summarize(pop=sum(pop)*1000)%>%filter(year<=2030,year>2020)

pop_merge<-pop_data%>%select(year=Year,prov=Code,pop=Prov_pop) %>%
  bind_rows(pop_proj)%>%mutate(year=as.numeric(year))

alloc<-proj_data%>% left_join(pop_merge)%>%
  filter(scenario %in% c("NIR 2022","2021 Reference Case"))%>%
  filter( ((scenario=="NIR 2022")& (year<2021))|((scenario=="2021 Reference Case")& (year>=2021)))

per_cap_2030<-pop_merge %>% filter(year==2030,prov!="Canada")%>%
  mutate(per_cap_2030=pop/sum(pop)*425)%>%
  ungroup()%>%
  select(prov,per_cap_2030)

last_5 <- proj_data %>% group_by(prov) %>% 
  filter(scenario %in% c("NIR 2022"),
         year %in%c(2016,2017,2018,2019,2020))%>%
  group_by(prov,year)%>% summarize(ghgs=sum(emissions))%>%
  summarize(ghgs=mean(ghgs))%>%
  ungroup()%>%
  mutate(last_5=ghgs/sum(ghgs)*425)%>%
  select(prov,last_5)


basis_2005 <- proj_data %>% group_by(prov) %>% 
  filter(scenario %in% c("NIR 2022"),
         year %in%c(2005))%>%
  group_by(prov,year)%>% summarize(ghgs=sum(emissions))%>%
  ungroup()%>%
  mutate(basis_2005=ghgs/sum(ghgs)*425)%>%
  select(prov,basis_2005)


#proj_data<-proj_data%>% filter(sector!="Total")


#for the plot data, we want NIR pre-2019 and projections post-2019
proj_data$prov<-factor(proj_data$prov,levels = (c("Canada","BC","AB","SK","MB","ON","QC","NB", "NS", "PE", "NL","ATL","OTHER ATL","TERR")))

proj_data<-proj_data %>% left_join(basis_2005)%>% left_join(last_5)%>% left_join(per_cap_2030)

proj_data<-proj_data %>% filter(!((scenario=="2021 Reference Case") & (year<=2020)))

proj_data<-proj_data %>%mutate(prov=factor(prov, levels=c("Canada" ,"BC","AB" ,"SK","MB", "ON",     "QC" ,  "ATL" ,   "TERR"  )))

testing<-filter(proj_data,emissions>0 & scenario%in% c("NIR 2022", "2021 Reference Case") & prov !="Canada")

inventory<-"NIR 2022"
project_case<-"2022 Additional Measures Scenario"
nir_year<-2020

proj_graph<-function(){
  theme_minimal()+theme(
  legend.position = "right",
  legend.margin=margin(c(.05,0,.05,0),unit="cm"),
  legend.text = element_text(colour="black", size = 12),
  plot.caption = element_text(size = 10, face = "italic",hjust=0),
  plot.title = element_text(size=16,face = "bold"),
  plot.subtitle = element_text(size = 10),
  panel.grid.minor = element_blank(),
  text = element_text(size = 20,face = "bold"),
  axis.text.y = element_text(size = 12,face = "bold", colour="black"),
  #axis.text.x = element_blank(),
  axis.text.x = element_text(size = 12, colour = "black", angle = 90,hjust=0.5,vjust=0.5),
  strip.text.x = element_text(size = 12, colour = "black", angle = 0),
  axis.title.y = element_text(size = 14,face = "bold", colour="black"),
)
  
}

proj_subtitle<-paste("2022 National Inventory (1990-2020) levels and 2022 Additional Measures Scenario projections (2020-2035, lighter fill)",sep="")
proj_caption<-"Source: Environment and Climate Change Canada. Graph by @andrew_leach."
proj_labs<- labs(x=NULL,y=expression('Annual Emissions  '*'(MtCO'[2]*'e)'),
                  #title="Canadian GHG Emissions by Province",
                  subtitle=proj_subtitle,
                  caption=proj_caption,
                  NULL)


prov_proj<-
  ggplot(proj_data %>% filter(scenario%in% c(inventory,project_case) & prov !="Canada")%>%
           filter((scenario==project_case & year>nir_year)|(scenario==inventory & year<=nir_year)))+
  geom_area(aes(year,emissions,fill=sector),color="black",position = "stack",size=0.1,alpha=.4)+
  geom_area(data=filter(proj_data,emissions>0 & scenario%in% c(inventory,project_case) & prov !="Canada" & year<=2020),
            aes(year,emissions,fill=sector),color="black",position = "stack",size=0.1,alpha=.8)+
  facet_wrap( ~ prov,nrow = 1)+
  scale_x_continuous(breaks=pretty_breaks())+
  scale_fill_viridis("",discrete=TRUE,option="B")+
  scale_linetype_manual("",values=c("11","22","33"),guide = "legend")+
  proj_graph()+
  NULL
  
prov_proj+proj_labs+
scale_fill_viridis("",discrete=TRUE,option="cividis")
ggsave("images/inventory_proj.png",dpi = 300,width=14, height=7,bg="white")


prov_proj+proj_labs+
scale_fill_grey("",guide = "legend",start = 0.85,end=0)+
ggsave("images/inventory_proj_bw.png",dpi = 300,width=14, height=7,bg="white")


  prov_proj+proj_labs+
  geom_line(aes(year,basis_2005,lty=str_wrap("Pro-rated share of 2030 target based on 2005 GHGs",width = 20)),color="black",size=1.05)+
  geom_line(aes(year,per_cap_2030,lty=str_wrap("Equal per capita share of 2030 target",width = 20)),color="black",size=1.05)+
  geom_line(aes(year,last_5,lty=str_wrap("Pro-rated share of 2030 target based on 2016-20 GHGs",width = 20)),color="black",size=1.05)
ggsave("images/inventory_proj_targets.png",dpi = 300,width=14, height=7,bg="white")


prov_proj+proj_labs+
geom_line(aes(year,basis_2005,lty=str_wrap("Pro-rated share of 2030 target based on 2005 GHGs",width = 20)),color="black",size=1.05)+
  NULL
ggsave("images/inventory_proj_target.png",dpi = 300,width=14, height=7,bg="white")

prov_proj+proj_labs+
geom_line(aes(year,per_cap_2030,lty=str_wrap("Equal per capita share of 2030 target",width = 20)),color="black",size=1.05)+
geom_line(aes(year,last_5,lty=str_wrap("Pro-rata share of 2030 target based on 2016-20 GHGs",width = 20)),color="black",size=1.05)
ggsave("images/inventory_proj_targets.png",dpi = 300,width=14, height=7,bg="white")



prov_proj+proj_labs+
scale_fill_grey("",guide = "legend",start = 0.85,end=0)+
geom_line(aes(year,basis_2005,lty=str_wrap("Pro-rated share of 2030 target based on 2005 GHGs",width = 20)),color="black",size=1.05)+
geom_line(aes(year,per_cap_2030,lty=str_wrap("Equal per capita share of 2030 target",width = 20)),color="black",size=1.05)+
geom_line(aes(year,last_5,lty=str_wrap("Pro-rated share of 2030 target based on 2016-20 GHGs",width = 20)),color="black",size=1.05)
ggsave("images/inventory_proj_targets_bw.png",dpi = 300,width=14, height=7,bg="white")


sector_proj<-
  ggplot(proj_data %>% filter(scenario%in% c(inventory,project_case) & prov !="Canada" )%>%
           filter((scenario==project_case & year>nir_year)|(scenario==inventory & year<=nir_year)))+
  geom_area(aes(year,emissions,fill=prov),color="black",position = "stack",size=0.1,alpha=.4)+
  geom_area(data=filter(proj_data,emissions>0 & scenario%in% c(inventory,project_case) & prov !="Canada" & year<=2020),
            aes(year,emissions,fill=prov),color="black",position = "stack",size=0.1,alpha=.8)+
  facet_wrap( ~ sector,nrow = 1)+
  scale_x_continuous(breaks=pretty_breaks())+
  scale_fill_viridis("",discrete=TRUE,option="B")+
  scale_colour_manual("",values="black",guide = "legend")+
  proj_graph()+
  NULL

sector_proj+proj_labs+
  scale_fill_viridis("",discrete=TRUE,option="cividis")
labs(title="Canadian GHG Emissions by Sector")
ggsave("images/sector_proj.png",dpi = 300,width=14, height=7,bg="white")

sector_proj+
  scale_fill_grey("",guide = "legend",start = 0.85,end=0)+
  proj_labs+
  labs(title="Canadian GHG Emissions by Sector")
ggsave("images/sector_proj_bw.png",dpi = 300,width=14, height=7,bg="white")


sector_proj_graph<-function(data_sent,inventory_sent=inventory,project_sent=project_case,cut_year=nir_year,
                            sector_sent="Oil and Gas",file_sent="images/oil_proj.png",dpi=300){
#data_sent<-proj_data
cases<-c(inventory_sent,project_sent)
data_sent%>% filter(scenario %in% cases & prov !="Canada" & sector==sector_sent)%>%
             filter((scenario==project_case & year>cut_year)|(scenario==inventory & year<=cut_year))%>%
  
ggplot()+
  geom_area(aes(year,ifelse(scenario==inventory,emissions,NA),fill=prov),color="black",position = "stack",linewidth=0.1)+
  geom_area(aes(year,emissions,fill=prov),color="black",position = "stack",linewidth=0.1,alpha=0.4)+
  scale_x_continuous(breaks=pretty_breaks(n=15))+
  #scale_fill_viridis("",discrete=TRUE,option="mako")+
  scale_fill_viridis("",discrete=TRUE,option="cividis")+
  scale_colour_manual("",values="black",guide = "legend")+
  proj_graph()+
  proj_labs+
  theme(axis.text.x = element_text(size = 12, colour = "black",angle = 0, hjust=0.5,vjust=0.5))+
  labs(title=paste("Canadian GHG Emissions from ",sector_sent,sep=""))
ggsave(file_sent,dpi = 300,width=14, height=7,bg="white")
}

sector_proj_graph(proj_data,sector_sent = "Oil and Gas",file_sent = "images/oil_proj.png")
sector_proj_graph(proj_data,sector_sent = "Transportation",file_sent = "images/transp_proj.png")
sector_proj_graph(proj_data,sector_sent = "Electricity",file_sent = "images/power_proj.png")
sector_proj_graph(proj_data,sector_sent = "Buildings",file_sent = "images/buildings_proj.png")
sector_proj_graph(proj_data,sector_sent = "Agriculture",file_sent = "images/agriculture_proj.png")


prov_proj_graph<-function(data_sent,inventory_sent=inventory,project_sent=project_case,cut_year=nir_year,
                            prov_name="Alberta",prov_sent="AB",file_sent="images/AB_proj.png",dpi=300){
  #data_sent<-proj_data
  cases<-c(inventory_sent,project_sent)
  data_sent%>% filter(scenario %in% cases & sector %in% main_sectors & prov==prov_sent)%>%
    filter((scenario==project_case & year>cut_year)|(scenario==inventory & year<=cut_year))%>%
    
    ggplot()+
    geom_area(aes(year,ifelse(scenario==inventory,emissions,NA),fill=sector),color="black",position = "stack",linewidth=0.1)+
    geom_area(aes(year,emissions,fill=sector),color="black",position = "stack",linewidth=0.1,alpha=0.4)+
    scale_x_continuous(breaks=pretty_breaks(n=15))+
    #scale_fill_viridis("",discrete=TRUE,option="mako")+
    scale_fill_viridis("",discrete=TRUE,option="cividis")+
    scale_colour_manual("",values="black",guide = "legend")+
    proj_graph()+
    proj_labs+
    theme(axis.text.x = element_text(size = 12, colour = "black",angle = 0, hjust=0.5,vjust=0.5))+
    labs(title=paste(prov_name," Provincial GHG Emissions by Sector",sep=""))
  ggsave(file_sent,dpi = 300,width=14, height=7,bg="white")
}

prov_proj_graph(proj_data,prov_name="Alberta",prov_sent = "AB",file_sent = "images/AB_proj.png")

prov_proj_graph(proj_data,prov_name="Ontario",prov_sent = "ON",file_sent = "images/ON_proj.png")

prov_proj_graph(proj_data,prov_name="Quebec",prov_sent = "QC",file_sent = "images/QC_proj.png")

prov_proj_graph(proj_data,prov_name="Alberta",prov_sent = "BC",file_sent = "images/BC_proj.png")


prov_sector_proj_graph<-function(data_sent,inventory_sent=inventory,project_sent=project_case,cut_year=nir_year,
                          sector_sent="Oil and Gas",
                          prov_name="Alberta",prov_sent="AB",file_sent="images/AB_proj.png",dpi=300){
  #data_sent<-proj_data
  cases<-c(inventory_sent,project_sent)
  data_sent%>% filter(scenario %in% cases & sector %in% sector_sent & prov==prov_sent)%>%
    filter((scenario==project_case & year>cut_year)|(scenario==inventory & year<=cut_year))%>%
    
    ggplot()+
    geom_area(aes(year,ifelse(scenario==inventory,emissions,NA),fill=sector),color="black",position = "stack",linewidth=0.1)+
    geom_area(aes(year,emissions,fill=sector),color="black",position = "stack",linewidth=0.1,alpha=0.4)+
    scale_x_continuous(breaks=pretty_breaks(n=15))+
    #scale_fill_viridis("",discrete=TRUE,option="mako")+
    scale_fill_viridis("",discrete=TRUE,option="cividis")+
    scale_colour_manual("",values="black",guide = "legend")+
    proj_graph()+
    proj_labs+
    theme(axis.text.x = element_text(size = 12, colour = "black",angle = 0, hjust=0.5,vjust=0.5),
          legend.position = "none")+
    labs(title=paste(prov_name," Provincial GHG Emissions from ",sector_sent,sep=""))
  ggsave(file_sent,dpi = 300,width=14, height=7,bg="white")
}

prov_sector_proj_graph(proj_data,prov_name="Alberta",prov_sent = "AB",sector_sent="Oil and Gas",file_sent = "images/AB_oil_proj.png")


power_proj<-ggplot(proj_data %>% filter(scenario%in% c(inventory,project_case) & prov !="Canada" & sector=="Electricity")%>%
         filter((scenario==project_case & year>nir_year)|(scenario==inventory & year<=nir_year)))+
  geom_area(aes(year,emissions,fill=prov),color="black",position = "stack",size=0.1,alpha=.4)+
  geom_area(data=filter(proj_data,emissions>0 & scenario%in% c(inventory,project_case) & prov !="Canada" & year<=2020 & sector=="Electricity"),
            aes(year,emissions,fill=prov),color="black",position = "stack",size=0.1,alpha=.8)+
  scale_x_continuous(breaks=pretty_breaks())+
  scale_fill_viridis("",discrete=TRUE,option="B")+
  scale_colour_manual("",values="black",guide = "legend")+
  proj_graph()+
  proj_labs+
  labs(title="Canadian Electricity Sector GHG Emissions")
power_proj       
ggsave("images/power_proj.png",dpi = 300,width=14, height=7,bg="white")


ab_power_proj<-ggplot(proj_data %>% filter(scenario%in% c(inventory,project_case) & prov =="AB" & sector=="Electricity")%>%
                     filter((scenario==project_case & year>nir_year)|(scenario==inventory & year<=nir_year)))+
  geom_area(aes(year,emissions,fill=prov),color="black",position = "stack",size=0.1,alpha=.4)+
  geom_area(data=filter(proj_data,emissions>0 & scenario%in% c(inventory,project_case) & prov =="AB" & year<=2020 & sector=="Electricity"),
            aes(year,emissions,fill=prov),color="black",position = "stack",size=0.1,alpha=.8)+
  scale_x_continuous(breaks=pretty_breaks())+
  scale_fill_viridis("",discrete=TRUE,option="B")+
  scale_colour_manual("",values="black",guide = "legend")+
  proj_graph()+
  theme(legend.position = "none")+
  proj_labs+
  labs(title="Alberta Electricity Sector GHG Emissions")
ab_power_proj       
ggsave("images/power_proj_AB.png",dpi = 300,width=14, height=7,bg="white")


ggplot(proj_data %>% filter(scenario%in% c(inventory,project_case) & prov !="Canada" & sector=="Oil and Gas")%>%
         filter((scenario==project_case & year>nir_year)|(scenario==inventory & year<=nir_year)))+
  geom_area(aes(year,emissions,fill=prov),color="black",position = "stack",size=0.1,alpha=.4)+
  geom_area(data=filter(proj_data,emissions>0 & scenario%in% c(inventory,project_case) & prov !="Canada" & year<=nir_year & sector=="Oil and Gas"),
            aes(year,emissions,fill=prov),color="black",position = "stack",size=0.1,alpha=.8)+
  scale_x_continuous(breaks=pretty_breaks())+
  scale_fill_viridis("",discrete=TRUE,option="B")+
  proj_graph()+ proj_labs+
  labs(title="Canadian GHG Emissions from Oil and Gas")+
  NULL
ggsave("images/oil_gas_proj.png",dpi = 300,width=14, height=7,bg="white")



ggplot(filter(proj_data,emissions>0 & scenario%in% c("NIR 2022") & prov !="Canada" & sector=="Oil and Gas"))+
  geom_area(aes(year,emissions,fill=prov),color="black",position = "stack",size=0.1)+
  scale_x_continuous(breaks=pretty_breaks())+
  #scale_color_viridis("",discrete=TRUE,guide_legend(NULL),option="E")+
  #scale_fill_viridis("",discrete=TRUE,option="E")+
  scale_fill_viridis("",discrete=TRUE,option="B")+
  #scale_fill_manual("",values = my_palette,guide = "legend")+
  #scale_fill_grey("",guide = "legend",start = 0.9,end=0.05)+
  scale_colour_manual("",values="black",guide = "legend")+
  #guides(fill=guide_legend(nrow =1,byrow=FALSE),color=guide_legend(nrow =1,byrow=FALSE))+
  theme_minimal()+theme(
    legend.position = "right",
    legend.margin=margin(c(.05,0,.05,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
    plot.caption = element_text(size = 10, face = "italic",hjust=0),
    plot.title = element_text(size=16,face = "bold"),
    plot.subtitle = element_text(size = 10),
    panel.grid.minor = element_blank(),
    text = element_text(size = 20,face = "bold"),
    axis.text.y = element_text(size = 12,face = "bold", colour="black"),
    #axis.text.x = element_blank(),
    axis.text.x = element_text(size = 10, colour = "black", hjust=0.5,vjust=0.5),
    strip.text.x = element_text(size = 12, colour = "black", angle = 0),
    axis.title.y = element_text(size = 14,face = "bold", colour="black"),
  )+
  labs(x=NULL,y=expression('Annual Emissions  '*'(MtCO'[2]*'e)'),
       title="Canadian GHG Emissions from Oil and Gas",
       #subtitle=paste("National Inventory Emissions from Oil and Gas",sep=""),
       caption=str_wrap("Source: Environment and Climate Change Canada. Graph by @andrew_leach.",width = 180))
ggsave("images/oil_gas.png",dpi = 300,width=14, height=7,bg="white")




ggplot(filter(proj_data,emissions>0 & scenario%in% c("NIR 2022", "2021 Reference Case") & prov !="Canada" & sector=="Oil and Gas"))+
  geom_area(aes(year,emissions,fill=prov),color="black",position = "stack",size=0.1,alpha=.4)+
  geom_area(data=filter(proj_data,emissions>0 & scenario%in% c("NIR 2022", "2020 Reference Case") & prov !="Canada" & year<=2018 & sector=="Oil and Gas"),
            aes(year,emissions,fill=prov),color="black",position = "stack",size=0.1,alpha=.8)+
  #geom_line(aes(year,net_30_2005,colour=str_wrap("30% below 2005 sector GHGs",width = 20)),linetype=1,size=1.05)+
  facet_wrap( ~ prov,nrow = 1)+
  scale_x_continuous(breaks=pretty_breaks())+
  #scale_color_viridis("",discrete=TRUE,guide_legend(NULL),option="E")+
  #scale_fill_viridis("",discrete=TRUE,option="E")+
  scale_fill_viridis("",discrete=TRUE,option="B")+
  #scale_fill_manual("",values = my_palette,guide = "legend")+
  #scale_fill_grey("",guide = "legend",start = 0.9,end=0.05)+
  scale_colour_manual("",values="black",guide = "legend")+
  #guides(fill=guide_legend(nrow =1,byrow=FALSE),color=guide_legend(nrow =1,byrow=FALSE))+
  blake_theme()+theme(
    panel.spacing = unit(2.5,"lines"),
    legend.position = "none",
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
       title="Canadian GHG Emissions from Oil and Gas",
       subtitle=paste("National Inventory (1990-2020) levels and 2021 Reference Case projections (2020-2030)",sep=""),
       caption=str_wrap("Source: Environment and Climate Change Canada. Graph by @andrew_leach.",width = 180))
ggsave("images/oil_gas_proj_prov.png",dpi = 300,width=14, height=7,bg="white")



ggplot(filter(proj_data,emissions>0 & scenario%in% c("NIR 2022", "2021 Reference Case") & prov !="Canada" & sector=="Electricity"))+
  geom_area(aes(year,emissions,fill=sector),color="black",position = "stack",size=0.1,alpha=.4)+
  geom_area(data=filter(proj_data,emissions>0 & scenario%in% c("NIR 2022", "2020 Reference Case") & prov !="Canada" & year<=2018 & sector=="Electricity"),
            aes(year,emissions,fill=sector),color="black",position = "stack",size=0.1,alpha=.8)+
  #geom_line(aes(year,net_30_2005,colour=str_wrap("30% below 2005 sector GHGs",width = 20)),linetype=1,size=1.05)+
  facet_wrap( ~ prov,nrow = 1)+
  scale_x_continuous(breaks=pretty_breaks())+
  #scale_color_viridis("",discrete=TRUE,guide_legend(NULL),option="E")+
  #scale_fill_viridis("",discrete=TRUE,option="E")+
  scale_fill_viridis("",discrete=TRUE,option="B")+
  #scale_fill_manual("",values = my_palette,guide = "legend")+
  #scale_fill_grey("",guide = "legend",start = 0.9,end=0.05)+
  scale_colour_manual("",values="black",guide = "legend")+
  #guides(fill=guide_legend(nrow =1,byrow=FALSE),color=guide_legend(nrow =1,byrow=FALSE))+
  blake_theme()+theme(
    panel.spacing = unit(2.5,"lines"),
    legend.position = "none",
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
       subtitle=paste("National Inventory (1990-2020) levels and 2021 Reference Case projections (2020-2030, lighter fill)",sep=""),
       caption=str_wrap("Source: Environment and Climate Change Canada. Graph by @andrew_leach.",width = 180))
ggsave("images/power_proj_prov.png",dpi = 300,width=14, height=7,bg="white")












inventory_provs<-ggplot(filter(proj_data,emissions>0 & scenario%in% c("NIR 2022") & prov !="Canada"))+
  geom_area(aes(year,emissions,fill=sector),color="black",position = "stack",size=0.1)+
  #geom_line(aes(year,net_30_2005,colour=str_wrap("30% below 2005 provincial GHGs",width = 20)),linetype=1,size=1.05)+
  facet_wrap( ~ prov,nrow = 1)+
  scale_x_continuous(breaks=pretty_breaks())+
  scale_y_continuous(breaks=pretty_breaks(n=5))+
  
  scale_fill_viridis("",discrete=TRUE,option="B")+
  #scale_fill_manual("",values = my_palette,guide = "legend")+
  #scale_fill_grey("",guide = "legend",start = 0.85,end=0.01)+
  scale_colour_manual("",values="black",guide = "legend")+
  geom_hline(aes(yintercept=0))+
  #guides(fill=guide_legend(nrow =1,byrow=FALSE),color=guide_legend(nrow =1,byrow=FALSE))+
  theme_tufte()+theme(
    legend.position = "right",
    legend.margin=margin(c(.05,0,.05,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
    plot.caption = element_text(size = 10, face = "italic",hjust=0),
    plot.title = element_text(size=16,face = "bold"),
    plot.subtitle = element_text(size = 10),
    #panel.grid.minor = element_blank(),
    text = element_text(size = 20,face = "bold"),
    axis.text.y = element_text(size = 12,face = "bold", colour="black"),
    #axis.text.x = element_blank(),
    axis.text.x = element_text(size = 10, colour = "black", angle = 90,hjust=0.5,vjust=0.5),
    strip.text.x = element_text(size = 12, colour = "black", angle = 0),
    axis.title.y = element_text(size = 14,face = "bold", colour="black"),
  )+labs(x=NULL,y=expression('Annual Emissions  '*'(MtCO'[2]*'e)'))
inventory_provs+
  theme(legend.position = c(.9,.8),
        text = element_text(size = 20,family="Times New Roman MS"))
ggsave("images/inventory_provs.png",dpi = 300,width=18, height=7,bg="white")

#inventory_provs+ theme(
#  text = element_text(size = 20,family="Times New Roman MS"))
#ggsave("images/inventory_provs_plain.png",dpi = 300,width=14, height=7,bg="white")
#ggsave("images/inventory_provs_plain.eps",width = 14,height=7,dpi=300,bg="white",device=cairo_ps)


inventory_provs+ 
  scale_fill_grey("",start=0.85, end=0)+
  theme(legend.position = c(.9,.8),
  text = element_text(size = 20,family="Times New Roman MS"))
ggsave("images/inventory_provs_plain.png",dpi = 300,width=14, height=7,bg="white")
ggsave("images/inventory_provs_plain.eps",width = 14,height=7,dpi=300,bg="white",device=cairo_ps)




nir_oil_elec_ab<-
  ggplot(new_nir %>% filter(Prov=="AB",sector%in%oil_sands_sectors)%>%
                     mutate(sector=as_factor(sector),
                            sector=fct_relevel(sector,"Conventional Light Oil Production")))+
  geom_area(aes(Year,GHGs,group=sector,fill=sector),position="stack",color="black",size=0.25)+
  geom_line(data=new_nir %>% filter(Prov=="AB",sector=="Electricity"),
            aes(Year,GHGs,group=sector,color=sector),size=1.5,lty="21")+
  #scale_fill_manual("",values = c(colors_tableau10(),colors_tableau10_medium()),guide = "legend")+
  scale_fill_manual("",values=viridis(10,option="B",direction = 1)[c(10,6,4)])+
  scale_color_manual("",values="black")+
  #scale_fill_viridis("",discrete=TRUE,option="D")+
  scale_x_continuous(breaks=pretty_breaks(n=15), expand = c(0,0))+
  scale_y_continuous(breaks=pretty_breaks(n=5), expand = c(0,0))+
  guides(fill=guide_legend(nrow =1,byrow=TRUE))+
  theme_tufte()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(.05,0,.05,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
    plot.caption = element_text(size = 10, face = "italic",hjust=0),
    plot.title = element_text(size=16,face = "bold"),
    plot.subtitle = element_text(size = 10),
    #panel.grid.minor = element_blank(),
    text = element_text(size = 20,face = "bold"),
    axis.text.y = element_text(size = 12,face = "bold", colour="black"),
    #axis.text.x = element_blank(),
    axis.text.x = element_text(size = 10, colour = "black", hjust=0.5,vjust=0.5),
    strip.text.x = element_text(size = 12, colour = "black", angle = 0),
    axis.title.y = element_text(size = 14,face = "bold", colour="black"),
  )+
  labs(x=NULL,y=expression('Annual GHG Emissions  '*'(Mt CO'[2]*'e)'))
nir_oil_elec_ab
ggsave("images/nir_oil_elec_ab.png",width=16,height=9,bg="white",dpi=300)

nir_oil_elec_ab+ 
  scale_fill_grey("",start=.75,end=.25)+
  theme(
  text = element_text(size = 20,family="Times New Roman MS"))
ggsave("images/nir_oilsands_elec_ab_plain.png",dpi = 300,width=14, height=7,bg="white")
ggsave("images/nir_oilsands_elec_ab_plain.eps",width = 14,height=7,dpi=300,bg="white",device=cairo_ps)



inventory_provs+
  labs(title="Canadian GHG Emissions by Province",
       subtitle=paste("2022 National Inventory (1990-2020)",sep=""),
       caption=str_wrap("Source: Environment and Climate Change Canada 2022 National Inventory (1990-2020). Graph by @andrew_leach.",width = 180),
       NULL
  )
ggsave("images/inventory_provs.png",dpi = 300,width=14, height=7,bg="white")

ggplot(filter(proj_data,year<=2015,emissions>0 & scenario%in% c("NIR 2022") & prov !="Canada"))+
  geom_area(aes(year,emissions,fill=sector),color="black",position = "stack",size=0.5,alpha=.6)+
  #geom_line(aes(year,net_30_2005,colour=str_wrap("30% below 2005 provincial GHGs",width = 20)),linetype=1,size=1.05)+
  facet_wrap( ~ prov,nrow = 1)+
  scale_x_continuous(breaks=pretty_breaks())+
  scale_y_continuous(breaks=pretty_breaks(n=5))+
  
  scale_fill_viridis("",discrete=TRUE,option="B")+
  
  #scale_fill_manual("",values = my_palette,guide = "legend")+
  #scale_fill_grey("",guide = "legend",start = 0.85,end=0.01)+
  scale_colour_manual("",values="black",guide = "legend")+
  geom_hline(aes(yintercept=0))+
  #guides(fill=guide_legend(nrow =1,byrow=FALSE),color=guide_legend(nrow =1,byrow=FALSE))+
  theme_tufte()+theme(
    legend.position = c(.95, .7),
    legend.margin=margin(c(.05,0,.05,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
    plot.caption = element_text(size = 10, face = "italic",hjust=0),
    plot.title = element_text(size=16,face = "bold"),
    plot.subtitle = element_text(size = 10),
    #panel.grid.minor = element_blank(),
    text = element_text(size = 20,face = "bold"),
    axis.text.y = element_text(size = 12,face = "bold", colour="black"),
    #axis.text.x = element_blank(),
    axis.text.x = element_text(size = 10, colour = "black", angle = 90,hjust=0.5,vjust=0.5),
    strip.text.x = element_text(size = 12, colour = "black", angle = 0),
    axis.title.y = element_text(size = 14,face = "bold", colour="black"),
  )+
  labs(x=NULL,y=expression('Annual Emissions  '*'(MtCO'[2]*'e)'),
       #title="Canadian GHG Emissions by Province as of 2015",
       #subtitle=paste("2021 National Inventory (1990-2015)",sep=""),
       #caption=str_wrap("Source: Environment and Climate Change Canada 2022 National Inventory (1990-2020). Graph by @andrew_leach.",width = 180),
       NULL
  )
ggsave("images/inventory_provs_2015.png",dpi = 600,width=14, height=7)



inventory_sector<-ggplot(filter(proj_data,emissions>0 & scenario%in% c("NIR 2022") & prov !="Canada")%>%
                           mutate(prov=factor(prov,levels=c("Canada" ,"BC","AB" ,"SK","MB", "ON",     "QC" ,  "ATL" ,   "TERR"  ))))+
  geom_area(aes(year,emissions,fill=prov),color="black",position = "stack",size=0.1)+
  #geom_line(aes(year,net_30_2005,colour=str_wrap("30% below 2005 provincial GHGs",width = 20)),linetype=1,size=1.05)+
  facet_wrap( ~ sector,nrow = 1)+
  scale_x_continuous(breaks=pretty_breaks())+
  #scale_color_viridis("",discrete=TRUE,guide_legend(NULL),option="E")+
  #scale_fill_viridis("",discrete=TRUE,option="E")+
  scale_fill_viridis("",discrete=TRUE,option="B")+
  #scale_fill_manual("",values = my_palette,guide = "legend")+
  #scale_fill_grey("",guide = "legend",start = 0.9,end=0.05)+
  scale_colour_manual("",values="black",guide = "legend")+
  geom_hline(aes(yintercept=0))+
  #guides(fill=guide_legend(nrow =1,byrow=FALSE),color=guide_legend(nrow =1,byrow=FALSE))+
  theme_tufte()+theme(
    legend.position = "right",
    legend.margin=margin(c(.05,0,.05,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
    plot.caption = element_text(size = 10, face = "italic",hjust=0),
    plot.title = element_text(size=16,face = "bold"),
    plot.subtitle = element_text(size = 10),
    #panel.grid.minor = element_blank(),
    text = element_text(size = 20,face = "bold"),
    axis.text.y = element_text(size = 12,face = "bold", colour="black"),
    #axis.text.x = element_blank(),
    axis.text.x = element_text(size = 10, colour = "black", angle = 90,hjust=0.5,vjust=0.5),
    strip.text.x = element_text(size = 12, colour = "black", angle = 0),
    axis.title.y = element_text(size = 14,face = "bold", colour="black"),
  )+
  labs(x=NULL,y=expression('Annual Emissions  '*'(MtCO'[2]*'e)'))
inventory_sector
ggsave("images/inventory_sector_plain.png",dpi = 300,width=18, height=7,bg="white")

inventory_sector+
  scale_fill_grey("",start=0.85, end=0)+
  theme(legend.position = c(.95,.6),
        text = element_text(size = 20,family="Times New Roman MS"))
ggsave("images/inventory_sector_plain.png",dpi = 300,width=14, height=7,bg="white")
ggsave("images/inventory_sector_plain.eps",width = 14,height=7,dpi=300,bg="white",device=cairo_ps)


inventory_sector+
labs( title="Canadian GHG Emissions by Sector",
       #subtitle=paste("2020 National Inventory (1990-2018)",sep=""),
       caption=str_wrap("Source: Environment and Climate Change Canada 2022 National Inventory (1990-2020). Graph by @andrew_leach.",width = 180),
       NULL
  )
ggsave("images/inventory_sector.png",dpi = 300,width=14, height=7,bg="white")


ggplot(filter(proj_data,year<=2015,emissions>0 & scenario%in% c("NIR 2022") & prov !="Canada"))+
  geom_area(aes(year,emissions,fill=prov),color="black",position = "stack",size=0.1,alpha=.6)+
  #geom_line(aes(year,net_30_2005,colour=str_wrap("30% below 2005 provincial GHGs",width = 20)),linetype=1,size=1.05)+
  facet_wrap( ~ sector,nrow = 1)+
  scale_x_continuous(breaks=pretty_breaks())+
  #scale_color_viridis("",discrete=TRUE,guide_legend(NULL),option="E")+
  #scale_fill_viridis("",discrete=TRUE,option="E")+
  scale_fill_viridis("",discrete=TRUE,option="B")+
  #scale_fill_manual("",values = my_palette,guide = "legend")+
  #scale_fill_grey("",guide = "legend",start = 0.9,end=0.05)+
  scale_colour_manual("",values="black",guide = "legend")+
  geom_hline(aes(yintercept=0))+
  #guides(fill=guide_legend(nrow =1,byrow=FALSE),color=guide_legend(nrow =1,byrow=FALSE))+
  theme_tufte()+theme(
    legend.position = "right",
    legend.margin=margin(c(.05,0,.05,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
    plot.caption = element_text(size = 10, face = "italic",hjust=0),
    plot.title = element_text(size=16,face = "bold"),
    plot.subtitle = element_text(size = 10),
    #panel.grid.minor = element_blank(),
    text = element_text(size = 20,face = "bold"),
    axis.text.y = element_text(size = 12,face = "bold", colour="black"),
    #axis.text.x = element_blank(),
    axis.text.x = element_text(size = 10, colour = "black", angle = 90,hjust=0.5,vjust=0.5),
    strip.text.x = element_text(size = 12, colour = "black", angle = 0),
    axis.title.y = element_text(size = 14,face = "bold", colour="black"),
  )+
  labs(x=NULL,y=expression('Annual Emissions  '*'(MtCO'[2]*'e)'),
       #title="Canadian GHG Emissions by Sector",
       #subtitle=paste("2020 National Inventory (1990-2018)",sep=""),
       #caption=str_wrap("Source: Environment and Climate Change Canada 2022 National Inventory (1990-2020). Graph by @andrew_leach.",width = 180),
       NULL
  )
ggsave("images/inventory_sector_2015.png",dpi = 300,width=14, height=7)



ggplot(filter(proj_data,emissions>0 & scenario%in% c("NIR 2022") & prov !="Canada" & sector=="Electricity"))+
  geom_area(aes(year,emissions,fill=prov),color="black",position = "stack",size=0.1,alpha=.6)+
  #geom_line(aes(year,net_30_2005,colour=str_wrap("30% below 2005 provincial GHGs",width = 20)),linetype=1,size=1.05)+
  #facet_wrap( ~ sector,nrow = 1)+
  scale_x_continuous(breaks=pretty_breaks())+
  #scale_color_viridis("",discrete=TRUE,guide_legend(NULL),option="E")+
  #scale_fill_viridis("",discrete=TRUE,option="E")+
  scale_fill_viridis("",discrete=TRUE,option="B")+
  #scale_fill_manual("",values = my_palette,guide = "legend")+
  #scale_fill_grey("",guide = "legend",start = 0.9,end=0.05)+
  scale_colour_manual("",values="black",guide = "legend")+
  geom_hline(aes(yintercept=0))+
  #guides(fill=guide_legend(nrow =1,byrow=FALSE),color=guide_legend(nrow =1,byrow=FALSE))+
  theme_tufte()+theme(
    legend.position = "right",
    legend.margin=margin(c(.05,0,.05,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
    plot.caption = element_text(size = 10, face = "italic",hjust=0),
    plot.title = element_text(size=16,face = "bold"),
    plot.subtitle = element_text(size = 10),
    #panel.grid.minor = element_blank(),
    text = element_text(size = 20,face = "bold"),
    axis.text.y = element_text(size = 12,face = "bold", colour="black"),
    #axis.text.x = element_blank(),
    axis.text.x = element_text(size = 10, colour = "black", angle = 90,hjust=0.5,vjust=0.5),
    strip.text.x = element_text(size = 12, colour = "black", angle = 0),
    axis.title.y = element_text(size = 14,face = "bold", colour="black"),
  )+
  labs(x=NULL,y=expression('Annual Emissions  '*'(MtCO'[2]*'e)'),
       title="Canadian GHG Emissions by Sector",
       #subtitle=paste("2020 National Inventory (1990-2018)",sep=""),
       caption=str_wrap("Source: Environment and Climate Change Canada 2022 National Inventory (1990-2020). Graph by @andrew_leach.",width = 180),
       NULL
  )
ggsave("images/inventory_electricity.png",dpi = 300,width=14, height=7)



#per capita

proj_data %>% left_join(pop_data %>% 
                                group_by(Code)%>%
                                mutate(pop_2005=sum(Prov_pop*(Year=="2005")))%>%ungroup()%>%
                                mutate(Code=as_factor(Code),Year=as.double(Year)) %>% rename(year=Year,prov=Code))%>%
  filter(scenario%in% c("NIR 2022") & prov !="Canada")%>%
  mutate(prov=factor(prov, levels=c("Canada" ,"BC","AB" ,"SK","MB", "ON",     "QC" ,  "ATL" ,   "TERR"  )))%>%
  ggplot()+
  geom_area(aes(year,emissions/Prov_pop*10^6,fill=sector),color="black",position = "stack",size=0.1,alpha=.6)+
  #geom_line(aes(year,net_30_2005/pop_2005*10^6,colour=str_wrap("30% below 2005 provincial GHGs per capita",width = 20)),linetype=1,size=1.05)+
  facet_wrap( ~ prov,nrow = 1)+
  scale_x_continuous(breaks=pretty_breaks())+
  #scale_color_viridis("",discrete=TRUE,guide_legend(NULL),option="E")+
  #scale_fill_viridis("",discrete=TRUE,option="E")+
  scale_fill_viridis("",discrete=TRUE,option="B")+
  #scale_fill_manual("",values = my_palette,guide = "legend")+
  #scale_fill_grey("",guide = "legend",start = 0.9,end=0.05)+
  scale_colour_manual("",values="black",guide = "legend")+
  #guides(fill=guide_legend(nrow =1,byrow=FALSE),color=guide_legend(nrow =1,byrow=FALSE))+
  theme_tufte()+theme(
    legend.position = "right",
    legend.margin=margin(c(.05,0,.05,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
    plot.caption = element_text(size = 10, face = "italic",hjust=0),
    plot.title = element_text(size=16,face = "bold"),
    plot.subtitle = element_text(size = 10),
    #panel.grid.minor = element_blank(),
    text = element_text(size = 20,face = "bold"),
    axis.text.y = element_text(size = 12,face = "bold", colour="black"),
    #axis.text.x = element_blank(),
    axis.text.x = element_text(size = 10, colour = "black", angle = 90,hjust=0.5,vjust=0.5),
    strip.text.x = element_text(size = 12, colour = "black", angle = 0),
    axis.title.y = element_text(size = 14,face = "bold", colour="black"),
  )+
  labs(x=NULL,y=expression('Annual Emissions  '*'(tCO'[2]*'e per capita)'),
       #title="Canadian GHG Emissions Per Capita by Province",
       #subtitle=paste("2020 National Inventory (1990-2018)",sep=""),
       #caption=str_wrap("Source: Population via Statistics Canada, emissions via Environment and Climate Change Canada 2020 National Inventory (1990-2018). Graph by @andrew_leach.",width = 180)
       NULL
  )
ggsave("images/inventory_per_capita.png",dpi = 300,width=14, height=7,bg="white")


proj_data %>% left_join(pop_data %>% 
                          group_by(Code)%>%
                          mutate(pop_2005=sum(Prov_pop*(Year=="2005")))%>%ungroup()%>%
                          mutate(Code=as_factor(Code),Year=as.double(Year)) %>% rename(year=Year,prov=Code))%>%
  filter(scenario%in% c("NIR 2022") & prov !="Canada",year<2016)%>%
  mutate(prov=factor(prov, levels=c("Canada" ,"BC","AB" ,"SK","MB", "ON",     "QC" ,  "ATL" ,   "TERR"  )))%>%
  ggplot()+
  geom_area(aes(year,emissions/Prov_pop*10^6,fill=sector),color="black",position = "stack",size=0.1,alpha=.6)+
  geom_line(aes(year,net_30_2005/pop_2005*10^6,colour=str_wrap("30% below 2005 provincial GHGs per capita",width = 20)),linetype=1,size=1.05)+
  facet_wrap( ~ prov,nrow = 1)+
  scale_x_continuous(breaks=pretty_breaks())+
  #scale_color_viridis("",discrete=TRUE,guide_legend(NULL),option="E")+
  #scale_fill_viridis("",discrete=TRUE,option="E")+
  scale_fill_viridis("",discrete=TRUE,option="B")+
  #scale_fill_manual("",values = my_palette,guide = "legend")+
  #scale_fill_grey("",guide = "legend",start = 0.9,end=0.05)+
  scale_colour_manual("",values="black",guide = "legend")+
  #guides(fill=guide_legend(nrow =1,byrow=FALSE),color=guide_legend(nrow =1,byrow=FALSE))+
  theme_tufte()+theme(
    legend.position = "right",
    legend.margin=margin(c(.05,0,.05,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
    plot.caption = element_text(size = 10, face = "italic",hjust=0),
    plot.title = element_text(size=16,face = "bold"),
    plot.subtitle = element_text(size = 10),
    #panel.grid.minor = element_blank(),
    text = element_text(size = 20,face = "bold"),
    axis.text.y = element_text(size = 12,face = "bold", colour="black"),
    #axis.text.x = element_blank(),
    axis.text.x = element_text(size = 10, colour = "black", angle = 90,hjust=0.5,vjust=0.5),
    strip.text.x = element_text(size = 12, colour = "black", angle = 0),
    axis.title.y = element_text(size = 14,face = "bold", colour="black"),
  )+
  labs(x=NULL,y=expression('Annual Emissions  '*'(tCO'[2]*'e per capita)'),
       #title="Canadian GHG Emissions Per Capita by Province",
       #subtitle=paste("2020 National Inventory (1990-2018)",sep=""),
       #caption=str_wrap("Source: Population via Statistics Canada, emissions via Environment and Climate Change Canada 2020 National Inventory (1990-2018). Graph by @andrew_leach.",width = 180)
       NULL
  )
ggsave("images/inventory_per_capita_2015.png",dpi = 300,width=14, height=7)






#get older natcom and trends information

#getting national communication info
library(tabulizer)
f <- "data/canada_nc7.pdf"

# extract tables from only second page
oil_sands_table<-data.frame(extract_tables(f, pages = 143)[1],stringsAsFactors = F)
oil_sands_table<-oil_sands_table[-1,] #drop the first row
oil_sands_table<-oil_sands_table[-14,] #drop the total row
oil_sands_table<-oil_sands_table[,-7] #drop the growth column
oil_sands_table[1,1]<-"sector" #add variable labels
colnames(oil_sands_table)<-oil_sands_table[1,]
oil_sands_table<-oil_sands_table[-1,] #drop the first row


sector_table<-data.frame(extract_tables(f, pages = 142)[1],stringsAsFactors = F)
sector_table<-sector_table[-1,] #drop the first row
sector_table<-sector_table[,-7] #drop the growth column
sector_table[1,1]<-"sector" #add variable labels
colnames(sector_table)<-sector_table[1,]
sector_table<-sector_table[-1,] #drop the first row

nat_com<-rbind(sector_table,oil_sands_table)
nat_com<-nat_com %>% pivot_longer(cols=-sector,values_to = "GHGs",names_to = "Year") %>% mutate(GHGs=as.numeric(GHGs))
nat_com$sector[nat_com$sector=="Oil Sandsf"]<-"Oil Sands"
nat_com$Year<-as.numeric(as.character(nat_com$Year))
#nat_com$sector[nat_com$sector=="Oil Sands"]<-"Oil Sands (Mining, In-situ, Upgrading)"
#nat_com$sector[nat_com$sector=="Bitumen In Situ"]<-"In Situ"
#nat_com$sector[nat_com$sector=="Bitumen Mining"]<-"Mining and Extraction"
#nat_com$sector[nat_com$sector=="Bitumen Upgrading"]<-"Mining and Extraction"


#ECCC 2018 December projections

ec_2018_file <- "data/ECCC_DEC_2018.pdf"
# extract tables from only second page
econ_table<-data.frame(extract_tables(ec_2018_file, pages = 9),stringsAsFactors = F)


ec_2019_file<-"data/progress-towards-ghg-emissions-target-en.pdf"
progress_annex_table<-data.frame(extract_tables(ec_2019_file, pages = 12),stringsAsFactors = F)
progress_annex_table<-progress_annex_table[-c(1:7),]
progress_annex_table[,1:6] <- sapply(progress_annex_table[,1:6], as.numeric)
names(progress_annex_table)<-c("Year",
                               "Second_Biennial",
                               "2017_Ref",
                               "2018_Ref",
                               "Adds_Case",
                               "Target Case")
progress_annex_table$`Target Case`<-NULL
progress_annex_table$sector<-"Total, Canada"
progress_annex_table$Adds_Case[progress_annex_table$Year==2030] <-592


sectors<-unique(nat_com$sector)

NIR_CAN<-NIR_data %>%group_by(sector,Year) %>% summarize(GHGs=sum(GHGs)) %>% ungroup()%>%
  mutate(sector=fct_collapse(sector,
                             "Total, Canada" = c("Total","GHG TOTAL","National Inventory Total","Inventory Total"))
  )

NIR_CAN<-bind_rows(NIR_CAN,nat_com)




targets<-data.frame(Year=c(2020,2030,2050),target=731.7137*c(1-.17,1-.30,0),projection=c(728,722,NA),bau=c(768,815,NA),adds=c(690,583,NA))
targets$Year<-as.numeric(as.character(targets$Year))
targets$sector<-"Total, Canada"
cdn_data<-full_join(NIR_CAN,targets,by=c("Year","sector"))
cdn_data$Year<-as.numeric(cdn_data$Year)
cdn_data$projection[cdn_data$Year==2017]<-715.759256
cdn_data$bau[cdn_data$Year==2017]<-715.759256
cdn_data$target[cdn_data$Year==2017]<-715.759256
cdn_data$adds[cdn_data$Year==2017]<-715.759256
palette<-rev(viridis(7,option = "C"))[-1]#[-c(1,2,3)]
cdn_data$kyoto[(cdn_data$Year>=2008 &cdn_data$Year<=2012)]<-565
cdn_data<-left_join(cdn_data,progress_annex_table,by=c("Year","sector"))



#ECCC Targets data
targets_csv<-"https://www.canada.ca/content/dam/eccc/documents/csv/cesindicators/progress-canada-ghg-emissions-reduction-target/2019/progress-towards-canada-ghg-emissions-target-en.csv"
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





cdn_data<-proj_data %>%filter(prov=="Canada",emissions>0)%>%select(year,scenario,sector,emissions)%>% group_by(year,scenario)%>%
  summarise(emissions=sum(emissions)) %>% bind_rows(target_data) %>%
  bind_rows(NIR_data %>% filter(sector %in% main_sectors)%>%
              group_by(Year)%>%
              summarize(emissions=sum(GHGs,na.rm=T),scenario="NIR 2020")%>%
              select(year=Year,emissions,scenario))%>%
  bind_rows(NIR_data %>% filter(sector %in% main_sectors)%>%
              group_by(Year)%>%
              summarize(emissions=sum(GHGs,na.rm=T),scenario="2020 Reference Case")%>%
              select(year=Year,emissions,scenario))

cdn_data$kyoto[(cdn_data$year>=2008 &cdn_data$year<=2012)]<-565


cdn_data <- cdn_data %>% mutate(scenario=as_factor(scenario),
                                scenario=fct_relevel(scenario,"2020 Reference Case",after = 0),
                                scenario=fct_relevel(scenario,"2019 Reference Case",after = 0),
                                scenario=fct_relevel(scenario,"2018 Reference Case",after = 0),
                                scenario=fct_relevel(scenario,"2017 Reference Case",after = 0),
                                scenario=fct_relevel(scenario,"2016 Reference Case",after = 0),
                                scenario=fct_relevel(scenario,"NIR 2020",after = 0),
                                scenario=fct_recode(scenario,"National Inventory Emissions (1990-2020)"="NIR 2022"))

#ggplot(canadian_data)+geom_line(aes(year,emissions,group=scenario,color=scenario))


palette<-c(colors_tableau10()[1:4],"dodgerblue",colors_tableau10()[5:10])

targets_graph<-ggplot(filter(cdn_data,!grepl('Additional', scenario))%>%filter(scenario!="2020 Reference Case",
                                                                               scenario!="2019 Reference Case",
                                                                scenario!="2018 Reference Case",
                                                                scenario!="2017 Reference Case",
                                                                scenario!="2016 Reference Case",
),aes(x=year))+
  geom_line(aes(year,emissions,lty=scenario),color="black",size=1.45)+
  geom_line(data=filter(cdn_data,grepl('National Inventory', scenario)),aes(year,emissions),color="black",size=1.45)+
  scale_linetype_manual("",values=c("solid","31"))+
  geom_point(data=targets,aes(Year,target),size=5,colour=palette[9])+
  geom_point(aes(2000,603.22),size=5,colour=palette[9])+ #rio target
  scale_color_manual("",values=palette[-1])+
  annotate("text",x=1990+(2019-1990)/2,y=790,label="National Inventory Emissions (1990-2020)",
           color="black",fontface="bold",hjust=0.5)+
  #Rio
  annotate("text",x=2001,y=603.22,label="Rio Target (return to 1990 levels by 2000)",
           colour=palette[9],fontface="bold",hjust=0)+
  #kyoto
  #geom_point(aes(2010,565),size=5,colour=palette[9])+ #kyoto target
  geom_errorbarh(aes(xmin=2008,y=565,xmax=2012),height=40,size=2,colour=palette[9])+
  annotate("text",x=2007,y=565,label="Kyoto Target (6% below 1990 levels, 2008-12)",
           colour=palette[9],fontface="bold",hjust=1)+
  #copenhagen
  annotate("text",x=2021,y=607.32,label="Copenhagen Target (17% below 2005 levels by 2020)",
           colour=palette[9],fontface="bold",hjust=0)+
  #paris
  annotate("text",x=2029,y=512.9,label="Paris Target (30% below 2005 levels by 2030)",
           colour=palette[9],fontface="bold",hjust=1)+
  #glasgow
  #geom_point(aes(2030,423),size=5,colour=palette[9])+ #glasgow target
  geom_errorbar(aes(x=2030,ymax=443,ymin=406.3),width=1.4,size=2,colour=palette[9])+
  annotate("text",x=2029,y=423,label="Glasgow Target (40-45% below 2005 levels by 2030)",
           colour=palette[9],fontface="bold",hjust=1)+
  #2050
  annotate("text",x=2049,y=30,label="2050 Net Zero Goal",
           colour=palette[9],fontface="bold",hjust=1)+
  
  geom_hline(yintercept=0,size=1)+
  scale_y_continuous(limit=c(0,825))+
  scale_x_continuous(limit=c(1990,2052),breaks=seq(1990,2050,10),expand = c(0,0))+
  tombe_theme()+theme(legend.position = "none")+
  labs(x="",y=expression('Annual Emissions '*'(MtCO'[2]*'e)'),
       )

targets_graph+ 
  labs(title="Canada's GHG Emissions, Projections and Future Targets",
       subtitle="Source: Environment and Climate Change Canada Emissions Inventory and Projections (2022).")+
annotate("text",x=2031,y=660,label="2021 ECCC Reference Case Projection (2021-2030)",color="black",fontface="bold",hjust=0)
ggsave("images/emissions_and_targets_simple.png",bg="white",dpi=300,width=13,height=6)


targets_graph+ 
  labs(subtitle="Source: Environment and Climate Change Canada Emissions Inventory and Projections (2022).")+
  annotate("text",x=2031,y=660,label="2021 ECCC Reference Case Projection (2021-2030)",color="black",fontface="bold",hjust=0)
ggsave("images/emissions_and_targets_ppt.png",bg="white",dpi=300,width=14,height=7)


targets_graph+annotate("text",x=2035.7,y=675,label="2021 Reference Cases",color="black",fontface="bold",hjust=0)+
  labs(title="Canada's GHG Emissions, Projections and Future Targets",
       subtitle="Source: Environment and Climate Change Canada Emissions Inventory and Projections (2022).")+
      annotate("text",x=2032.18,y=675,label="2019 and ",color=palette[1],fontface="bold",hjust=0)+
  geom_line(data=filter(cdn_data,!grepl('Additional', scenario),scenario!="2020 Reference Case",scenario!="National Inventory Emissions (1990-2018)"),
                  aes(year,emissions,colour=scenario),lty="21",size=2)+
  geom_text(data=cdn_data%>%filter(year==2030,!grepl('Additional', scenario),scenario!="2020 Reference Case",scenario!="2019 Reference Case"),
            aes(x=2036,y=emissions+(scenario=="2019 Reference Case")*0+(scenario=="2017 Reference Case")*10
                ,label=scenario,color=scenario))
ggsave("images/emissions_and_targets_proj.png",bg="white",dpi=300,width=13,height=6)

os_proj<-proj_data_2022%>%filter(subsector_level_2=="Oil Sands",scenario=="2021 Reference Case",region=="Canada",year>=2020)%>%
  group_by(year,subsector_level_2)%>%summarize(emissions=sum(emissions))

oil_proj<-proj_data_2022%>%filter(sector=="Oil and Gas",scenario=="2021 Reference Case",region=="Canada",year>=2020)%>%
  group_by(year,sector)%>%summarize(emissions=sum(emissions,na.rm = T))



targets_graph+ annotate("text",x=2031,y=675,label="2021 ECCC Reference Case Projection (2021-2030)",color="black",fontface="bold",hjust=0)+
  geom_line(data=NIR_natl%>%filter(sector=="Oil and Gas",Prov=="Canada"),aes(Year,GHGs),color=palette[7],size=1.5)+
  geom_line(data=oil_proj,aes(year,emissions),color=palette[7],size=1.5,linetype="31")+
  annotate("text",x=2010,y=230,label="Inventory (1990-2020) and projected (2021-2030) emissions from oil and gas",  color=palette[7],fontface="bold",hjust=0)+

  geom_line(data=new_nir%>%filter(sector=="Oil Sands",Prov=="Canada"),aes(Year,GHGs),color=palette[4],size=1.5)+
  geom_line(data=os_proj,aes(year,emissions),color=palette[4],size=1.5,linetype="31")+
  annotate("text",x=2010,y=130,label="Inventory (1990-2020) and projected (2021-2030) emissions from oil sands",  color=palette[4],fontface="bold",hjust=0)  
  
ggsave("images/emissions_and_targets_oil.png",bg="white",dpi=600,width=13,height=6)

targets_graph+ annotate("text",x=2031,y=675,label="2020 ECCC Reference Case Projection (2021-2030)",color="black",fontface="bold",hjust=0)+
  geom_line(data=new_nir%>%filter(sector=="Oil Sands",Prov=="Canada"),aes(Year,GHGs),color=palette[4],size=2)+
  #geom_line(data=new_nir%>%filter(sector=="Oil and Gas",Prov=="Canada"),aes(Year,GHGs),color=palette[4],size=2)+
  #geom_line(data=filter(proj_data,sector=="Oil and Gas",scenario=="NIR 2022",Prov=="Canada"),aes(year,emissions),color=palette[4],size=2,linetype="solid")+
  #geom_line(data=proj_data%>%filter(sector=="Oil and Gas",scenario=="2020 Reference Case",prov=="Canada"),aes(year,emissions),color=palette[4],size=2,linetype="11")+
  #geom_point(aes(2030,100),size=3,colour=palette[4])+ #rio target
  annotate("text",x=2029,y=120,label="ECCC (2022) oil sands emissions (1990-2020) and 2030 projection",
           colour=palette[4],fontface="bold",hjust=1)
  ggsave("images/emissions_and_targets_oil_sands.png",bg="white",dpi=300,width=13,height=6)




plot_2016<-ggplot(filter(cdn_data,!grepl('Additional', scenario))%>%filter(scenario!="2019 Reference Case",
                                                                 scenario!="2018 Reference Case",
                                                                 scenario!="2017 Reference Case",
                                                                 scenario!="2020 Reference Case"),aes(x=year))+
  geom_line(data=filter(cdn_data,grepl('2016', scenario)),aes(year,emissions,lty=scenario),color="orange",size=2)+
  geom_line(data=filter(cdn_data,grepl('National Inventory', scenario),year<2016),aes(year,emissions,lty=scenario),color="black",size=2)+
  scale_linetype_manual("",values=c("11","solid","11"))+
  geom_point(data=targets%>%filter(Year<2050),aes(Year,target),size=5,colour=palette[9])+
  geom_point(aes(2000,603.22),size=5,colour=palette[9])+ #rio target
  scale_color_manual("",values=palette[-1])+
  annotate("text",x=1990+(2019-1990)/2,y=790,label="National Inventory Emissions (1990-2019)",
           color="black",fontface="bold",hjust=0.5)+
  #Rio
  annotate("text",x=2001,y=603.22,label="Rio Target (return to 1990 levels by 2000)",
           colour=palette[9],fontface="bold",hjust=0)+
  #kyoto
  #geom_point(aes(2010,565),size=5,colour=palette[9])+ #kyoto target
  geom_errorbarh(aes(xmin=2008,y=565,xmax=2012),height=40,size=2,colour=palette[9])+
  annotate("text",x=2007,y=565,label="Kyoto Target (6% below 1990 levels, 2008-12)",
           colour=palette[9],fontface="bold",hjust=1)+
  #copenhagen
  annotate("text",x=2021,y=607.32,label="Copenhagen Target (17% below 2005 levels by 2020)",
           colour=palette[9],fontface="bold",hjust=0)+
  #paris
  annotate("text",x=2029,y=512.9,label="Paris Target (30% below 2005 levels by 2030)",
           colour=palette[9],fontface="bold",hjust=1)+
  #harper 2050
  #geom_point(aes(2030,423),size=5,colour=palette[9])+ #glasgow target
  geom_errorbar(aes(x=2050,ymax=730*(1-.6),ymin=730*(1-.7)),width=1.4,size=2,colour=palette[9])+
  annotate("text",x=2049,y=730*(1-.65),label="Harper era 2050 Target (60-70% below 2006 levels by 2050)",
           colour=palette[9],fontface="bold",hjust=1)+
  geom_hline(yintercept=0,size=1)+

  scale_y_continuous(limit=c(0,825))+
  scale_x_continuous(limit=c(1990,2052),breaks=seq(1990,2050,10),expand = c(0,0))+
  tombe_theme()+theme(legend.position = "none")+
  labs(x="",y=expression('Annual Emissions '*'(MtCO'[2]*'e)'),
       title="Canada's GHG Emissions, Projections and Future Targets",
       subtitle="Source: Environment and Climate Change Canada Emissions Inventory (2021) and Projections (2016).")+
   annotate("text",x=2031,y=815,label="2016 ECCC Reference Case Projection (2016-2030)",color="orange",fontface="bold",hjust=0)
plot_2016
ggsave("images/emissions_and_targets_2016.png",bg="white",dpi=300,width=13,height=6)


targets_graph

targets_graph+
  annotate("text",x=2031,y=815,label="2016 ECCC Reference Case Projection (2016-2030)",color="orange",fontface="bold",hjust=0)+
  geom_line(data=filter(cdn_data,grepl('2016 Reference', scenario)),aes(year,emissions),lty="11",color="orange",size=2)+
  #geom_line(data=filter(cdn_data,grepl('2020 Reference', scenario)),aes(year,emissions),lty="11",color="black",size=2)+
  annotate("text",x=2031,y=675,label="2020 ECCC Reference Case Projection (2020-2030)",color="black",fontface="bold",hjust=0)
ggsave("images/emissions_and_targets_2016-2020.png",bg="white",dpi=300,width=13,height=6)


oil_bbl<-read_excel("Crude_Oil_Production.xlsx",range = "A8:AU16",col_names = TRUE) %>%rename("product"=1)%>%
  pivot_longer(-product,names_to = "year",values_to="production")%>%filter(product=="Total")%>%
  mutate(year=as.numeric(year),production=production*365)%>%select(-product)
oil_prod<-proj_data_2020 %>% filter(subsector_level_2 %in% c("Conventional Oil Production" ,"Oil Sands"),region=="Canada",scenario=="2020 Reference Case") %>%
  group_by(year,region,scenario) %>% summarise(emissions=sum(emissions)*10^6) %>% na.omit()%>%
  left_join(oil_bbl)
    

ggplot(oil_prod)+
  geom_line(aes(color="ERP Low Tech Case",year,emissions/production*(year<2022)+sum(emissions/production*(year==2022))*(year>=2022)*0.95^(year-2022)),size=1.5)+
  geom_line(aes(color="ERP High Tech Case",year,emissions/production*(year<2022)+sum(emissions/production*(year==2022))*(year>=2022)*0.85^(year-2022)),size=1.5,lty="11")+
  geom_line(aes(year,emissions/production,color="2020 Reference Case"),size=1.5)+
  geom_line(aes(year,10.3*1800/1000*.99^(year-2018),color="Global Average Barrel in 2015 per Masnadi et al. net 1% per year"),lty="11",size=1.5)+
  expand_limits(y=0)+
  scale_color_manual("",values = c("black","red","red","green"),guide=guide_legend(nrow = 1, keywidth = 2, override.aes = list(lty=c("solid","11","solid","11"))))+
  tombe_theme()+theme(legend.position = "bottom")+
  scale_x_continuous(breaks = pretty_breaks(n=10))+
  labs(x="",y=expression('Emissions Intensity '*'(kgCO'[2]*'e/bbl)'),
       title="Canada's GHG Emissions, Projections and Future Targets",
       subtitle="Source: Made up data derived from ECCC Projections (2020) and CER Energy Futures (2021).")
ggsave("made_up.png",width=13,height = 7,bg="white",dpi=100)


#NIR but with IPCC Sectors


nir_ipcc<-function() {
  #read_csv("http://donnees.ec.gc.ca/data/substances/monitor/canada-s-official-greenhouse-gas-inventory/GHG_IPCC_Can_Prov_Terr.csv")
  #download.file("http://donnees.ec.gc.ca/data/substances/monitor/canada-s-official-greenhouse-gas-inventory/GHG_IPCC_Can_Prov_Terr.csv","data/canada_ghg_ipcc.csv",mode="wb")
  #temp_nir<-read.csv("canada_ghg_prelimi.csv",stringsAsFactors = F)
  nir_2021<-read.csv("data/canada_ghg_ipcc.csv",stringsAsFactors = F)%>% filter(Rollup==TRUE) %>% clean_names()%>%
    mutate(category=tolower(category))%>%select(year,region,sector=category,co2eq)
  
  ipcc_nir<-nir_2021 %>% 
    mutate(prov=as.factor(region),
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
           NULL)%>% select(-region)%>%
    #mutate(sector=factor(sector),
    #       sector=fct_collapse(sector,"Inventory Total"=
    #                            c("Territories Inventory Total","Territory Inventory Total","Provincial Inventory Total"))
    #)%>%
    group_by(year,prov,sector) %>% summarize(GHGs=sum(as.numeric(co2eq),na.rm = T)) %>% ungroup()%>%
    mutate(year=as.numeric(year),prov=factor(prov, levels=c("Canada" ,"BC","AB" ,"SK","MB", "ON",     "QC" ,  "ATL" ,   "TERR"  )))
  
  
  ipcc_nir
}
ipcc_nir<-nir_ipcc()

save(ipcc_nir,file = "ipcc_nir.Rdata")


ipcc_main_sectors<-c("Total","Stationary Combustion Sources","Transport","Fugitive Sources",
                     "industrial processes and product use","agriculture","waste")




#making air travel graph

air_graph<-ipcc_nir %>% pivot_wider(names_from = "sector",values_from = GHGs,values_fill=0)%>%
  mutate(other_transport=transport-aviation)%>% select(c(year,prov,tolower(ipcc_main_sectors),other_transport,aviation))%>%
  select(-transport)%>%pivot_longer(-c(year,prov),names_to = "sector",values_to = "GHGs")%>%
  mutate(sector=factor(str_to_title(gsub("_"," ",(sector)))),
         sector=fct_reorder(sector,-GHGs),
         sector=fct_relevel(sector,"Other Transport",after = 0),
         sector=fct_relevel(sector,"Aviation",after = 0),
         sector=fct_recode(sector,"Stationary Sources"="Stationary Combustion Sources",
                           "Industrial P&P"="Industrial Processes And Product Use"
         ),
         NULL
  )



ggplot(filter(air_graph,GHGs>0 & prov !="Canada",sector!="Total"))+
  geom_area(aes(year,GHGs/10^3,fill=sector),color="black",position = "stack",size=0.1,alpha=.6)+
  #geom_line(aes(year,net_30_2005,colour=str_wrap("30% below 2005 provincial GHGs",width = 20)),linetype=1,size=1.05)+
  facet_wrap( ~ prov,nrow = 1)+
  scale_x_continuous(breaks=pretty_breaks())+
  #scale_color_viridis("",discrete=TRUE,guide_legend(NULL),option="E")+
  #scale_fill_viridis("",discrete=TRUE,option="E")+
  scale_fill_viridis("",discrete=TRUE,option="B")+
  #scale_fill_manual("",values = my_palette,guide = "legend")+
  #scale_fill_grey("",guide = "legend",start = 0.9,end=0.05)+
  scale_colour_manual("",values="black",guide = "legend")+
  geom_hline(aes(yintercept=0))+
  #guides(fill=guide_legend(nrow =1,byrow=FALSE),color=guide_legend(nrow =1,byrow=FALSE))+
  theme_tufte()+theme(
    legend.position = "right",
    legend.margin=margin(c(.05,0,.05,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
    plot.caption = element_text(size = 10, face = "italic",hjust=0),
    plot.title = element_text(size=16,face = "bold"),
    plot.subtitle = element_text(size = 10),
    #panel.grid.minor = element_blank(),
    text = element_text(size = 20,face = "bold"),
    axis.text.y = element_text(size = 12,face = "bold", colour="black"),
    #axis.text.x = element_blank(),
    axis.text.x = element_text(size = 10, colour = "black", angle = 90,hjust=0.5,vjust=0.5),
    strip.text.x = element_text(size = 8, colour = "black", angle = 0),
    axis.title.y = element_text(size = 14,face = "bold", colour="black"),
  )+
  labs(x=NULL,y=expression('Annual Emissions  '*'(MtCO'[2]*'e)'),
       title="Canadian GHG Emissions by Province (IPCC Sectors)",
       #subtitle=paste("2020 National Inventory (1990-2018)",sep=""),
       caption=str_wrap("Source: Environment and Climate Change Canada 2022 National Inventory (1990-2020). Graph by @andrew_leach.",width = 180),
       NULL
  )
ggsave("images/inventory_ipcc_prov.png",dpi = 300,width=14, height=7)

ggplot(filter(air_graph,GHGs>0 & prov !="Canada",sector!="Total"))+
  geom_area(aes(year,GHGs/10^3,fill=prov),color="black",position = "stack",size=0.1,alpha=.6)+
  #geom_line(aes(year,net_30_2005,colour=str_wrap("30% below 2005 provincial GHGs",width = 20)),linetype=1,size=1.05)+
  facet_wrap( ~ sector,nrow = 1)+
  scale_x_continuous(breaks=pretty_breaks())+
  #scale_color_viridis("",discrete=TRUE,guide_legend(NULL),option="E")+
  #scale_fill_viridis("",discrete=TRUE,option="E")+
  scale_fill_viridis("",discrete=TRUE,option="B")+
  #scale_fill_manual("",values = my_palette,guide = "legend")+
  #scale_fill_grey("",guide = "legend",start = 0.9,end=0.05)+
  scale_colour_manual("",values="black",guide = "legend")+
  geom_hline(aes(yintercept=0))+
  #guides(fill=guide_legend(nrow =1,byrow=FALSE),color=guide_legend(nrow =1,byrow=FALSE))+
  theme_tufte()+theme(
    legend.position = "right",
    legend.margin=margin(c(.05,0,.05,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
    plot.caption = element_text(size = 10, face = "italic",hjust=0),
    plot.title = element_text(size=16,face = "bold"),
    plot.subtitle = element_text(size = 10),
    #panel.grid.minor = element_blank(),
    text = element_text(size = 20,face = "bold"),
    axis.text.y = element_text(size = 12,face = "bold", colour="black"),
    #axis.text.x = element_blank(),
    axis.text.x = element_text(size = 10, colour = "black", angle = 90,hjust=0.5,vjust=0.5),
    strip.text.x = element_text(size = 8, colour = "black", angle = 0),
    axis.title.y = element_text(size = 14,face = "bold", colour="black"),
  )+
  labs(x=NULL,y=expression('Annual Emissions  '*'(MtCO'[2]*'e)'),
       title="Canadian GHG Emissions by IPCC Sectors",
       #subtitle=paste("2020 National Inventory (1990-2018)",sep=""),
       caption=str_wrap("Source: Environment and Climate Change Canada 2022 National Inventory (1990-2020). Graph by @andrew_leach.",width = 180),
       NULL
  )
ggsave("images/inventory_ipcc_sector.png",dpi = 300,width=14, height=7)



ggplot(filter(air_graph,GHGs>0 & prov !="Canada",sector=="Aviation"))+
  geom_area(aes(year,GHGs/10^3,fill=prov),color="black",position = "stack",size=0.1,alpha=.6)+
  #geom_line(aes(year,net_30_2005,colour=str_wrap("30% below 2005 provincial GHGs",width = 20)),linetype=1,size=1.05)+
  #facet_wrap( ~ sector,nrow = 1)+
  scale_x_continuous(breaks=pretty_breaks())+
  #scale_color_viridis("",discrete=TRUE,guide_legend(NULL),option="E")+
  #scale_fill_viridis("",discrete=TRUE,option="E")+
  scale_fill_viridis("",discrete=TRUE,option="B")+
  #scale_fill_manual("",values = my_palette,guide = "legend")+
  #scale_fill_grey("",guide = "legend",start = 0.9,end=0.05)+
  scale_colour_manual("",values="black",guide = "legend")+
  geom_hline(aes(yintercept=0))+
  #guides(fill=guide_legend(nrow =1,byrow=FALSE),color=guide_legend(nrow =1,byrow=FALSE))+
  theme_tufte()+theme(
    legend.position = "right",
    legend.margin=margin(c(.05,0,.05,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
    plot.caption = element_text(size = 10, face = "italic",hjust=0),
    plot.title = element_text(size=16,face = "bold"),
    plot.subtitle = element_text(size = 10),
    #panel.grid.minor = element_blank(),
    text = element_text(size = 20,face = "bold"),
    axis.text.y = element_text(size = 12,face = "bold", colour="black"),
    #axis.text.x = element_blank(),
    axis.text.x = element_text(size = 10, colour = "black", angle = 90,hjust=0.5,vjust=0.5),
    strip.text.x = element_text(size = 8, colour = "black", angle = 0),
    axis.title.y = element_text(size = 14,face = "bold", colour="black"),
  )+
  labs(x=NULL,y=expression('Annual Emissions  '*'(MtCO'[2]*'e)'),
       title="Canadian Aviation Emissions (IPCC Definition)",
       #subtitle=paste("2020 National Inventory (1990-2018)",sep=""),
       caption=str_wrap("Source: Environment and Climate Change Canada 2022 National Inventory (1990-2020). Graph by @andrew_leach.",width = 180),
       NULL
  )
ggsave("images/inventory_aviation_sector.png",dpi = 300,width=14, height=7)