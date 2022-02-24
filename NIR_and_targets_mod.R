
if(R.version$platform ==  "x86_64-w64-mingw32")
  setwd("C:/Users/aleach/Documents/data_projects/NIR")
print(getwd())

source("andrew_base.R")




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
                c("Territories Inventory Total","Territory Inventory Total","Provincial Inventory Total"))
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
new_nir<-get_new_nir()
save(new_nir,file = "nir_data.Rdata")

#load(file = "nir_data.Rdata")




NIR_data<-filter(new_nir,Prov!="Canada")
NIR_data$Year<-as.numeric(NIR_data$Year)
prov_samp<-filter(NIR_data,grepl("otal",sector))
prov_samp<-prov_samp[,-1] #take out un-needed sector column
colnames(prov_samp)[3] <- "Prov_GHGs" # the first row will be the header


NIR_data<-NIR_data %>% group_by(Prov) %>% mutate(GHGs_2005_net_30=.7*sum(((Year==2005)&(sector=="National Inventory Total"))*GHGs))





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

save(file="pop_data.Rdata",pop_data)
}


load(file = "pop_data.Rdata")

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
  
  save(file="gdp_data.Rdata",gdp_data)
}


get_gdp_data()
load(file = "gdp_data.Rdata")


totals<-new_nir %>% filter(sector%in% c("Inventory Total","Total, Canada"))%>% select(-sector)%>% group_by(Prov) %>% mutate(
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
ggsave("index_ghgs.png",width=16,height=16,dpi=300)

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
ggsave("index_ghgs_AB.png",width=16,height=16,dpi=300)



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
ggsave("GHG_per_GDP.png",width=16,height=9,dpi=400)



#top level sectors - other sector levels of subsectors  
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

my_palette<-c("#313695",brewer.pal(9, "Set1"),"Black")




prov_ghgs <- NIR_data%>% filter(sector %in% main_sectors)%>%group_by(Prov,Year)%>% summarize(GHGs=sum(GHGs,na.rm = T))
save(prov_ghgs,file = "prov_ghgs.Rdata")


NIR_natl<-filter(new_nir,Prov=="Canada")


NIR_natl$Year<-as.numeric(NIR_natl$Year)
NIR_natl<-merge(filter(NIR_natl,sector %in% main_sectors),pop_data,by.x=c("Prov","Year"),by.y=c("Code","Year"))
NIR_natl$per_cap<-NIR_natl$GHGs/NIR_natl$Prov_pop

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("nir_natl.png")
ggplot(filter(NIR_natl,sector!="National Inventory Total"))+
  geom_area(aes(Year,GHGs,group=sector,fill=sector))+
  scale_fill_manual("",values = my_palette,guide = "legend")+
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
       title="Canadian GHG Emissions, 1990-2018",
       #subtitle="According to Ian Brodie, they first declined after March, 2008.",
       caption="Source: Environment Canada National Inventory Data, graph by @andrew_leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


ggplot(filter(NIR_natl,sector!="National Inventory Total"))+
  geom_line(aes(Year,GHGs,group=sector,color=sector))+
  scale_fill_manual("",values = my_palette,guide = "legend")+
  scale_colour_manual("",values=c(colors_tableau10(),colors_tableau10_light()),guide = "legend")+
  scale_x_continuous(breaks=pretty_breaks(n=16), expand = c(0,0))+
  guides(fill=guide_legend(nrow =2,byrow=FALSE),color=guide_legend(nrow =3,byrow=FALSE))+
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
       title="Canadian GHG Emissions, 1990-2018",
       #subtitle="According to Ian Brodie, they first declined after March, 2008.",
       caption="Source: Environment Canada National Inventory Data, graph by @andrew_leach")


oil_sectors<-c("Conventional Heavy Oil Production","Conventional Light Oil Production","Frontier Oil Production",
                   "Oil Sands")


png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("nir_natl_oil_gas.png")
ggplot(filter(new_nir,Prov=="Canada",sector%in%oil_sectors))+
  geom_area(aes(Year,GHGs,group=sector,fill=sector),position="stack")+
  scale_fill_manual("",values = colors_tableau10(),guide = "legend")+
  scale_colour_manual("",values="black",guide = "legend")+
  #scale_color_viridis("",discrete=TRUE,guide_legend(NULL),option="D")+
  #scale_fill_viridis("",discrete=TRUE,option="D")+
  scale_x_continuous(breaks=pretty_breaks(n=15), expand = c(0,0))+
  scale_y_continuous(breaks=pretty_breaks(n=5), expand = c(0,0))+
    guides(fill=guide_legend(nrow =2,byrow=FALSE),color=guide_legend(nrow =2,byrow=FALSE))+
  theme_minimal()+slide_theme()+
  labs(x=NULL,y=expression('Annual GHG Emissions  '*'(Mt CO'[2]*'e)'),
       title="Canadian GHG Emissions from Crude Oil and Oil Sands Production, 1990-2017",
       #subtitle="According to Ian Brodie, they first declined after March, 2008.",
       caption="Source: Environment Canada National Inventory Data, graph by @andrew_leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()






png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("nir_natl_oil_gas.png")
ggplot(filter(new_nir,Prov=="Canada",sector%in%oil_sectors))+
  geom_area(aes(Year,GHGs,group=sector,fill=sector),position="stack")+
  scale_fill_manual("",values = colors_tableau10(),guide = "legend")+
  scale_colour_manual("",values="black",guide = "legend")+
  #scale_color_viridis("",discrete=TRUE,guide_legend(NULL),option="D")+
  #scale_fill_viridis("",discrete=TRUE,option="D")+
  scale_x_continuous(breaks=pretty_breaks(n=15), expand = c(0,0))+
  scale_y_continuous(breaks=pretty_breaks(n=5), expand = c(0,0))+
  guides(fill=guide_legend(nrow =2,byrow=FALSE),color=guide_legend(nrow =2,byrow=FALSE))+
  theme_minimal()+slide_theme()+
  labs(x=NULL,y=expression('Annual GHG Emissions  '*'(Mt CO'[2]*'e)'),
       title="Canadian GHG Emissions from Crude Oil and Oil Sands Production, 1990-2017",
       #subtitle="According to Ian Brodie, they first declined after March, 2008.",
       caption="Source: Environment Canada National Inventory Data, graph by @andrew_leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()







ggplot(filter(new_nir,Prov=="AB",sector%in%oil_sectors))+
  geom_area(aes(Year,GHGs,group=sector,fill=sector),position="stack")+
  scale_fill_manual("",values = colors_tableau10(),guide = "legend")+
  scale_colour_manual("",values="black",guide = "legend")+
  #scale_color_viridis("",discrete=TRUE,guide_legend(NULL),option="D")+
  #scale_fill_viridis("",discrete=TRUE,option="D")+
  scale_x_continuous(breaks=pretty_breaks(n=15), expand = c(0,0))+
  scale_y_continuous(breaks=pretty_breaks(n=5), expand = c(0,0))+
  guides(fill=guide_legend(nrow =1,byrow=FALSE),color=guide_legend(nrow =2,byrow=FALSE))+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(.05,0,.05,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
    plot.caption = element_text(size = 10, face = "italic",hjust=0.5,vjust=0.5),
    plot.title = element_text(size=16,face = "bold"),
    plot.subtitle = element_text(size = 10),
    panel.grid.minor = element_blank(),
    text = element_text(size = 20,face = "bold"),
    axis.text.y = element_text(size = 12,face = "bold", colour="black"),
    #axis.text.x = element_blank(),
    axis.text.x = element_text(size = 10, colour = "black", angle = 90,hjust=0.5,vjust=0.5),
    strip.text.x = element_text(size = 12, colour = "black", angle = 0),
    axis.title.y = element_text(size = 14,face = "bold", colour="black"),
  )+
  labs(x=NULL,y=expression('Annual GHG Emissions  '*'(Mt CO'[2]*'e)'),
       #title="Canadian GHG Emissions from Crude Oil and Oil Sands Production, 1990-2017",
       #subtitle="According to Ian Brodie, they first declined after March, 2008.",
       #caption="Source: Environment aCanada National Inventory Data, graph by @andrew_leach"
       NULL
       )
ggsave("nir_AB_oil_gas.png",width=12,height=5,dpi=300)



#Conventional Oil Production
#Conventional Light Oil Production
#Conventional Heavy Oil Production
#Frontier Oil Production
#Oil Sands (Mining, In-situ, Upgrading)
#Oil Sands Mining and Extraction
#In-situ Bitumen
#Upgrading


#get some EIA data on oil and natural gas production

series<-c("INTL.57-1-CAN-TBPD.A")
names<-c("oil_lease_condensate")
canada_data<-pdfetch_EIA(series,KEY) 
canada_data <- setNames(canada_data, names)
canada_data<-data.frame(date=index(canada_data), coredata(canada_data))
canada_data$date<-ymd(canada_data$date)
canada_melt <- melt(canada_data,id="date") 

ggplot(filter(canada_melt))+ geom_line(aes(date,value,group=variable))
canada_melt$Year<-year(canada_melt$date)

ei_data<- new_nir %>% filter(Prov=="Canada",sector %in% oil_sectors) %>% group_by(Year) %>% summarize(total_oil_ghgs=sum(GHGs))%>%
  left_join(filter(canada_melt))

ei_data$EI<-ei_data$total_oil_ghgs/ei_data$value

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("nir_natl_oil_gas_ei.png")
ggplot(filter(ei_data))+
  geom_line(aes(Year,EI*1000))+
  scale_fill_manual("",values = my_palette,guide = "legend")+
  scale_colour_manual("",values="black",guide = "legend")+
  scale_x_continuous(breaks=pretty_breaks(n=15), expand = c(0,0))+
  guides(fill=guide_legend(nrow =2,byrow=FALSE),color=guide_legend(nrow =1,byrow=FALSE))+
  theme_minimal()+slide_theme()+
  labs(x=NULL,y=expression('Emissions Intensity  '*'(g CO'[2]*'e/bbl)'),
       title="GHG Emissions Intensity, Canadian Upstream Oil sector, 1990-2017",
       #subtitle="According to Ian Brodie, they first declined after March, 2008.",
       caption="Source: Environment Canada National Emissions Inventory and EIA annual production of crude oil and lease condensate.\nGraph by @andrew_leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()

#Jens code
#Yes, that's what I do. Have two geom_line with
#geom_line(data=~filter(.,!highlight), color = 'grey') + 
#  geom_line(data=~filter(.,highlight),aes(color=c))



png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("natl_per_cap.png")
ggplot(filter(NIR_natl,sector!="National Inventory Total"))+
  geom_area(aes(Year,per_cap*10^6,group=sector,fill=sector))+
  annotate("rect", fill = "black", 
           xmin = 2008+3/12, xmax =2008+4/12,
           ymin = -Inf, ymax = 23) +
  annotate("text", x = 2008+3.5/12, y = 24.5, label = "Turning the Corner Plan\nIntroduced",size=4)+
  scale_fill_manual("",values = my_palette,guide = "legend")+
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
       title="Canadian Per Capita GHG Emissions, 1990-2018",
       subtitle="According to Ian Brodie, they first declined after March, 2008.",
       caption="Source: Environment Canada National Inventory Data, graph by @andrew_leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()

#find the largest sectors

major_sectors<-NIR_natl %>% group_by(sector) %>% summarize(max_ghg=max(GHGs)) %>% arrange(-max_ghg) %>% head(6) %>% select(sector)
#use forcats to pick 5 largest sectors

sub_samp<-NIR_data %>% filter(sector %in% main_sectors,sector!="National Inventory Total") %>% mutate(sector=fct_other(as.factor(sector),keep = major_sectors$sector)) %>%
  group_by(sector,Prov,Year) %>% summarize(GHGs=sum(GHGs),GHGs_2005_net_30=sum(GHGs_2005_net_30))


sub_samp<-merge(sub_samp,prov_samp,by=c("Prov","Year"))


sub_samp<-merge(sub_samp,pop_data,by.x=c("Prov","Year"),by.y=c("Code","Year"))

#sub_samp$Prov<-fct_rev(sub_samp$Prov)





#sub_samp<-merge(sub_samp,power_data_exp,by.x=c("Prov","Year"),by.y=c("Code","Year"))

ggplot(filter(sub_samp),group=as.numeric(as.character(Year)))+
  geom_area(aes(as.numeric(as.character(Year)),GHGs,fill=sector),color="black",position = "stack",size=0.1)+
  #geom_line(data=filter(sub_samp, sector=="Agriculture"),aes(as.numeric(as.character(Year)),GHGs_2005_net_30,colour="30% below\n2005"),linetype=2,size=1)+
  facet_grid( ~ Prov)+
  #scale_color_viridis("",discrete=TRUE,guide_legend(NULL),option="E")+
  #scale_fill_viridis("",discrete=TRUE,option="E")+
  #scale_fill_manual("",values = my_palette,guide = "legend")+
  scale_fill_grey("",guide = "legend",start = 0.9,end=0.05)+
  #scale_colour_manual("",values="black",guide = "legend")+
  #guides(fill=guide_legend(nrow =1,byrow=FALSE),color=guide_legend(nrow =1,byrow=FALSE))+
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
       title="1990-2018 Provincial GHG Emissions",
       #subtitle="Excluding Electricity",
       #caption="Source: Environment Canada National Inventory Data, graph by @andrew_leach",
       NULL
       )
ggsave("inventory_ghgs.png",width=16,height=9,dpi=600)



png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("inventory_ghgs_percap.png")
ggplot(filter(sub_samp,Prov!="TERR"),group=as.numeric(as.character(Year)))+
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
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("inventory_ghg_shares.png")
ggplot(filter(sub_samp,Prov!="TERR"),group=as.numeric(as.character(Year)))+
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
       title="1990-2017 Provincial GHG Emissions Shares",
       #subtitle="Excluding Electricity",
       caption="Source: Environment Canada National Inventory Data, graph by @andrew_leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()




png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("bc_inventory_ghgs.png")
ggplot(subset(sub_samp,Prov=="BC"),group=as.numeric(as.character(Year)))+
  geom_area(aes(as.numeric(as.character(Year)),GHGs,colour=sector,fill=sector),position = "stack")+
  #facet_grid( ~ Prov)+
  #scale_color_viridis("",discrete=TRUE,guide_legend(NULL),option="E")+
  #scale_fill_viridis("",discrete=TRUE,option="E")+
  scale_x_continuous(breaks=seq(1990,2016,2))+
  scale_fill_manual("",values = c(colors_tableau10(),colors_tableau10_light()),guide = "legend")+
  scale_colour_manual("",values = c(colors_tableau10(),colors_tableau10_light()),guide = "legend")+
  guides(fill=guide_legend(nrow =2,byrow=FALSE))+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(.10,0,.10,0),unit="cm"),
    legend.text = element_text(colour="black", size = 14),
    plot.caption = element_text(size = 18, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 20, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 20,face = "bold"),
    axis.text.y = element_text(size = 14,face = "bold", colour="black"),
    #axis.text.x = element_blank(),
    axis.text.x = element_text(size = 14, colour = "black", angle = 90,hjust = 0),
    strip.text.x = element_text(size = 14, colour = "black", angle = 0)
  )+
  labs(x=NULL,y=expression('Annual Emissions '*'(MtCO'[2]*'e)'),
       title="1990-2016 BC Provincial GHG Emissions",
       #subtitle="Excluding Electricity",
       caption="Source: Environment Canada National Inventory Data\nGraph by @andrew_leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


png<-1 
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("on_inventory_ghgs.png")
ggplot(subset(sub_samp,Prov=="ON"),group=as.numeric(as.character(Year)))+
  geom_area(aes(as.numeric(as.character(Year)),GHGs,colour=sector,fill=sector),position = "stack")+
  #facet_grid( ~ Prov)+
  #scale_color_viridis("",discrete=TRUE,guide_legend(NULL),option="E")+
  #scale_fill_viridis("",discrete=TRUE,option="E")+
  scale_x_continuous(breaks=seq(1990,2016,2))+
  scale_fill_manual("",values = c(colors_tableau10(),colors_tableau10_light()),guide = "legend")+
  scale_colour_manual("",values = c(colors_tableau10(),colors_tableau10_light()),guide = "legend")+
  guides(fill=guide_legend(nrow =2,byrow=FALSE))+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(.10,0,.10,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 20, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 20,face = "bold"),
    axis.text.y = element_text(size = 12,face = "bold", colour="black"),
    #axis.text.x = element_blank(),
    axis.text.x = element_text(size = 12, colour = "black", angle = 90,hjust = 0),
    strip.text.x = element_text(size = 12, colour = "black", angle = 0)
  )+
  labs(x=NULL,y=expression('Annual Emissions '*'(MtCO'[2]*'e)'),
       title="1990-2016 Ontario Provincial GHG Emissions",
       #subtitle="Excluding Electricity",
       caption="Source: Environment Canada National Inventory Data\nGraph by @andrew_leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


png<-1 
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("terr_inventory_ghgs.png")
ggplot(subset(sub_samp,Prov=="TERR"),group=as.numeric(as.character(Year)))+
  geom_area(aes(as.numeric(as.character(Year)),GHGs,colour=sector,fill=sector),position = "stack")+
  #facet_grid( ~ Prov)+
  #scale_color_viridis("",discrete=TRUE,guide_legend(NULL),option="E")+
  #scale_fill_viridis("",discrete=TRUE,option="E")+
  scale_x_continuous(breaks=seq(1990,2016,2))+
  scale_fill_manual("",values = c(colors_tableau10(),colors_tableau10_light()),guide = "legend")+
  scale_colour_manual("",values = c(colors_tableau10(),colors_tableau10_light()),guide = "legend")+
  guides(fill=guide_legend(nrow =2,byrow=FALSE))+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(.10,0,.10,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 20, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 20,face = "bold"),
    axis.text.y = element_text(size = 12,face = "bold", colour="black"),
    #axis.text.x = element_blank(),
    axis.text.x = element_text(size = 12, colour = "black", angle = 90,hjust = 0),
    strip.text.x = element_text(size = 12, colour = "black", angle = 0)
  )+
  labs(x=NULL,y=expression('Annual Emissions '*'(MtCO'[2]*'e)'),
       title="1990-2018 Territorial GHG Emissions",
       #subtitle="Excluding Electricity",
       caption="Source: Environment Canada National Inventory Data\nGraph by @andrew_leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()




df1<-sub_samp
#df1$Prov <- fct_collapse(df1$Prov,
#                         "TERR" = c("NT", "NU","YT","NT & NU"),
#                         "ATL" = c("NL", "NB","NS","PE"))
df1<-df1 %>% group_by(sector) %>% mutate(sector_GHGs=max(GHGs))
#sort levels by sector GHGs
df1$sector<-fct_reorder(df1$sector,-df1$sector_GHGs)
#Keep top 6
df1$sector<-fct_other(df1$sector,keep = levels(df1$sector)[1:5],  other_level = "Other")

df1<-df1 %>% group_by(sector,Prov,Year) %>% summarise(GHGs=sum(GHGs),Prov_GHGs=sum(Prov_GHGs),Prov_Pop=sum(Prov_pop))
df1$Prov<-fct_rev(df1$Prov)


png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("inventory_ghgs_mod.png")
ggplot(subset(df1,sector!="Placeholder"),group=as.numeric(as.character(Year)))+
  geom_area(aes(as.numeric(as.character(Year)),GHGs,colour=sector,fill=sector),position = "stack")+
  facet_grid( ~ Prov)+
  scale_color_viridis("",discrete=TRUE,guide_legend(NULL),option="E")+
  scale_fill_viridis("",discrete=TRUE,option="E")+
  scale_x_continuous(breaks=seq(1990,2016,8))+
  #scale_fill_manual("",values = my_palette,guide = "legend")+
  #scale_colour_manual("",values=my_palette,guide = "legend")+
  guides(fill=guide_legend(nrow =1,byrow=FALSE))+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(.10,0,.10,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
    plot.caption = element_text(size = 12, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 14, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(colour="black",size = 12,face = "bold"),
    axis.text.y = element_text(size = 12,face = "bold", colour="black"),
    #axis.text.x = element_blank(),
    axis.text.x = element_text(size = 12, face = "bold",colour = "black", angle = 90,hjust = 0.5,vjust = 0.5),
    strip.text.x = element_text(size = 14, colour = "black", angle = 0)
  )+
  labs(x=NULL,y=expression('Annual Emissions '*'(MtCO'[2]*'e)'),
       title="1990-2016 Provincial GHG Emissions",
       #subtitle="Excluding Electricity",
       caption="Source: Environment Canada National Inventory Data\nGraph by @andrew_leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()



df1<-sub_samp

df1<-df1 %>% group_by(sector,Prov_GHGs,Prov_pop) %>% mutate(sector_GHGs=sum(GHGs))
#sort levels by sector GHGs
df1$sector<-fct_reorder(df1$sector,-df1$sector_GHGs)
#Keep top 6
df1$sector<-fct_other(df1$sector,keep = levels(df1$sector)[1:7],  other_level = "Other")

df1<-df1 %>% group_by(sector,Prov,Year) %>% summarise(GHGs=sum(GHGs),Prov_GHGs=sum(Prov_GHGs),Prov_Pop=sum(Prov_pop))
df1$Prov<-fct_rev(df1$Prov)
df1$sector<-fct_relevel(df1$sector,"Oil and Gas")
df1$sector<-fct_relevel(df1$sector,"Agriculture",after = 5)

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("sector_ghgs.png",width = 1200)
ggplot(filter(df1,Prov!="TERR"),group=as.numeric(as.character(Year)))+
  geom_area(aes(as.numeric(as.character(Year)),GHGs,colour=Prov,fill=Prov),position = "stack")+
  facet_grid( ~ sector)+
  #geom_col(aes(Prov,prov_ghgs/10^6,colour=sector,fill=sector,group=Ref_Year),size=1.5,position = "dodge")+
  #scale_color_viridis("",discrete=TRUE,guide_legend(NULL),option="E")+
  #scale_fill_viridis("",discrete=TRUE,option="E")+
  #scale_x_discrete(breaks=c(seq(1990,2016,5)))+
  scale_fill_manual("",values = my_palette[-7],guide = "legend")+
  scale_colour_manual("",values=my_palette[-7],guide = "legend")+
  #scale_fill_manual("",values =colors_tableau10() ,guide = "legend")+
  #scale_colour_manual("",values=colors_tableau10() ,guide = "legend")+
  guides(fill=guide_legend())+
  theme_minimal()+theme(
    legend.position = "right",
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
    strip.text.x = element_text(size = 9, colour = "black", angle = 0),
    axis.title.y = element_text(size = 16,face = "bold", colour="black"),
  )+
  labs(x=NULL,y=expression('Annual Emissions '*'(MtCO'[2]*'e)'),
       title="1990-2018 sectoral GHG Emissions by Province",
       #subtitle="Excluding Electricity",
       caption="Source: Environment Canada National Inventory Data\nGraph by @andrew_leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()

df1 %>% ungroup()%>% filter(sector=="Electricity",Year==2016) %>% mutate(sum=sum(GHGs))


sub_samp$Prov<-fct_rev(sub_samp$Prov)
png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("inventory_ghgs_2004.png")
ggplot(subset(sub_samp,as.numeric(as.character(Year))>=2004),group=Year)+
  #geom_col(aes(Year,GHGs,colour=sector,fill=sector),size=.35,position = "stack")+
  geom_area(aes(as.numeric(as.character(Year)),GHGs,colour=sector,fill=sector),position = "stack")+
  facet_grid( ~ Prov)+
  #geom_col(aes(Prov,prov_ghgs/10^6,colour=sector,fill=sector,group=Ref_Year),size=1.5,position = "dodge")+
  #scale_color_viridis("",discrete=TRUE,guide_legend(NULL))+
  #scale_fill_viridis("",discrete=TRUE)+
  scale_x_continuous(breaks=c(seq(1990,2016,5)))+
  scale_fill_manual("",values = my_palette,guide = "legend")+
  scale_colour_manual("",values=my_palette,guide = "legend")+
  guides(fill=guide_legend(nrow =2,byrow=FALSE))+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(.10,0,.10,0),unit="cm"),
    legend.text = element_text(colour="black", size = 14),
    plot.caption = element_text(size = 18, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 20, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 20,face = "bold"),
    axis.text.y = element_text(size = 12,face = "bold", colour="black"),
    #axis.text.x = element_blank(),
    axis.text.x = element_text(size = 8, colour = "black", angle = 90,hjust = 0),
    strip.text.x = element_text(size = 14, colour = "black", angle = 0)
  )+
  labs(x=NULL,y=expression('Annual Emissions '*'(MtCO'[2]*'e)'),
       title="2004-2016 Provincial GHG Emissions",
       #subtitle="Excluding Electricity",
       caption="Source: Environment Canada Preliminary Data\nGraph by @andrew_leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()



#Upstream oil and gas
upstream<-c("Natural Gas Production and Processing",
            "Conventional Light Oil Production",                   
            "Conventional Heavy Oil Production",                   
            "Frontier Oil Production",                             
            "Oil Sands")

#Upstream oil and gas
upstream_oil<-c(
  "Natural Gas Production and Processing",
  "Conventional Oil Production",
            "Oil Sands Mining",
            "Oil Sands In Situ",
            "Oil Sands Upgrading",
  "Oil, Natural Gas and CO2  Transmission")

            


sub_samp<-subset(NIR_data,sector %in% upstream_oil)%>%
  mutate(sector=fct_recode(sector,"Oil and Gas Transmission"="Oil, Natural Gas and CO2  Transmission"),
         sector=fct_relevel(sector,"Oil and Gas Transmission",after = Inf),
         sector=fct_relevel(sector,"Natural Gas Production and Processing")
         )
df1<- sub_samp %>% group_by(Year,sector) %>% summarise(GHGs=sum(GHGs,na.rm = TRUE))

ggplot(df1,aes(Year,GHGs,group = sector,fill=sector)) +
  geom_area(position = "stack",size=.5,colour="black") +
  scale_fill_viridis("",discrete=TRUE,option="B",direction = -1)+
  #scale_colour_manual("",values=my_palette,guide = "legend")+
  scale_x_continuous(breaks=pretty_breaks(),expand = c(0,0))+
  scale_y_continuous(limits=c(0,200),breaks=seq(0,200,25),expand = c(0,0))+
  guides(fill=guide_legend(nrow =2,byrow=FALSE))+
  theme_minimal()+theme(
    legend.position = "bottom",
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
  labs(y=expression('Annual Emissions '*'(MtCO'[2]*'e)'),x="",
       title=paste("Canadian upstream oil and gas emissions by subsector",sep=""),
       caption="Source: Environment Canada Inventory Data, graph by Andrew Leach.")
ggsave("images/upstream_ghgs.png",width=14,height=7,dpi=300)

df1<- subset(NIR_data,sector %in% upstream_oil) %>% group_by(Year,sector) %>% summarise(GHGs=sum(GHGs,na.rm = TRUE))

df_cement<- filter(NIR_data,sector %in% c(upstream_oil,"Cement")) %>% group_by(Year,sector) %>% summarise(GHGs=sum(GHGs,na.rm = TRUE))

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("upstream_oil_ghgs_cement.png")
ggplot(df_cement,aes(Year,GHGs,group = sector,colour=sector,fill=sector)) +
  geom_area(data=df_cement%>%filter(sector!="Cement"),position = "stack") +
  geom_line(data=df_cement%>%filter(sector=="Cement"),size=2) +
  scale_fill_manual("",values = my_palette,guide = "legend")+
  scale_colour_manual("",values=my_palette,guide = "legend")+
  scale_x_continuous(breaks=pretty_breaks(),expand = c(0,0))+
  scale_y_continuous(breaks = pretty_breaks(),expand = c(0,0))+
  guides(fill=guide_legend(nrow =3,byrow=FALSE))+
  slide_theme()+
  labs(y=expression('Annual Emissions '*'(MtCO'[2]*'e)'),x="Date",
       title=paste("Upstream oil emissions by sector compared to cement",sep=""),
       caption="Source: Environment Canada Inventory Data, graph by Andrew Leach.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()




png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("upstream_oil_ghgs.png")
ggplot(df1,aes(Year,GHGs,group = sector,colour=sector,fill=sector)) +
  geom_area(position = "stack") +
  scale_fill_manual("",values = my_palette,guide = "legend")+
  scale_colour_manual("",values=my_palette,guide = "legend")+
  scale_x_discrete(breaks=seq(1990,2016,2),expand = c(0,0))+
  scale_y_continuous(limits=c(0,200),breaks=seq(0,200,25),expand = c(0,0))+
  guides(fill=guide_legend(nrow =3,byrow=FALSE))+
  slide_theme()+
  labs(y=expression('Annual Emissions '*'(MtCO'[2]*'e)'),x="Date",
       title=paste("Upstream oil emissions by sector",sep=""),
       caption="Source: Environment Canada Inventory Data, graph by Andrew Leach.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


df1<- filter(NIR_data,sector %in% upstream_oil,Prov=="AB") %>% group_by(Year,sector) %>% summarise(GHGs=sum(GHGs,na.rm = TRUE))


png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("upstream_AB_oil_ghgs.png",width = 1500)
ggplot(df1,aes(Year,GHGs,group = sector,colour=sector,fill=sector)) +
  geom_area(position = "stack") +
  scale_fill_manual("",values = my_palette,guide = "legend")+
  scale_colour_manual("",values=my_palette,guide = "legend")+
  scale_x_discrete(breaks=seq(1990,2016,2),expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  guides(fill=guide_legend(nrow =1,byrow=FALSE))+
  slide_theme()+
  labs(y=expression('Annual Emissions '*'(MtCO'[2]*'e)'),x="Date",
       title=paste("Alberta upstream oil emissions by sector",sep=""),
       caption="Source: Environment Canada Inventory Data, graph by Andrew Leach.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


#"Oil Sands (Mining, In-situ, Upgrading)",
oil_sands<-c("Oil Sands Mining","Oil Sands In Situ","Oil Sands Upgrading")
sub_samp<-subset(NIR_data,sector %in% oil_sands & Prov == "AB")

ggplot(filter(NIR_data,sector %in% oil_sands & Prov == "AB")%>%mutate(sector=fct_rev(sector)))+
  geom_area(aes(as.numeric(Year),GHGs,group = sector,fill=sector),color="black",position = "stack",alpha=0.6) +
  geom_line(data=filter(NIR_data,sector=="Electricity" & Prov == "AB"),
         aes(as.numeric(Year),GHGs,group = sector,colour=sector),size=1.4) +
  
  scale_fill_manual("",values = rev(colors_ua10()),guide = "legend")+
  scale_colour_manual("",values=colors_ua10()[3],guide = "legend")+
  scale_x_continuous(breaks=pretty_breaks(n=10),expand = c(0,0))+
  #scale_y_continuous(limits=c(0,80),breaks=seq(0,80,20),expand = c(0,0))+
  guides(fill=guide_legend(nrow =1,byrow=FALSE))+
  slide_theme()+
  labs(y=expression('Annual Emissions '*'(MtCO'[2]*'e)'),x="",
       #title=paste("Oil sands emissions by sector",sep=""),
       #caption="Source: Environment Canada Inventory Data, graph by Andrew Leach.",
       NULL)
ggsave("oil_sands_vs_power.png",width=12,height=6,dpi=300)


#2019 AER data: ST98-2019-CrudeBitumen-SupplyDemand-Data-May-2019


capp_data <- read_excel("Reports/CAPP/CAPP2019Forecast.xlsx", sheet = "Appendix A.1 Production", range = "A33:AA36")

names(capp_data)<-c("supply",2010+seq(0:25)-1)
capp_data<-capp_data%>%pivot_longer(-supply,values_to = "Production",names_to = "Year")


st_98_bitumen<-"http://www1.aer.ca/st98/2020/data/crude-bitumen/ST98-CrudeBitumen-SupplyDemand.xlsx"
download.file(st_98_bitumen,"Reports/ERCB/ST_98_bitumen.xlsx",mode="wb")

prod_data <- read.xlsx(xlsxFile = "Reports/ERCB/ST98-2019-CrudeBitumen-SupplyDemand-Data-May-2019.xlsx", sheet = "Production", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)

prod_data <- read.xlsx(xlsxFile = "Reports/ERCB/ST_98_bitumen.xlsx", sheet = "Production", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)

#prod_data <- read.xlsx(xlsxFile = "Reports/ERCB/ST98-2018_BitumenSupplyDemand.xlsx", sheet = "Production", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
names(prod_data)<-c("Year","Oil Sands In Situ","Oil Sands Mining","Oil Sands Upgrading")
prod_data<-prod_data %>% pivot_longer(-Year,values_to = "Production",names_to = "sector")
bitumen_data<-prod_data %>% filter(sector!="Upgrading")%>% group_by(Year) 
production_ghgs<-NIR_data %>% filter(Prov=="AB",sector %in% c("Oil Sands Mining","Oil Sands In Situ")) 
  bitumen_data<-bitumen_data %>% right_join(production_ghgs) %>% select(Year,sector,Production,GHGs)

  bitumen_total<-bitumen_data %>% group_by(Year)%>% summarize(sector="Oil Sands Average",Production=sum(Production),GHGs=sum(GHGs))
  bitumen_data<-bitumen_data %>% bind_rows(bitumen_total) %>% mutate(ghg_bbl=GHGs*10^6/Production/10^3/365)

  

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("AB_oil_sands_ghgs_bbl.png",height = 950,width = 1700)
ggplot(bitumen_data,) +
  geom_line(aes(Year,ghg_bbl,colour=sector,group=sector),size=2,lty=3)+
  scale_colour_manual("",values=c("black",my_palette),guide = "legend")+
  scale_x_continuous(breaks=pretty_breaks(n=10),expand = c(0,0))+
  scale_y_continuous(breaks=pretty_breaks(n=5),expand = c(0,0))+
  expand_limits(y=c(0,.12))+
  guides(color=guide_legend(nrow=2,byrow=FALSE))+
  slide_theme() +
  labs(y=expression('Emissions Intensity  '*'(tCO'[2]*'e/bbl)'),x="Date",
       title=paste("Alberta oil sands production emissions intensity by subsector",sep=""),
       caption="Source: Environment Canada National Emissions Inventory Data and AER ST-3 Production Data\nGraph by Andrew Leach.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()




#sub_samp$Year<-as.character(sub_samp$Year)
prod_data$Year<-as.character(prod_data$Year)
df1<-merge(filter(NIR_data,Prov=="AB"),prod_data,by=c("Year","sector"))
df1$Production<-df1$Production*365*1000 #barrels per year
df1$GHG_bbl<-df1$GHGs*10^6/df1$Production




#use st3 links
load(file = "st3_link.Rdata") #production in million barrels per year
df2<- NIR_data %>% filter(Prov=="AB",sector %in% upstream_oil) %>% left_join(ghg_production,by=c("Year"="year","sector"="product")) %>% na.omit() %>%
  mutate(GHG_bbl=GHGs/production)%>% group_by(Year) %>%
  mutate(avg_ghg_bbl=sum(GHGs)/sum(production)) %>% ungroup()

prod_data_test<-prod_data %>% left_join(ghg_production,by=c("Year"="year","sector"="product")) %>% na.omit() %>% mutate(prod_bbld=production/365*10^3)


png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("AB_oil_ghgs_bbl.png",height = 950,width = 1700)
ggplot(df2,aes(Year,GHG_bbl,group = sector,colour=sector,fill=sector)) +
  geom_line(,size=2) +
  geom_line(aes(Year,avg_ghg_bbl,colour="Barrel-weighted Average"),size=2,lty=3)+
  scale_colour_manual("",values=c("black",my_palette),guide = "legend")+
  scale_x_continuous(breaks=pretty_breaks(n=8),expand = c(0,0))+
  scale_y_continuous(limits=c(0,.08),breaks=pretty_breaks(),expand = c(0,0))+
  guides(color=guide_legend(nrow=2,byrow=FALSE))+
  slide_theme() +
  labs(y=expression('Emissions Intensity  '*'(tCO'[2]*'e/bbl)'),x="Date",
       title=paste("Alberta oil sands and conventional oil emissions intensity by sector",sep=""),
       caption="Source: Environment Canada National Emissions Inventory Data and AER ST-3 Production Data\nGraph by Andrew Leach.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()



png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("oil_sands_history.png",height=1100,width=1600)
#set_tiff("oil_sands_history.tiff",h_sent =800,w_sent =1200)
ggplot(data=filter(prod_data,sector!="Oil Sands Upgrading"),aes(Year,Production,group = sector,colour=sector,fill=sector)) +
  geom_area(position="stack") +
  scale_fill_manual("",values = my_palette,guide = "legend")+
  scale_colour_manual("",values=my_palette,guide = "legend")+
  scale_x_continuous(breaks=pretty_breaks(),expand = c(0,0))+
  scale_y_continuous(breaks=pretty_breaks(),expand = c(0,0))+
  guides(fill=guide_legend(nrow =1,byrow=FALSE))+
  annotate("rect", fill = "grey70", alpha = 0.5, 
           xmin = 2019, xmax =2028,
           ymin = -Inf, ymax = Inf) +
  annotate("text", x = 2024, y =450, label = "ST-98\nForecast",size=5)+
  #slide_theme()+
  theme(panel.border = element_blank(),
        plot.margin = margin(1, 1, 1, 1, "cm"))+
  labs(y=expression('Bitumen production (1000s of barrels per day)'),x="Date",
       #title=paste("Oil sands production history and forecast",sep=""),
       caption="Source: Alberta Energy Regulator Production Data and ST-98 (2019) Projection \nGraph by Andrew Leach.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("oil_sands_history_bw.png",h_sent =600,w_sent =1000)
ggplot(filter(prod_data,sector %in% c("In-situ", "Mining and Extraction")),aes(Year,Production,group = sector,colour=sector,fill=sector)) +
  geom_area(position="stack") +
  scale_fill_manual("",values = c("grey40","grey10"),guide = "legend")+
  scale_colour_manual("",values=c("grey40","grey10"),guide = "legend")+
  scale_x_discrete(breaks=seq(1970,2030,5),expand = c(0,0))+
  scale_y_continuous(limits=c(0,4000),breaks=seq(0,4000,500),expand = c(0,0))+
  guides(fill=guide_legend(nrow =1,byrow=FALSE))+
  annotate("rect", fill = "grey70", alpha = 0.5, 
           xmin = 2019-1967, xmax =2028-1967,
           ymin = -Inf, ymax = Inf) +
  annotate("text", x = 2023-1967, y = 3850, label = "ST-98\nForecast",size=2)+
  
  theme_classic() +
  theme(panel.border = element_blank(),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "gray",linetype="dotted"),
        axis.line.x = element_line(color = "gray"),
        axis.line.y = element_line(color = "gray"),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(margin = margin(t = 5)),
        axis.title = element_text(size = 12),
        #axis.label.x = element_text(size=20,vjust=+5),
        plot.subtitle = element_text(size = 12,hjust=0.5),
        plot.caption = element_text(face="italic",size = 12,hjust=0),
        legend.key.width=unit(2,"line"),
        legend.position = "bottom",
        #legend.direction = "horizontal",
        #legend.box = "horizontal",
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust=0,size = 14,margin = margin(b = 10))
  )+
  labs(y=expression('Bitumen production (1000s of barrels per day)'),x="Date")
#title=paste("Oil sands production history and forecast",sep=""),
#caption="Source: AlbeProduction Data and ST-98 (2018) Projection \nGraph by Andrew Leach.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


#national production data

eia_prod<-pdfetch_EIA("INTL.55-1-CAN-TBPD.A",KEY)
eia_prod<-data.frame(date=index(eia_prod), coredata(eia_prod),stringsAsFactors = F)
names(eia_prod)<-c("date","oil_production")
eia_prod$Year<-year(eia_prod$date)
oil_emissions<-NIR_CAN %>% filter(sector %in% upstream_oil) %>%
  group_by(Year) %>% summarize(oil_ghgs=sum(GHGs)) %>%
  left_join(eia_prod) %>% mutate(oil_ei=oil_ghgs*10^6/(oil_production*1000*365))


png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("oil_ei.png",height =600,width =1000)
ggplot(oil_emissions,aes(date,oil_ei)) +
  geom_line(size=2) +
  tombe_theme()+theme(plot.margin = margin(1, 1, 1, 1, "cm"))+
  scale_x_date(breaks = date_breaks("2 years"),date_labels = "%Y")+
  labs(y=expression('Oil Emissions Intensity (tonnes per barrel)'),x="Year",
  title="Canadian oil production average emissions intensity",
  caption="Sources: Oil production data via EIA, upstream oil production emissions (including upgrading) from Canada's National Inventory Report (2019).\nGraph by Andrew Leach.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()



oil_sands<-c("Oil Sands (Mining, In-situ, Upgrading)")
oil_sands<-c("Mining and Extfraction","In-situ","Upgrading")

sub_samp<-subset(NIR_data,sector %in% oil_sands & Prov == "AB")

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("oil_sands_ghgs.png")
ggplot(df1,aes(Year,GHGs,group = sector,colour=sector,fill=sector)) +
  geom_area(position = "stack") +
  scale_fill_manual("",values = my_palette,guide = "legend")+
  scale_colour_manual("",values=my_palette,guide = "legend")+
  scale_x_discrete(breaks=seq(1990,2016,2),expand = c(0,0))+
  scale_y_continuous(limits=c(0,80),breaks=seq(0,80,20),expand = c(0,0))+
  guides(fill=guide_legend(nrow =1,byrow=FALSE))+
  slide_theme()+
  labs(y=expression('Annual Emissions '*'(MtCO'[2]*'e)'),x="Date",
       title=paste("Oil sands emissions",sep=""),
       caption="Source: Environment Canada Inventory Data, graph by Andrew Leach.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


sub_samp<-subset(NIR_data, Prov == "AB")

sub_samp<-subset(NIR_data,sector=="National Inventory Total" & Prov == "AB")

sub_samp<-subset(NIR_data,sector=="Transportation" & Prov == "AB")

transport_sec<-c("Cars, Trucks and Motorcycles",
"Bus, Rail and Domestic Aviation",
"Freight Transport",
"Other: Recreational, Commercial and Residential"
)
sub_samp<-subset(NIR_data,sector%in%transport_sec & Prov == "AB")



png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("AB_transport.png")
ggplot(sub_samp,aes(Year,GHGs,group = sector,colour=sector,fill=sector)) +
  geom_area(position = "stack") +
  scale_fill_manual("",values = my_palette,guide = "legend")+
  scale_colour_manual("",values=my_palette,guide = "legend")+
  scale_x_continuous(breaks=seq(1990,2016,2),expand = c(0,0))+
  scale_y_continuous(limits=c(0,40),breaks=seq(0,40,10),expand = c(0,0))+
  guides(fill=guide_legend(nrow =2,byrow=FALSE))+
  slide_theme()+
  labs(y=expression('Annual Emissions '*'(MtCO'[2]*'e)'),x="Date",
       title=paste("Alberta Transport Emissions",sep=""),
       caption="Source: Environment Canada Inventory Data, graph by Andrew Leach.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


oil_sands_sger_data <- read.xlsx(xlsxFile = "2016_Compliance_Report.xlsx", sheet = "oilsands", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
oil_sands_sger_data <-oil_sands_sger_data %>% select(Facility.Name,Facility.ID,Reporting.Company.Name,
                                                     `Total.Annual.Emissions.(TAE)`,
                                                     `Net.electricity.import(+)/.export(-)`,
                                                     Total.Production,
                                                     `OBA,.bitumen`,`OBA,.net.electricity`,OBA.total,SGER.Baseline.Emissions.Intensity,Emissions
  
) %>% filter(!is.na(`Total.Annual.Emissions.(TAE)`))

ggplot(oil_sands_sger_data)+
  geom_point(aes(Emissions/(Total.Production*6.2929),OBA.total/(Total.Production*6.2929)) )




power_sger_data <- read.xlsx(xlsxFile = "2016_Compliance_Report.xlsx", sheet = "Power", startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
#power_sger_data$`SGER.Free.Credits.($/MWh)`
dat.g <- gather(power_sger_data, `SGER.Free.Credits.($/MWh)`, `CCR.Free.Credits`)
df1<-melt(power_sger_data,id=c("Facility.Name","Emissions","Emissions.Intensity","Plant.Type"),measure.vars = c("SGER.Free.Credits.($/MWh)","CCR.Free.Credits"), value.name = "Credits",variable.name = "Policy")
df1$test<-factor(df1$Policy,labels =c("SGER","CCR") )
ggplot(df1, aes(test, Credits)) + 
  geom_bar(aes(fill = Plant.Type), stat = "identity", position = "dodge")+
  scale_fill_manual("",values = my_palette,guide = "legend")+
  slide_theme()+
  labs(y=expression('Value of Emissions Credits ($/MWh)'),x="",
       title=paste("Credit Value By Plant Type",sep=""),
       caption="Source: Alberta Environment Emissions Intensity Data, graph by Andrew Leach.")

df1<-melt(power_sger_data,id=c("Facility.Name","Emissions","Emissions.Intensity","Plant.Type"),measure.vars = c("SGER.Free.Credits.($/MWh)","CCR.Free.Credits"), value.name = "Credits",variable.name = "Policy")
df1$test<-factor(df1$Policy,labels =c("SGER","CCR") )
ggplot(df1, aes(Emissions.Intensity, Credits,group=test)) + 
  geom_point(aes(colour = Plant.Type,fill=Plant.Type,shape=test,size=Emissions/10^6))+
  scale_fill_manual("",values = my_palette,guide = "legend")+
  #scale_shape_manual(values=c(15,18,16,17,19))+
  scale_y_continuous(limits=c(0,40))+
  slide_theme()+
  labs(y=expression('Value of Emissions Credits ($/MWh)'),x="Emissions Intensity (t/MWh)",
       title=paste("Credit Value By Plant Type",sep=""),
       caption="Source: Alberta Environment Emissions Intensity Data, graph by Andrew Leach.")





cdn_data<-NULL
cdn_data<-filter(NIR_data,sector=="Inventory Total") %>% group_by(Year) %>% summarise(GHGs=sum(GHGs,na.rm = TRUE)) %>%
  mutate(Year=as.numeric(as.character(Year)), ghg=as.numeric(GHGs)) %>%
  bind_rows(data.frame(Year=c(2020,2030,2050),target=731.7137*c(1-.17,1-.30,1-1),projection=c(728,722,NA),bau=c(768,815,NA),adds=c(690,583,NA)))
cdn_data$projection[cdn_data$Year==2016]<-cdn_data$ghg[cdn_data$Year==2016]
cdn_data$bau[cdn_data$Year==2016]<-cdn_data$ghg[cdn_data$Year==2016]
cdn_data$target[cdn_data$Year==2016]<-cdn_data$ghg[cdn_data$Year==2016]
cdn_data$adds[cdn_data$Year==2016]<-cdn_data$ghg[cdn_data$Year==2016]

# Ontario's GHG levels and future targets
#data<-read_excel("EN-Tables_ON_1990-2015_Unrounded_Data.xlsx",
#                 sheet="Summary",range="F6:AE7") %>%
#  gather(year,ghg) %>%
#  mutate(year=as.numeric(year),
#         ghg=as.numeric(ghg)) %>%
#  bind_rows(data.frame(year=c(2020,2030,2050),target=181267.9*c(1-.15,1-.37,1-.8)))

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("emissions_and_targets.png")
ggplot(cdn_data,aes(Year,ghg))+
  geom_line(size=2,color="firebrick")+
  geom_point(aes(y=target),size=3,color="#262164")+
  geom_point(aes(y=projection),size=3,color="#FFDB05")+
  geom_point(aes(y=bau),size=3,color="firebrick")+
  geom_point(aes(y=adds),size=3,color="#007C41")+
  geom_line(aes(y=projection),size=2,color="#FFDB05",linetype=3)+
  geom_line(aes(y=bau),size=2,color="firebrick",linetype=3)+
  geom_line(aes(y=target),size=2,color="#262164",linetype=3)+
  geom_line(aes(y=adds),size=2,color="#007C41",linetype=3)+
  geom_point(data=cdn_data %>% filter(Year==2016),size=3,color="firebrick")+
  geom_hline(yintercept=0,size=1)+
  geom_text_repel(data=subset(cdn_data,Year!=2016),aes(y=target-10,label=paste("   ",Year,"Target   ")),color="#262164",
                  fontface="bold",point.padding = unit(1,"mm"),segment.alpha = 0)+
  annotate("text",x=2031,y=815,label="Second Biennial Report Projection (2016)",
           color="firebrick",fontface="bold",hjust=0)+
  annotate("text",x=2031,y=701,label="2018 Reference Case",
           color="#FFDB05",fontface="bold",hjust=0)+
  annotate("text",x=2031,y=583,label="Pan Canadian Framework Projection (2018)",
           color="#007C41",fontface="bold",hjust=0)+
  annotate("text",x=1997,y=790,label="Historical Emissions (1990-2016)",
           color="firebrick",fontface="bold",hjust=0)+
  tombe_theme+
  scale_y_continuous(limit=c(0,825))+
  scale_x_continuous(limit=c(1990,2052),breaks=seq(1990,2050,10),expand = c(0,0))+
  labs(x="",y=expression('Annual Emissions '*'(MtCO'[2]*'e)'),
       title="Canada's GHG Emissions, Projections and Future Targets",
       subtitle="Source: Environment and Climate Change Canada 2016 Preliminary Emissions Inventory (2018); Second and Third Biennial Report to the United Nations (2016,2018), and February 2018 PCF Update.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()




#ZEKE's target graph

data<-read_excel("Papers/Pipelines/Gap Analysis 2018.xlsx") 
#  gather(year,ghg) %>%
#  mutate(year=as.numeric(year),
#         ghg=as.numeric(ghg)) %>%
#  bind_rows(data.frame(year=c(2020,2030,2050),target=181267.9*c(1-.15,1-.37,1-.8)))
tombe_theme<-theme_minimal()+theme(
  axis.title.y = element_text(size=9),
  axis.title.x = element_text(size=9),
  legend.position = "top",
  legend.text=element_text(size=10),
  legend.margin=margin(c(0,0,-0.25,0),unit="cm"),
  plot.caption = element_text(size = 6, color = "gray40",hjust=1),
  plot.title = element_text(face = "bold"),
  plot.subtitle = element_text(size = 8, color = "gray40"),
  panel.grid.minor = element_blank(),
  panel.grid.major.x = element_blank()
)


data_long<-melt(data,id="year",value.name = "ghgs",variable.name = "scenario")
palette<-rev(viridis(7,option = "C"))[-1]#[-c(1,2,3)]
#palette<-c("firebrick",colors_tableau10()[-c(3,4,7,8,9)])

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("emissions_gap.png")
ggplot(data_long)+
  geom_line(aes(x=year,y=ghgs,group=scenario,colour=scenario),size=3)+
  #geom_text_repel(data=subset(cdn_data,Year!=2016),aes(y=target,label=paste("   ",Year,"Target   ")),color="#262164",
  #                fontface="bold",point.padding = unit(1,"mm"),segment.alpha = 0)+
  annotate("text",x=2031,y=65,label="Baseline",
           color=palette[1],fontface="bold",hjust=0)+
  annotate("text",x=2031,y=60,label="Current Policies Projection",
           color=palette[2],fontface="bold",hjust=0)+
  annotate("text",x=2031,y=56,label="Unconditional NDCs",
           color=palette[3],fontface="bold",hjust=0)+
  annotate("text",x=2031,y=53,label="Conditional NDCs",
           color=palette[4],fontface="bold",hjust=0)+
  annotate("text",x=2040,y=35,label="2C Scenario",
           color=palette[5],fontface="bold",hjust=0)+
  annotate("text",x=2040,y=15,label="1.5C Scenario",
           color=palette[6],fontface="bold",hjust=0)+
  annotate("rect", fill = "grey", alpha = 0.25, 
           xmin = 2025, xmax =2030, ymin = -Inf, ymax = Inf)+
  annotate("text", x = 2027.5, y = 70, label = "Paris Commitment\nPeriod",size=3)+
  annotate("rect", fill = "grey", alpha = 0.25, 
           xmin = 2018, xmax =2018.1, ymin = -Inf, ymax = Inf)+
  annotate("text", x = 2017, y = 70, label = "Today",size=3)+
  #annotate("text",x=2010,y=65,label="Historical Emissions (2010-2018)",
  #         color="firebrick",fontface="bold",hjust=0)+
  theme(legend.position = "none")+
  scale_y_continuous(limit=c(0,70))+
  scale_x_continuous(limit=c(2010,2052),breaks=seq(2010,2050,10),expand = c(0,0))+
  scale_color_manual(values=palette)+
  labs(x="",y=expression('Annual Emissions '*'(GtCO'[2]*'e)'),
       title="Global GHG Emissions, Projections and Mitigation Scenarios",
       subtitle="Source: UNEP Emissions Gap Report, Data via Zeke Hausfather, Carbon Brief")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()





# Treemap of Ontario's Emissions
library(treemapify)

makeColors <- function(){
  maxColors <- 11
  usedColors <- c()
  possibleColors <- colorRampPalette( brewer.pal( 11 , "Set3" ) )(maxColors)
  
  function(values){
    newKeys <- setdiff(values, names(usedColors))
    newColors <- possibleColors[1:length(newKeys)]
    usedColors.new <-  c(usedColors, newColors)
    names(usedColors.new) <- c(names(usedColors), newKeys)
    usedColors <<- usedColors.new
    
    possibleColors <<- possibleColors[length(newKeys)+1:maxColors]
    usedColors
  }
} 

mkColor <- makeColors()

#plotdata<-read_excel("../Dropbox/Tweets Data/GHG/EN-Tables_ON_1990-2015_Unrounded_Data.xlsx",
#                     sheet="Summary",range="AL9:AN30") %>%
#  group_by(BigType) %>%
#  mutate(totalghg=sum(GHGs))
sector_data<-NULL
sector_data<-filter(NIR_data,sector %in% main_sectors) %>% group_by(Year,sector) %>% summarise(GHGs=sum(GHGs,na.rm = TRUE)) %>%
  ungroup() %>% mutate(Year=as.numeric(as.character(Year)), ghg=as.numeric(GHGs))





png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("emissions_treemap.png")
ggplot(subset(sector_data,Year==2016),aes(subgroup=reorder(sector,-ghg),
                                          fill=reorder(sector,-ghg),area=ghg)) + 
  geom_treemap(color="white",size=2)+
  geom_treemap_subgroup_border(size=2,color="white")+
  geom_treemap_text(aes(label=sector),fontface="bold",place="center",reflow=T,size=10)+
  scale_fill_manual(name="Major Category:",values=mkColor(unique(sector_data$sector)))+
  theme_minimal()+
  theme(legend.position = "right")+
  labs(x="",y="",
       title="Canada's Greenhouse Gas Emissions in 2016",
       subtitle="Source: Environment and Climate Change Canada, National Inventory Report (2018)",
       caption="Graph by Andrew Leach, code courtesy of Trevor Tombe"
  )
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()

prov_data<-NULL
prov_data<-NIR_data %>% group_by(Year,Prov) %>% summarise(GHGs=sum(GHGs,na.rm = TRUE)) %>%
  ungroup() %>% mutate(Year=as.numeric(as.character(Year)), ghg=as.numeric(GHGs))





png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("prov_treemap.png")
ggplot(subset(prov_data,Year==2016),aes(subgroup=reorder(Prov,-ghg),
                                        fill=reorder(Prov,-ghg),area=ghg)) + 
  geom_treemap(color="white",size=2)+
  geom_treemap_subgroup_border(size=2,color="white")+
  geom_treemap_text(aes(label=Prov),fontface="bold",place="center",reflow=T,size=10)+
  scale_fill_manual(name="Province:",values=mkColor(unique(prov_data$Prov)))+
  tombe_theme+
  theme(legend.position = "right")+
  labs(x="",y="",
       title="Canada's Greenhouse Gas Emissions in 2016",
       subtitle="Source: Environment and Climate Change Canada, National Inventory Report (2018)",
       caption="Graph by Andrew Leach, code courtesy of Trevor Tombe"
  )
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()



#getting national communication info
library(tabulizer)
f <- "canada_nc7.pdf"

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
nat_com<-melt(nat_com,id="sector",value.name = "GHGs",variable.name = "Year") %>% mutate(GHGs=as.numeric(GHGs))
nat_com$sector[nat_com$sector=="Oil Sandsf"]<-"Oil Sands"
nat_com$Year<-as.numeric(as.character(nat_com$Year))
#nat_com$sector[nat_com$sector=="Oil Sands"]<-"Oil Sands (Mining, In-situ, Upgrading)"
#nat_com$sector[nat_com$sector=="Bitumen In Situ"]<-"In Situ"
#nat_com$sector[nat_com$sector=="Bitumen Mining"]<-"Mining and Extraction"
#nat_com$sector[nat_com$sector=="Bitumen Upgrading"]<-"Mining and Extraction"


#ECCC 2018 December projections

ec_2018_file <- "ECCC_DEC_2018.pdf"
# extract tables from only second page
econ_table<-data.frame(extract_tables(ec_2018_file, pages = 9),stringsAsFactors = F)


ec_2019_file<-"Reports/Environment Canada/progress-towards-ghg-emissions-target-en.pdf"
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

targets_csv<-"https://www.canada.ca/content/dam/eccc/documents/csv/cesindicators/progress-canada-ghg-emissions-reduction-target/2019/progress-towards-canada-ghg-emissions-target-en.csv"
download.file(targets_csv,"targets.csv",mode="wb")
target_data<-read_csv("targets.csv",skip = 2,na = "n/a")[-c(27:30),]


target_data<-target_data %>% melt(id="Year",variable.name="projection") %>% mutate(value=as.numeric(gsub("\\[.*\\]", "", value)),
                                                        projection=gsub("\\[.*\\]", "", projection),
                                                        projection=gsub(" \\(megatonnes of carbon dioxide equivalent\\)","",projection),
                                                        Year=as.numeric(Year))%>%
  rename(scenario=projection, year=Year,emissions=value)%>%
  mutate(scenario=gsub("Second Biennial Report Reference Case","2016 Reference Case",scenario))%>%
  filter(scenario %in% c("2016 Reference Case","2017 Reference Case"))%>%
  mutate(scen_year=as.numeric(str_sub(scenario,start=1,end=4))-2)%>%
  filter(year>scen_year)%>%select(-scen_year)
  


#ggplot(target_data)+geom_line(aes(Year,value,color=projection,group=projection))



#Environment Canada Emissions Projection Data

#2020 projections data
file_loc<-"http://data.ec.gc.ca/data/substances/monitor/canada-s-greenhouse-gas-emissions-projections/Current-Projections-Actuelles/GHG-GES/detailed_GHG_emissions_GES_detaillees.csv"
download.file(file_loc,destfile="ec_projections.csv",mode = "wb")

proj_data_2020<-read.csv("ec_projections.csv",skip = 0,na = "-") %>% 
  clean_names()%>%select(-c(2,4,6,8,10,12,14,28)) %>%
  pivot_longer(-c(1:7),names_to = "year",values_to = "emissions")%>%
  mutate(year=gsub("x","",year),emissions=gsub("-","0",emissions),year=as.numeric(year),emissions=as.numeric(emissions))
  names(proj_data_2020)<-c("region","scenario","sector","subsector_level_1","subsector_level_2","subsector_level_3","unit","year","emissions")
  

#load 2018 detailed projections data
  
file_loc<-"http://data.ec.gc.ca/data/substances/monitor/canada-s-greenhouse-gas-emissions-projections/Previous-Projections-Precedentes/2018/GHG-GES/detailed_GHG_emissions_GES_detaillees.csv"
download.file(file_loc,destfile="ec_projections_2018.csv",mode = "wb")
proj_data_2018<-read.csv("ec_projections_2018.csv",skip = 0,na = "-") %>% 
  clean_names()%>%select(-c(2,4,6,8,10,12,14,28)) %>%
  pivot_longer(-c(1:7),names_to = "year",values_to = "emissions")%>%
  mutate(year=gsub("x","",year),emissions=gsub("-","0",emissions),year=as.numeric(year),emissions=as.numeric(emissions))
  
names(proj_data_2018)<-c("region","scenario","sector","subsector_level_1","subsector_level_2","subsector_level_3","unit","year","emissions")


#load 2019 detailed projections data

file_loc<-"https://data-donnees.ec.gc.ca/data/substances/monitor/canada-s-greenhouse-gas-emissions-projections/Previous-Projections-Precedentes/2019/GHG-GES/detailed_GHG_emissions_GES_detaillees.csv"
download.file(file_loc,destfile="ec_projections_2019.csv",mode = "wb")

proj_data<-
  read_csv("ec_projections_2019.csv",skip = 0,na = "-",col_types = cols(.default = "c")) %>%
  #read_csv("detailed_GHG_emissions_GES_detaillees_2019.csv",skip = 0,na = "-",col_types = cols(.default = "c")) %>%
  clean_names()%>%select(-c(2,4,6,8,10,12,14,28)) %>%
  mutate_if(is.double,as.character()) %>%
  pivot_longer(-c(1:7),names_to = "year",values_to = "emissions")%>%
  mutate(year=gsub("x","",year),emissions=gsub("-","0",emissions),year=as.numeric(year),emissions=as.numeric(emissions))
names(proj_data)<-c("region","scenario","sector","subsector_level_1","subsector_level_2","subsector_level_3","unit","year","emissions")


proj_data<-proj_data %>% bind_rows(proj_data_2020,proj_data_2018)%>%
  filter(scenario!="NIR 2018",scenario!="NIR 2019",sector!="Total")

  



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


#proj_data [1] "Agriculture"      "Buildings"        "Electricity"      "Heavy Industry"   "Oil and Gas"     
#[6] "Transportation"   "Waste and Others"

#main sectors  [1] "Oil and Gas"x         "Electricity" x        "Transportation"x      "Heavy Industry"x      
#"Buildings"  x [6] "Agriculture" x         "Waste"               "Coal Production"     "Light Manufacturing" "Construction"       
#[11] "Forest Resources" 

#strip out nir from this csv, build in mine


proj_data<-proj_data%>% filter(scenario!="NIR 2018",scenario!="NIR 2019",scenario!="NIR 2020")%>% bind_rows(test=NIR_data %>%
                                                                    filter(sector %in% main_sectors)%>%
                                                                    mutate(sector=fct_other(sector,
                                                                    keep=c("Agriculture","Buildings","Electricity","Heavy Industry","Oil and Gas",     
                                                                           "Transportation"),                        
                                                                              other_level = "Waste and Others"
                                                                                            ))%>%
                                                                    select(sector,year=Year,prov=Prov,emissions=GHGs)%>%
                                                                    group_by(year,prov,sector) %>% summarise(emissions=sum(emissions))%>% ungroup()%>%
                                                                    mutate(scenario="NIR 2021")
)



#set terr agriculture equal to zero in years it doesn't appear


terr_ag_fix<-as_tibble(x=seq(1990,2018,1))%>% rename(year=value) %>% left_join(proj_data %>% filter(prov=="TERR",sector=="Agriculture",scenario=="NIR 2021")%>%
                                                                          select(year,prov,sector,emissions))%>%
  mutate(emissions=na.fill(emissions,0),prov="TERR",sector="Agriculture",scenario="NIR 2021")


proj_data<-proj_data %>% filter(!((prov=="TERR")&(sector=="Agriculture") &(scenario=="NIR 2021"))) %>% bind_rows(terr_ag_fix)




#proj_data<-proj_data%>% filter(sector!="Total")


#for the plot data, we want NIR pre-2019 and projections post-2019
  proj_data$prov<-factor(proj_data$prov,levels = (c("Canada","BC","AB","SK","MB","ON","QC","NB", "NS", "PE", "NL","ATL","OTHER ATL","TERR")))
  
  proj_data<-proj_data %>% group_by(prov) %>% mutate(level_2005=sum(emissions*(year==2005)),level_2017=sum(emissions*(year==2017)*(scenario=="2020 Reference Case")),
                                                     net_30_2005=level_2005*0.7) %>%
    group_by(prov,year)%>% mutate(ref_case_GHGs=sum(emissions*(scenario=="2020 Reference Case")),
                                  addl_case_GHGs=sum(emissions*(scenario=="2020 Additional Measures Scenario")),
                                  ref_cuts=1-ref_case_GHGs/level_2005,
                                  addl_cuts=1-addl_case_GHGs/level_2005
                                  
                                  )%>%
    filter((scenario=="NIR 2021")+(year>=2019)==1)
  
  
  
    
  
proj_outcome<-proj_data %>% filter(year==2030) %>% group_by(prov)%>%
  summarise(level_2017=mean(level_2017),level_2005=mean(level_2005),ref_case_GHGs=mean(ref_case_GHGs), addl_case=mean(addl_case_GHGs),ref_redn=mean(ref_cuts),add_cuts=mean(addl_cuts))
  
ggplot(filter(proj_data,emissions>0 & scenario%in% c("NIR 2021", "2020 Reference Case") & prov !="Canada"))+
  geom_area(aes(year,emissions,fill=sector),color="black",position = "stack",size=0.1,alpha=.4)+
  geom_area(data=filter(proj_data,emissions>0 & scenario%in% c("NIR 2021", "2020 Reference Case") & prov !="Canada" & year<=2018),
            aes(year,emissions,fill=sector),color="black",position = "stack",size=0.1,alpha=.8)+
  geom_line(aes(year,net_30_2005,colour=str_wrap("30% below 2005 provincial GHGs",width = 20)),linetype=1,size=1.05)+
  facet_wrap( ~ prov,nrow = 1)+
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
    axis.text.x = element_text(size = 10, colour = "black", angle = 90,hjust=0.5,vjust=0.5),
    strip.text.x = element_text(size = 12, colour = "black", angle = 0),
    axis.title.y = element_text(size = 14,face = "bold", colour="black"),
  )+
  labs(x=NULL,y=expression('Annual Emissions  '*'(MtCO'[2]*'e)'),
       title="Canadian GHG Emissions by Province",
       subtitle=paste("2021 National Inventory (1990-2019) levels and 2020 Reference Case projections (2020-2030)",sep=""),
       caption=str_wrap("Source: Environment and Climate Change Canada. Reference Case projections include policies and measures that were in place as of September 2019. Graph by @andrew_leach.",width = 180))
ggsave("inventory_proj.png",dpi = 300,width=14, height=7)




ggplot(filter(proj_data,emissions>0 & scenario%in% c("NIR 2021", "2020 Reference Case") & prov !="Canada"))+
  geom_area(aes(year,emissions,fill=prov),color="black",position = "stack",size=0.1,alpha=.4)+
  geom_area(data=filter(proj_data,emissions>0 & scenario%in% c("NIR 2021", "2020 Reference Case") & prov !="Canada" & year<=2018),
            aes(year,emissions,fill=prov),color="black",position = "stack",size=0.1,alpha=.8)+
  #geom_line(aes(year,net_30_2005,colour=str_wrap("30% below 2005 sector GHGs",width = 20)),linetype=1,size=1.05)+
  facet_wrap( ~ sector,nrow = 1)+
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
    axis.text.x = element_text(size = 10, colour = "black", angle = 90,hjust=0.5,vjust=0.5),
    strip.text.x = element_text(size = 12, colour = "black", angle = 0),
    axis.title.y = element_text(size = 14,face = "bold", colour="black"),
  )+
  labs(x=NULL,y=expression('Annual Emissions  '*'(MtCO'[2]*'e)'),
       title="Canadian GHG Emissions by Sector",
       subtitle=paste("2021 National Inventory (1990-2019) levels and 2020 Reference Case projections (2019-2030)",sep=""),
       caption=str_wrap("Source: Environment and Climate Change Canada (2021). Reference Case projections include policies and measures that were in place as of September 2019. Graph by @andrew_leach.",width = 180))
ggsave("sector_proj.png",dpi = 300,width=14, height=7)

ggplot(filter(proj_data,emissions>0 & scenario%in% c("NIR 2021", "2020 Reference Case") & prov !="Canada" & sector=="Electricity"))+
  geom_area(aes(year,emissions,fill=prov),color="black",position = "stack",size=0.1,alpha=.4)+
  geom_area(data=filter(proj_data,emissions>0 & scenario%in% c("NIR 2021", "2020 Reference Case") & prov !="Canada" & year<=2018 & sector=="Electricity"),
            aes(year,emissions,fill=prov),color="black",position = "stack",size=0.1,alpha=.8)+
  #geom_line(aes(year,net_30_2005,colour=str_wrap("30% below 2005 sector GHGs",width = 20)),linetype=1,size=1.05)+
  #facet_wrap( ~ sector,nrow = 1)+
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
       title="Canadian Electricity Sector GHG Emissions",
       subtitle=paste("2021 National Inventory (1990-2019) levels and 2020 Reference Case projections (2019-2030)",sep=""),
       caption=str_wrap("Source: Environment and Climate Change Canada (2021). Reference Case projections include policies and measures that were in place as of September 2019. Graph by @andrew_leach.",width = 180))
ggsave("power_proj.png",dpi = 300,width=14, height=7)

ggplot(filter(proj_data,emissions>0 & scenario%in% c("NIR 2021", "2020 Reference Case") & prov !="Canada" & sector=="Oil and Gas"))+
  geom_area(aes(year,emissions,fill=prov),color="black",position = "stack",size=0.5,alpha=.6)+
  geom_area(data=filter(proj_data,emissions>0 & scenario%in% c("NIR 2021", "2020 Reference Case") & prov !="Canada" & year<=2018 & sector=="Oil and Gas"),
            aes(year,emissions,fill=prov),color="black",position = "stack",size=0.5,alpha=1)+
  #geom_line(aes(year,net_30_2005,colour=str_wrap("30% below 2005 sector GHGs",width = 20)),linetype=1,size=1.05)+
  #facet_wrap( ~ sector,nrow = 1)+
  scale_x_continuous(breaks=pretty_breaks(n=10))+
  #scale_color_viridis("",discrete=TRUE,guide_legend(NULL),option="E")+
  #scale_fill_viridis("",discrete=TRUE,option="E")+
  scale_fill_viridis("",discrete=TRUE,option="B",direction = -1)+
  #scale_fill_manual("",values = my_palette,guide = "legend")+
  #scale_fill_grey("",guide = "legend",start = 0.9,end=0.05)+
  scale_colour_manual("",values="black",guide = "legend")+
  guides(fill=guide_legend(nrow =1,byrow=FALSE),color=guide_legend(nrow =1,byrow=FALSE))+
  theme_minimal()+theme(
    legend.position = "bottom",
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
    panel.background = element_rect(fill = "transparent", colour = NA)
  )+
  labs(x=NULL,y=expression('Annual Emissions  '*'(MtCO'[2]*'e)'),
       title="Canadian Oil and Gas Inventory GHG Emissions (1990-2019) and Projections (2020-2030)",
       #subtitle=paste("2021 National Inventory (1990-2019) levels and 2020 Reference Case projections (2019-2030)",sep=""),
       caption=str_wrap("Source: Environment and Climate Change Canada (2021). Reference Case projections include policies and measures that were in place as of September 2019. Graph by Andrew Leach.",width = 180))
ggsave("images/oil_gas_proj.png",width=14, height=7, bg = "transparent",dpi=300)
ggsave("images/oil_gas_proj.jpg",width=14, height=7, bg = "transparent")



proj_data %>% filter(sector=="Oil and Gas", year==2030, scenario=="2020 Reference Case")%>% group_by(scenario)%>% summarize(ghgs=sum(emissions))


ggplot(filter(proj_data,emissions>0 & scenario%in% c("NIR 2021") & prov !="Canada"))+
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
       title="Canadian GHG Emissions by Province",
       subtitle=paste("2021 National Inventory (1990-2019)",sep=""),
       caption=str_wrap("Source: Environment and Climate Change Canada 2021 National Inventory (1990-2019). Graph by @andrew_leach.",width = 180),
       NULL
       )
ggsave("inventory_only.png",dpi = 600,width=14, height=7)


ggplot(filter(proj_data,emissions>0 & scenario%in% c("NIR 2021") & prov !="Canada"))+
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
       title="Canadian GHG Emissions by Sector",
       #subtitle=paste("2020 National Inventory (1990-2018)",sep=""),
       caption=str_wrap("Source: Environment and Climate Change Canada 2021 National Inventory (1990-2019). Graph by @andrew_leach.",width = 180),
       NULL
  )
ggsave("inventory_sector.png",dpi = 300,width=14, height=7)


ggplot(filter(proj_data,emissions>0 & scenario%in% c("NIR 2021") & prov !="Canada" & sector=="Electricity"))+
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
       caption=str_wrap("Source: Environment and Climate Change Canada 2021 National Inventory (1990-2019). Graph by @andrew_leach.",width = 180),
       NULL
  )
ggsave("inventory_electricity.png",dpi = 300,width=14, height=7)



#per capita

test<-proj_data %>% left_join(pop_data %>% 
                                group_by(Code)%>%
                                mutate(pop_2005=sum(Prov_pop*(Year=="2005")))%>%ungroup()%>%
                                mutate(Code=as_factor(Code),Year=as.double(Year)) %>% rename(year=Year,prov=Code))



ggplot(filter(test,scenario%in% c("NIR 2021") & prov !="Canada"))+
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
ggsave("inventory_per_capita.png",dpi = 300,width=14, height=7)






legend_labels<-c(paste(str_wrap("ECCC 'Reference Case 2019' emissions projection",width = 20),"\n",sep=""),
                 paste(str_wrap("Emissions-weighted share of Canada's Paris target: 30% below 2005 emissions",width = 20),"\n",sep=""))


png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("inventory_measures.png",width = 1850)
ggplot(data=filter(proj_data,emissions>0 & scenario%in% c("NIR 2020", "2020 Additional Measures Scenario") & prov !="Canada"))+
  geom_area(aes(year,emissions,fill=sector),color="black",position = "stack",size=0.2,alpha=.4)+
  geom_area(data=filter(proj_data,year<=2018,emissions>0 & scenario%in% c("NIR 2020", "2020 Additional Measures Scenario") & prov !="Canada"),
            aes(year,emissions,fill=sector),color="black",position = "stack",size=0.2,alpha=.6)+
  geom_line(aes(year,net_30_2005,colour=legend_labels[2],linetype=legend_labels[2]),size=.85)+
  geom_line(data=filter(proj_data,emissions>0 & scenario%in% c("NIR 2019", "2019 Additional Measures Scenario") & prov !="Canada",year>2018),
            aes(year,ref_case_GHGs,colour=legend_labels[1],
                linetype=legend_labels[1]),size=.85)+
  annotate("text",x=2011,y=295,label="Historical\nEmissions\n(2005-2017)",
           color="black",fontface="bold",hjust=0.5,size=1.8)+
  annotate("text",x=2023,y=295,label="ECCC\nProjections\n(2018-2030)",
           color="black",fontface="bold",hjust=0.5,size=1.8)+
  facet_grid( ~ prov)+
  
  scale_y_continuous(breaks=pretty_breaks())+
  scale_x_continuous(breaks=pretty_breaks(n=5))+
  #scale_color_viridis("",discrete=TRUE,guide_legend(NULL),option="E")+
  #scale_fill_viridis("",discrete=TRUE,option="E")+
  #scale_fill_manual("",values = my_palette,guide = "legend")+
  scale_fill_viridis(str_wrap("ECCC 'Additional Measures' Scenario Emissions Components",width =22),discrete=TRUE,option="B")+
  #scale_fill_grey("",guide = "legend",start = 0.9,end=0.05)+
  scale_colour_manual("",values=c("black","black"),guide = "legend")+
  scale_linetype_manual("",values=c("31","11"),guide = "legend")+
  #guides(fill=guide_legend(nrow =1,byrow=FALSE),color=guide_legend(nrow =1,byrow=FALSE))+
  theme_minimal()+theme(
    legend.position = "right",
    legend.key.width = unit(0.8, 'cm'),
    legend.margin=margin(c(.05,0,.05,0),unit="cm"),
    legend.text = element_text(colour="black", size = 10),
    legend.title = element_text(colour="black", size = 10),
    legend.spacing = unit(1.0, 'cm'),
    plot.caption = element_text(size = 10, face = "italic"),
    plot.title = element_text(size=16,face = "bold"),
    plot.subtitle = element_text(size = 12),
    panel.grid.minor = element_blank(),
    text = element_text(size = 20,face = "bold"),
    axis.text.y = element_text(size = 12,face = "bold", colour="black"),
    #axis.text.x = element_blank(),
    axis.text.x = element_text(size = 10, colour = "black", angle = 90,vjust = 0.5),
    strip.text.x = element_text(size = 12, colour = "black", angle = 0),
    axis.title.y = element_text(size = 14,face = "bold", colour="black"),
  )+
  labs(x=NULL,y=expression('Annual Emissions  '*'(MtCO'[2]*'e)'),
       title="Canada's GHG Emissions and Projections, 2005-2030",
       subtitle=paste("Inventory GHG Emissions (2005-2017) and Environment and Climate Change Canada (ECCC) Projections (2018-2030)\n",
                      str_wrap("Projections include policies and measures that were in place as of September 2018 (Reference Case) and
policies and measures that under development but not yet fully implemented (Additional Measures)",width=120),sep=""),
       caption="Source: Environment Canada National Inventory Data and Detailed Emisssions Projections, graph by @andrew_leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()



legend_labels<-c(paste(str_wrap("ECCC 'Additional Measures Scenario 2018' emissions projection",width = 20),"\n",sep=""),
                 paste(str_wrap("Emissions-weighted share of Canada's Paris target: 30% below 2005 emissions",width = 20),"\n",sep=""))


png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("inventory_measures2.png",width=1400)
ggplot(filter(proj_data,emissions>0 & scenario%in% c("NIR 2019", "2019 Reference Case") & prov !="Canada"))+
  geom_area(aes(year,emissions,fill=sector),color="black",position = "stack",size=0.15,alpha=.6)+
  geom_area(data=filter(proj_data,year<=2018,emissions>0 & scenario%in% c("NIR 2019", "2019 Reference Case") & prov !="Canada"),
            aes(year,emissions,fill=sector),colour="black",size=0.15,position = "stack",alpha=.6)+
  geom_line(aes(year,net_30_2005,colour=legend_labels[2],linetype=legend_labels[2]),size=.85)+
  #geom_line(data=filter(proj_data,emissions>0 & scenario%in% c("NIR 2018", "2018 Additional Measures Scenario") & prov !="Canada",year>=2017),
  #          aes(year, addl_case_GHGs,colour=legend_labels[1],
  #              linetype=legend_labels[1]),size=0.85)+
  #annotate("text",x=2011,y=295,label="Historical\nEmissions\n(2005-2017)",
  #         color="black",fontface="bold",hjust=0.5,size=1.8)+
  annotate("text",x=2023,y=295,label="Projection",
           color="black",fontface="bold",hjust=0.5,size=1.8)+
  facet_grid( ~ prov)+
  
  scale_y_continuous(breaks=pretty_breaks())+
  scale_x_continuous(breaks=c(2005,2017,2030))+
  #scale_color_viridis("",discrete=TRUE,guide_legend(NULL),option="E")+
  #scale_fill_viridis("",discrete=TRUE,option="E")+
  #scale_fill_manual("",values = my_palette,guide = "legend")+
  scale_fill_viridis(str_wrap("ECCC '2018 Reference Case' Emissions Components",width =22),discrete=TRUE,option="B")+
  #scale_fill_grey("",guide = "legend",start = 0.9,end=0.05)+
  scale_colour_manual("",values=c("black","black"),guide = "legend")+
  scale_linetype_manual("",values=c("31","11"),guide = "legend")+
  #guides(fill=guide_legend(nrow =1,byrow=FALSE),color=guide_legend(nrow =1,byrow=FALSE))+
  theme_minimal()+theme(
    legend.position = "right",
    legend.key.width = unit(0.8, 'cm'),
    legend.margin=margin(c(.05,0,.05,0),unit="cm"),
    legend.text = element_text(colour="black", size = 10),
    legend.title = element_text(colour="black", size = 10),
    legend.spacing = unit(1.0, 'cm'),
    plot.caption = element_text(size = 10, face = "italic"),
    plot.title = element_text(size=16,face = "bold"),
    plot.subtitle = element_text(size = 12),
    panel.grid.minor = element_blank(),
    text = element_text(size = 20,face = "bold"),
    axis.text.y = element_text(size = 12,face = "bold", colour="black"),
    #axis.text.x = element_blank(),
    axis.text.x = element_text(size = 10, colour = "black", angle = 90),
    strip.text.x = element_text(size = 10, colour = "black", angle = 0),
    axis.title.y = element_text(size = 14,face = "bold", colour="black"),
  )+
  labs(x=NULL,y=expression('Annual Emissions  '*'(MtCO'[2]*'e)'),
       title="Canada's GHG Emissions and Projections, 2005-2030",
       subtitle="Inventory GHG Emissions (2005-2017) and ECCC Reference Case Projections (2018-2030)",
       #               str_wrap("Projections include policies and measures that were in place as of September 2018 (Reference Case) and
#policies and measures that under development but not yet fully implemented (Additional Measures)",width=120),sep=""),
       caption="Source: Environment Canada National Inventory Data and Detailed Emisssions Projections, graph by @andrew_leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("inventory_ref_canada.png",width=1400)
ggplot(filter(proj_data,emissions>0 & scenario%in% c("NIR 2019", "2019 Reference Case") & prov =="Canada"))+
  geom_area(aes(year,emissions,fill=sector),color="black",position = "stack",size=0.15,alpha=1)+
  geom_area(data=filter(proj_data,year<=2017,emissions>0 & scenario%in% c("NIR 2019", "2019 Reference Case") & prov =="Canada"),
            aes(year,emissions,fill=sector),colour="black",size=0.15,position = "stack",alpha=.03)+
  geom_line(aes(year,net_30_2005,colour=legend_labels[2],linetype=legend_labels[2]),size=.85)+
  geom_line(data=filter(proj_data,emissions>0 & scenario%in% c("NIR 2019", "2019 Additional Measures Scenario") & prov =="Canada",year>=2017),
            aes(year, addl_case_GHGs,colour=legend_labels[1],
                linetype=legend_labels[1]),size=0.85)+
  #annotate("text",x=2011,y=295,label="Historical\nEmissions\n(2005-2017)",
  #         color="black",fontface="bold",hjust=0.5,size=1.8)+
  annotate("text",x=2023,y=790,label="Projection",
           color="black",fontface="bold",hjust=0.5,size=1.8)+
  facet_grid( ~ prov)+
  
  scale_y_continuous(breaks=pretty_breaks())+
  scale_x_continuous(breaks=c(2005:2030))+
  #scale_color_viridis("",discrete=TRUE,guide_legend(NULL),option="E")+
  #scale_fill_viridis("",discrete=TRUE,option="E")+
  #scale_fill_manual("",values = my_palette,guide = "legend")+
  scale_fill_viridis(str_wrap("ECCC '2018 Reference Case' Emissions Components",width =22),discrete=TRUE,option="B")+
  #scale_fill_grey("",guide = "legend",start = 0.9,end=0.05)+
  scale_colour_manual("",values=c("black","black"),guide = "legend")+
  scale_linetype_manual("",values=c("31","11"),guide = "legend")+
  #guides(fill=guide_legend(nrow =1,byrow=FALSE),color=guide_legend(nrow =1,byrow=FALSE))+
  theme_minimal()+theme(
    legend.position = "right",
    legend.key.width = unit(0.8, 'cm'),
    legend.margin=margin(c(.05,0,.05,0),unit="cm"),
    legend.text = element_text(colour="black", size = 10),
    legend.title = element_text(colour="black", size = 10),
    legend.spacing = unit(1.0, 'cm'),
    plot.caption = element_text(size = 10, face = "italic"),
    plot.title = element_text(size=16,face = "bold"),
    plot.subtitle = element_text(size = 12),
    panel.grid.minor = element_blank(),
    text = element_text(size = 20,face = "bold"),
    axis.text.y = element_text(size = 12,face = "bold", colour="black"),
    #axis.text.x = element_blank(),
    axis.text.x = element_text(size = 10, colour = "black", angle = 90),
    strip.text.x = element_text(size = 10, colour = "black", angle = 0),
    axis.title.y = element_text(size = 14,face = "bold", colour="black"),
  )+
  labs(x=NULL,y=expression('Annual Emissions  '*'(MtCO'[2]*'e)'),
       title="Canada's GHG Emissions and Projections, 2005-2030",
       subtitle="Inventory GHG Emissions (2005-2017) and ECCC Reference Case Projections (2018-2030)",
       #               str_wrap("Projections include policies and measures that were in place as of September 2018 (Reference Case) and
       #policies and measures that under development but not yet fully implemented (Additional Measures)",width=120),sep=""),
       caption="Source: Environment Canada National Inventory Data and Detailed Emisssions Projections, graph by @andrew_leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()



png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("inventory_ref_ab.png",width=1400)
ggplot(filter(proj_data,emissions>0 & scenario%in% c("NIR 2019", "2019 Reference Case") & prov =="AB"))+
  geom_area(aes(year,emissions,fill=sector),color="black",position = "stack",size=0.15,alpha=.3)+
  geom_area(data=filter(proj_data,year<=2019,emissions>0 & scenario%in% c("NIR 2019", "2019 Reference Case") & prov =="AB"),
            aes(year,emissions,fill=sector),colour="black",size=0.15,position = "stack",alpha=.8)+
  geom_line(aes(year,net_30_2005,colour=legend_labels[2],linetype=legend_labels[2]),size=.85)+
  geom_line(data=filter(proj_data,emissions>0 & scenario%in% c("NIR 2019", "2019 Additional Measures Scenario") & prov =="AB",year>=2018),
            aes(year, addl_case_GHGs,colour=legend_labels[1],
                linetype=legend_labels[1]),size=0.85)+
  #annotate("text",x=2011,y=295,label="Historical\nEmissions\n(2005-2017)",
  #         color="black",fontface="bold",hjust=0.5,size=1.8)+
  #annotate("text",x=2023,y=790,label="Projection",
  #         color="black",fontface="bold",hjust=0.5,size=1.8)+
  #facet_grid( ~ prov)+
  
  scale_y_continuous(breaks=pretty_breaks(),expand=c(0,0))+
  scale_x_continuous(breaks=pretty_breaks(n=20),expand=c(0,0))+
  expand_limits(y=0)+
  #scale_color_viridis("",discrete=TRUE,guide_legend(NULL),option="E")+
  #scale_fill_viridis("",discrete=TRUE,option="E")+
  #scale_fill_manual("",values = my_palette,guide = "legend")+
  scale_fill_viridis(str_wrap("ECCC '2018 Reference Case' Emissions Components",width =22),discrete=TRUE,option="B")+
  #scale_fill_grey("",guide = "legend",start = 0.9,end=0.05)+
  scale_colour_manual("",values=c("black","black"),guide = "legend")+
  scale_linetype_manual("",values=c("31","11"),guide = "legend")+
  #guides(fill=guide_legend(nrow =1,byrow=FALSE),color=guide_legend(nrow =1,byrow=FALSE))+
  theme_minimal()+theme(
    legend.position = "right",
    legend.key.width = unit(0.8, 'cm'),
    legend.margin=margin(c(.05,0,.05,0),unit="cm"),
    legend.text = element_text(colour="black", size = 10),
    legend.title = element_text(colour="black", size = 10),
    legend.spacing = unit(1.0, 'cm'),
    plot.caption = element_text(size = 10, face = "italic"),
    plot.title = element_text(size=16,face = "bold"),
    plot.subtitle = element_text(size = 12),
    panel.grid.minor = element_blank(),
    text = element_text(size = 20,face = "bold"),
    axis.text.y = element_text(size = 12,face = "bold", colour="black"),
    #axis.text.x = element_blank(),
    axis.text.x = element_text(size = 10, colour = "black", angle = 90),
    strip.text.x = element_text(size = 10, colour = "black", angle = 0),
    axis.title.y = element_text(size = 14,face = "bold", colour="black"),
  )+
  labs(x=NULL,y=expression('Annual Emissions  '*'(MtCO'[2]*'e)'),
       title="Alberta's GHG Emissions and Projections, 1990-2030",
       subtitle="Inventory GHG Emissions (2005-2017) and ECCC Reference Case Projections (2018-2030)",
       #               str_wrap("Projections include policies and measures that were in place as of September 2018 (Reference Case) and
       #policies and measures that under development but not yet fully implemented (Additional Measures)",width=120),sep=""),
       caption="Source: Environment Canada National Inventory Data and Detailed Emisssions Projections, graph by @andrew_leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()

#thesis graph with inventories and projections by province, and then three allocation rules

load(file = "pop_data.Rdata")

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

proj_data<-proj_data %>% left_join(pop_merge)

agg_proj_data<-proj_data %>% group_by(year,prov,pop,scenario)%>%
  summarize(emissions=sum(emissions))%>% mutate(date=ymd(paste(year,"12-31",sep="-")))



#for the plot data, we want NIR pre-2018 and projections post-2018
agg_proj_data$prov<-factor(agg_proj_data$prov,levels = (c("Canada","BC","AB","SK","MB","ON","QC","NB", "NS", "PE", "NL","ATL","OTHER ATL","TERR")))


#figure out last 5 years pro-rata by prov
year_5 <- proj_data %>% filter(scenario=="NIR 2019",year%in%c(2014,2015,2016,2017,2018))%>% 
  group_by(prov,scenario) %>% 
  summarize(last5=sum(emissions)/5)%>% 
  ungroup()%>%
  mutate(share=last5/sum(last5*(prov=="Canada"))*511)

#figure out last 5 years pro-rata by prov
year_2030 <- proj_data %>% filter(year%in%c(2030))%>% 
  group_by(prov,scenario) %>% 
  summarize(emissions=sum(emissions))%>% 
  ungroup()%>% group_by(scenario)%>%
  mutate(share_2030=emissions/sum(emissions*(prov=="Canada")),
         pro_rate=emissions/sum(emissions*(prov=="Canada"))*511)



#percapita share
per_cap <- proj_data %>% filter(year==2030,scenario=="2019 Reference Case")%>%
  group_by(prov) %>% 
  summarize(pop=mean(pop))%>% ungroup()%>%
  mutate(per_cap_share=pop/sum(pop*(prov=="Canada"))*511) %>% select(prov,per_cap_share)


sector_shares <- proj_data %>% filter(year==2017,scenario=="NIR 2019")%>%
  filter(prov=="Canada")%>%
  group_by(sector) %>% 
  mutate(sector_ghgs=sum(emissions))%>% ungroup()%>%
  mutate(sector_share=sector_ghgs/sum(sector_ghgs))%>%
  select(sector,sector_ghgs,sector_share)






#figure out last 5 years pro-rata by prov
year_5 <- proj_data %>% filter(scenario=="NIR 2019",year%in%c(2014,2015,2016,2017,2018))%>% 
  group_by(prov,scenario) %>% 
  summarize(last_5_share=sum(emissions)/5)%>% select(prov,last_5_share)



proj_data <- proj_data %>% group_by(prov,sector) %>% 
  mutate(level_last_5=sum(emissions*(year%in%c(2014,2015,2016,2017,2018))*(scenario=="NIR 2019"))/5,
        last_per_cap=sum(emissions*10^6/pop*(year==2020)*(scenario=="NIR 2019")),
        )%>%ungroup()%>%
  left_join(year_5)%>%left_join(per_cap)



label_vals<-c(
  paste(str_wrap("30% below 2005 provincial GHGs",width = 20),"\n"),
  paste(str_wrap("Paris target allocation based on last 5 years' share of emissions",width = 20),"\n"),
  paste(str_wrap("Paris target allocation on an equal per-capita (2030) basis",width = 20),"\n")
)


ggplot(filter(proj_data,emissions>0 & scenario%in% c("NIR 2019", "2019 Reference Case") & prov !="Canada"))+
  geom_area(aes(year,emissions,fill=sector),color="black",position = "stack",size=0.1,alpha=.6)+
  geom_line(aes(year,net_30_2005,linetype="A"),size=1.05)+
  geom_line(aes(year,last_5_share,linetype="B"),size=1.05)+
  geom_line(aes(year,per_cap_share,linetype="C"),size=1.05)+
  facet_wrap( ~ prov,nrow = 1)+
  scale_x_continuous(breaks=pretty_breaks(),minor_breaks=waiver(),expand = c(0,0))+
  scale_y_continuous(breaks=pretty_breaks(),minor_breaks=waiver(),expand = c(0,0))+
  expand_limits(y = 280)+ 
  #scale_color_viridis("",discrete=TRUE,guide_legend(NULL),option="E")+
  #scale_fill_viridis("",discrete=TRUE,option="E")+
  #scale_fill_viridis("",discrete=TRUE,option="B")+
  #scale_fill_manual("",values = my_palette,guide = "legend")+
  scale_fill_grey("",guide = "legend",start = 0.98,end=0)+
  scale_linetype_manual("",values=c(1,2,3),labels=label_vals, guide = "legend")+
  #guides(fill=guide_legend(nrow =1,byrow=FALSE),color=guide_legend(nrow =1,byrow=FALSE))+
  blake_theme()+ theme(panel.spacing = unit(.75,"lines"))+
  theme(
    plot.margin = margin(t = .05, r = .1, b = .025, l = .1,unit= "cm"),
    axis.ticks.x = element_line(size = rel(1),colour="#D8D8D8"),    # Change x axis ticks only
    axis.ticks.length = unit(5, "pt"), # Change the length of tick marks
    #legend.position = c(.95,.6),
    legend.position = "right",
    legend.key.width = unit(1.25,"cm"),
    axis.text.x = element_text(angle=90,hjust = .5,vjust=.5,size=rel(.7)),
    strip.text = element_text(size=rel(.7) ),
    legend.spacing = unit(1.0, 'cm')
    
  )+
  labs(x=NULL,y=expression('Annual Emissions  '*'(MtCO'[2]*'e)'),
       title="2005-2030 Provincial GHG Emissions",
       subtitle=paste("Inventory GHG Emissions (2005-2018) and 2019 Environment Canada Reference Case Projections (2019-2030)"
                      ,sep=""),
       caption="Source: Environment Canada National Inventory Data and Reference Case projections including policies and measures that were in place as of September 2019, graph by @andrew_leach")
ggsave("can_ghgs.png",width = 16,height = 10)


ggplot(filter(proj_data,emissions>0 & scenario%in% c("NIR 2019", "2019 Reference Case") & prov !="Canada"))+
  geom_area(aes(year,emissions,fill=sector),color="black",position = "stack",size=0.5,alpha=.6)+
  geom_line(aes(year,net_30_2005,linetype="A"),size=1.05)+
  geom_line(aes(year,last_5_share,linetype="B"),size=1.05)+
  geom_line(aes(year,per_cap_share,linetype="C"),size=1.05)+
  facet_wrap( ~ prov,nrow = 1)+
  scale_x_continuous(breaks=pretty_breaks(),minor_breaks=waiver(),expand = c(0,0))+
  scale_y_continuous(breaks=pretty_breaks(),minor_breaks=waiver(),expand = c(0,0))+
  expand_limits(y = 280)+ 
  #scale_color_viridis("",discrete=TRUE,guide_legend(NULL),option="E")+
  #scale_fill_viridis("",discrete=TRUE,option="E")+
  #scale_fill_viridis("",discrete=TRUE,option="B")+
  #scale_fill_manual("",values = my_palette,guide = "legend")+
  scale_fill_grey("",guide = "legend",start = 0.85,end=0)+
  scale_linetype_manual("",values=c(1,2,3),labels=label_vals, guide = "legend")+
  #guides(fill=guide_legend(nrow =1,byrow=FALSE),color=guide_legend(nrow =1,byrow=FALSE))+
  blake_theme()+ theme(panel.spacing = unit(.75,"lines"))+
  theme(
    plot.margin = margin(t = .05, r = .1, b = .025, l = .1,unit= "cm"),
    axis.ticks.x = element_line(size = rel(1),colour="#D8D8D8"),    # Change x axis ticks only
    axis.ticks.length = unit(5, "pt"), # Change the length of tick marks
    #legend.position = c(.95,.6),
    legend.position = "right",
    legend.key.width = unit(1.25,"cm"),
    axis.text.x = element_text(angle=90,hjust = .5,vjust=.5,size=rel(.7)),
    strip.text = element_text(size=rel(.7) ),
    legend.spacing = unit(1.0, 'cm')
    
  )+
  labs(x=NULL,y=expression('Annual Emissions  '*'(MtCO'[2]*'e)'),
       #title="2005-2030 Provincial GHG Emissions",
       #subtitle=paste("Inventory GHG Emissions (2005-2018) and 2019 Environment Canada Reference Case Projections (2019-2030)"
      #                ,sep=""),
      # caption="Source: Environment Canada National Inventory Data and Reference Case projections including policies and measures that were in place as of September 2019, graph by @andrew_leach")
      NULL)
ggsave("can_ghgs_plain.png",width = 16,height = 10,dpi=600 )


ggplot(filter(proj_data,year<2019,emissions>0 & scenario%in% c("NIR 2019", "2019 Reference Case") & prov !="Canada"))+
  geom_area(aes(year,emissions,fill=sector),color="black",position = "stack",size=0.5,alpha=.6)+
  #geom_line(aes(year,net_30_2005,linetype="A"),size=1.05)+
  #geom_line(aes(year,last_5_share,linetype="B"),size=1.05)+
  #geom_line(aes(year,per_cap_share,linetype="C"),size=1.05)+
  facet_wrap( ~ prov,nrow = 1)+
  scale_x_continuous(breaks=pretty_breaks(),minor_breaks=waiver(),expand = c(0,0))+
  scale_y_continuous(breaks=pretty_breaks(),minor_breaks=waiver(),expand = c(0,0))+
  expand_limits(y = 280)+ 
  #scale_color_viridis("",discrete=TRUE,guide_legend(NULL),option="E")+
  #scale_fill_viridis("",discrete=TRUE,option="E")+
  #scale_fill_viridis("",discrete=TRUE,option="B")+
  #scale_fill_manual("",values = my_palette,guide = "legend")+
  scale_fill_grey("",guide = "legend",start = 0.85,end=0)+
  scale_linetype_manual("",values=c(1,2,3),labels=label_vals, guide = "legend")+
  #guides(fill=guide_legend(nrow =1,byrow=FALSE),color=guide_legend(nrow =1,byrow=FALSE))+
  blake_theme()+ theme(panel.spacing = unit(.75,"lines"))+
  theme(
    plot.margin = margin(t = .05, r = .1, b = .025, l = .1,unit= "cm"),
    axis.ticks.x = element_line(size = rel(1),colour="#D8D8D8"),    # Change x axis ticks only
    axis.ticks.length = unit(5, "pt"), # Change the length of tick marks
    #legend.position = c(.95,.6),
    legend.position = "right",
    legend.key.width = unit(1.25,"cm"),
    axis.text.x = element_text(angle=90,hjust = .5,vjust=.5,size=rel(.7)),
    strip.text = element_text(size=rel(.7) ),
    legend.spacing = unit(1.0, 'cm')
    
  )+
  labs(x=NULL,y=expression('Annual Emissions  '*'(MtCO'[2]*'e)'),
       #title="2005-2030 Provincial GHG Emissions",
       #subtitle=paste("Inventory GHG Emissions (2005-2018) and 2019 Environment Canada Reference Case Projections (2019-2030)"
       #                ,sep=""),
       # caption="Source: Environment Canada National Inventory Data and Reference Case projections including policies and measures that were in place as of September 2019, graph by @andrew_leach")
       NULL)
ggsave("can_ghgs_inv.png",width = 16,height = 10,dpi=600 )






ggplot(filter(proj_data,emissions>0 & scenario%in% c("NIR 2019", "2019 Reference Case") & prov !="Canada"))+
  geom_area(aes(year,emissions*10^6/pop,fill=sector),color="black",position = "stack",size=0.1,alpha=.6)+
  #geom_line(aes(year,net_30_2005,colour=str_wrap("30% below 2005 provincial GHGs",width = 20)),linetype=1,size=1.05)+
  facet_wrap( ~ prov,nrow = 1)+
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
    plot.caption = element_text(size = 12, face = "italic"),
    plot.title = element_text(size=16,face = "bold"),
    plot.subtitle = element_text(size = 10),
    panel.grid.minor = element_blank(),
    text = element_text(size = 20,face = "bold"),
    axis.text.y = element_text(size = 12,face = "bold", colour="black"),
    #axis.text.x = element_blank(),
    axis.text.x = element_text(size = 10, colour = "black", angle = 90,hjust = -1),
    strip.text.x = element_text(size = 12, colour = "black", angle = 0),
    axis.title.y = element_text(size = 14,face = "bold", colour="black"),
  )+
  labs(x=NULL,y=expression('Annual Emissions  '*'(MtCO'[2]*'e)'),
       title="2005-2030 Provincial GHG Emissions",
       subtitle=paste("Inventory GHG Emissions (2005-2017) and Reference Case Projections (2018-2030)\n",
                      str_wrap("Projections include policies and measures that were in place as of September 2019 (Reference Case) and
policies and measures under development but not yet fully implemented (Additional Measures)",width=120),sep=""),
       caption="Source: Environment Canada National Inventory Data and Reference Case Projections, graph by @andrew_leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


legend_labels<-c(paste(str_wrap("ECCC 'Reference Case 2019' emissions projection",width = 20),"\n",sep=""),
                 paste(str_wrap("Emissions-weighted share of Canada's Paris target: 30% below 2005 emissions",width = 20),"\n",sep=""))







str_pad_custom <- function(labels){
  #new_labels <- stringr::str_pad(labels, 10, "right")
  new_labels <- paste(labels,"\n",sep="")
  return(new_labels)
}


png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("ns_iso_measures.png")
ggplot(data=filter(proj_data,prov=="NS" & scenario%in% c("NIR 2018", "2018 Additional Measures Scenario") & prov !="Canada"))+
  geom_area(aes(year,emissions,fill=sector),color="black",position = "stack",size=0.1,alpha=.4)+
  geom_line(aes(year,net_30_2005,colour=str_wrap("30% below 2005 provincial GHGs",width = 20)),linetype=1,size=1.25)+
  geom_line(data=filter(proj_data,prov=="NS",emissions>0 & scenario%in% c("NIR 2018", "2018 Additional Measures Scenario") & prov !="Canada",year>2017),
            aes(year,ref_case_GHGs,colour=str_wrap("Provincial Reference Case GHG Projection",width = 20)),linetype=1,size=1.25)+
  
  #facet_grid( ~ prov)+
  #scale_x_continuous(breaks=pretty_breaks())+
  scale_fill_viridis("",discrete=TRUE,option="B")+
  #scale_color_viridis("",discrete=TRUE,guide_legend(NULL),option="E")+
  #scale_fill_viridis("",discrete=TRUE,option="E")+
  #scale_fill_manual("",values = my_palette,guide = "legend")+
  #scale_fill_grey("",guide = "legend",start = 0.9,end=0.05)+
  scale_colour_manual("",values=c("black","dodgerblue"),guide = "legend",labels  = str_pad_custom)+
  #guides(fill=guide_legend(nrow =1,byrow=FALSE),color=guide_legend(nrow =1,byrow=FALSE))+
  theme_minimal()+theme(
    legend.position = "right",
    legend.margin=margin(c(.05,0,.05,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
    plot.caption = element_text(size = 11, face = "italic"),
    plot.title = element_text(size=16,face = "bold"),
    plot.subtitle = element_text(size = 14),
    panel.grid.minor = element_blank(),
    text = element_text(size = 20,face = "bold"),
    axis.text.y = element_text(size = 12,face = "bold", colour="black"),
    #axis.text.x = element_blank(),
    axis.text.x = element_text(size = 10, colour = "black", angle = 90,hjust = -1),
    strip.text.x = element_text(size = 12, colour = "black", angle = 0),
    axis.title.y = element_text(size = 14,face = "bold", colour="black"),
  )+
  labs(x=NULL,y=expression('Annual Emissions  '*'(MtCO'[2]*'e)'),
       title="Nova Scotia 2005-2030 GHG Emissions",
       subtitle="Inventory GHG Emissions (2005-2017) and Additional Measures Case Projections (2018-2030)",
       caption="Source: Environment Canada National Inventory Data and Detailed Emisssions Projections, graph by @andrew_leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("terr_iso_measures.png")
ggplot(data=filter(proj_data,prov=="TERR",emissions>=0 & scenario%in% c("NIR 2018", "2018 Additional Measures Scenario") & prov !="Canada"))+
  geom_area(aes(year,emissions,fill=sector),color="black",position = "stack",size=0.1,alpha=.4)+
  geom_line(aes(year,net_30_2005,colour=str_wrap("30% below 2005 provincial GHGs",width = 20)),linetype=1,size=1.25)+
  geom_line(data=filter(proj_data,prov=="TERR",emissions>=0 & scenario%in% c("NIR 2018", "2018 Additional Measures Scenario") & prov !="Canada",year>2017),
            aes(year,ref_case_GHGs,colour=str_wrap("Provincial Reference Case GHG Projection",width = 20)),linetype=1,size=1.25)+
  
  #facet_grid( ~ prov)+
  scale_x_continuous(breaks=pretty_breaks())+
  #scale_color_viridis("",discrete=TRUE,guide_legend(NULL),option="E")+
  #scale_fill_viridis("",discrete=TRUE,option="E")+
  #scale_fill_manual("",values = my_palette,guide = "legend")+
  scale_fill_viridis("",discrete=TRUE,option="B")+
  #scale_fill_grey("",guide = "legend",start = 0.9,end=0.05)+
  scale_colour_manual("",values=c("black","dodgerblue"),guide = "legend")+
  #guides(fill=guide_legend(nrow =1,byrow=FALSE),color=guide_legend(nrow =1,byrow=FALSE))+
  theme_minimal()+theme(
    legend.position = "right",
    legend.margin=margin(c(.05,0,.05,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
    plot.caption = element_text(size = 12, face = "italic"),
    plot.title = element_text(size=16,face = "bold"),
    plot.subtitle = element_text(size = 14),
    panel.grid.minor = element_blank(),
    text = element_text(size = 20,face = "bold"),
    axis.text.y = element_text(size = 12,face = "bold", colour="black"),
    #axis.text.x = element_blank(),
    axis.text.x = element_text(size = 10, colour = "black", angle = 90,hjust = -1),
    strip.text.x = element_text(size = 12, colour = "black", angle = 0),
    axis.title.y = element_text(size = 14,face = "bold", colour="black"),
  )+
  labs(x=NULL,y=expression('Annual Emissions  '*'(MtCO'[2]*'e)'),
       title="Territories' 2005-2030 GHG Emissions",
       subtitle="Inventory GHG Emissions (2005-2017) and Additional Measures Case Projections (2018-2030)",
       caption="Source: Environment Canada National Inventory Data and Reference Case Projections, graph by @andrew_leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("mb_iso_measures.png")
ggplot(data=filter(proj_data,prov=="MB",emissions>=0 & scenario%in% c("NIR 2018", "2018 Additional Measures Scenario") & prov !="Canada"))+
  geom_area(aes(year,emissions,fill=sector),color="black",position = "stack",size=0.1,alpha=.4)+
  geom_line(aes(year,net_30_2005,colour=str_wrap("30% below 2005 provincial GHGs",width = 20)),linetype=1,size=1.25)+
  geom_line(data=filter(proj_data,prov=="MB",emissions>=0 & scenario%in% c("NIR 2018", "2018 Additional Measures Scenario") & prov !="Canada",year>2017),
            aes(year,ref_case_GHGs,colour=str_wrap("Provincial Reference Case GHG Projection",width = 20)),linetype=1,size=1.25)+
  
  #facet_grid( ~ prov)+
  scale_x_continuous(breaks=pretty_breaks())+
  #scale_color_viridis("",discrete=TRUE,guide_legend(NULL),option="E")+
  #scale_fill_viridis("",discrete=TRUE,option="E")+
  #scale_fill_manual("",values = my_palette,guide = "legend")+
  scale_fill_viridis("",discrete=TRUE,option="B")+
  #scale_fill_grey("",guide = "legend",start = 0.9,end=0.05)+
  scale_colour_manual("",values=c("black","dodgerblue"),guide = "legend")+
  #guides(fill=guide_legend(nrow =1,byrow=FALSE),color=guide_legend(nrow =1,byrow=FALSE))+
  theme_minimal()+theme(
    legend.position = "right",
    legend.margin=margin(c(.05,0,.05,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
    plot.caption = element_text(size = 12, face = "italic"),
    plot.title = element_text(size=16,face = "bold"),
    plot.subtitle = element_text(size = 14),
    panel.grid.minor = element_blank(),
    text = element_text(size = 20,face = "bold"),
    axis.text.y = element_text(size = 12,face = "bold", colour="black"),
    #axis.text.x = element_blank(),
    axis.text.x = element_text(size = 10, colour = "black", angle = 90,hjust = -1),
    strip.text.x = element_text(size = 12, colour = "black", angle = 0),
    axis.title.y = element_text(size = 14,face = "bold", colour="black"),
  )+
  labs(x=NULL,y=expression('Annual Emissions  '*'(MtCO'[2]*'e)'),
       title="Manitoba 2005-2030 GHG Emissions",
       subtitle="Inventory GHG Emissions (2005-2017) and Additional Measures Case Projections (2018-2030)",
       caption="Source: Environment Canada National Inventory Data and Reference Case Projections, graph by @andrew_leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("ab_iso_measures.png")
ggplot(data=filter(proj_data,prov=="AB",emissions>=0 & scenario%in% c("NIR 2018", "2018 Additional Measures Scenario") & prov !="Canada"))+
  geom_area(aes(year,emissions,fill=sector),color="black",position = "stack",size=0.1,alpha=.4)+
  geom_line(aes(year,net_30_2005,colour=str_wrap("30% below 2005 provincial GHGs",width = 20)),linetype=1,size=1.25)+
  geom_line(data=filter(proj_data,prov=="AB",emissions>=0 & scenario%in% c("NIR 2018", "2018 Additional Measures Scenario") & prov !="Canada",year>2017),
            aes(year,ref_case_GHGs,colour=str_wrap("Provincial Reference Case GHG Projection",width = 20)),linetype=1,size=1.25)+
  
  #facet_grid( ~ prov)+
  scale_x_continuous(breaks=pretty_breaks())+
  #scale_color_viridis("",discrete=TRUE,guide_legend(NULL),option="E")+
  #scale_fill_viridis("",discrete=TRUE,option="E")+
  #scale_fill_manual("",values = my_palette,guide = "legend")+
  scale_fill_viridis("",discrete=TRUE,option="B")+
  #scale_fill_grey("",guide = "legend",start = 0.9,end=0.05)+
  scale_colour_manual("",values=c("black","dodgerblue"),guide = "legend")+
  #guides(fill=guide_legend(nrow =1,byrow=FALSE),color=guide_legend(nrow =1,byrow=FALSE))+
  theme_minimal()+theme(
    legend.position = "right",
    legend.margin=margin(c(.05,0,.05,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
    plot.caption = element_text(size = 12, face = "italic"),
    plot.title = element_text(size=16,face = "bold"),
    plot.subtitle = element_text(size = 14),
    panel.grid.minor = element_blank(),
    text = element_text(size = 20,face = "bold"),
    axis.text.y = element_text(size = 12,face = "bold", colour="black"),
    #axis.text.x = element_blank(),
    axis.text.x = element_text(size = 10, colour = "black", angle = 90,hjust = -1),
    strip.text.x = element_text(size = 12, colour = "black", angle = 0),
    axis.title.y = element_text(size = 14,face = "bold", colour="black"),
  )+
  labs(x=NULL,y=expression('Annual Emissions  '*'(MtCO'[2]*'e)'),
       title="Alberta 2005-2030 GHG Emissions",
       subtitle="Inventory GHG Emissions (2005-2017) and Additional Measures Case Projections (2018-2030)",
       caption="Source: Environment Canada National Inventory Data and Reference Case Projections, graph by @andrew_leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("bc_iso_measures.png")
ggplot(data=filter(proj_data,prov=="BC",emissions>=0 & scenario%in% c("NIR 2018", "2018 Additional Measures Scenario") & prov !="Canada"))+
  geom_area(aes(year,emissions,fill=sector),color="black",position = "stack",size=0.1,alpha=.4)+
  geom_line(aes(year,net_30_2005,colour=str_wrap("30% below 2005 provincial GHGs",width = 20)),linetype=1,size=1.25)+
  geom_line(data=filter(proj_data,prov=="BC",emissions>=0 & scenario%in% c("NIR 2018", "2018 Additional Measures Scenario") & prov !="Canada",year>2017),
            aes(year,ref_case_GHGs,colour=str_wrap("Provincial Reference Case GHG Projection",width = 20)),linetype=1,size=1.25)+
  
  #facet_grid( ~ prov)+
  scale_x_continuous(breaks=pretty_breaks())+
  #scale_color_viridis("",discrete=TRUE,guide_legend(NULL),option="E")+
  #scale_fill_viridis("",discrete=TRUE,option="E")+
  #scale_fill_manual("",values = my_palette,guide = "legend")+
  scale_fill_viridis("",discrete=TRUE,option="B")+
  #scale_fill_grey("",guide = "legend",start = 0.9,end=0.05)+
  scale_colour_manual("",values=c("black","dodgerblue"),guide = "legend")+
  #guides(fill=guide_legend(nrow =1,byrow=FALSE),color=guide_legend(nrow =1,byrow=FALSE))+
  theme_minimal()+theme(
    legend.position = "right",
    legend.margin=margin(c(.05,0,.05,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
    plot.caption = element_text(size = 12, face = "italic"),
    plot.title = element_text(size=16,face = "bold"),
    plot.subtitle = element_text(size = 14),
    panel.grid.minor = element_blank(),
    text = element_text(size = 20,face = "bold"),
    axis.text.y = element_text(size = 12,face = "bold", colour="black"),
    #axis.text.x = element_blank(),
    axis.text.x = element_text(size = 10, colour = "black", angle = 90,hjust = -1),
    strip.text.x = element_text(size = 12, colour = "black", angle = 0),
    axis.title.y = element_text(size = 14,face = "bold", colour="black"),
  )+
  labs(x=NULL,y=expression('Annual Emissions  '*'(MtCO'[2]*'e)'),
       title="Manitoba 2005-2030 GHG Emissions",
       subtitle="Inventory GHG Emissions (2005-2017) and Additional Measures Case Projections (2018-2030)",
       caption="Source: Environment Canada National Inventory Data and Reference Case Projections, graph by @andrew_leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()

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
#cdn_data$rio[(cdn_data$year==2000)]<-603.22

cdn_data <- cdn_data %>% mutate(scenario=as_factor(scenario),
                                scenario=fct_relevel(scenario,"2020 Reference Case",after = 0),
                                scenario=fct_relevel(scenario,"2019 Reference Case",after = 0),
                                scenario=fct_relevel(scenario,"2018 Reference Case",after = 0),
                                scenario=fct_relevel(scenario,"2017 Reference Case",after = 0),
                                scenario=fct_relevel(scenario,"2016 Reference Case",after = 0),
                                scenario=fct_relevel(scenario,"NIR 2020",after = 0),
                                scenario=fct_recode(scenario,"National Inventory Emissions (1990-2018)"="NIR 2020"))

#ggplot(canadian_data)+geom_line(aes(year,emissions,group=scenario,color=scenario))


palette<-c("Black",colors_tableau10()[1:4],"dodgerblue",colors_tableau10()[5:10])

ggplot(filter(cdn_data,!grepl('Additional', scenario)),aes(x=year))+
  geom_line(aes(year,emissions,color=scenario),size=2)+
  geom_line(data=filter(cdn_data,grepl('National Inventory', scenario)),aes(year,emissions),color=palette[1],size=2)+
  geom_point(data=targets,aes(Year,target),size=5,colour=palette[10])+
  geom_point(aes(2000,603.22),size=5,colour=palette[10])+ #rio target
  scale_color_manual("",values=palette)+
  annotate("text",x=1990+(2019-1990)/2,y=790,label="National Inventory Emissions (1990-2019)",
           color=palette[1],fontface="bold",hjust=0.5)+
  geom_text(data=cdn_data%>%filter(year==2030,!grepl('Additional', scenario)),
           aes(x=2036,y=emissions+(scenario=="2020 Reference Case")*-30+(scenario=="2017 Reference Case")*10
                 ,label=scenario,color=scenario))+
  
  geom_point(aes(2010,565),size=5,colour=palette[10])+ #rio target
  #geom_line(aes(y=kyoto),size=5,colour=palette[10],linetype=1)+
  annotate("text",x=2009,y=565,label="Kyoto Target (6% below 1990 levels, 2008-2012)",
           colour=palette[10],fontface="bold",hjust=1)+
  #geom_line(data=filter(cdn_data,sector=="Oil Sands"),aes(year,GHGs,group=sector),colour=palette[5],size=2)+
  #annotate("text",x=2005,y=110,label="Third Biennial Report (2018)\nOil Sands Projection",
  #         colour=palette[5],fontface="bold",hjust=1)+
  annotate("text",x=2001,y=603.22,label="Rio Target (return to 1990 levels by 2000)",
           colour=palette[10],fontface="bold",hjust=0)+
  
  annotate("text",x=2021,y=607.32,label="Copenhagen Target (17% below 2005 levels by 2020)",
           colour=palette[10],fontface="bold",hjust=0)+
  annotate("text",x=2029,y=512.9,label="Paris Target (30% below 2005 levels by 2030)",
       colour=palette[10],fontface="bold",hjust=1)+
  geom_point(aes(2030,423),size=5,colour=palette[10])+ #glasgow target
  annotate("text",x=2029,y=423,label="Glasgow Target (40-45% below 2005 levels by 2030)",
           colour=palette[10],fontface="bold",hjust=1)+
  annotate("text",x=2049,y=30,label="2050 Net Zero Goal",
           colour=palette[10],fontface="bold",hjust=1)+
  geom_hline(yintercept=0,size=1)+
  #geom_text_repel(data=filter(cdn_data,sector=="Total, Canada",year %in% c(2020,2030,2050)),aes(y=target,label=paste("   ",year,"Target   ")),colour=palette[5],
  #                fontface="bold",point.padding = unit(2,"mm"),segment.alpha = 0)+
  scale_y_continuous(limit=c(0,825))+
  scale_x_continuous(limit=c(1990,2052),breaks=seq(1990,2050,10),expand = c(0,0))+
  tombe_theme()+theme(legend.position = "none")+
  labs(x="",y=expression('Annual Emissions '*'(MtCO'[2]*'e)'),
       title="Canada's GHG Emissions, Projections and Future Targets",
       subtitle="Source: Environment and Climate Change Canada Emissions Inventory (2020) and Emissions Projections (2016-2020).")
ggsave("emissions_and_targets.png",dpi=300,width=14,height=7)



ggplot(filter(cdn_data,!grepl('Additional', scenario))%>%filter(scenario!="2019 Reference Case",
                                                                scenario!="2018 Reference Case",
                                                                scenario!="2017 Reference Case",
                                                                scenario!="2016 Reference Case",
),aes(x=year))+
  geom_line(aes(year,emissions,lty=scenario),color="black",size=2)+
  geom_line(data=filter(cdn_data,grepl('National Inventory', scenario)),aes(year,emissions),color=palette[1],size=2)+
  geom_point(data=targets,aes(Year,target),size=5,colour=palette[10])+
  geom_point(aes(2000,603.22),size=5,colour=palette[10])+ #rio target
  scale_color_manual("",values=palette)+
  annotate("text",x=1990+(2019-1990)/2,y=790,label="National Inventory Emissions (1990-2019)",
           color=palette[1],fontface="bold",hjust=0.5)+
  #geom_text(data=cdn_data%>%filter(year==2030,!grepl('Additional', scenario)),
  #         aes(x=2036,y=emissions+(scenario=="2020 Reference Case")*-30+(scenario=="2017 Reference Case")*10
  #               ,label=scenario,color=scenario))+
  
  annotate("text",x=2031,y=675,label="2020 ECCC Reference Case Projection (2020-2030)",color="black",fontface="bold",hjust=0)+
  
  
  geom_point(aes(2010,565),size=5,colour=palette[10])+ #rio target
  #geom_line(aes(y=kyoto),size=5,colour=palette[10],linetype=1)+
  annotate("text",x=2009,y=565,label="Kyoto Target (6% below 1990 levels, 2008-2012)",
           colour=palette[10],fontface="bold",hjust=1)+
  #geom_line(data=filter(cdn_data,sector=="Oil Sands"),aes(year,GHGs,group=sector),colour=palette[5],size=2)+
  #annotate("text",x=2005,y=110,label="Third Biennial Report (2018)\nOil Sands Projection",
  #         colour=palette[5],fontface="bold",hjust=1)+
  annotate("text",x=2001,y=603.22,label="Rio Target (return to 1990 levels by 2000)",
           colour=palette[10],fontface="bold",hjust=0)+
  
  annotate("text",x=2021,y=607.32,label="Copenhagen Target (17% below 2005 levels by 2020)",
           colour=palette[10],fontface="bold",hjust=0)+
  annotate("text",x=2029,y=512.9,label="Paris Target (30% below 2005 levels by 2030)",
           colour=palette[10],fontface="bold",hjust=1)+
  geom_point(aes(2030,423),size=5,colour=palette[10])+ #glasgow target
  annotate("text",x=2029,y=423,label="Glasgow Target (40-45% below 2005 levels by 2030)",
           colour=palette[10],fontface="bold",hjust=1)+
  annotate("text",x=2049,y=30,label="2050 Net Zero Goal",
           colour=palette[10],fontface="bold",hjust=1)+
  geom_hline(yintercept=0,size=1)+
  #geom_text_repel(data=filter(cdn_data,sector=="Total, Canada",year %in% c(2020,2030,2050)),aes(y=target,label=paste("   ",year,"Target   ")),colour=palette[5],
  #                fontface="bold",point.padding = unit(2,"mm"),segment.alpha = 0)+
  scale_y_continuous(limit=c(0,825))+
  scale_x_continuous(limit=c(1990,2052),breaks=seq(1990,2050,10),expand = c(0,0))+
  tombe_theme()+theme(legend.position = "none")+
  labs(x="",y=expression('Annual Emissions '*'(MtCO'[2]*'e)'),
       title="Canada's GHG Emissions, Projections and Future Targets",
       subtitle="Source: Environment and Climate Change Canada Emissions Inventory and Projections (2020).")

ggsave("emissions_and_targets_simple.png",dpi=300,width=13,height=6)


#Grab US emissions and projections
#power gen 25-10-0028-01
#get gdp data
get_power_data<-function(){
  #Cansim 36-10-0402-02
  canada_power_data<-get_cansim("25-10-0028-01")%>% filter(Source=="Total industry and utilities")%>%
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
           Code=fct_collapse(Code,
                             "TERR" = c("NT", "NU","YT","NT & NU"),
                             "ATL" = c("NL", "NB","NS","PE")))%>%
    select(Year=REF_DATE,Code,fuel="Fuel type",gen=VALUE,UOM)%>% group_by(Year,Code,fuel)%>%summarize(gen=sum(gen,na.rm=T))%>%
    ungroup()%>%na.omit()
  
  save(file="canada_power_data.Rdata",canada_power_data)
}


get_power_data()
load(file = "canada_power_data.Rdata")









ggplot(filter(cdn_data,!grepl('Additional', scenario)),aes(x=year))+
  geom_line(aes(year,emissions,color=scenario),size=2)+
  geom_line(data=filter(cdn_data,grepl('National Inventory', scenario)),aes(year,emissions),color=palette[1],size=2)+
  #add oil and gas emissions
  geom_line(data=filter(NIR_natl,sector=="Oil and Gas"),aes(Year,GHGs),color=palette[1],size=2)+
  geom_line(data=filter(proj_data,sector=="Oil and Gas",scenario=="2020 Reference Case",prov=="Canada"),aes(year,emissions),color=palette[1],size=2,linetype="dotted")+
  geom_point(data=targets,aes(Year,target),size=5,colour=palette[10])+
  geom_point(aes(2000,603.22),size=5,colour=palette[10])+ #rio target
  scale_color_manual("",values=palette)+
  annotate("text",x=1990+(2019-1990)/2,y=790,label="National Inventory Emissions (1990-2019)",
           color=palette[1],fontface="bold",hjust=0.5)+
  annotate("text",x=1990+(2030-1990)/2,y=300,label="Inventory (1990-2019) and projected (2020-2030) emissions from oil and gas",
           color=palette[1],fontface="bold",hjust=0.5)+
  
  geom_text(data=cdn_data%>%filter(year==2030,!grepl('Additional', scenario)),
            aes(x=2036,y=emissions+(scenario=="2020 Reference Case")*-30+(scenario=="2017 Reference Case")*10
                ,label=scenario,color=scenario))+
  
  
  geom_line(aes(y=kyoto),size=5,colour=palette[10],linetype=1)+
  annotate("text",x=2007,y=568,label="Kyoto Target (6% below 1990 levels)",
           colour=palette[10],fontface="bold",hjust=1)+
  #geom_line(data=filter(cdn_data,sector=="Oil Sands"),aes(year,GHGs,group=sector),colour=palette[5],size=2)+
  #annotate("text",x=2005,y=110,label="Third Biennial Report (2018)\nOil Sands Projection",
  #         colour=palette[5],fontface="bold",hjust=1)+
  annotate("text",x=2001,y=603.22,label="Rio Target (return to 1990 levels by 2000)",
           colour=palette[10],fontface="bold",hjust=0)+
  
  annotate("text",x=2021,y=607.32,label="2020 Target (17% below 2005 levels)",
           colour=palette[10],fontface="bold",hjust=0)+
  annotate("text",x=2029,y=512.9,label="2030 Target (30% below 2005 levels)",
           colour=palette[10],fontface="bold",hjust=1)+
  annotate("text",x=2049,y=30,label="2050 Net Zero Goal",
           colour=palette[10],fontface="bold",hjust=1)+
  geom_hline(yintercept=0,size=1)+
  #geom_text_repel(data=filter(cdn_data,sector=="Total, Canada",year %in% c(2020,2030,2050)),aes(y=target,label=paste("   ",year,"Target   ")),colour=palette[5],
  #                fontface="bold",point.padding = unit(2,"mm"),segment.alpha = 0)+
  scale_y_continuous(limit=c(0,825))+
  scale_x_continuous(limit=c(1990,2052),breaks=seq(1990,2050,10),expand = c(0,0))+
  tombe_theme()+theme(legend.position = "none")+
  labs(x="",y=expression('Annual Emissions '*'(MtCO'[2]*'e)'),
       title="Canada's GHG Emissions, Projections and Future Targets",
       subtitle="Source: Environment and Climate Change Canada Emissions Inventory (2020) and Emissions Projections (2016-2020).")

ggsave("emissions_and_targets_oil_sands.png",dpi=300,width=13,height=6)













png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("oil_sands_emissions_and_targets.png",width = 2800,height = 1500)
ggplot(cdn_data,aes(x=year))+
  #geom_line(data=filter(cdn_data,sector=="Total, Canada",Year<=2017),aes(Year,GHGs,group=sector),color=palette[1],size=2)+
  #geom_line(data=target_data,aes(Year,value,color=projection,group=projection))+
  annotate("text",x=1997,y=790,label="Historical Emissions (1990-2017)",
           color=palette[1],fontface="bold",hjust=0)+
  geom_line(data=filter(cdn_data,sector=="Total, Canada",Year>=2014,Year!=2020),aes(Year,target),size=2,color=palette[6],linetype=2)+
  geom_point(data=filter(cdn_data,Year>2017),aes(y=target),size=5,colour=palette[6])+
  
  geom_line(data=filter(cdn_data,sector=="Total, Canada",Year>=2014),aes(Year,Second_Biennial),size=2,color=palette[2],linetype=3)+
  annotate("text",x=2031,y=815,label="Second Biennial Report Projection (2016)",
           color=palette[2],fontface="bold",hjust=0)+
  
  geom_line(data=filter(cdn_data,sector=="Total, Canada",Year>=2015),aes(Year,`2017_Ref`),size=2,color=palette[3],linetype=3)+
  annotate("text",x=2031,y=730,label="2017 Reference Case",
           colour=palette[3],fontface="bold",hjust=0)+
  geom_line(data=filter(cdn_data,sector=="Total, Canada",Year>=2017),aes(Year,`2018_Ref`),size=2,color=palette[4],linetype=3)+
  annotate("text",x=2031,y=700,label="2018 Reference Case",
           colour=palette[4],fontface="bold",hjust=0)+
  geom_line(data=filter(cdn_data,sector=="Total, Canada",Year>=2017),aes(Year,`Adds_Case`),size=2,color=palette[5],linetype=3)+
  annotate("text",x=2031,y=592,label="Pan Canadian Framework Additional Measures\nThird Biennial Report (2018)",
           colour=palette[5],fontface="bold",hjust=0)+
    #geom_point(aes(y=projection),size=3,colour=palette[3])+
  #geom_line(aes(y=projection),size=2,colour=palette[3],linetype=3)+
  
  geom_line(aes(y=kyoto),size=2,colour=palette[6],linetype=1)+
  annotate("text",x=2007,y=565,label="Kyoto Target (6% below 1990 levels)",
           colour=palette[6],fontface="bold",hjust=1)+
  
  #geom_point(aes(y=bau),size=3,colour=palette[2])+
  #geom_line(aes(y=bau),size=2,colour=palette[2],linetype=3)+

  geom_line(data=filter(cdn_data,sector=="Oil Sands"),aes(Year,GHGs,group=sector),colour=palette[5],size=2)+
  annotate("text",x=2005,y=110,label="Third Biennial Report (2018)\nOil Sands Projection",
           colour=palette[5],fontface="bold",hjust=1)+
  annotate("text",x=2019,y=605,label="2020 Target (17% below 2005 levels)",
           colour=palette[6],fontface="bold",hjust=1)+
  annotate("text",x=2029,y=513,label="2030 Target (30% below 2005 levels)",
           colour=palette[6],fontface="bold",hjust=1)+
  annotate("text",x=2049,y=146,label="2050 Goal (80% below 2005 levels)",
           colour=palette[6],fontface="bold",hjust=1)+
  geom_hline(yintercept=0,size=1)+
  #geom_text_repel(data=filter(cdn_data,sector=="Total, Canada",Year %in% c(2020,2030,2050)),aes(y=target,label=paste("   ",Year,"Target   ")),colour=palette[5],
  #                fontface="bold",point.padding = unit(2,"mm"),segment.alpha = 0)+
  scale_y_continuous(limit=c(0,825))+
  scale_x_continuous(limit=c(1990,2052),breaks=seq(1990,2050,10),expand = c(0,0))+
  #tombe_theme()+
  labs(x="",y=expression('Annual Emissions '*'(MtCO'[2]*'e)'),
       title="Canada's GHG Emissions, Projections and Future Targets",
       subtitle="Source: ECCC 2017 Preliminary Emissions Inventory (2019); Second and Third Biennial Reports to the UN (2016,2018), and 2019 PCF Progress Update.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("emissions_and_targets.png")
ggplot(cdn_data,aes(x=Year))+
  geom_line(data=filter(cdn_data,sector=="Total, Canada",Year<=2017),aes(Year,GHGs,group=sector),color=palette[1],size=2)+
  
  annotate("text",x=1997,y=790,label="Historical Emissions (1990-2017)",
           color=palette[1],fontface="bold",hjust=0)+
  geom_line(data=filter(cdn_data,sector=="Total, Canada",Year>=2014,Year!=2020),aes(Year,target),size=2,color=palette[6],linetype=2)+
  geom_point(data=filter(cdn_data,Year>2017),aes(y=target),size=5,colour=palette[6])+
  
  geom_point(data=filter(target_data,Year==2030, projection=="Second Biennial Report Reference Case"),aes(Year,value,group=projection),colour=palette[2],size=5)+
  geom_line(data=filter(target_data,Year>=2013, projection=="Second Biennial Report Reference Case"),aes(Year,value,group=projection),colour=palette[2],size=2,linetype=3)+
  #geom_line(data=filter(cdn_data,sector=="Total, Canada",Year>=2014),aes(Year,Second_Biennial),size=2,color=palette[2],linetype=3)+
  annotate("text",x=2031,y=815,label="Second Biennial Report to the UN\nReference Case (2016)",
           color=palette[2],fontface="bold",hjust=0)+
  
  
  geom_point(data=filter(target_data,Year==2030, projection=="2017 Reference Case"),aes(Year,value,group=projection),colour=palette[10],size=5)+
  geom_line(data=filter(target_data,Year>=2015, projection=="2017 Reference Case"),aes(Year,value,group=projection),colour=palette[10],size=2,linetype=3)+
  #geom_line(data=filter(cdn_data,sector=="Total, Canada",Year>=2015),aes(Year,`2017_Ref`),size=2,color=palette[3],linetype=3)+
  annotate("text",x=2031,y=730,label="2017 Reference Case",
           colour=palette[10],fontface="bold",hjust=0)+
  
  geom_point(data=filter(target_data,Year==2030, projection=="2018 Reference Case"),aes(Year,value,group=projection),colour=palette[8],size=5)+
  geom_line(data=filter(target_data,Year>=2017, projection=="2018 Reference Case"),aes(Year,value,group=projection),colour=palette[8],size=2,linetype=3)+
  #geom_line(data=filter(cdn_data,sector=="Total, Canada",Year>=2017),aes(Year,`2018_Ref`),size=2,color=palette[4],linetype=3)+
  annotate("text",x=2031,y=700,label="2018 Reference Case",
           colour=palette[8],fontface="bold",hjust=0)+
  
  geom_point(data=filter(target_data,Year==2030, projection=="2018 Additional Measures"),aes(Year,value,group=projection),colour=palette[5],size=5)+
  geom_line(data=filter(target_data,Year>=2017, projection=="2018 Additional Measures"),aes(Year,value,group=projection),colour=palette[5],size=2,linetype=3)+
  #geom_line(data=filter(cdn_data,sector=="Total, Canada",Year>=2017),aes(Year,`Adds_Case`),size=2,color=palette[5],linetype=3)+
  annotate("text",x=2031,y=592,label="2018 'Additional Measures' Case\nClosest we have to LPC Climate Plan",
           colour=palette[5],fontface="bold",hjust=0)+
  #geom_point(aes(y=projection),size=3,colour=palette[3])+
  #geom_line(aes(y=projection),size=2,colour=palette[3],linetype=3)+
  
  geom_line(aes(y=kyoto),size=2,colour=palette[6],linetype=1)+
  annotate("text",x=2007,y=565,label="Kyoto Target (6% below 1990 levels)",
           colour=palette[6],fontface="bold",hjust=1)+
  #NDP Targets
  geom_point(aes(x=2030,y=450),size=5,colour=palette[3])+
  annotate("text",x=2029,y=450,label="NDP Climate Plan Estimated\n2030 Emissions (450 MT)",
           colour=palette[3],fontface="bold",hjust=1)+
  #geom_line(aes(y=bau),size=2,colour=palette[2],linetype=3)+
  geom_point(aes(x=2030,y=370),size=5,colour=palette[3])+
  annotate("text",x=2029,y=370,label="Estimate of NDP Climate Plan Commitment\nto Additional Unspecified Measures (370 MT)",
           colour=palette[3],fontface="bold",hjust=1)+
  
  #Green Targets
  geom_point(aes(x=2030,y=277),size=5,colour=palette[4])+
  annotate("text",x=2029,y=277,label="Green Party Mission Possible\n2030 Emissions Target (278 MT)",
           colour=palette[4],fontface="bold",hjust=1)+
  #geom_line(aes(y=bau),size=2,colour=palette[2],linetype=3)+
  geom_point(aes(x=2050,y=0),size=5,colour=palette[4])+
  annotate("text",x=2049,y=0,label="\nGreen Party Mission Possible 2050 Emissions Target (0 MT)",
   colour=palette[4],fontface="bold",hjust=1)+
           
  
  #LPC
  geom_point(aes(x=2030,y=513),size=5,colour=palette[5])+
  annotate("text",x=2031,y=513,label="LPC committed to 2030 Paris Target",
           colour=palette[5],fontface="bold",hjust=0)+
  
  geom_point(aes(x=2050,y=0),size=3,colour=palette[5])+
  annotate("text",x=2049,y=0,label="LPC committed to Net Zero Emissions by 2050\n",
           colour=palette[5],fontface="bold",hjust=1)+
  
  #geom_point(aes(x=2030,y=513),size=3,colour="dark blue")+
  #annotate("text",x=2031,y=513,label="       and CPC(?)",
  #         colour="dark blue",fontface="bold",hjust=0)+
  
  #geom_line(data=filter(cdn_data,sector=="Oil Sands"),aes(Year,GHGs,group=sector),colour=palette[5],size=2)+
  #annotate("text",x=2005,y=110,label="Third Biennial Report (2018)\nOil Sands Projection",
  #         colour=palette[5],fontface="bold",hjust=1)+
  annotate("text",x=2019,y=605,label="2020 Target (17% below 2005 levels)",
           colour=palette[6],fontface="bold",hjust=1)+
  annotate("text",x=2029,y=511,label="2030 Target (30% below 2005 levels)",
           colour=palette[6],fontface="bold",hjust=1)+
  annotate("text",x=2049,y=146,label="2050 Goal (80% below 2005 levels)",
           colour=palette[6],fontface="bold",hjust=1)+
  geom_hline(yintercept=0,size=1)+
  #geom_text_repel(data=filter(cdn_data,sector=="Total, Canada",Year %in% c(2020,2030,2050)),aes(y=target,label=paste("   ",Year,"Target   ")),colour=palette[5],
  #                fontface="bold",point.padding = unit(2,"mm"),segment.alpha = 0)+
  scale_y_continuous(limit=c(0,825))+
  scale_x_continuous(limit=c(1990,2052),breaks=seq(1990,2050,10),expand = c(0,0))+
  tombe_theme()+
  labs(x="",y=expression('Annual Emissions '*'(MtCO'[2]*'e)'),
       title="Canada's GHG Emissions, Projections and Future Targets",
       subtitle="Source: ECCC 2017 Emissions Inventory (2019), ECCC 2018 Emissions Projections Data, and NDP and Green Party Climate Plans.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()

test<-cdn_data%>%left_join(proj_data %>% ungroup()%>%
                       filter(prov=="Canada")%>%
                       mutate(total=gsub("Total","Total, Canada",sector))%>%
                       select(sector,scenario,emissions,year)%>%
                       pivot_wider(names_from=scenario,values_from=emissions)
                       ,by=c("sector","Year"="year"))


png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("emissions_and_targets_plain.png")
ggplot(cdn_data,aes(x=Year))+
  geom_line(data=filter(cdn_data,sector=="Total, Canada",Year<=2017),aes(Year,GHGs,group=sector),color=palette[1],size=2)+
  
  annotate("text",x=1997,y=790,label="Historical Emissions (1990-2017)",
           color=palette[1],fontface="bold",hjust=0)+
  geom_line(data=filter(cdn_data,sector=="Total, Canada",Year>=2014,Year!=2020),aes(Year,target),size=2,color=palette[6],linetype=2)+
  geom_point(data=filter(cdn_data,Year>2017),aes(y=target),size=5,colour=palette[6])+
  
  geom_point(data=filter(target_data,Year==2030, projection=="Second Biennial Report Reference Case"),aes(Year,value,group=projection),colour=palette[2],size=5)+
  geom_line(data=filter(target_data,Year>=2013, projection=="Second Biennial Report Reference Case"),aes(Year,value,group=projection),colour=palette[2],size=2,linetype=3)+
  #geom_line(data=filter(cdn_data,sector=="Total, Canada",Year>=2014),aes(Year,Second_Biennial),size=2,color=palette[2],linetype=3)+
  annotate("text",x=2031,y=815,label="Second Biennial Report to the UN\nReference Case (2016)",
           color=palette[2],fontface="bold",hjust=0)+
  
  
  geom_point(data=filter(target_data,Year==2030, projection=="2017 Reference Case"),aes(Year,value,group=projection),colour=palette[10],size=5)+
  geom_line(data=filter(target_data,Year>=2015, projection=="2017 Reference Case"),aes(Year,value,group=projection),colour=palette[10],size=2,linetype=3)+
  #geom_line(data=filter(cdn_data,sector=="Total, Canada",Year>=2015),aes(Year,`2017_Ref`),size=2,color=palette[3],linetype=3)+
  annotate("text",x=2031,y=730,label="2017 Reference Case",
           colour=palette[10],fontface="bold",hjust=0)+
  
  geom_point(data=filter(target_data,Year==2030, projection=="2018 Reference Case"),aes(Year,value,group=projection),colour=palette[8],size=5)+
  geom_line(data=filter(target_data,Year>=2017, projection=="2018 Reference Case"),aes(Year,value,group=projection),colour=palette[8],size=2,linetype=3)+
  #geom_line(data=filter(cdn_data,sector=="Total, Canada",Year>=2017),aes(Year,`2018_Ref`),size=2,color=palette[4],linetype=3)+
  annotate("text",x=2031,y=700,label="2018 Reference Case",
           colour=palette[8],fontface="bold",hjust=0)+
  
  geom_point(data=filter(target_data,Year==2030, projection=="2018 Additional Measures"),aes(Year,value,group=projection),colour=palette[5],size=5)+
  geom_line(data=filter(target_data,Year>=2017, projection=="2018 Additional Measures"),aes(Year,value,group=projection),colour=palette[5],size=2,linetype=3)+
  #geom_line(data=filter(cdn_data,sector=="Total, Canada",Year>=2017),aes(Year,`Adds_Case`),size=2,color=palette[5],linetype=3)+
  annotate("text",x=2031,y=592,label="2018 'Additional Measures' Case\nCurrent Policies Plus Unannounced Measures",
           colour=palette[5],fontface="bold",hjust=0)+
  #geom_point(aes(y=projection),size=3,colour=palette[3])+
  #geom_line(aes(y=projection),size=2,colour=palette[3],linetype=3)+
  
  geom_line(aes(y=kyoto),size=2,colour=palette[6],linetype=1)+
  annotate("text",x=2007,y=565,label="Kyoto Target (6% below 1990 levels)",
           colour=palette[6],fontface="bold",hjust=1)+
  #geom_line(data=filter(cdn_data,sector=="Oil Sands"),aes(Year,GHGs,group=sector),colour=palette[5],size=2)+
  #annotate("text",x=2005,y=110,label="Third Biennial Report (2018)\nOil Sands Projection",
  #         colour=palette[5],fontface="bold",hjust=1)+
  annotate("text",x=2019,y=605,label="2020 Target (17% below 2005 levels)",
           colour=palette[6],fontface="bold",hjust=1)+
  annotate("text",x=2029,y=511,label="2030 Target (30% below 2005 levels)",
           colour=palette[6],fontface="bold",hjust=1)+
  annotate("text",x=2039,y=30,label="2050 Net Zero Goal",
           colour=palette[6],fontface="bold",hjust=0)+
  geom_hline(yintercept=0,size=1)+
  #geom_text_repel(data=filter(cdn_data,sector=="Total, Canada",Year %in% c(2020,2030,2050)),aes(y=target,label=paste("   ",Year,"Target   ")),colour=palette[5],
  #                fontface="bold",point.padding = unit(2,"mm"),segment.alpha = 0)+
  scale_y_continuous(limit=c(0,825))+
  scale_x_continuous(limit=c(1990,2052),breaks=seq(1990,2050,10),expand = c(0,0))+
  tombe_theme()+
  labs(x="",y=expression('Annual Emissions '*'(MtCO'[2]*'e)'),
       title="Canada's GHG Emissions, Projections and Future Targets",
       subtitle="Source: ECCC Emissions Inventory (2019), ECCC 2019 Emissions Projections Data.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()





png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("AB_emissions_and_targets.png")
ggplot(cdn_data,aes(x=Year))+
  geom_line(data=filter(cdn_data,sector=="Total, Canada",Year<=2017),aes(Year,GHGs,group=sector),color=palette[1],size=2)+
  annotate("text",x=1997,y=790,label="Historical Emissions (1990-2017)",
           color=palette[1],fontface="bold",hjust=0)+
  geom_line(data=filter(cdn_data,sector=="Total, Canada",Year>=2014,Year!=2020),aes(Year,target),size=2,color=palette[6],linetype=2)+
  geom_point(data=filter(cdn_data,Year>2017),aes(y=target),size=5,colour=palette[6])+
  
  geom_line(data=filter(cdn_data,sector=="Total, Canada",Year>=2014),aes(Year,Second_Biennial),size=2,color=palette[2],linetype=3)+
  annotate("text",x=2031,y=815,label="Second Biennial Report Projection (2016)",
           color=palette[2],fontface="bold",hjust=0)+
  
  geom_line(data=filter(cdn_data,sector=="Total, Canada",Year>=2015),aes(Year,`2017_Ref`),size=2,color=palette[3],linetype=3)+
  annotate("text",x=2031,y=730,label="2017 Reference Case",
           colour=palette[3],fontface="bold",hjust=0)+
  geom_line(data=filter(cdn_data,sector=="Total, Canada",Year>=2017),aes(Year,`2018_Ref`),size=2,color=palette[4],linetype=3)+
  annotate("text",x=2031,y=700,label="2018 Reference Case",
           colour=palette[4],fontface="bold",hjust=0)+
  geom_line(data=filter(cdn_data,sector=="Total, Canada",Year>=2017),aes(Year,`Adds_Case`),size=2,color=palette[5],linetype=3)+
  annotate("text",x=2031,y=592,label="Pan Canadian Framework Additional Measures\nThird Biennial Report (2018)",
           colour=palette[5],fontface="bold",hjust=0)+
  #geom_point(aes(y=projection),size=3,colour=palette[3])+
  #geom_line(aes(y=projection),size=2,colour=palette[3],linetype=3)+
  
  geom_line(aes(y=kyoto),size=2,colour=palette[6],linetype=1)+
  annotate("text",x=2007,y=565,label="Kyoto Target (6% below 1990 levels)",
           colour=palette[6],fontface="bold",hjust=1)+
  
  #geom_point(aes(y=bau),size=3,colour=palette[2])+
  #geom_line(aes(y=bau),size=2,colour=palette[2],linetype=3)+
  
  geom_line(data=filter(cdn_data,sector=="Oil Sands"),aes(Year,GHGs,group=sector),colour=palette[5],size=2)+
  geom_line(data=filter(prov_ghgs,Prov=="AB"),aes(Year,GHGs,group=sector),colour="black",size=2,linetype=2)+
  annotate("text",x=2031,y=100,label="Third Biennial Report (2018)\nOil Sands Projections",
           colour=palette[5],fontface="bold",hjust=0)+
  annotate("text",x=2017,y=250,label="National Inventory Report (2019)\n Alberta Emissions",
           colour="black",fontface="bold",hjust=0)+
  
  annotate("text",x=2019,y=605,label="2020 Target (17% below 2005 levels)",
           colour=palette[6],fontface="bold",hjust=1)+
  annotate("text",x=2029,y=511,label="2030 Target (30% below 2005 levels)",
           colour=palette[6],fontface="bold",hjust=1)+
  annotate("text",x=2049,y=146,label="2050 Goal (80% below 2005 levels)",
           colour=palette[6],fontface="bold",hjust=1)+
  geom_hline(yintercept=0,size=1)+
  #geom_text_repel(data=filter(cdn_data,sector=="Total, Canada",Year %in% c(2020,2030,2050)),aes(y=target,label=paste("   ",Year,"Target   ")),colour=palette[5],
  #                fontface="bold",point.padding = unit(2,"mm"),segment.alpha = 0)+
  scale_y_continuous(limit=c(0,825))+
  scale_x_continuous(limit=c(1990,2052),breaks=seq(1990,2050,10),expand = c(0,0))+
  tombe_theme()+
  labs(x="",y=expression('Annual Emissions '*'(MtCO'[2]*'e)'),
       title="Canada's GHG Emissions, Projections and Future Targets",
       subtitle="Source: ECCC 2017 Preliminary Emissions Inventory (2019); Second and Third Biennial Reports to the UN (2016,2018), and 2019 PCF Progress Update.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()




#NIR but with IPCC Sectors


nir_ipcc<-function() {
  download.file("http://donnees.ec.gc.ca/data/substances/monitor/canada-s-official-greenhouse-gas-inventory/GHG_IPCC_Can_Prov_Terr.csv","canada_ghg_ipcc.csv",mode="wb")
  #temp_nir<-read.csv("canada_ghg_prelimi.csv",stringsAsFactors = F)
  nir_2021<-read.csv("canada_ghg_ipcc.csv",stringsAsFactors = F)%>% filter(Rollup==TRUE) %>% clean_names()%>%
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

sector_graph<-ipcc_nir %>% pivot_wider(names_from = "sector",values_from = GHGs,values_fill=0)%>% #clean_names() %>%
  mutate(other_transport=transport-aviation,
         other_stationary=`stationary combustion sources`-`oil and natural gas`)%>% select(c(year,prov,tolower(ipcc_main_sectors),other_stationary,`oil and natural gas`))%>%
  select(-`stationary combustion sources`)%>%
  pivot_longer(-c(year,prov),names_to = "sector",values_to = "GHGs")%>%
  mutate(sector=factor(str_to_title(gsub("_"," ",(sector)))),
         sector=fct_reorder(sector,-GHGs),
         sector=fct_relevel(sector,"Other Stationary",after = 0),
         sector=fct_relevel(sector,"Oil And Natural Gas",after = 0),
         sector=fct_recode(sector,"Other Stationary Sources"="Other Stationary",
                           "Industrial P&P"="Industrial Processes And Product Use",
                           "Oil And Natural Gas Extraction"="Oil And Natural Gas"
                           ),
         NULL
         )
         


ggplot(filter(sector_graph,GHGs>0 & prov !="Canada",sector!="Total"))+
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
  guides(fill=guide_legend(nrow =1,byrow=FALSE),color=guide_legend(nrow =1,byrow=FALSE))+
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
    axis.text.x = element_text(size = 10, colour = "black", angle = 90,hjust=0.5,vjust=0.5),
    strip.text.x = element_text(size = 8, colour = "black", angle = 0),
    axis.title.y = element_text(size = 14,face = "bold", colour="black"),
  )+
  labs(x=NULL,y=expression('Annual Emissions  '*'(MtCO'[2]*'e)'),
       title="Canadian GHG Emissions by Province (IPCC Sectors)",
       #subtitle=paste("2020 National Inventory (1990-2018)",sep=""),
       caption=str_wrap("Source: Environment and Climate Change Canada 2021 National Inventory (1990-2019). Graph by @andrew_leach.",width = 180),
       NULL
  )
ggsave("images/inventory_ipcc_prov.png",dpi = 300,width=14, height=7)


ggplot(filter(sector_graph,GHGs>0 & prov =="Canada",sector!="Total"))+
  geom_area(aes(year,GHGs/10^3,fill=sector),color="black",position = "stack",size=0.1,alpha=.6)+
  #geom_line(aes(year,net_30_2005,colour=str_wrap("30% below 2005 provincial GHGs",width = 20)),linetype=1,size=1.05)+
  #facet_wrap( ~ prov,nrow = 1)+
  scale_x_continuous(breaks=pretty_breaks())+
  #scale_color_viridis("",discrete=TRUE,guide_legend(NULL),option="E")+
  #scale_fill_viridis("",discrete=TRUE,option="E")+
  scale_fill_viridis("",discrete=TRUE,option="B")+
  #scale_fill_manual("",values = my_palette,guide = "legend")+
  #scale_fill_grey("",guide = "legend",start = 0.9,end=0.05)+
  scale_colour_manual("",values="black",guide = "legend")+
  geom_hline(aes(yintercept=0))+
  guides(fill=guide_legend(nrow =1,byrow=FALSE),color=guide_legend(nrow =1,byrow=FALSE))+
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
    axis.text.x = element_text(size = 10, colour = "black", angle = 90,hjust=0.5,vjust=0.5),
    strip.text.x = element_text(size = 8, colour = "black", angle = 0),
    axis.title.y = element_text(size = 14,face = "bold", colour="black"),
  )+
  labs(x=NULL,y=expression('Annual Emissions  '*'(MtCO'[2]*'e)'),
       title="Canadian GHG Emissions (IPCC Sectors)",
       #subtitle=paste("2020 National Inventory (1990-2018)",sep=""),
       caption=str_wrap("Source: Environment and Climate Change Canada 2021 National Inventory (1990-2019). Graph by @andrew_leach.",width = 180),
       NULL
  )
ggsave("images/inventory_ipcc.png",dpi = 300,width=14, height=7)


