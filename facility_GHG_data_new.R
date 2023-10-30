library(tidyverse)
library(readxl)

#download.file("http://data.ec.gc.ca/data/substances/monitor/greenhouse-gas-reporting-program-ghgrp-facility-greenhouse-gas-ghg-data/PDGES-GHGRP-GHGEmissionsGES-2004-Present.xlsx", destfile="LFE_2004_present.xlsx", mode = "wb")


#load plant data
plant_data <- read_excel("LFE_2004_Present.xlsx", sheet = 3)

names(plant_data) <- do.call(rbind,str_split(names(plant_data),"./."))[,1]
names(plant_data)[grepl("\\(",names(plant_data))& !grepl("\\)",names(plant_data))]<-
  paste(names(plant_data)[grepl("\\(",names(plant_data))& !grepl("\\)",names(plant_data))],")",sep="")
#plant_data <- read.xlsx(xlsxFile = "LFE-GHG-2004-Present.xlsx", sheet = 3, startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)

names(plant_data)[grepl("^Facility.NAICS.Code$",names(plant_data))]<-"NAICS"

plant_data<-plant_data%>%clean_names()%>%
  mutate(naics3=substr(naics,1,3),
         naics4=substr(naics,1,4),
         total_co2e=as.numeric(total_emissions_tonnes_co2e))

naics3_data <- read_excel("lfe_naics.xlsx", sheet = "NAICS3")%>%clean_names()
naics4_data <- read_excel("lfe_naics.xlsx", sheet = "NAICS4")%>%clean_names()

load("data/prov_ghgs.Rdata")
prov_ghgs$Sector<-NULL
prov_ghgs<-prov_ghgs %>% group_by(Year) %>% mutate(national_ghgs=sum(GHGs))

plant_data<-merge(plant_data,naics3_data,by="naics3")
plant_data<-merge(plant_data,naics4_data,by="naics4")
naics3_data<-NULL
naics4_data<-NULL





plant_data<-plant_data %>% mutate(
  prov=as_factor(facility_province_or_territory),
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
                       "YT"="Yukon Territory",
                       "YT"="Yukon",
                       "PE"="Prince Edward Island",
                       #"NT & NU"="Northwest Territories and Nunavut",
                       #"NT & NU"="Northwest Territories including Nunavut"
                       ),
)       

#NS_2017<-plant_data %>% filter(Code=="NS") %>% group_by(Reference.Year) %>% summarize(facilities=n(),ghgs=sum(as.numeric(`Total.Emissions.(tonnes.CO2e)`)))

plant_data$prov <- fct_collapse(plant_data$code,
                                "TERR" = c("NT", "NU","YT"),
                                "ATL" = c("NL", "NB","NS","PE"))

plant_data<-merge(plant_data,prov_ghgs,by.x=c("prov","reference_year"),by.y = c("Prov","Year"),all.x=TRUE)

#AB_2017<-plant_data %>% filter(Prov=="AB", Reference.Year==2017)

#NS_2017<-plant_data %>% filter(Prov=="NS")
#get population data

#Cansim 17-10-0005-01
pop_data<-get_cansim("1710000501")%>% filter(Sex=="Both sexes",`Age group`=="All ages")%>%
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

#prov_pop <- read.xlsx(xlsxFile = "LFE-GHG-2004-Present.xlsx", sheet = 8, startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
#prov_pop<-melt(prov_pop,id=c("Prov_name","Code"), value.name = "Prov_Pop",variable.name = "Year")
#prov_pop$Prov_Pop<-as.numeric(prov_pop$Prov_Pop)
#save(prov_pop,file = "prov_pop.Rdata")

plant_data<-merge(plant_data,pop_data,by.x=c("prov","reference_year"),by.y = c("Code","Year"),all.x=TRUE)

plant_data<-plant_data%>%rename(ref_year=reference_year)



#making the full graph

#who are the lfe facilities?

plant_data<-plant_data %>%
  group_by(facility_name)%>%
  mutate(lfe_ind=(total_co2e>=10000)*1,
         lfe=sum(lfe_ind)>=1) %>%
  ungroup()%>%
  I()


df1 <- plant_data %>% 
  filter(lfe) %>% 
  group_by(prov,ref_year,naics2_desc) %>% 
  summarise(ghg=sum(total_co2e,na.rm = T),prov_ghgs=mean(GHGs),prov_pop=max(Prov_pop))%>% ungroup()%>%
  group_by(naics2_desc,ref_year) %>% mutate(sector_ghgs=sum(ghg/10^6),na.rm=T) %>% ungroup()%>%
  mutate(sector=as_factor(naics2_desc), sector=fct_reorder(sector,-sector_ghgs))%>%
  mutate(sector=fct_other(sector,keep = levels(sector)[1:5],  other_level = "Other")) %>%
  ungroup()%>% 
  group_by(prov,sector,ref_year) %>% 
  summarize(ghg=sum(ghg,na.rm=T)/10^6,
            prov_ghgs=unique(prov_ghgs),
            prov_pop=unique(prov_pop)) %>% ungroup()


#Keep top 6


df1 <- df1 %>% group_by(prov,ref_year) %>% 
  mutate(lfe_ghg=sum(ghg),other_ghg=prov_ghgs-lfe_ghg) %>% ungroup


ggplot(df1)+
  geom_line(aes(ref_year,prov_ghgs),colour="red",size=3)+
  geom_line(aes(ref_year,lfe_ghg),colour="blue")+
  geom_line(aes(ref_year,other_ghg),colour="green")+
  geom_line(aes(ref_year,(lfe_ghg+other_ghg)),colour="orange")+
  facet_grid( ~ prov)


df2<-df1 %>% pivot_wider(names_from=sector,values_from=ghg)
names(df2) <- sub("Other","Other Large Final Emitters", names(df2))
names(df2) <- sub("other_ghg","Small Emitters", names(df2))
#names(df2) <- sub("prov_ghgs","Provincial Emissions", names(df2))

#we get NA if there are no LFEs in a sector in a year - replace with zeros so area chart is smooth

df1<-df2 %>% pivot_longer(cols=-c("prov","ref_year","prov_ghgs","lfe_ghg","prov_pop"),values_to = "GHGs",names_to = "Sector") %>% 
  mutate(GHGs = replace_na(GHGs, 0))


df1$Sector<-fct_relevel(df1$Sector, "Other Large Final Emitters", after = Inf)
df1$Sector<-fct_relevel(df1$Sector, "Small Emitters", after = Inf)

#df1$Sector<-relevel(df1$Sector, )

#df1$Sector<-relevel(df1$Sector, "Small Emitters")

#df1$Sector<-factor(df1$Sector, levels=rev(unique(df1$Sector)))

my_palette<-c("#313695",brewer.pal(9, "Set1"))

lfe_proc<-df1
save(lfe_proc,file = "lfe_proc.Rdata")


load(file = "lfe_proc.Rdata")
df1<-lfe_proc
df1<-df1 %>% mutate(label=paste(ref_year," large emitter GHGs of ",round(lfe_ghg,0),"Mt, or ",round(lfe_ghg/prov_ghgs*100,0),"% of provincial emissions",sep=""))
df2<-df1 %>% group_by(ref_year,prov) %>% summarize(lfe_ghg=mean(lfe_ghg),prov_ghgs=mean(prov_ghgs)) %>% ungroup()%>%
  group_by(ref_year) %>% summarize(lfe_ghg=sum(lfe_ghg),prov_ghgs=sum(prov_ghgs)) %>% ungroup() 
natl_share<-last(df2$lfe_ghg)/last(df2$prov_ghgs)

df1<-df1 %>%
  mutate(prov=fct_relevel(prov,c("BC","AB",  "SK","MB", "ON","QC",   "ATL",  "TERR")))

#png<-1
#if(png==1)#set these to only turn on if you're making PNG graphs
#  set_png("prov_ghgs_stack.png",width = 1800,height=1000)

library(ggrepel)
ggplot(df1)+
  geom_area(aes(ref_year,GHGs,fill=Sector),colour="black",position = "stack",size=.1)+
  #geom_line(aes(Ref_Year,`Provincial Emissions`),size=2)+
  facet_grid( ~ prov)+
  geom_label(data=df1 %>% filter(ref_year==2021,Sector=="Transportation"),
                   aes(x=2004+(2021-2004)/2,
                       y=300,label=str_wrap(label, width = 20)),
                   fontface = 'bold',
                   size = 2.8,
                   #box.padding = unit(0, "lines"),
                   #point.padding = unit(0, "lines"),
                   label.size=1,
                   #segment.color = colors_ua10()[1],
                   #segment.size = 0,
                   #force = 10,ylim = 3000,
                   #min.segment.length = unit(0.001, 'lines')
  )+
  
  #geom_col(aes(Prov,prov_ghgs/10^6,colour=Sector,fill=Sector,group=Ref_Year),size=1.5,position = "dodge")+
  #scale_color_viridis("",discrete=TRUE,guide_legend(NULL))+
  #scale_fill_viridis("",discrete=TRUE)+
  scale_x_continuous(breaks=pretty_breaks())+
  scale_y_continuous(breaks=pretty_breaks())+
  expand_limits(y=300)+
  scale_fill_manual("",values = colors_tableau10()[-7],guide = "legend")+
  #scale_colour_manual("",values=colors_tableau10()[-7],guide = "legend")+
  #scale_fill_grey(start = 0.8,end = 0.2)+
  #scale_color_grey(start = 0.8,end = 0.2)+
  #guides(fill=guide_legend(nrow =2,byrow=FALSE))+
  theme_minimal()+
  theme(
     plot.margin = margin(.5, .5, .5, .5, "cm"),
     legend.position = "right",
     legend.margin=margin(c(.10,0,.10,0),unit="cm"),
     legend.text = element_text(size = 10,face = "bold"),
     
     plot.caption = element_text(size = 12, face = "italic"),
    plot.title = element_text(size = 18,face = "bold"),
    plot.subtitle = element_text(size = 10, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text.y = element_text(size = 12,face = "bold", colour="black"),
    axis.text.x = element_text(size = 12, colour = "black", angle = 90,hjust = 0.5,vjust=0.5),
    strip.text.x = element_text(size = 14, colour = "black", angle = 0)
    )+
  labs(x=NULL,y=expression('Annual Emissions '*'(MtCO'[2]*'e)'),
       title="2004-2021 Emissions from Large Emitters and the Rest of the Economy",
       subtitle=paste("Large, point source emitters (facilities with annual emissions above 10kt/yr CO2e in any year) represented ",round(natl_share*100,1),"% of Canada's emissions in ",max(df1$ref_year),sep=""),
       caption="Source: Environment Canada data, graph by @andrew_leach")
ggsave("prov_ghgs_stack.png",bg="white",width=16,height=9,dpi=300)

df1<-df1 %>% mutate(pc_label=paste("2017 large emitter GHGs of ",round(lfe_ghg,0),"Mt, or ",round(lfe_ghg/prov_ghgs*100,0),"% of provincial emissions. Population of ",round(prov_pop/10^6,2),
                                   " million",sep=""))


#png<-1
#if(png==1)#set these to only turn on if you're making PNG graphs
#  set_png("prov_ghgs_lfe_percap.png")
ggplot(df1)+
  geom_area(aes(ref_year,GHGs/prov_pop*10^6,colour=Sector,fill=Sector),position = "stack",size=2)+
  #geom_line(aes(Ref_Year,`Provincial Emissions`),size=2)+
  facet_grid( ~ prov)+
  geom_label_repel(data=df1 %>% filter(Ref_Year==2017,Sector=="Transportation"),
                   aes(x=2014,y=100,label=str_wrap(pc_label, width = 20)),
                   fontface = 'bold',
                   size = 2.3,
                   #box.padding = unit(0, "lines"),
                   #point.padding = unit(0, "lines"),
                   label.size=1,
                   #segment.color = colors_ua10()[1],
                   segment.size = 0,
                   #force = 10,ylim = 3000,
                   min.segment.length = unit(0.2, 'lines'))+
  
  #geom_col(aes(Prov,prov_ghgs/10^6,colour=Sector,fill=Sector,group=Ref_Year),size=1.5,position = "dodge")+
  #scale_color_viridis("",discrete=TRUE,guide_legend(NULL))+
  #scale_fill_viridis("",discrete=TRUE)+
  scale_x_continuous(breaks=c(2004,2008,2012,2016))+
  scale_y_continuous(breaks=seq(0,100,10))+
  scale_fill_manual("",values = colors_tableau10()[-7],guide = "legend")+
  scale_colour_manual("",values=colors_tableau10()[-7],guide = "legend")+
  guides(fill=guide_legend(nrow =2,byrow=FALSE))+
  theme_minimal()+theme(
    plot.margin = margin(.5, .5, .5, .5, "cm"),
    legend.position = "bottom",
    legend.margin=margin(c(.10,0,.10,0),unit="cm"),
    legend.text = element_text(colour="black", size = 14),
    plot.caption = element_text(size = 12, face = "italic"),
    plot.title = element_text(size = 18,face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text.y = element_text(size = 12,face = "bold", colour="black"),
    axis.text.x = element_text(size = 12, colour = "black", angle = 90,hjust = 0),
    strip.text.x = element_text(size = 14, colour = "black", angle = 0)
  )+
  labs(x=NULL,y=expression('Per-capita Emissions '*'(tCO'[2]*'e per capita)'),
       title="2004-2017 Per-capita Emissions from Large Emitters and the Rest of the Economy",
       subtitle="Large, point source emitters are defined as facilities with annual emissions above 40kt/yr CO2e in any year.",
       caption="Source: Environment Canada emissions and Statistics Canada population data, graph by @andrew_leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()








lfes<-plant_data %>% group_by(ref_year) %>% 
  summarize(total_ghgs=sum(total_co2e))
  



lfe_2017_trans<-plant_data %>% filter(Ref_Year==2017,NAICS4_Desc=="Pipeline Transportation of Natural Gas")%>%
  group_by(NAICS4_Desc,GHG.ID.No.,Reporting.Company.Trade.Name,Facility.Name,Code) %>% 
  summarize(max_ghgs=max(Total_CO2e),methane=max(as.numeric(`CH4.(tonnes.CO2e)`)),CO2_only=max(as.numeric(`CO2.(tonnes.CO2e)`)),methane_t=max(as.numeric(`CH4.(tonnes)`))) %>% ungroup() %>%
  mutate(coverage=sum(max_ghgs))

lfe_2017_trans_AB<-plant_data %>% filter(Ref_Year==2017,NAICS4_Desc=="Pipeline Transportation of Natural Gas",Prov=="AB")%>%
  group_by(NAICS4_Desc,GHG.ID.No.,Reporting.Company.Trade.Name,Facility.Name,Code) %>% 
  summarize(CO2e=max(Total_CO2e),methane=max(as.numeric(`CH4.(tonnes.CO2e)`)),CO2_only=max(as.numeric(`CO2.(tonnes.CO2e)`))) %>% ungroup()%>%
  mutate(methane_share=methane/CO2e)

atco_AB<-plant_data %>% filter(NAICS4_Desc=="Pipeline Transportation of Natural Gas",Prov=="AB",grepl("ATCO",Facility.Name))%>%
  group_by(Reference.Year) %>% 
  summarize(CO2e=max(Total_CO2e),methane=max(as.numeric(`CH4.(tonnes.CO2e)`)),CO2_only=max(as.numeric(`CO2.(tonnes.CO2e)`))) %>% ungroup()%>%
  mutate(methane_share=methane/CO2e)%>%
  melt(id=c("Reference.Year","methane_share","CO2e"))

ggplot(atco_AB) + geom_area(aes(Reference.Year,value,group=variable,fill=variable))+
scale_x_continuous(breaks=c(2004,2008,2012,2016))+
  scale_y_continuous()+
  scale_fill_manual("",values = colors_tableau10()[-7],guide = "legend")+
  scale_colour_manual("",values=colors_tableau10()[-7],guide = "legend")+
  guides(fill=guide_legend(nrow =1,byrow=FALSE))+
  theme_minimal()+theme(
    plot.margin = margin(.5, .5, .5, .5, "cm"),
    legend.position = "bottom",
    legend.margin=margin(c(.10,0,.10,0),unit="cm"),
    legend.text = element_text(colour="black", size = 14),
    plot.caption = element_text(size = 12, face = "italic"),
    plot.title = element_text(size = 18,face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text.y = element_text(size = 12,face = "bold", colour="black"),
    axis.text.x = element_text(size = 12, colour = "black", angle = 90,hjust = 0),
    strip.text.x = element_text(size = 14, colour = "black", angle = 0)
  )+
  labs(x=NULL,y=expression('Annual Emissions '*'(tCO'[2]*'e)'),
       title="2004-2017 Emissions from ATCO Pipelines in Alberta",
       #subtitle="Large, point source emitters are defined as facilities with annual emissions above 40kt/yr CO2e in any year.",
       caption="Source: Environment Canada data, graph by @andrew_leach")
ggsave("atco_pipes.png",width = 20,height = 16)

all_pipes_AB<-plant_data %>% filter(NAICS4_Desc=="Pipeline Transportation of Natural Gas",Prov=="AB")%>%
  group_by(Reference.Year) %>% 
  summarize(CO2e=sum(Total_CO2e),methane=sum(as.numeric(`CH4.(tonnes.CO2e)`)),CO2_only=sum(as.numeric(`CO2.(tonnes.CO2e)`))) %>% ungroup()%>%
  mutate(methane_share=methane/CO2e)%>%
  melt(id=c("Reference.Year","methane_share","CO2e"))

ggplot(all_pipes_AB) + geom_area(aes(Reference.Year,value,group=variable,fill=variable))+
  scale_x_continuous(breaks=c(2004,2008,2012,2016))+
  scale_y_continuous()+
  scale_fill_manual("",values = colors_tableau10()[-7],guide = "legend")+
  scale_colour_manual("",values=colors_tableau10()[-7],guide = "legend")+
  guides(fill=guide_legend(nrow =1,byrow=FALSE))+
  theme_minimal()+theme(
    plot.margin = margin(.5, .5, .5, .5, "cm"),
    legend.position = "bottom",
    legend.margin=margin(c(.10,0,.10,0),unit="cm"),
    legend.text = element_text(colour="black", size = 14),
    plot.caption = element_text(size = 12, face = "italic"),
    plot.title = element_text(size = 18,face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text.y = element_text(size = 12,face = "bold", colour="black"),
    axis.text.x = element_text(size = 12, colour = "black", angle = 90,hjust = 0),
    strip.text.x = element_text(size = 14, colour = "black", angle = 0)
  )+
  labs(x=NULL,y=expression('Annual Emissions '*'(tCO'[2]*'e)'),
       title="2004-2017 Emissions from Gas Pipelines in Alberta",
       #subtitle="Large, point source emitters are defined as facilities with annual emissions above 40kt/yr CO2e in any year.",
       caption="Source: Environment Canada data, graph by @andrew_leach")
ggsave("all_pipes.png",width = 20,height = 16)


lfe_2017_all<-plant_data %>% filter(Ref_Year==2017)%>%
  group_by(NAICS4_Desc,GHG.ID.No.,Reporting.Company.Trade.Name,Facility.Name,Code) %>% 
  summarize(max_ghgs=max(Total_CO2e)) %>% ungroup() %>%
  filter(max_ghgs>10000) %>% mutate(coverage=sum(max_ghgs)) %>% ungroup() 
  

lfe_AB_defn<-plant_data %>% filter(Ref_Year==2017)%>%
  group_by(NAICS4_Desc,GHG.ID.No.,Reporting.Company.Trade.Name,Facility.Name,Code) %>% 
  summarize(max_ghgs=max(Total_CO2e)) %>% ungroup() %>%
  filter(max_ghgs>100000) %>% mutate(coverage=sum(max_ghgs)) %>% ungroup() %>% group_by(Code)%>%
  mutate(by_prov=sum(max_ghgs))

lfe_10k<-plant_data %>% filter(Ref_Year==2017)%>%
  group_by(NAICS4_Desc,GHG.ID.No.,Reporting.Company.Trade.Name,Facility.Name,Code) %>% 
  summarize(max_ghgs=max(Total_CO2e)) %>% ungroup() %>%
  filter(max_ghgs>10000) %>% mutate(coverage=sum(max_ghgs)) %>% ungroup() %>% group_by(Code)%>%
  mutate(by_prov=sum(max_ghgs))




prov_2017_all<-lfe_2017_all %>% group_by(Code) %>% 
  summarize(share=sum(max_ghgs)/mean(coverage))




coal_plants<- c("Keephills Thermal Electric Power Generating Plant", "Poplar Hill Generating Station",
"Genesee Thermal Generating Station", "Sheerness Generating Station","H.R.Milner Generating Station",
 "Sundance Thermal Electric Power Generating Plant",
 "Battle River Generating Station","Brandon Generating Station","Boundary Dam Power Station" ,
 "Belledune Generating Station","Lingan Generating Station","Poplar River Power Station",
 "Trenton Generating Station","Shand Power Station")


lfe_200<-plant_data %>% filter(Ref_Year==2017)%>%
  group_by(GHG.ID.No.,Reporting.Company.Trade.Name,Facility.Name,Code) %>% 
  summarize(max_ghgs=max(Total_CO2e)) %>% ungroup() %>% arrange(-max_ghgs) %>%
  head(200) %>% mutate(coverage=sum(max_ghgs))


coal_power<-plant_data %>% filter(Ref_Year==2017,Facility.Name %in% coal_plants)
cement<-plant_data %>% filter(Ref_Year==2017,NAICS4_Desc=="Cement and Concrete Product Manufacturing")

may_data<-rbind(coal_power,cement)



lfe_2017<-plant_data %>% filter(ref_year==2017) %>% group_by(ghg_id_no,facility_name) %>% 
  summarize(max_ghgs=max(total_co2e)) %>% ungroup() %>%
  filter(max_ghgs>100000) %>% 
  mutate(coverage=sum(max_ghgs))

lfe_test<-plant_data %>% filter(Ref_Year==2017,!Code %in% c("AB","BC","ON","SK","QC","NB")) %>% 
  group_by(GHG.ID.No.,Facility.Name,Code) %>% 
  summarize(max_ghgs=max(Total_CO2e)) %>% ungroup() %>%
  #filter(max_ghgs>40000) %>% 
  mutate(coverage=sum(max_ghgs))


plant_data$Prov<-factor(plant_data$Prov,levels =  c("BC","AB",  "SK","MB", "ON","QC",   "ATL",  "TERR"))


#making the full graph




df1 <- plant_data %>% #filter(Facility.Name %in% lfe_2017_all$Facility.Name) %>% 
  group_by(prov,ref_year,naics2_desc) %>% 
  summarise(ghg=sum(total_co2e,na.rm = T),prov_ghgs=mean(GHGs),prov_pop=max(Prov_pop))%>% ungroup()%>%
  group_by(naics2_desc,ref_year) %>% mutate(sector_ghgs=sum(ghg/10^6),na.rm=T) %>% ungroup()%>%
  mutate(sector=as_factor(naics2_desc), sector=fct_reorder(sector,-sector_ghgs))%>%
  mutate(sector=fct_other(sector,keep = levels(sector)[1:5],  other_level = "Other")) %>%
  ungroup()%>% 
  group_by(prov,sector,ref_year) %>% 
  summarize(ghg=sum(ghg,na.rm=T)/10^6,
  prov_ghgs=unique(prov_ghgs),
  prov_pop=unique(prov_pop)) %>% ungroup()


#Keep top 6


df1 <- df1 %>% group_by(Prov,Ref_Year) %>% mutate(lfe_ghg=sum(ghg),other_ghg=prov_ghgs-lfe_ghg) %>% ungroup


ggplot(df1)+
  geom_line(aes(Ref_Year,prov_ghgs),colour="red",size=3)+
  geom_line(aes(Ref_Year,lfe_ghg),colour="blue")+
  geom_line(aes(Ref_Year,other_ghg),colour="green")+
  geom_line(aes(Ref_Year,(lfe_ghg+other_ghg)),colour="orange")+
  facet_grid( ~ Prov)


df2<-dcast(df1,Prov + Ref_Year+prov_ghgs+lfe_ghg+other_ghg+prov_pop ~ Sector,value.var="ghg")

names(df2) <- sub("Other","Other Large Final Emitters", names(df2))
names(df2) <- sub("other_ghg","Small Emitters", names(df2))
#names(df2) <- sub("prov_ghgs","Provincial Emissions", names(df2))

#we get NA if there are no LFEs in a sector in a year - replace with zeros so area chart is smooth

df1<-melt(df2,id=c("Prov","Ref_Year","prov_ghgs","lfe_ghg","prov_pop"),value.name = "GHGs",variable.name = "Sector") %>% mutate(GHGs = replace_na(GHGs, 0))


df1$Sector<-fct_relevel(df1$Sector, "Other Large Final Emitters", after = Inf)
df1$Sector<-fct_relevel(df1$Sector, "Small Emitters", after = Inf)

#df1$Sector<-relevel(df1$Sector, )

#df1$Sector<-relevel(df1$Sector, "Small Emitters")

#df1$Sector<-factor(df1$Sector, levels=rev(unique(df1$Sector)))

my_palette<-c("#313695",brewer.pal(9, "Set1"))

lfe_proc<-df1
save(lfe_proc,file = "lfe_proc.Rdata")


load(file = "lfe_proc.Rdata")
df1<-lfe_proc
df1<-df1 %>% mutate(label=paste("2017 large emitter GHGs of ",round(lfe_ghg,0),"Mt, or ",round(lfe_ghg/prov_ghgs*100,0),"% of provincial emissions",sep=""))
df2<-df1 %>% group_by(Ref_Year,Prov) %>% summarize(lfe_ghg=mean(lfe_ghg),prov_ghgs=mean(prov_ghgs)) %>% ungroup()%>%
  group_by(Ref_Year) %>% summarize(lfe_ghg=sum(lfe_ghg),prov_ghgs=sum(prov_ghgs)) %>% ungroup() 
natl_share<-last(df2$lfe_ghg)/last(df2$prov_ghgs)
png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("prov_ghgs_stack.png",width = 1800,height=1000)
ggplot(df1)+
  geom_area(aes(Ref_Year,GHGs,fill=Sector),colour="black",position = "stack",size=.1)+
  #geom_line(aes(Ref_Year,`Provincial Emissions`),size=2)+
  facet_grid( ~ Prov)+
  geom_label_repel(data=df1 %>% filter(Ref_Year==2017,Sector=="Transportation"),
                   aes(x=2014,y=330,label=str_wrap(label, width = 20)),
                   fontface = 'bold',
                   size = 2.3,
                   #box.padding = unit(0, "lines"),
                   #point.padding = unit(0, "lines"),
                   label.size=1,
                   #segment.color = colors_ua10()[1],
                   segment.size = 0,
                   #force = 10,ylim = 3000,
                   min.segment.length = unit(0.2, 'lines'))+
  
  #geom_col(aes(Prov,prov_ghgs/10^6,colour=Sector,fill=Sector,group=Ref_Year),size=1.5,position = "dodge")+
  #scale_color_viridis("",discrete=TRUE,guide_legend(NULL))+
  #scale_fill_viridis("",discrete=TRUE)+
  scale_x_continuous(breaks=c(2004,2008,2012,2016))+
  scale_y_continuous(breaks=seq(0,250,50))+
  scale_fill_manual("",values = colors_tableau10()[-7],guide = "legend")+
  #scale_colour_manual("",values=colors_tableau10()[-7],guide = "legend")+
  #scale_fill_grey(start = 0.8,end = 0.2)+
  #scale_color_grey(start = 0.8,end = 0.2)+
  #guides(fill=guide_legend(nrow =2,byrow=FALSE))+
  theme_minimal()+theme(
    plot.margin = margin(.5, .5, .5, .5, "cm"),
    legend.position = "right",
    legend.margin=margin(c(.10,0,.10,0),unit="cm"),
    legend.text = element_text(colour="black", size = 10),
    plot.caption = element_text(size = 12, face = "italic"),
    plot.title = element_text(size = 18,face = "bold"),
    plot.subtitle = element_text(size = 10, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text.y = element_text(size = 12,face = "bold", colour="black"),
    axis.text.x = element_text(size = 12, colour = "black", angle = 90,hjust = 0),
    strip.text.x = element_text(size = 14, colour = "black", angle = 0)
  )+
  labs(x=NULL,y=expression('Annual Emissions '*'(MtCO'[2]*'e)'),
       title="2004-2017 Emissions from Large Emitters and the Rest of the Economy",
       subtitle=paste("Large, point source emitters (facilities with annual emissions above 10kt/yr CO2e in any year) represented ",round(natl_share*100,1),"% of Canada's emissions in 2017",sep=""),
       caption="Source: Environment Canada data, graph by @andrew_leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()

df1<-df1 %>% mutate(pc_label=paste("2017 large emitter GHGs of ",round(lfe_ghg,0),"Mt, or ",round(lfe_ghg/prov_ghgs*100,0),"% of provincial emissions. Population of ",round(prov_pop/10^6,2),
                                  " million",sep=""))


png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("prov_ghgs_lfe_percap.png")
ggplot(df1)+
  geom_area(aes(Ref_Year,GHGs/prov_pop*10^6,colour=Sector,fill=Sector),position = "stack",size=2)+
  #geom_line(aes(Ref_Year,`Provincial Emissions`),size=2)+
  facet_grid( ~ Prov)+
  geom_label_repel(data=df1 %>% filter(Ref_Year==2017,Sector=="Transportation"),
                   aes(x=2014,y=100,label=str_wrap(pc_label, width = 20)),
                   fontface = 'bold',
                   size = 2.3,
                   #box.padding = unit(0, "lines"),
                   #point.padding = unit(0, "lines"),
                   label.size=1,
                   #segment.color = colors_ua10()[1],
                   segment.size = 0,
                   #force = 10,ylim = 3000,
                   min.segment.length = unit(0.2, 'lines'))+
  
  #geom_col(aes(Prov,prov_ghgs/10^6,colour=Sector,fill=Sector,group=Ref_Year),size=1.5,position = "dodge")+
  #scale_color_viridis("",discrete=TRUE,guide_legend(NULL))+
  #scale_fill_viridis("",discrete=TRUE)+
  scale_x_continuous(breaks=c(2004,2008,2012,2016))+
  scale_y_continuous(breaks=seq(0,100,10))+
  scale_fill_manual("",values = colors_tableau10()[-7],guide = "legend")+
  scale_colour_manual("",values=colors_tableau10()[-7],guide = "legend")+
  guides(fill=guide_legend(nrow =2,byrow=FALSE))+
  theme_minimal()+theme(
    plot.margin = margin(.5, .5, .5, .5, "cm"),
    legend.position = "bottom",
    legend.margin=margin(c(.10,0,.10,0),unit="cm"),
    legend.text = element_text(colour="black", size = 14),
    plot.caption = element_text(size = 12, face = "italic"),
    plot.title = element_text(size = 18,face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text.y = element_text(size = 12,face = "bold", colour="black"),
    axis.text.x = element_text(size = 12, colour = "black", angle = 90,hjust = 0),
    strip.text.x = element_text(size = 14, colour = "black", angle = 0)
  )+
  labs(x=NULL,y=expression('Per-capita Emissions '*'(tCO'[2]*'e per capita)'),
       title="2004-2017 Per-capita Emissions from Large Emitters and the Rest of the Economy",
       subtitle="Large, point source emitters are defined as facilities with annual emissions above 40kt/yr CO2e in any year.",
       caption="Source: Environment Canada emissions and Statistics Canada population data, graph by @andrew_leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


ggplot(filter(df1,Prov=="ON"))+
  geom_area(aes(Ref_Year,GHGs,colour=Sector,fill=Sector),position = "stack",size=2)+
  geom_line(aes(Ref_Year,`Provincial Emissions`),size=2)
  

df_small<-df1%>% filter(Sector=="Small Emitters") %>% group_by(Ref_Year) %>% mutate(
  natl_ghg=sum(`Provincial Emissions`,na.rm = T),
  lfe_natl=sum(lfe_ghg),
  small_emitters=`Provincial Emissions`-lfe_ghg,
  natl_small=natl_ghg-lfe_natl,
  small_share_natl=natl_small/natl_ghg,
  lfe_share=lfe_ghg/`Provincial Emissions`,
  small_share=1-lfe_share
) %>%
  melt(id=c("Ref_Year","Prov","Provincial Emissions"),measure.vars=c("lfe_ghg","small_emitters"))

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("prov_ghgs_small.png")
ggplot(df_small)+
  geom_area(aes(Ref_Year,value,fill=variable),position = "stack")+
  geom_line(aes(Ref_Year,`Provincial Emissions`,color="A"))+
  facet_grid( ~ Prov)+
  #geom_col(aes(Prov,prov_ghgs/10^6,colour=Sector,fill=Sector,group=Ref_Year),size=1.5,position = "dodge")+
  #scale_color_viridis("",discrete=TRUE,guide_legend(NULL))+
  #scale_fill_viridis("",discrete=TRUE)+
  scale_x_continuous(breaks=c(2004,2008,2012,2016))+
  scale_fill_manual("",values = colors_tableau10()[-7],guide = "legend",labels=c("Large Emitters","Small Emitters"))+
  scale_colour_manual("",values="black",guide = "legend",labels="Provincial Emissions")+
  guides(fill=guide_legend(nrow =2,byrow=FALSE))+
  theme_minimal()+theme(
    plot.margin = margin(.5, .5, .5, .5, "cm"),
    legend.position = "bottom",
    legend.margin=margin(c(.10,0,.10,0),unit="cm"),
    legend.text = element_text(colour="black", size = 14),
    plot.caption = element_text(size = 12, face = "italic"),
    plot.title = element_text(size = 16,face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text.y = element_text(size = 12,face = "bold", colour="black"),
    axis.text.x = element_text(size = 12, colour = "black", angle = 90,hjust = 0),
    strip.text.x = element_text(size = 14, colour = "black", angle = 0)
  )+
  labs(x=NULL,y=expression('Annual Emissions '*'(MtCO'[2]*'e)'),
       title="2004-2016 Emissions from Large Emitters and the Rest of the Economy",
       subtitle="Small emitters include emissions from all sectors from sources other than large, point source emitters\nwith federal reporting requirements. Federal reporting is required if annual emissions are above 50kt CO2e.",
       caption="Source: Environment Canada data, graph by @andrew_leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()




#redo it but keep all provinces in backstop

df1 <- plant_data %>% group_by(Prov,Ref_Year,NAICS2_Desc) %>% 
  summarise(ghg=sum(Total_CO2e),prov_ghgs=mean(prov_ghgs),prov_pop=max(Prov_Pop))%>% ungroup()

df1 <- df1 %>% group_by(NAICS2_Desc,Ref_Year) %>% mutate(sector_ghgs=sum(ghg)) %>% ungroup()

df1$NAICS2_Desc<-fct_reorder(df1$NAICS2_Desc,-df1$sector_ghgs)
#Keep top 6

#first, collapse the sectors
df1$Sector<-fct_other(df1$NAICS2_Desc,keep = levels(df1$NAICS2_Desc)[1:5],  other_level = "Other")



df1 <- df1 %>% group_by(Prov,Ref_Year,Sector) %>% summarize(ghg=sum(ghg)) %>% ungroup()


load("prov_pop.Rdata")
load("prov_ghgs.Rdata")
prov_pop$Prov<-prov_pop$Code
prov_pop<-prov_pop %>% group_by(Prov,Year) %>% summarise(Prov_Pop=sum(Prov_Pop))

df1<-merge(df1,prov_pop[ , c("Prov","Year","Prov_Pop")],by.x = c("Prov","Ref_Year"), by.y = c("Prov","Year"),all.x = T)


prov_ghgs$Sector<-NULL
prov_ghgs<-prov_ghgs %>% group_by(Prov,Year) %>% summarize(GHGs=sum(GHGs))

df1<-merge(df1,prov_ghgs[ , c("Prov","Year","GHGs")],by.x = c("Prov","Ref_Year"), by.y = c("Prov","Year"),all.x = T)

df1 <- df1 %>% group_by(Prov,Ref_Year) %>% mutate(lfe_ghg=sum(ghg)/10^6,other_ghg=GHGs-lfe_ghg,ghg=ghg/10^6) %>% ungroup()


df2<-dcast(df1,Prov + Ref_Year+GHGs+lfe_ghg+other_ghg+Prov_Pop ~ Sector,value.var="ghg")

names(df2) <- sub("Other","Other Large Final Emitters", names(df2))
names(df2) <- sub("other_ghg","Small Emitters", names(df2))
names(df2) <- sub("GHGs","Provincial Emissions", names(df2))

df1<-melt(df2,id=c("Prov","Ref_Year","Provincial Emissions","lfe_ghg","Prov_Pop"),value.name = "GHGs",variable.name = "Sector")


df1$Sector<-fct_relevel(df1$Sector, "Other Large Final Emitters", after = Inf)
df1$Sector<-fct_relevel(df1$Sector, "Small Emitters", after = Inf)

#df1$Sector<-relevel(df1$Sector, )

#df1$Sector<-relevel(df1$Sector, "Small Emitters")

#df1$Sector<-factor(df1$Sector, levels=rev(unique(df1$Sector)))



png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("backstop_prov_ghgs_stack.png")
ggplot(filter(df1,Prov %in% c("MB","SK","ON","NB","PE","TERR")))+
  geom_area(aes(Ref_Year,GHGs,colour=Sector,fill=Sector),position = "stack")+
  geom_line(aes(Ref_Year,`Provincial Emissions`))+
  facet_grid( ~ Prov)+
  #geom_col(aes(Prov,prov_ghgs/10^6,colour=Sector,fill=Sector,group=Ref_Year),size=1.5,position = "dodge")+
  #scale_color_viridis("",discrete=TRUE,guide_legend(NULL))+
  #scale_fill_viridis("",discrete=TRUE)+
  scale_x_continuous(breaks=c(2004,2008,2012,2016))+
  scale_fill_manual("",values = colors_tableau10()[-7],guide = "legend")+
  scale_colour_manual("",values=colors_tableau10()[-7],guide = "legend")+
  guides(fill=guide_legend(nrow =2,byrow=FALSE))+
  theme_minimal()+theme(
    plot.margin = margin(.5, .5, .5, .5, "cm"),
    legend.position = "bottom",
    legend.margin=margin(c(.10,0,.10,0),unit="cm"),
    legend.text = element_text(colour="black", size = 14),
    plot.caption = element_text(size = 12, face = "italic"),
    plot.title = element_text(size = 18,face = "bold"),
    plot.subtitle = element_text(size = 14, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text.y = element_text(size = 12,face = "bold", colour="black"),
    axis.text.x = element_text(size = 12, colour = "black", angle = 90,hjust = 0),
    strip.text.x = element_text(size = 14, colour = "black", angle = 0)
  )+
  labs(x=NULL,y=expression('Annual Emissions '*'(MtCO'[2]*'e)'),
       title="2004-2016 Emissions from Large Emitters and the Rest of the Economy",
       subtitle="Small emitters include emissions from all sectors from sources other than large, point source emitters\nwith federal reporting requirements. Federal reporting is required if annual emissions are above 50kt CO2e.",
       caption="Source: Environment Canada data, graph by @andrew_leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("ATL_ghgs_stack.png")
ggplot(filter(df1,Prov=="ATL"))+
  geom_area(aes(Ref_Year,GHGs,colour=Sector,fill=Sector),position = "stack")+
  geom_line(aes(Ref_Year,`Provincial Emissions`))+
  #facet_grid( ~ Prov)+
  #geom_col(aes(Prov,prov_ghgs/10^6,colour=Sector,fill=Sector,group=Ref_Year),size=1.5,position = "dodge")+
  #scale_color_viridis("",discrete=TRUE,guide_legend(NULL))+
  #scale_fill_viridis("",discrete=TRUE)+
  scale_x_continuous(breaks=c(2004,2008,2012,2016))+
  scale_fill_manual("",values = colors_tableau10()[-7],guide = "legend")+
  scale_colour_manual("",values=colors_tableau10()[-7],guide = "legend")+
  guides(fill=guide_legend(nrow =2,byrow=FALSE))+
  theme_minimal()+theme(
    plot.margin = margin(.5, .5, .5, .5, "cm"),
    legend.position = "bottom",
    legend.margin=margin(c(.10,0,.10,0),unit="cm"),
    legend.text = element_text(colour="black", size = 14),
    plot.caption = element_text(size = 12, face = "italic"),
    plot.title = element_text(size = 16,face = "bold"),
    plot.subtitle = element_text(size = 14, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text.y = element_text(size = 12,face = "bold", colour="black"),
    axis.text.x = element_text(size = 12, colour = "black", angle = 90,hjust = 0),
    strip.text.x = element_text(size = 14, colour = "black", angle = 0)
  )+
  labs(x=NULL,y=expression('Annual Emissions '*'(MtCO'[2]*'e)'),
       title="2004-2016 Emissions from Large Emitters and the Rest of the Economy in Atlantic Canada",
       subtitle="Small emitters include emissions from all sectors from sources other than large, point source emitters\nwith federal reporting requirements. Federal reporting is required if annual emissions are above 50kt CO2e.",
       caption="Source: Environment Canada data, graph by @andrew_leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()



png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("ON_ghgs_stack.png")
ggplot(filter(df1,Prov=="ON"))+
  geom_area(aes(Ref_Year,GHGs,colour=Sector,fill=Sector),position = "stack")+
  geom_line(aes(Ref_Year,`Provincial Emissions`))+
  #facet_grid( ~ Prov)+
  #geom_col(aes(Prov,prov_ghgs/10^6,colour=Sector,fill=Sector,group=Ref_Year),size=1.5,position = "dodge")+
  #scale_color_viridis("",discrete=TRUE,guide_legend(NULL))+
  #scale_fill_viridis("",discrete=TRUE)+
  scale_x_continuous(breaks=c(2004,2008,2012,2016))+
  scale_fill_manual("",values = colors_tableau10()[-7],guide = "legend")+
  scale_colour_manual("",values=colors_tableau10()[-7],guide = "legend")+
  guides(fill=guide_legend(nrow =2,byrow=FALSE))+
  theme_minimal()+theme(
    plot.margin = margin(.5, .5, .5, .5, "cm"),
    legend.position = "bottom",
    legend.margin=margin(c(.10,0,.10,0),unit="cm"),
    legend.text = element_text(colour="black", size = 14),
    plot.caption = element_text(size = 12, face = "italic"),
    plot.title = element_text(size = 18,face = "bold"),
    plot.subtitle = element_text(size = 14, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text.y = element_text(size = 12,face = "bold", colour="black"),
    axis.text.x = element_text(size = 12, colour = "black", angle = 90,hjust = 0),
    strip.text.x = element_text(size = 14, colour = "black", angle = 0)
  )+
  labs(x=NULL,y=expression('Annual Emissions '*'(MtCO'[2]*'e)'),
       title="2004-2016 Emissions from Large Emitters and the Rest of the Economy in Ontario",
       subtitle="Small emitters include emissions from all sectors from sources other than large, point source emitters\nwith federal reporting requirements. Federal reporting is required if annual emissions are above 50kt CO2e.",
       caption="Source: Environment Canada data, graph by @andrew_leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


#get facility-specific data for ON
df2<-plant_data %>% filter(Prov=="ON") %>% filter(Ref_Year==2016) %>% dcast(., Facility ~Ref_Year,value.var="Total_CO2e",sum)  
names(df2)[2]<-"GHGs_2016"

ON_fac <- plant_data %>% filter(Prov=="ON") %>% left_join(df2,by = "Facility") %>%filter(!is.na(GHGs_2016))%>%
  mutate(Facility=factor(Facility), Facility=fct_reorder(Facility,-GHGs_2016),
         Fac_short=fct_other(Facility,keep=head(levels(Facility),11)),
         Fac_short=fct_collapse(Fac_short,
                                "Sarnia Refinery Plant, Imperial Oil" = "Sarnia Refinery Plant",
                                "Nanticoke Refinery, Imperial Oil" = "Nanticoke Refinery",
                                "Essar Power Canada Ltd, Sault Ste. Marie"="Essar Power Canada Ltd",
                                "Kenora Compressor Station, TransCanada" ="TransCanada Pipeline, Ontario",
                                "NOVA Chemicals Corunna"="Corunna Site",
                                "Holcim Mississauga Cement Plant"="Mississauga Cement Plant",
                                "St. Mary's Cement Inc. Bowmanville Cement Plant"="Bowmanville Cement Plant"
         ),
         Fac_short=fct_reorder(Fac_short,-GHGs_2016)
  )


p <- 
  ggplot(data=ON_fac,aes(Ref_Year,Total_CO2e/10^6,group = Facility)) +
  geom_line(aes(colour=Fac_short),alpha=0.2,size = .4)+
  geom_line(data=subset(ON_fac,Fac_short!="Other"),aes(colour=Fac_short),alpha=0.8,size = 1)+
  geom_point(data=subset(ON_fac,Fac_short!="Other"),aes(colour=Fac_short),alpha=0.8,size = 1)+
  #geom_line(data=subset(ON_fac,Fac_short=="Other"),aes(col = Fac_short),size = .5,alpha=.2)+
  scale_color_manual("",values=c(colors_tableau10()[1:10],colors_tableau10_light()[1:1],"grey80"))+
  guides(colour=guide_legend(nrow =3,byrow=F))+
  scale_y_continuous(expand = c(0,0),limits=c(0,15))+
  scale_x_continuous(expand = c(0,0.05),limits=c(2008,2016))+
  theme_minimal()+theme(
    plot.margin = margin(.5, .5, .5, .5, "cm"),
    legend.position = "bottom",
    legend.margin=margin(c(.10,0,.10,0),unit="cm"),
    legend.text = element_text(colour="black", size = 8),
    plot.caption = element_text(size = 8, face = "italic"),
    plot.title = element_text(size = 14,face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 12,face = "bold"),
    axis.text.y = element_text(size = 12,face = "bold", colour="black"),
    axis.text.x = element_text(size = 12, colour = "black", angle = 90,hjust = 0),
    strip.text.x = element_text(size = 14, colour = "black", angle = 0)
  )+
  theme(legend.position = "bottom")+
  labs(x=expression('Year'),y=expression('Annual Emissions '*'(MtCO'[2]*'e)'),
       title="Emissions from Ontario's Large Emitters (Mt, 2004-2016)",
       subtitle="Largest 11 out of 132 total emitters chosen based on 2016 reported emissions data. ",
       caption="Source: Environment Canada Data, graph by @andrew_leach")


set_png(file="on_emitters.png", width = 1400, height = 1000)
p
dev.off()


df2<-plant_data %>% filter(Prov=="AB") %>% filter(Ref_Year==2016) %>% dcast(., Facility ~Ref_Year,value.var="Total_CO2e",sum)  
names(df2)[2]<-"GHGs_2016"

AB_fac <- plant_data %>% filter(Prov=="AB") %>% left_join(df2,by = "Facility") %>%filter(!is.na(GHGs_2016))%>%
  mutate(Facility=factor(Facility), Facility=fct_reorder(Facility,-GHGs_2016),
         Fac_short=fct_other(Facility,keep=head(levels(Facility),11)),
         Fac_short=fct_collapse(Fac_short,
                                "CNRL Wolf Lake and Primrose" ="Wolf Lake and Primrose Plant",
                                "Shell Scotford Upgrader"="Scotford Upgrader and Upgrader Cogeneration",
                                "Suncor Firebag" ="Firebag" ,
                                "CNRL Horizon Oil Sands Mine"="Horizon Oil Sands Processing Plant and Mine" ,
                                "Imperial Cold Lake Oil Sands" ="Cold Lake" ,
                                "Keephills Thermal Generating Station"="Keephills Thermal Electric Power Generating Plant",
                                "Sundance Thermal Generating Station"="Sundance Thermal Electric Power Generating Plant",
                                "Syncrude Mildred Lake and Aurora North Oil Sands"="Mildred Lake and Aurora North Plant Sites"
         ),
         
         Fac_short=fct_reorder(Fac_short,-GHGs_2016)
  )


p <- 
  ggplot(data=AB_fac,aes(Ref_Year,Total_CO2e/10^6,group = Facility)) +
  geom_line(aes(colour=Fac_short),alpha=0.2,size = .4)+
  geom_line(data=subset(AB_fac,Fac_short!="Other"),aes(colour=Fac_short),alpha=0.8,size = 1)+
  geom_point(data=subset(AB_fac,Fac_short!="Other"),aes(colour=Fac_short),alpha=0.8,size = 1)+
  #geom_line(data=subset(ON_fac,Fac_short=="Other"),aes(col = Fac_short),size = .5,alpha=.2)+
  scale_color_manual("",values=c(colors_tableau10()[1:10],colors_tableau10_light()[1:1],"grey80"))+
  guides(colour=guide_legend(nrow =3,byrow=F))+
  #scale_y_continuous(expand = c(0,0),limits=c(0,4.99))+
  scale_x_continuous(expand = c(0,0.05),limits=c(2008,2016))+
  theme_minimal()+theme(
    plot.margin = margin(.5, .5, .5, .5, "cm"),
    legend.position = "bottom",
    legend.margin=margin(c(.10,0,.10,0),unit="cm"),
    legend.text = element_text(colour="black", size = 8),
    plot.caption = element_text(size = 8, face = "italic"),
    plot.title = element_text(size = 14,face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 12,face = "bold"),
    axis.text.y = element_text(size = 12,face = "bold", colour="black"),
    axis.text.x = element_text(size = 12, colour = "black", angle = 90,hjust = 0),
    strip.text.x = element_text(size = 14, colour = "black", angle = 0)
  )+
  theme(legend.position = "bottom")+
  labs(x=expression('Year'),y=expression('Annual Emissions '*'(MtCO'[2]*'e)'),
       title="Emissions from Alberta's Large Emitters (Mt, 2004-2016)",
       subtitle="Largest 11 out of 200 total emitters chosen based on 2016 reported emissions data. ",
       caption="Source: Environment Canada Data, graph by @andrew_leach")

p

set_png(file="AB_emitters.png", width = 1400, height = 1000)
p
dev.off()


df2<-plant_data %>% filter(Prov=="NB") %>% filter(Ref_Year==2016) %>% dcast(., Facility ~Ref_Year,value.var="Total_CO2e",sum)  
names(df2)[2]<-"GHGs_2016"


NB_fac <- plant_data %>% filter(Prov=="NB") %>% left_join(df2,by = "Facility") %>%filter(!is.na(GHGs_2016))%>%
  mutate(Facility=factor(Facility), Facility=fct_reorder(Facility,-GHGs_2016),
         Fac_short=fct_other(Facility,keep=head(levels(Facility),11)),
         Fac_short=fct_collapse(Fac_short,
                                "Brunswick Smelter" ="Brunswick Smelter",
                                "Irving Pulp & Paper, Saint John"="Irving Pulp & Paper",  
                                "Irving Paper, Saint John"="Irving Paper",
                                "Nackawic Mill"="Nackawic Mill",
                                "FPS Edmundston Pulp Mill"="Edmundston Pulp Mill",
                                "Havelock Lime Manufacturing Plant"="Havelock Plant",
                                "Irving Lake Utopia Paper"="Lake Utopia Paper A Division of J.D. Irving, Limited")
         Fac_short=fct_reorder(Fac_short,-GHGs_2016)
  )


p <- 
  ggplot(data=NB_fac,aes(Ref_Year,Total_CO2e/10^6,group = Facility)) +
  geom_line(aes(colour=Fac_short),alpha=0.2,size = .4)+
  geom_line(data=subset(NB_fac,Fac_short!="Other"),aes(colour=Fac_short),alpha=0.8,size = 1)+
  geom_point(data=subset(NB_fac,Fac_short!="Other"),aes(colour=Fac_short),alpha=0.8,size = 1)+
  #geom_line(data=subset(ON_fac,Fac_short=="Other"),aes(col = Fac_short),size = .5,alpha=.2)+
  scale_color_manual("",values=c(colors_tableau10()[1:10],colors_tableau10_light()[1:1],"grey80"))+
  guides(colour=guide_legend(nrow =3,byrow=F))+
  #scale_y_continuous(expand = c(0,0),limits=c(0,4.99))+
  scale_x_continuous(expand = c(0,0.05),limits=c(2008,2016))+
  theme_minimal()+theme(
    plot.margin = margin(.5, .5, .5, .5, "cm"),
    legend.position = "bottom",
    legend.margin=margin(c(.10,0,.10,0),unit="cm"),
    legend.text = element_text(colour="black", size = 8),
    plot.caption = element_text(size = 8, face = "italic"),
    plot.title = element_text(size = 14,face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 12,face = "bold"),
    axis.text.y = element_text(size = 12,face = "bold", colour="black"),
    axis.text.x = element_text(size = 12, colour = "black", angle = 90,hjust = 0),
    strip.text.x = element_text(size = 14, colour = "black", angle = 0)
  )+
  theme(legend.position = "bottom")+
  labs(x=expression('Year'),y=expression('Annual Emissions '*'(MtCO'[2]*'e)'),
       title="Emissions from New Brunswick's Large Emitters (Mt, 2008-2016)",
       subtitle="Largest 11 out of 17 total emitters chosen based on 2016 reported emissions data. ",
       caption="Source: Environment Canada Data, graph by @andrew_leach")


set_png(file="NB_emitters.png", width = 1400, height = 1000)
p
dev.off()

twitter=data.frame(seq.Date(ymd("2015-11-22"),ymd("2018-11-30"),by="1 month"))
twitter$red_herrings=rnorm(NROW(twitter),0,.25)+as.numeric(rownames(twitter))*.2+as.numeric(rownames(twitter))^2*-0.0002
names(twitter)[1]="Date"

tw<-ggplot(twitter)+geom_line(aes(Date,red_herrings))+
  theme_minimal()+theme(
    plot.margin = margin(.5, .5, .5, .5, "cm"),
    legend.position = "bottom",
    legend.margin=margin(c(.10,0,.10,0),unit="cm"),
    legend.text = element_text(colour="black", size = 8),
    plot.caption = element_text(size = 8, face = "italic"),
    plot.title = element_text(size = 14,face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 12,face = "bold"),
    axis.text.y = element_text(size = 12,face = "bold", colour="black"),
    axis.text.x = element_text(size = 12, colour = "black", angle = 90,hjust = 0),
    strip.text.x = element_text(size = 14, colour = "black", angle = 0)
  )+
  theme(legend.position = "none")+
  labs(x=expression('Date'),y=expression('Red Herrings and Non-Sequiturs per Day '*'(RHe)'),
       title="Carbon-tax-related non-sequiturs and red herrings posted on Twitter",
       caption="Source: Not real data, graph by @andrew_leach")

set_png(file="non-sequiturs.png")
tw
dev.off()


png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("prov_ghgs_percap.png")
ggplot(subset(df1,Ref_Year %in% seq(2010,2015)),group=Ref_Year)+
  geom_col(aes(Ref_Year,GHGs/prov_pop,colour=Sector,fill=Sector),size=1,position = "stack")+
  #geom_area(aes(Ref_Year,GHGs/prov_pop,colour=Sector,fill=Sector),size=4,position = "stack")+
  facet_grid( ~ Prov)+
  #geom_col(aes(Prov,prov_ghgs/10^6,colour=Sector,fill=Sector,group=Ref_Year),size=1.5,position = "dodge")+
  #scale_color_viridis("",discrete=TRUE,guide_legend(NULL))+
  #scale_fill_viridis("",discrete=TRUE)+
  scale_fill_manual("",values = my_palette,guide = "legend")+
  scale_colour_manual("",values=my_palette,guide = "legend")+
  guides(fill=guide_legend(ncol =3,byrow=FALSE))+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(.10,0,.10,0),unit="cm"),
    legend.text = element_text(colour="black", size = 14),
    plot.caption = element_text(size = 18, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 18, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 20,face = "bold"),
    axis.text.y = element_text(size = 14,face = "bold", colour="black"),
    axis.text.x = element_blank(),
    #axis.text.x = element_text(size = 14, colour = "black", angle = 90),
    strip.text.x = element_text(size = 14, colour = "black", angle = 0)
  )+
  labs(x=NULL,y=expression('Annual Emissions per Capita '*'(tCO'[2]*'e)'),
       title="2010-2015 Per Capita Emissions by Province",
       subtitle="Includes Only Provinces With Significant Large Point Source Emitters",
       caption="Source: Environment Canada Data\nGraph by @andrew_leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("prov_ghg_shares.png")
ggplot(subset(df1,Ref_Year %in% seq(2010,2016)),group=Ref_Year)+
  geom_area(aes(Ref_Year,GHGs/(prov_ghgs*100*100),colour=Sector,fill=Sector),size=1,position = "stack")+
  facet_grid( ~ Prov)+
  #geom_col(aes(Prov,prov_ghgs/10^6,colour=Sector,fill=Sector,group=Ref_Year),size=1.5,position = "dodge")+
  #scale_color_viridis("",discrete=TRUE,guide_legend(NULL))+
  #scale_fill_viridis("",discrete=TRUE)+
  scale_fill_manual("",values = my_palette,guide = "legend")+
  scale_colour_manual("",values=my_palette,guide = "legend")+
  guides(fill=guide_legend(ncol =2,byrow=FALSE))+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(.10,0,.10,0),unit="cm"),
    legend.text = element_text(colour="black", size = 14),
    plot.caption = element_text(size = 18, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 18, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 20,face = "bold"),
    axis.text.y = element_text(size = 14,face = "bold", colour="black"),
    axis.text.x = element_blank(),
    #axis.text.x = element_text(size = 14, colour = "black", angle = 90),
    strip.text.x = element_text(size = 14, colour = "black", angle = 0)
  )+
  labs(x=NULL,y="Emissions Share (%)",
       title=expression('2010-2016 Shares of Emissions from Large, Industrial Emitters'),
       subtitle="Includes Only Provinces With Significant Large Point Source Emitters",
       #subtitle="Excluding Electricity",
       caption="Source: Environment Canada Data\nGraph by @andrew_leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()









sub_samp<-subset(plant_data,Prov%in%c("AB"))






sub_samp<-subset(plant_data,Prov%in%c("AB","SK") & Total_CO2e>8000000 & Ref_Year==2016)
sub_samp<-arrange(sub_samp,Total_CO2e*-1)

load(file="prov_power.RData")

hdf<-get_googlemap(center = c(lon = -110, lat = 54), zoom = 5,size = c(640,640), scale = 2)
ggmap(hdf, extent = "normal")
sub_samp<-sub_samp[!is.na(sub_samp$Latitude),]
library(ggrepel)
png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("AB_sk_map.png")
ggmap(hdf, extent = "normal")+  theme_bw() +
  geom_point(aes(x = Longitude, y = Latitude, size=Total_CO2e/10^6),
             data = sub_samp, alpha = .5, color="darkblue")+
  scale_size(range=c(1,15),name="Annual\nEmissions (Mt)")+
  #geom_label_repel(data = sub_samp,
  #                 aes(x = Longitude, y = Latitude, size=1.9, label = Facility),show.legend = FALSE,
  #                 ) +
  #geom_point(data = sub_samp,aes(x = Longitude, y = Latitude), size = 5, color = 'grey') +
  geom_label_repel(data = sub_samp,
                   aes(Longitude,Latitude, label = "Test"),
                   fontface = 'bold', color = 'white',
                   box.padding = 0, point.padding = 0,
                   segment.color = 'grey50'
  ) +
  theme_minimal()+theme(
    legend.position = "right",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 20, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 20, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 20,face = "bold"),
    axis.text = element_text(size = 20,face = "bold", colour="black"),
    axis.title.y=element_text(vjust=0)
  )+
  labs(x="Longitude",y="Latitude",
       title="Alberta and Saskatchewan Largest Emitters",
       subtitle="2015 Emissions Above 1.8Mt",
       caption="Source: Environment Canada data, Map by Andrew Leach")  
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()

Alpac<-plant_data[grep("Alberta-Pacific",plant_data$Company_Name),]
ggplot(data=Alpac, aes(Ref_Year,Total_CO2e),size=2.5) +
  geom_line(size=1.5) +
  scale_color_viridis("",labels=c("Total CO2e emissions"),discrete=TRUE)+   
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0),limits=c(0,170000))+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12, face = "bold"),
    plot.caption = element_text(size = 12, face = "italic",margin = margin(t = 10)),
    plot.title = element_text(size = 18,face = "bold"),
    plot.subtitle = element_text(size = 14, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 18,face = "bold"),
    plot.margin=unit(c(1,1,1.5,1.2),"cm"),
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.text = element_text(size = 16,face = "bold", colour="black")
  )+    labs(y="Monthly Emissions (tCO2e)",x="\nYear",
             title="Al-Pac Emissions, 2004-2016",
             caption="Source: Environment Canada Data, Graph by Andrew Leach")




sub_samp<-subset(plant_data,Prov=="NT")
sub_samp<-subset(plant_data,Prov=="SK")
df1 <- sub_samp %>% group_by(NAICS4,NAICS4_Desc,Ref_Year) %>% summarise(ghg=sum(Total_CO2e))
df1<- df1[df1$Ref_Year>=2013,]
df1$Ref_Year<-as.factor(df1$Ref_Year)

df1<- df1[df1$NAICS4!=2211,]
png(file="SK_GHGs_no_elec.png", width = 1400, height = 750)
ggplot(df1,aes(Ref_Year,ghg/10^6,colour=NAICS4_Desc,fill=NAICS4_Desc),alpha=0.5)+
  geom_col(aes(Ref_Year,ghg/10^6,colour=NAICS4_Desc,fill=NAICS4_Desc),size=1.5,position = position_dodge(width = .9),width = .6)+
  scale_color_viridis("",discrete=TRUE,guide_legend(NULL))+
  scale_fill_viridis("",discrete=TRUE)+
  guides(fill=guide_legend(ncol=2,byrow=TRUE))+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(.10,0,.10,0),unit="cm"),
    legend.text = element_text(colour="black", size = 16, face = "bold"),
    plot.caption = element_text(size = 24, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 20, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 24,face = "bold"),
    axis.text = element_text(size = 24,face = "bold", colour="black")
  )+
  labs(x="Year",y="GHGs (Mt/yr)",
       title="Saskatchewan GHGs from Large Emitters",
       subtitle="Excluding Electricity,by NAICS 4-Digit Code",
       caption="Source: Environment Canada Data, \nGraph by @andrew_leach")
dev.off()


df1 <- sub_samp %>% group_by(NAICS4,NAICS4_Desc,Ref_Year) %>% summarise(ghg=sum(Total_CO2e))
df1<- df1[df1$Ref_Year>=2013,]
df1$Ref_Year<-as.factor(df1$Ref_Year)

#df1<- df1[df1$NAICS4!=2211,]
png(file="SK_GHGs.png", width = 1400, height = 750)
ggplot(df1,aes(Ref_Year,ghg/10^6,colour=NAICS4_Desc,fill=NAICS4_Desc),alpha=0.5)+
  geom_col(aes(Ref_Year,ghg/10^6,colour=NAICS4_Desc,fill=NAICS4_Desc),size=1.5,position = position_dodge(width = .9),width = .6)+
  scale_color_viridis("",discrete=TRUE,guide_legend(NULL))+
  scale_fill_viridis("",discrete=TRUE)+
  guides(fill=guide_legend(ncol=2,byrow=TRUE))+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(.10,0,.10,0),unit="cm"),
    legend.text = element_text(colour="black", size = 16, face = "bold"),
    plot.caption = element_text(size = 24, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 20, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 24,face = "bold"),
    axis.text = element_text(size = 24,face = "bold", colour="black")
  )+
  labs(x="Year",y="GHGs (Mt/yr)",
       title="Saskatchewan GHGs from Large Emitters",
       #subtitle="Excluding Electricity,by NAICS 4-Digit Code",
       caption="Source: Environment Canada Data, \nGraph by @andrew_leach")
dev.off()


df1 <- subset(sub_samp,as.numeric(NAICS3)<200)


sub_samp<-subset(plant_data, Prov == "AB")
df1 <- sub_samp %>% group_by(NAICS4,NAICS4_Desc,Ref_Year) %>% summarise(ghg=sum(Total_CO2e))
df1<- df1[df1$Ref_Year>=2013,]
df1$Ref_Year<-as.factor(df1$Ref_Year)

df1<- df1[df1$NAICS4!=2211,]
png(file="AB_GHGs_no_elec.png", width = 1400, height = 750)
ggplot(df1,aes(Ref_Year,ghg/10^6,colour=NAICS4_Desc,fill=NAICS4_Desc),alpha=0.5)+
  geom_col(aes(Ref_Year,ghg/10^6,colour=NAICS4_Desc,fill=NAICS4_Desc),size=1.5,position = position_dodge(width = .9),width = .6)+
  scale_color_viridis("",discrete=TRUE,guide_legend(NULL))+
  scale_fill_viridis("",discrete=TRUE)+
  guides(fill=guide_legend(ncol=2,byrow=TRUE))+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(.10,0,.10,0),unit="cm"),
    legend.text = element_text(colour="black", size = 16, face = "bold"),
    plot.caption = element_text(size = 24, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 20, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 24,face = "bold"),
    axis.text = element_text(size = 24,face = "bold", colour="black")
  )+
  labs(x="Year",y="GHGs (Mt/yr)",
       title="Alberta GHGs from Large Emitters",
       subtitle="Excluding Electricity,by NAICS 4-Digit Code",
       caption="Source: Environment Canada Data, \nGraph by @andrew_leach")
dev.off()


df1 <- sub_samp %>% group_by(NAICS4,NAICS4_Desc,Ref_Year) %>% summarise(ghg=sum(Total_CO2e))
df1<- df1[df1$Ref_Year>=2013,]
df1$Ref_Year<-as.factor(df1$Ref_Year)

#df1<- df1[df1$NAICS4!=2211,]
png(file="AB_GHGs.png", width = 1400, height = 750)
ggplot(df1,aes(Ref_Year,ghg/10^6,colour=NAICS4_Desc,fill=NAICS4_Desc),alpha=0.5)+
  geom_col(aes(Ref_Year,ghg/10^6,colour=NAICS4_Desc,fill=NAICS4_Desc),size=1.5,position = position_dodge(width = .9),width = .6)+
  scale_color_viridis("",discrete=TRUE,guide_legend(NULL))+
  scale_fill_viridis("",discrete=TRUE)+
  guides(fill=guide_legend(ncol=2,byrow=TRUE))+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(.10,0,.10,0),unit="cm"),
    legend.text = element_text(colour="black", size = 16, face = "bold"),
    plot.caption = element_text(size = 24, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 20, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 24,face = "bold"),
    axis.text = element_text(size = 24,face = "bold", colour="black")
  )+
  labs(x="Year",y="GHGs (Mt/yr)",
       title="Alberta GHGs from Large Emitters",
       #subtitle="Excluding Electricity,by NAICS 4-Digit Code",
       caption="Source: Environment Canada Data, \nGraph by @andrew_leach")
dev.off()


sub_samp<-subset(plant_data, NAICS4==2111)
df1 <- sub_samp %>% group_by(Prov,Ref_Year) %>% summarise(ghg=sum(Total_CO2e))
#df1<- df1[df1$Ref_Year>=2013,]
df1$Ref_Year<-as.factor(df1$Ref_Year)

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("oil_gas_ghgs.png")
ggplot(df1,aes(Ref_Year,ghg/10^6,colour=Prov,fill=Prov),alpha=0.5)+
  geom_col(aes(Ref_Year,ghg/10^6,colour=Prov,fill=Prov),size=1.5,position = position_dodge(width = .9),width = .6)+
  scale_color_viridis("",discrete=TRUE,guide_legend(NULL))+
  scale_fill_viridis("",discrete=TRUE)+
  guides(fill=guide_legend(nrow =1,byrow=TRUE))+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(.10,0,.10,0),unit="cm"),
    legend.text = element_text(colour="black", size = 16, face = "bold"),
    plot.caption = element_text(size = 24, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 20, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 24,face = "bold"),
    axis.text = element_text(size = 24,face = "bold", colour="black")
  )+
  labs(x="Year",y="GHGs (Mt/yr)",
       title="Oil and Gas GHGs from Large Emitters",
       subtitle="NAICS 2111 facilities with GHGRP Obligations",
       caption="Source: Environment Canada Data, \nGraph by @andrew_leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


sub_samp<-subset(plant_data, NAICS4==2211)
df1 <- sub_samp %>% group_by(Prov,Ref_Year) %>% summarise(ghg=sum(Total_CO2e))
#df1<- df1[df1$Ref_Year>=2013,]
df1$Ref_Year<-as.factor(df1$Ref_Year)

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("electricity_ghgs.png")
ggplot(df1,aes(Ref_Year,ghg/10^6,colour=Prov,fill=Prov),alpha=0.5)+
  geom_col(aes(Ref_Year,ghg/10^6,colour=Prov,fill=Prov),size=1.5,position = position_dodge(width = .9),width = .6)+
  scale_color_viridis("",discrete=TRUE,guide_legend(NULL))+
  scale_fill_viridis("",discrete=TRUE)+
  guides(fill=guide_legend(nrow =1,byrow=TRUE))+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(.10,0,.10,0),unit="cm"),
    legend.text = element_text(colour="black", size = 16, face = "bold"),
    plot.caption = element_text(size = 24, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 20, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 24,face = "bold"),
    axis.text = element_text(size = 24,face = "bold", colour="black")
  )+
  labs(x="Year",y="GHGs (Mt/yr)",
       title="Electricity GHGs from Large Emitters",
       subtitle="NAICS 2211 facilities with GHGRP Obligations",
       caption="Source: Environment Canada Data, \nGraph by @andrew_leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()




png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("sk_ghgs_stack.png")
ggplot(subset(df1,Ref_Year %in% seq(2010,2015) & Prov=="SK"),group=Ref_Year)+
  geom_col(aes(Ref_Year,GHGs/10^6,colour=Sector,fill=Sector),size=1.5,position = "stack")+
  #facet_grid( ~ Prov)+
  #geom_col(aes(Prov,prov_ghgs/10^6,colour=Sector,fill=Sector,group=Ref_Year),size=1.5,position = "dodge")+
  #scale_color_viridis("",discrete=TRUE,guide_legend(NULL))+
  #scale_fill_viridis("",discrete=TRUE)+
  scale_fill_manual("",values = my_palette,guide = "legend")+
  scale_colour_manual("",values=my_palette,guide = "legend")+
  guides(fill=guide_legend(ncol =2,byrow=FALSE))+
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
    axis.text.x = element_text(size = 14, colour = "black", angle = 90),
    strip.text.x = element_text(size = 14, colour = "black", angle = 0)
  )+
  labs(x=NULL,y="Emissions (Mt/yr)",
       title="2010-2015 Emissions in Saskatchewan",
       #subtitle="Excluding Electricity",
       caption="Source: Environment Canada Data\nGraph by @andrew_leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("sask_ghg_shares.png")
ggplot(subset(df1,Ref_Year %in% seq(2010,2015) & Prov=="SK"),group=Ref_Year)+
  geom_col(aes(Ref_Year,GHGs/prov_ghgs*100,colour=Sector,fill=Sector),size=1.5,position = "stack")+
  #facet_grid( ~ Prov)+
  #geom_col(aes(Prov,prov_ghgs/10^6,colour=Sector,fill=Sector,group=Ref_Year),size=1.5,position = "dodge")+
  #scale_color_viridis("",discrete=TRUE,guide_legend(NULL))+
  #scale_fill_viridis("",discrete=TRUE)+
  scale_fill_manual("",values = my_palette,guide = "legend")+
  scale_colour_manual("",values=my_palette,guide = "legend")+
  guides(fill=guide_legend(ncol =2,byrow=FALSE))+
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
    axis.text.x = element_text(size = 14, colour = "black", angle = 90),
    strip.text.x = element_text(size = 14, colour = "black", angle = 0)
  )+
  labs(x=NULL,y="Emissions Share (%)",
       title="2010-2015 Saskatchewan EMissions Shares",
       #subtitle="Excluding Electricity",
       caption="Source: Environment Canada Data\nGraph by @andrew_leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()



