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

options(scipen=999)

get_data<-function() {
  file_path<-"nir_electricity.xlsx"
  #download.file("https://data.ec.gc.ca/data/substances/monitor/canada-s-official-greenhouse-gas-inventory/C-Tables-Electricity-Canada-Provinces-Territories/EN_Electricity_Can_Prov_Terr.xlsx",file_path,mode="wb")
  #get the listing of sheets
  sheet_names<-excel_sheets(file_path)
  index_val<-1
  prov_list<-list()
  for(target_sheet in sheet_names){
  #target_sheet<-sheet_names[1]
  prov<-read_excel(file_path,target_sheet,range = "B1",col_names = FALSE)%>%
    rename(province=1)%>%
    mutate(province=gsub("Table A11-13: Electricity Generation and GHG Emission Details for the Nunavut","Nunavut",province),
           province=gsub("Electricity Generation and GHG Emission Details for ","",province))%>%
    as.character()
  print(prov)
  col_names<-read_excel(file_path,target_sheet,range = "B3:U3",col_names = FALSE)
  col_names[1]<-"fuel"
  col_names<-gsub("a","",col_names)
  elec_data<-read_excel(file_path,target_sheet,range = "B6:U11",col_names = FALSE,
                        na = c("N/A", "n/a","x","**"))
  names(elec_data)<-as.character(col_names)
  
  elec_data <-elec_data %>% 
    mutate(fuel=row_number(),
           fuel=factor(fuel),
           fuel=fct_recode(fuel,"Combustion"="1",
                           "Coal"="2",
                           "Natural Gas"="3",
                           "Other Fuel"="4",
                           "Other Generation"="5",
                           "Overall Total"="6"
           ),
           prov=prov           )%>%
    pivot_longer(-c(fuel,prov),names_to = "year",values_to = "ghg")%>%
    mutate(year=as.numeric(year))
  
  gen_data<-read_excel(file_path,target_sheet,range = "B15:U23",col_names = FALSE,
                       na = c("N/A", "n/a","x","**"))
  names(gen_data)<-as.character(col_names)
  gen_data <-gen_data %>% 
    mutate(fuel=row_number(),
           fuel=factor(fuel),
           fuel=fct_recode(fuel,"Combustion"="1",
                           "Coal"="2",
                           "Natural Gas"="3",
                           "Other Fuel"="4",
                           "Nuclear"="5",
                           "Hydro"="6",
                           "Other Renewables"="7",
                           "Other Generation"="8",
                           "Overall Total"="9"
           ),
           #fuel=fct_collapse(fuel,
           #                 "Other Generation" = c("Nuclear", "Hydro","Other Renewables","Other Generation")),
           
           prov=prov           )%>%
    pivot_longer(-c(fuel,prov),names_to = "year",values_to = "gen")%>%
    mutate(year=as.numeric(year))%>%
    group_by(fuel,year,prov)%>%
    summarize(gen=sum(gen,na.rm = T),.groups = "drop")
  
    elec_data<-elec_data %>% full_join(gen_data)%>% #only joining emissions associated with 
    mutate(ei=ghg/gen)
  
  
  
    prov_list[[index_val]]<-elec_data
    index_val<-index_val+1
  }  
  
  
  
  
  prov_data<-bind_rows(prov_list)
  prov_data
}

#all_provs_data<-get_data()
#save(all_provs_data,file = "prov_elec.Rdata")

load(file = "prov_elec.Rdata")

# all the provinces and territories

prov_data<-all_provs_data%>%
  mutate(prov=fct_recode(prov,"AB"="Alberta",
                         "BC"="British Columbia",
                         "NL"="Newfoundland and Labrador",
                         "MB"="Manitoba",
                         "SK"="Saskatchewan",
                         "NS"="Nova Scotia",
                         "ON"="Ontario",
                         "NT"="Northwest Territories",
                         "NT"="the Northwest Territories",
                         "QC"="Quebec",
                         "NU"="Nunavut",
                         "NB"="New Brunswick",
                         "YT"="Yukon",
                         "PE"="Prince Edward Island",
                         "NT & NU"="Northwest Territories and Nunavut",
                         "NU"="the Nunavut"),
         prov=fct_collapse(prov,
                           "TERR" = c("NT", "NU","YT","NT & NU"),
                           #"MARITIMES" = c("NB","NS","PE")
         )#,
         #fuel=fct_other(fuel,drop=c("Other Fuel","Other Generation")
  )%>%
  group_by(prov,year,fuel)%>%
  summarize(ghg=sum(ghg,na.rm=T),gen=sum(gen,na.rm=T))%>%
  ungroup()%>%
  mutate(prov=factor(prov,levels=c("Canada" ,"BC","AB" ,"SK","MB", "ON", "QC","NB","NS","PE","NL","TERR")))

prov_data <- prov_data %>%
  mutate(fuel=fct_recode(fuel,"Non-hydro Renewables"="Other Renewables"),
         fuel=fct_relevel(fuel,"Nuclear",after=0),
         fuel=fct_relevel(fuel,"Non-hydro Renewables",after=0),
         fuel=fct_relevel(fuel,"Hydro",after=0)
  )


prov_data<-prov_data %>% group_by(prov,year)%>%mutate(ci=sum(ghg)/sum(gen))

ci_data<-prov_data %>% group_by(prov,year)%>%summarize(ci=sum(ghg)/sum(gen))
scale_fac<-150

library(ggpattern)
elec_provs<-
ggplot(prov_data %>% filter(year==2021,!prov%in%c("Canada"),!fuel%in%c("Combustion","Overall Total","Other Generation"))%>%
         mutate(prov=factor(prov,levels=c("Canada" ,"BC","AB" ,"SK","MB", "ON","QC","NL","NB","NS","PE","TERR"))))+
  #geom_col(aes(prov,ghg,group=fuel,fill=fuel),position = "stack")+
  geom_col_pattern(aes(prov,gen/1000,group=fuel,fill=fuel,pattern=fuel),position = "stack",colour="black",size=.2,width = .8,pattern_spacing = .02)+
  #geom_line(data=prov_data %>% filter(year==2020,prov!="Canada",fuel=="Overall Total"),
  #          aes(prov,ghg/1000*5,group=year,color="GHG Emissions (right axis)"),size=1.25)+
  geom_point(data=ci_data %>% filter(year==2021,!prov%in%c("Canada"),),
             aes(prov,ci*scale_fac,group=year,color="2021 Emissions Intensity (right axis)"),size=3,
             shape=21, stroke=2, fill="white")+
  geom_point(data=ci_data %>% filter(year==1990,!prov%in%c("Canada")),
             aes(prov,ci*scale_fac,group=year,color="1990 Emissions Intensity (right axis)"),size=3,
             shape=21, stroke=2, fill="white")+
  #geom_point(data=ci_data %>% filter(year==2005,!prov%in%c("Canada","TERR","PE")),
  #           aes(prov,ci*scale_fac,group=year,color="2005 Emissions Intensity (right axis)"),size=3,
  #           shape=21, stroke=2, fill="white")+
  scale_y_continuous(
    # Features of the first axis
    name = expression('Generation  '*'(TWh)'),
    # Add a second axis and specify its features
    sec.axis = sec_axis( trans=~./scale_fac, name=expression('Emissions Intensity  '*'(tCO'[2]*'e/MWh)'))
  )+
  theme_tufte()+
  scale_pattern_manual("",values=c("Hydro"="none","Non-hydro Renewables"= "none", "Natural Gas"="none", "Coal"="none",
                                   "Other Fuel"="stripe","Nuclear"="crosshatch"))+
  
  theme(
    legend.position = "bottom",
    #legend.margin=margin(c(.05,0,.05,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
    plot.caption = element_text(size = 10, face = "italic",hjust=0),
    plot.title = element_text(size=16,face = "bold"),
    plot.subtitle = element_text(size = 10),
    #panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 12, colour="black"),
    axis.text.y.right = element_text(margin = margin(t = 0, r = 10, b = 0, l = 2),color="red"),
    axis.title.y.right = element_text(margin = margin(t = 0, r = 10, b = 0, l = 2),color="red"),
    #axis.text.x = element_blank(),
    axis.text.x = element_text(size = 10, colour = "black", hjust=0.5,vjust=0.5),
    axis.title.y = element_text(size = 14, colour="black"),
    axis.ticks = element_blank(),
    text = element_text(size = 20,family="Times New Roman MS")
  )+
  labs(x=NULL,
       #title="Electricity Generation and GHG Emissions by Province (2020)",
       #subtitle=paste("2022 National Inventory (2020 emissions)",sep=""),
       #caption=str_wrap("Source: Environment and Climate Change Canada 2022 National Inventory. Graph by @andrew_leach.",width = 180),
       NULL
  )+
  scale_fill_manual("",values=c("white","grey20","grey80","black","grey60","grey90"))+
  scale_colour_manual("",values=c("grey65","black"),guide = "legend")+
  guides(color=guide_legend(nrow =3,byrow=FALSE,label.theme=element_text(colour='grey30')),
         fill=guide_legend(nrow=3))+
  theme(
    text = element_text(size = 18,family="Times New Roman MS"),
    axis.text.y.right = element_text(margin = margin(t = 0, r = 10, b = 0, l = 2),color="black"),
    axis.title.y.right = element_text(margin = margin(t = 0, r = 10, b = 0, l = 2),color="black"),
    axis.text.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 2),color="black"),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 2),color="black")
  )


elec_provs+
  scale_fill_manual("",values=c(colors_ua10()[4],colors_ua10()[1],viridis(10,option="cividis",direction = 1,alpha = .5)[9],"grey20","grey40",viridis(10,option="cividis",direction = 1,alpha = .5)[7]))+
  labs(
    caption="Data via Environment and Climate Change Canada 2023 National Inventory Report, graph by @andrew_leach.",
    NULL
  )
ggsave("images/all_prov_elec_col_caption.png",width = 10,height=7,dpi=300,bg="white")

  
  


prov_data<-all_provs_data%>%
  mutate(prov=fct_recode(prov,"AB"="Alberta",
                         "BC"="British Columbia",
                         "NL"="Newfoundland and Labrador",
                         "MB"="Manitoba",
                         "SK"="Saskatchewan",
                         "NS"="Nova Scotia",
                         "ON"="Ontario",
                         "NT"="Northwest Territories",
                         "NT"="the Northwest Territories",
                         "QC"="Quebec",
                         "NU"="Nunavut",
                         "NB"="New Brunswick",
                         "YT"="Yukon",
                         "PE"="Prince Edward Island",
                         "NT & NU"="Northwest Territories and Nunavut",
                         "NU"="the Nunavut"),
         prov=fct_collapse(prov,
                           "TERR" = c("NT", "NU","YT","NT & NU"),
                           #"MARITIMES" = c("NB","NS","PE")
         )#,
         #fuel=fct_other(fuel,drop=c("Other Fuel","Other Generation")
         )%>%
  group_by(prov,year,fuel)%>%
  summarize(ghg=sum(ghg,na.rm=T),gen=sum(gen,na.rm=T))%>%
  ungroup()%>%
  mutate(prov=factor(prov,levels=c("Canada" ,"BC","AB" ,"SK","MB", "ON", "QC","NL","NB","NS","PE","TERR"  )))

prov_data <- prov_data %>%
  mutate(fuel=fct_recode(fuel,"Non-hydro Renewables"="Other Renewables"),
         fuel=fct_relevel(fuel,"Nuclear",after=0),
         fuel=fct_relevel(fuel,"Non-hydro Renewables",after=0),
         fuel=fct_relevel(fuel,"Hydro",after=0)
  )


prov_data<-prov_data %>% group_by(prov,year)%>%mutate(ci=sum(ghg)/sum(gen))

ci_data<-prov_data %>% group_by(prov,year)%>%summarize(ci=sum(ghg)/sum(gen))
scale_fac<-200
  
  elec_provs<-ggplot(prov_data %>% filter(year==2021,prov!="Canada",!fuel%in%c("Combustion","Overall Total")))+
    #geom_col(aes(prov,ghg,group=fuel,fill=fuel),position = "stack")+
    geom_col(aes(prov,gen/1000,group=fuel,fill=fuel),position = "stack",colour="black",size=.6)+
    #geom_line(data=prov_data %>% filter(year==2020,prov!="Canada",fuel=="Overall Total"),
    #          aes(prov,ghg/1000*5,group=year,color="GHG Emissions (right axis)"),size=1.25)+
    geom_point(data=ci_data %>% filter(year==2021,prov!="Canada"),
            aes(prov,ci*scale_fac,group=year,color="2021 Emissions Intensity (right axis)"),size=4.5,
            shape=21, stroke=2, fill="white")+
    geom_point(data=ci_data %>% filter(year==1990,prov!="Canada"),
               aes(prov,ci*scale_fac,group=year,color="1990 Emissions Intensity (right axis)"),size=4.5,
               shape=21, stroke=2, fill="white")+
    geom_point(data=ci_data %>% filter(year==2005,prov!="Canada"),
               aes(prov,ci*scale_fac,group=year,color="2005 Emissions Intensity (right axis)"),size=4.5,
               shape=21, stroke=2, fill="white")+
    scale_y_continuous(
      # Features of the first axis
      name = expression('Electricity Generation  '*'(TWh)'),
      # Add a second axis and specify its features
      sec.axis = sec_axis( trans=~./scale_fac, name=expression('Electricity Emissions Intensity  '*'(tCO'[2]*'e/MWh)'))
    )+
  #scale_fill_viridis("",discrete=TRUE,option="B")+
    scale_fill_manual("",values=c(colors_ua10()[4],colors_ua10()[1],viridis(10,option="cividis",direction = 1,alpha = .5)[9],"grey20","grey40",viridis(10,option="cividis",direction = 1,alpha = .5)[7]))+
    scale_colour_manual("",values=c("red","orange","black"),guide = "legend")+
  guides(color=guide_legend(ncol =1,byrow=FALSE,label.theme=element_text(colour='red')))+
    theme_tufte()+
  theme(
      legend.position = c(.86, .75),
      legend.margin=margin(c(.05,0,.05,0),unit="cm"),
      legend.text = element_text(colour="black", size = 12),
      plot.caption = element_text(size = 10, face = "italic",hjust=0),
      plot.title = element_text(size=16,face = "bold"),
      plot.subtitle = element_text(size = 10),
      #panel.grid.minor = element_blank(),
      axis.text.y = element_text(size = 12, colour="black"),
      axis.text.y.right = element_text(margin = margin(t = 0, r = 10, b = 0, l = 2),color="red"),
      axis.title.y.right = element_text(margin = margin(t = 0, r = 10, b = 0, l = 2),color="red"),
      #axis.text.x = element_blank(),
      axis.text.x = element_text(size = 10, colour = "black", hjust=0.5,vjust=0.5),
          axis.title.y = element_text(size = 14, colour="black"),
      axis.ticks = element_blank(),
      text = element_text(size = 20,family="Times New Roman MS")
      )+
    labs(x=NULL,
         #title="Electricity Generation and GHG Emissions by Province (2020)",
         #subtitle=paste("2022 National Inventory (2020 emissions)",sep=""),
         #caption=str_wrap("Source: Environment and Climate Change Canada 2022 National Inventory. Graph by @andrew_leach.",width = 180),
         NULL
    )
  elec_provs
    ggsave("images/prov_elec_col.png",width = 14.5,height=7,dpi=300,bg="white")

  elec_provs+
    labs(
       caption="Data via Environment and Climate Change Canada 2023 National Inventory Report, graph by @andrew_leach.",
       NULL
  )
  ggsave("images/prov_elec_col_caption.png",width = 14.5,height=7,dpi=300,bg="white")
  
  
  #remotes::install_github("coolbutuseless/ggpattern")
  
  
  ggplot(prov_data %>% filter(year==2021,!prov%in%c("Canada","TERR","PE"),!fuel%in%c("Combustion","Overall Total","Other Generation"))%>%
           mutate(prov=factor(prov,levels=c("Canada" ,"BC","AB" ,"SK","MB", "ON","QC","NL","NB","NS","PE","TERR"))))+
    #geom_col(aes(prov,ghg,group=fuel,fill=fuel),position = "stack")+
    geom_col(aes(prov,gen/1000,group=fuel,fill=fuel),position = "stack",colour="black",size=.2,width = .8)+
    #geom_line(data=prov_data %>% filter(year==2020,prov!="Canada",fuel=="Overall Total"),
    #          aes(prov,ghg/1000*5,group=year,color="GHG Emissions (right axis)"),size=1.25)+
    geom_point(data=ci_data %>% filter(year==2021,!prov%in%c("Canada","TERR","PE"),),
               aes(prov,ci*scale_fac,group=year,color="2021 Emissions Intensity (right axis)"),size=3,
               shape=21, stroke=2, fill="white")+
    geom_point(data=ci_data %>% filter(year==1990,!prov%in%c("Canada","TERR","PE")),
               aes(prov,ci*scale_fac,group=year,color="1990 Emissions Intensity (right axis)"),size=3,
               shape=21, stroke=2, fill="white")+
    #geom_point(data=ci_data %>% filter(year==2005,!prov%in%c("Canada","TERR","PE")),
    #           aes(prov,ci*scale_fac,group=year,color="2005 Emissions Intensity (right axis)"),size=3,
    #           shape=21, stroke=2, fill="white")+
    scale_y_continuous(
      # Features of the first axis
      name = expression('Generation  '*'(TWh)'),
      # Add a second axis and specify its features
      sec.axis = sec_axis( trans=~./scale_fac, name=expression('Emissions Intensity  '*'(tCO'[2]*'e/MWh)'))
    )+
    theme_tufte()+
    theme(
      legend.position = "bottom",
      #legend.margin=margin(c(.05,0,.05,0),unit="cm"),
      legend.text = element_text(colour="black", size = 12),
      plot.caption = element_text(size = 10, face = "italic",hjust=0),
      plot.title = element_text(size=16,face = "bold"),
      plot.subtitle = element_text(size = 10),
      #panel.grid.minor = element_blank(),
      axis.text.y = element_text(size = 12, colour="black"),
      axis.text.y.right = element_text(margin = margin(t = 0, r = 10, b = 0, l = 2),color="red"),
      axis.title.y.right = element_text(margin = margin(t = 0, r = 10, b = 0, l = 2),color="red"),
      #axis.text.x = element_blank(),
      axis.text.x = element_text(size = 10, colour = "black", hjust=0.5,vjust=0.5),
      axis.title.y = element_text(size = 14, colour="black"),
      axis.ticks = element_blank(),
      text = element_text(size = 20,family="Times New Roman MS")
    )+
    labs(x=NULL,
         #title="Electricity Generation and GHG Emissions by Province (2020)",
         #subtitle=paste("2022 National Inventory (2020 emissions)",sep=""),
         #caption=str_wrap("Source: Environment and Climate Change Canada 2022 National Inventory. Graph by @andrew_leach.",width = 180),
         NULL
    )+
    scale_fill_manual("",values=c("white","grey20","grey80","black","grey60","grey90"))+
    scale_colour_manual("",values=c("grey65","black"),guide = "legend")+
    guides(color=guide_legend(nrow =3,byrow=FALSE,label.theme=element_text(colour='grey30')),
           fill=guide_legend(nrow=3))+
    theme(
      text = element_text(size = 18,family="Times New Roman MS"),
      axis.text.y.right = element_text(margin = margin(t = 0, r = 10, b = 0, l = 2),color="black"),
      axis.title.y.right = element_text(margin = margin(t = 0, r = 10, b = 0, l = 2),color="black"),
      axis.text.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 2),color="black"),
      axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 2),color="black")
      )
  ggsave("images/prov_elec_bw.png",width = 8,height=8,dpi=600,bg="white")
  ggsave("images/prov_elec_bw.tiff",width = 8,height=8,dpi=600,bg="white")
  

  
  library(ggpattern)
  ggplot(prov_data %>% filter(year==2021,!prov%in%c("Canada","TERR","PE"),!fuel%in%c("Combustion","Overall Total","Other Generation"))%>%
           mutate(prov=factor(prov,levels=c("Canada" ,"BC","AB" ,"SK","MB", "ON","QC","NL","NB","NS","PE","TERR"))))+
    #geom_col(aes(prov,ghg,group=fuel,fill=fuel),position = "stack")+
    geom_col_pattern(aes(prov,gen/1000,group=fuel,fill=fuel,pattern=fuel),position = "stack",colour="black",size=.2,width = .8,pattern_spacing = .02)+
    #geom_line(data=prov_data %>% filter(year==2020,prov!="Canada",fuel=="Overall Total"),
    #          aes(prov,ghg/1000*5,group=year,color="GHG Emissions (right axis)"),size=1.25)+
    geom_point(data=ci_data %>% filter(year==2021,!prov%in%c("Canada","TERR","PE"),),
               aes(prov,ci*scale_fac,group=year,color="2021 Emissions Intensity (right axis)"),size=3,
               shape=21, stroke=2, fill="white")+
    geom_point(data=ci_data %>% filter(year==1990,!prov%in%c("Canada","TERR","PE")),
               aes(prov,ci*scale_fac,group=year,color="1990 Emissions Intensity (right axis)"),size=3,
               shape=21, stroke=2, fill="white")+
    #geom_point(data=ci_data %>% filter(year==2005,!prov%in%c("Canada","TERR","PE")),
    #           aes(prov,ci*scale_fac,group=year,color="2005 Emissions Intensity (right axis)"),size=3,
    #           shape=21, stroke=2, fill="white")+
    scale_y_continuous(
      # Features of the first axis
      name = expression('Generation  '*'(TWh)'),
      # Add a second axis and specify its features
      sec.axis = sec_axis( trans=~./scale_fac, name=expression('Emissions Intensity  '*'(tCO'[2]*'e/MWh)'))
    )+
    theme_tufte()+
    scale_pattern_manual("",values=c("Hydro"="none","Non-hydro Renewables"= "none", "Natural Gas"="none", "Coal"="none",
                                  "Other Fuel"="stripe","Nuclear"="crosshatch"))+
    
    theme(
      legend.position = "bottom",
      #legend.margin=margin(c(.05,0,.05,0),unit="cm"),
      legend.text = element_text(colour="black", size = 12),
      plot.caption = element_text(size = 10, face = "italic",hjust=0),
      plot.title = element_text(size=16,face = "bold"),
      plot.subtitle = element_text(size = 10),
      #panel.grid.minor = element_blank(),
      axis.text.y = element_text(size = 12, colour="black"),
      axis.text.y.right = element_text(margin = margin(t = 0, r = 10, b = 0, l = 2),color="red"),
      axis.title.y.right = element_text(margin = margin(t = 0, r = 10, b = 0, l = 2),color="red"),
      #axis.text.x = element_blank(),
      axis.text.x = element_text(size = 10, colour = "black", hjust=0.5,vjust=0.5),
      axis.title.y = element_text(size = 14, colour="black"),
      axis.ticks = element_blank(),
      text = element_text(size = 20,family="Times New Roman MS")
    )+
    labs(x=NULL,
         #title="Electricity Generation and GHG Emissions by Province (2020)",
         #subtitle=paste("2022 National Inventory (2020 emissions)",sep=""),
         #caption=str_wrap("Source: Environment and Climate Change Canada 2022 National Inventory. Graph by @andrew_leach.",width = 180),
         NULL
    )+
    scale_fill_manual("",values=c("white","grey20","grey80","black","grey60","grey90"))+
    scale_colour_manual("",values=c("grey65","black"),guide = "legend")+
    guides(color=guide_legend(nrow =3,byrow=FALSE,label.theme=element_text(colour='grey30')),
           fill=guide_legend(nrow=3))+
    theme(
      text = element_text(size = 18,family="Times New Roman MS"),
      axis.text.y.right = element_text(margin = margin(t = 0, r = 10, b = 0, l = 2),color="black"),
      axis.title.y.right = element_text(margin = margin(t = 0, r = 10, b = 0, l = 2),color="black"),
      axis.text.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 2),color="black"),
      axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 2),color="black")
    )
  ggsave("images/prov_elec_bw.png",width = 8.5,height=8.5,dpi=600,bg="white")
  ggsave("images/prov_elec_bw.tiff",width = 8.5,height=8.5,dpi=600,bg="white")
  
  
    
  ggsave("images/prov_elec.eps",width = 7,height=5,dpi=300,bg="white",device=cairo_ps)

  
  
  
  
  elec_provs_15<-ggplot(prov_data %>% filter(year==2015,prov!="Canada",!fuel%in%c("Combustion","Overall Total")))+
    #geom_col(aes(prov,ghg,group=fuel,fill=fuel),position = "stack")+
    geom_col(aes(prov,gen/1000,group=fuel,fill=fuel),position = "stack",colour="black",size=.6)+
    #geom_line(data=prov_data %>% filter(year==2020,prov!="Canada",fuel=="Overall Total"),
    #          aes(prov,ghg/1000*5,group=year,color="GHG Emissions (right axis)"),size=1.25)+
    geom_point(data=prov_data %>% filter(year==2015,prov!="Canada",fuel=="Overall Total"),
               aes(prov,ghg/1000*3,group=year,color="GHG Emissions (right axis)"),size=4.5,
               shape=21, stroke=2, fill="white")+
    scale_y_continuous(
      # Features of the first axis
      name = expression('Electricity Generation  '*'(TWh)'),
      # Add a second axis and specify its features
      sec.axis = sec_axis( trans=~./3, name=expression('Emissions from Electricity Generation  '*'(MtCO'[2]*'e)'))
    )+
    #scale_fill_viridis("",discrete=TRUE,option="B")+
    scale_fill_manual("",values=c(colors_ua10()[4],colors_ua10()[1],viridis(10,option="C",direction = 1,alpha = .5)[8],"grey20","grey50","white"))+
    scale_colour_manual("",values="red",guide = "legend")+
    guides(color=guide_legend(nrow =1,byrow=FALSE,label.theme=element_text(colour='red')))+
    theme_tufte()+
    theme(
      legend.position = c(.85, .7),
      legend.margin=margin(c(.05,0,.05,0),unit="cm"),
      legend.text = element_text(colour="black", size = 12),
      plot.caption = element_text(size = 10, face = "italic",hjust=0),
      plot.title = element_text(size=16,face = "bold"),
      plot.subtitle = element_text(size = 10),
      #panel.grid.minor = element_blank(),
      axis.text.y = element_text(size = 12, colour="black"),
      axis.text.y.right = element_text(margin = margin(t = 0, r = 10, b = 0, l = 2),color="red"),
      axis.title.y.right = element_text(margin = margin(t = 0, r = 10, b = 0, l = 2),color="red"),
      #axis.text.x = element_blank(),
      axis.text.x = element_text(size = 10, colour = "black", hjust=0.5,vjust=0.5),
      axis.title.y = element_text(size = 14, colour="black"),
      axis.ticks = element_blank(),
      text = element_text(size = 20,family="Times New Roman MS")
    )+
    labs(x=NULL,
         #title="Electricity Generation and GHG Emissions by Province (2020)",
         #subtitle=paste("2022 National Inventory (2020 emissions)",sep=""),
         #caption=str_wrap("Source: Environment and Climate Change Canada 2022 National Inventory. Graph by @andrew_leach.",width = 180),
         NULL
    )
  elec_provs_15+
    scale_fill_manual("",values=c("white","grey60","grey75","black","grey40","grey90"))+
    scale_colour_manual("",values="grey30",guide = "legend")+
    guides(color=guide_legend(nrow =1,byrow=FALSE,label.theme=element_text(colour='grey30')))+
    theme(
      text = element_text(size = 20,family="Times New Roman MS"),
      axis.text.y.right = element_text(margin = margin(t = 0, r = 10, b = 0, l = 2),color="grey30"),
      axis.title.y.right = element_text(margin = margin(t = 0, r = 10, b = 0, l = 2),color="grey30")
    )
  
  
  
  ggsave("images/prov_elec_2015.png",width = 14,height=7,dpi=300,bg="white")
  ggsave("images/prov_elec_2015.eps",width = 14,height=7,dpi=300,bg="white",device=cairo_ps)
  
  
  
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
  prov_plot<-  
    ggplot(data = prov_data %>% filter(prov !="Canada",fuel!="Combustion",,fuel!="Overall Total"))+
    geom_area(aes(year,gen/1000,fill=fuel),color="black",position = "stack",size=0.1,alpha=.8)+
    facet_wrap( ~ prov,nrow = 1)+
    scale_x_continuous(breaks=pretty_breaks())+
    scale_fill_manual("",values=c(colors_ua10()[4],colors_ua10()[1],viridis(10,option="cividis",direction = 1,alpha = .5)[9],"grey20","grey40",viridis(10,option="cividis",direction = 1,alpha = .5)[7]))+
    scale_colour_manual("",values="black",guide = "legend")+
    proj_graph()+#proj_labs+
    NULL
  
  
  
  
#adding on graph with Ben's data
  ben<-read_excel("bens_data.xlsx") %>% as_tibble()%>%clean_names()%>%
    pivot_longer(-year)%>%
    filter(!is.na(value))%>%
    filter(name!="additional_projects_with_offtake_deals_announced")%>%
    mutate(label_gen=paste(round(value*100,1),"%",sep=""))
    
  
  #"actual"                                                    "legislated_milestones"                                    
  #[3] "aeso_2019_long_term_outlook"                               "aeso_2021_lto"                                            
  #[5] "recent_capacity_additions_and_projects_under_construction" "additional_projects_with_offtake_deals_announced" 
  
  ggplot()+
    geom_line(data=ben%>%filter(!name %in% c("legislated_milestones")),
                                aes(year,value,group=name,color=name,lty=name),size=1.25)+
     geom_text(data=ben%>%filter(name %in% c("actual"),year<2022),
       aes(x=year,y=value,label=label_gen,color=name),nudge_x=-.1,nudge_y = .01,size=rel(4),show.legend = FALSE)+
     geom_text(data=ben%>%filter(name %in% c("actual"),year>=2022),
               aes(x=year,y=value,label=label_gen,color=name),nudge_x=-.25,nudge_y = .008,size=rel(4),show.legend = FALSE)+
    geom_text(data=ben%>%filter(name %in% c("aeso_2021_lto"),year>2022),
               aes(x=year,y=value,label=label_gen,color=name),nudge_y = -.008,nudge_x=0.2,size=rel(4),show.legend = FALSE)+
     geom_text(data=ben%>%filter(name %in% c("legislated_milestones")),
               aes(x=year,y=value,label=label_gen),nudge_y = .007,size=rel(4),color="grey50",show.legend = FALSE)+
     geom_text(data=ben%>%filter(name %in% c("aeso_2019_lto")),
               aes(x=year,y=value,label=label_gen,color=name),nudge_y = -.0075,size=rel(4),show.legend = FALSE)+
     geom_text(data=ben%>%filter(name %in% c("aeso_2021_lto"),year==2022),
               aes(x=year,y=value,label=label_gen,color=name),nudge_x = 0.5,size=rel(4),show.legend = FALSE)+
    geom_text(data=ben%>%filter(name %in% c("aeso_2021_lto"),year==2021),
              aes(x=year,y=value,label=label_gen,color=name),nudge_x = -0.32,size=rel(4),show.legend = FALSE)+
     geom_text(data=ben%>%filter(name %in% c("recent_capacity_additions_and_projects_under_construction")),
               aes(x=year,y=value,label=label_gen,color=name),nudge_x = -0.5,size=rel(4),show.legend = FALSE)+
    geom_point(data=ben%>%filter(name %in% c("legislated_milestones")),
               aes(year,value,group=name,shape=name),size=4,color="grey50",stroke=1,fill="white")+
    geom_point(data=ben%>%filter(!name %in% c("legislated_milestones")),
               aes(year,value,group=name,color=name),shape=20,size=4,show.legend = FALSE)+
    scale_color_manual("",values=c("black","grey30","grey30","grey60"),
                       labels=c("Actual Generation","AESO 2019 LTO","AESO 2021 LTO","Capacity Recently Added\nor Under Construction"))+
    #guides(color = guide_legend(override.aes = list(text=c("b","b","b","b"),linetype = c(1, 0, 0,0) ) ) )+
    scale_linetype_manual("",values=c("solid","11","32","solid"),
                       labels=c("Actual Generation","AESO 2019 LTO","AESO 2021 LTO","Capacity Recently Added\nor Under Construction"))+
    
    scale_shape_manual("",values=c(21),
                       labels=c("Legislated Milestones"))+
    scale_y_continuous(labels = scales::percent)+
    scale_x_continuous(breaks=pretty_breaks(n=10))+
    #geom_text(aes(x=year,y=value,label=label_gen),nudge_y = .01,size=rel(2))+
    theme_tufte()+
    theme(
      legend.position = "bottom",
      legend.key.width=unit(3,"line"),
      legend.margin=margin(c(.05,0,.05,0),unit="cm"),
      legend.text = element_text(colour="black", size = 14),
      plot.caption = element_text(size = 10, face = "italic",hjust=0),
      plot.title = element_text(size=16,face = "bold"),
      plot.subtitle = element_text(size = 10),
      #panel.grid.minor = element_blank(),
      axis.text.y = element_text(size = 14, colour="black"),
      axis.text.y.right = element_text(margin = margin(t = 0, r = 10, b = 0, l = 2),color="red"),
      axis.title.y.right = element_text(margin = margin(t = 0, r = 10, b = 0, l = 2),color="red"),
      #axis.text.x = element_blank(),
      axis.text.x = element_text(size = 14, colour = "black", hjust=0.5,vjust=0.5),
      axis.title.y = element_text(size = 14, colour="black"),
      axis.ticks = element_blank(),
      text = element_text(size = 20,family="Times New Roman MS")
    )+
    labs(x=NULL,y="Share of Generation",
         #title="Electricity Generation and GHG Emissions by Province (2020)",
         #subtitle=paste("2022 National Inventory (2020 emissions)",sep=""),
         #caption=str_wrap("Source: Environment and Climate Change Canada 2022 National Inventory. Graph by @andrew_leach.",width = 180),
         NULL
    )
    
  ggsave("images/ben.png",width = 14,height=7,dpi=300,bg="white")
  ggsave("images/ben.eps",width = 14,height=7,dpi=300,bg="white",device=cairo_ps)
  
  
  
  
  ccir<-read_excel("bens_data.xlsx",sheet = "Sheet2") %>% as_tibble()%>%clean_names()
  
  ccir<-ccir %>% mutate(plant=factor(generation,labels = paste(generation,"\nGHG Rate=",ei,"t/MWh\n",sep="")[1:2]),
                        oba_val=oba*price,
                        net_charge=ctax-oba_val,
                        net=ctax-oba_val
                        )%>%
    pivot_longer(cols=c(oba_val,net),names_to = "charge",values_to = "amt")%>%
    mutate(policy=factor(policy),
           policy=fct_relevel(policy,"CCIR",after = Inf),
           charge=factor(charge),
           charge=fct_relevel(charge,"net",after=0),
           policy=fct_recode(policy,"Carbon Competitiveness Incentive Regulation ($30/tonne, 2018)"="CCIR",
                             "Specicified Gas Emitters Regulation ($15/tonne, 2015)"="SGER"),
           charge=fct_recode(charge,"Estimated value of output-based allocation of emissions credits ($/MWh)"="oba_val",
                             "Estimated residual carbon charge owing ($/MWh)"="net"),
           
           
           )
 
      ggplot(ccir)+
      #geom_col(aes(prov,ghg,group=fuel,fill=fuel),position = "stack")+
      #geom_col(aes(plant,ctax),fill=NA,position = "stack",colour="black",size=.6)+
      geom_col(aes(plant,amt,group=charge,fill=charge),position = "stack",colour="black",size=.6)+
      geom_text(aes(x=plant,y=oba*price/2,label=paste("$",format(oba*price,nsmall=2),"/MWh",sep="")),colour="black",size=rel(4),show.legend = FALSE)+
      
      geom_text(aes(x=plant,y=ctax-net_charge/2,label=ifelse(net_charge!=0,paste("$",format(ctax-oba*price,nsmall=2),"/MWh",sep=""),NA)),colour="black",size=rel(4),show.legend = FALSE)+
         
      scale_fill_manual("",values=c("grey90","grey60"),
                        breaks =c("Estimated value of output-based allocation of emissions credits ($/MWh)",
                                  "Estimated residual carbon charge owing ($/MWh)") )+
      facet_wrap(~policy)+
      theme_tufte()+
      theme(
        legend.position = "bottom",
        legend.margin=margin(c(.05,0,.05,0),unit="cm"),
        legend.text = element_text(colour="black", size = 12),
        plot.caption = element_text(size = 10, face = "italic",hjust=0),
        plot.title = element_text(size=16,face = "bold"),
        plot.subtitle = element_text(size = 10),
        #panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 12, colour="black"),
        axis.text.y.right = element_text(margin = margin(t = 0, r = 10, b = 0, l = 2),color="red"),
        axis.title.y.right = element_text(margin = margin(t = 0, r = 10, b = 0, l = 2),color="red"),
        #axis.text.x = element_blank(),
        axis.text.x = element_text(size = 10, colour = "black", hjust=0.5,vjust=0.5),
        axis.title.y = element_text(size = 14, colour="black"),
        axis.ticks = element_blank(),
        text = element_text(size = 20,family="Times New Roman MS")
      )+
      labs(x=NULL,y="Value of output-based allocations and residual carbon charges ($/MWh)",
           #title="Electricity Generation and GHG Emissions by Province (2020)",
           #subtitle=paste("2022 National Inventory (2020 emissions)",sep=""),
           #caption=str_wrap("Source: Environment and Climate Change Canada 2022 National Inventory. Graph by @andrew_leach.",width = 180),
           NULL
      )
    
  
  

    ggsave("images/ccir_sger.png",width = 14,height=9,dpi=300,bg="white")
    ggsave("images/ccir_sger.eps",width = 14,height=9,dpi=300,bg="white",device=cairo_ps)
    

    
    ccir<-read_excel("bens_data.xlsx",sheet = "Sheet5") %>% as_tibble()%>%clean_names()
    
    ccir<-ccir %>% mutate(plant=factor(generation,labels = unique(paste(generation,"\nEmissions intensity=",ei,"t/MWh\n",sep=""))),
                          oba_val=oba*price,
                          net_charge=ctax-oba_val,
                          net=ctax-oba_val
    )%>%
      pivot_longer(cols=c(oba_val,net),names_to = "charge",values_to = "amt")%>%
      mutate(policy=factor(policy),
             policy=fct_relevel(policy,"TIER",after = Inf),
             charge=factor(charge),
             charge=fct_relevel(charge,"net",after=0),
             policy=fct_recode(policy,"Technology Innovation Emissions Reduction (TIER) Regulation\n($50/tonne, 2022)"="TIER",
                               "Specicified Gas Emitters Regulation (SGER)\n($15/tonne, 2015)"="SGER"),
             charge=fct_recode(charge,"Estimated value of output-based allocation of emissions credits ($/MWh)"="oba_val",
                               "Estimated residual carbon charge owing ($/MWh)"="net"),
             
             
      )
    
    ggplot(ccir)+
      #geom_col(aes(prov,ghg,group=fuel,fill=fuel),position = "stack")+
      #geom_col(aes(plant,ctax),fill=NA,position = "stack",colour="black",size=.6)+
      geom_col(aes(plant,amt,group=charge,fill=charge),position = "stack",colour="black",size=.6)+
      geom_text(aes(x=plant,y=oba*price/2,label=paste("$",format(oba*price,nsmall=2),"/MWh",sep="")),colour="black",size=rel(5),show.legend = FALSE)+
  
      geom_text(aes(x=plant,y=case_when(
        net_charge>2 ~ ctax-net_charge/2,
        abs(net_charge)<2 ~ ctax+2,
        net_charge<0 ~ net_charge/2,
        TRUE ~ ctax-net_charge/2),
        
        label=ifelse(net_charge!=0,paste("$",format(ctax-oba*price,nsmall=2),"/MWh",sep=""),NA)),colour="black",size=rel(5),show.legend = FALSE)+

      scale_fill_manual("",values=c("grey90","grey60"),
                        breaks =c("Estimated value of output-based allocation of emissions credits ($/MWh)",
                                  "Estimated residual carbon charge owing ($/MWh)") )+
      facet_wrap(~policy)+
      theme_tufte()+
      theme(
        legend.position = "bottom",
        legend.margin=margin(c(.05,0,.05,0),unit="cm"),
        legend.text = element_text(colour="black", size = 12),
        plot.caption = element_text(size = 10, face = "italic",hjust=0),
        plot.title = element_text(size=16,face = "bold"),
        plot.subtitle = element_text(size = 10),
        #panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 16, colour="black"),
        axis.text.y.right = element_text(margin = margin(t = 0, r = 10, b = 0, l = 2),color="red"),
        axis.title.y.right = element_text(margin = margin(t = 0, r = 10, b = 0, l = 2),color="red"),
        #axis.text.x = element_blank(),
        axis.text.x = element_text(size = 13, colour = "black", hjust=0.5,vjust=0.5),
        axis.title.y = element_text(size = 13, colour="black"),
        axis.ticks = element_blank(),
        text = element_text(size = 20,family="Times New Roman MS")
      )+
      labs(x=NULL,y="Value of output-based allocations and residual carbon charges ($/MWh)",
           #title="Electricity Generation and GHG Emissions by Province (2020)",
           #subtitle=paste("2022 National Inventory (2020 emissions)",sep=""),
           caption=str_wrap("Assumes renewable generation uses output-based pricing system under TIER, not the offset protocol. Using offsets would increase the per MWh revenue from carbon pricing to $25.50/MWh. Graph by @andrew_leach.",width = 185),
           NULL
      )
    
    
    
    
    ggsave("images/tier_sger.png",width = 18,height=8,dpi=300,bg="white")
    #ggsave("images/ccir_sger.eps",width = 14,height=9,dpi=300,bg="white",device=cairo_ps)
    
         
    
    
ggppa_comp<-read_excel("bens_data.xlsx",sheet = "Sheet3") %>% 
  as_tibble()%>%clean_names()

ggppa_comp <-ggppa_comp%>%mutate(
  generation=ifelse(generation!="Zero-emissions",paste(generation,"\nGHGs: ",ei,"t/MWh\n",sep=""),generation),
  plant=factor(generation),
  ctax=ei*price,
  oba_val=oba*price,
  net_charge=ctax-oba_val,
  net_charge=ctax-oba_val,
  net=ctax-oba_val
)%>%
  pivot_longer(cols=c(oba_val,net),names_to = "charge",values_to = "amt")%>%
  mutate(policy=factor(policy),
         policy=fct_relevel(policy,"CCIR",after = Inf),
         charge=factor(charge),
         charge=fct_relevel(charge,"net",after=0),
         policy=fct_recode(policy,"Technology Innovation Emissions Reduction Regulation\n($65/tonne, 2023)"="CCIR",
                           "Greenhouse Gas Pollution Pricing Act\n($65/tonne, 2023)"="GGPPA"),
         charge=fct_recode(charge,"Value of output-based allocation of emissions credits ($/MWh)"="oba_val",
                           "Residual carbon charge ($/MWh)"="net"),
         
      
          )

  
ggplot(ggppa_comp)+
  geom_col(aes(plant,amt,group=charge,fill=charge),position = "stack",colour="black",size=.6)+
  geom_text(aes(x=plant,y=ifelse(oba!=0,oba*price/2,-2),label=paste("$",format(oba*price,nsmall=2),"/MWh",sep="")),colour="black",size=rel(5),show.legend = FALSE)+
  
  #geom_text(aes(x=plant,y=ctax-net_charge/2,label=paste("$",format(ctax-oba*price,nsmall=2),"/MWh",sep="")),colour="black",size=rel(4),show.legend = FALSE)+
  
  
  geom_text(aes(x=plant,y=ifelse(net_charge>0,ctax-net_charge/2,net_charge/2),label=ifelse(net_charge!=0,paste("$",format(ctax-oba*price,nsmall=2),"/MWh",sep=""),NA)),colour="black",size=rel(5),show.legend = FALSE)+
  
  scale_fill_manual("",values=c("grey90","grey60"),
                    breaks =c("Estimated value of output-based allocation of emissions credits ($/MWh)",
                              "Estimated residual carbon charge owing ($/MWh)") )+
  facet_wrap(~policy)+
  theme_tufte()+
  theme(
    legend.position = "bottom",
    legend.margin=margin(c(.05,0,.05,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12),
    plot.caption = element_text(size = 10, face = "italic",hjust=0),
    plot.title = element_text(size=16,face = "bold"),
    plot.subtitle = element_text(size = 10),
    #panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 12, colour="black"),
    axis.text.y.right = element_text(margin = margin(t = 0, r = 10, b = 0, l = 2),color="red"),
    axis.title.y.right = element_text(margin = margin(t = 0, r = 10, b = 0, l = 2),color="red"),
    #axis.text.x = element_blank(),
    axis.text.x = element_text(size = 13, colour = "black", hjust=0.5,vjust=0.5),
    axis.title.y = element_text(size = 13, colour="black"),
    axis.ticks = element_blank(),
    text = element_text(size = 20,family="Times New Roman MS")
  )+
  labs(x=NULL,y="Value of output-based allocations and residual carbon charges ($/MWh)",
       #title="Electricity Generation and GHG Emissions by Province (2020)",
       #subtitle=paste("2022 National Inventory (2020 emissions)",sep=""),
       #caption=str_wrap("Source: Environment and Climate Change Canada 2022 National Inventory. Graph by @andrew_leach.",width = 180),
       NULL
  )

ggsave("images/ccir_ggppa.png",width = 19,height=9,dpi=300,bg="white")


windowsFonts("Times" = windowsFont("Times New Roman"))

ggplot(ggppa_comp)+
  geom_col(aes(plant,amt,group=charge,fill=charge),position = "stack",colour="black",size=.6)+
  geom_text(aes(x=plant,y=ifelse(oba!=0,oba*price/2,-2),label=paste("$",format(oba*price,nsmall=2),"/MWh",sep="")),colour="black",family="Times", size=rel(2),show.legend = FALSE)+
  
  #geom_text(aes(x=plant,y=ctax-net_charge/2,label=paste("$",format(ctax-oba*price,nsmall=2),"/MWh",sep="")),colour="black",size=rel(4),show.legend = FALSE)+
  
  
  geom_text(aes(x=plant,y=ifelse(net_charge>0,ctax-net_charge/2+(net_charge<2)*2.2,net_charge/2),label=ifelse(net_charge!=0,paste("$",format(ctax-oba*price,nsmall=2),"/MWh",sep=""),NA)),
            colour="black", family="Times", size=rel(2),show.legend = FALSE)+
  
  scale_fill_manual("",values=c("grey70","white"),
                    breaks =c("Value of output-based allocation of emissions credits ($/MWh)",
                              "Residual carbon charge ($/MWh)") )+
  facet_wrap(~policy)+
  theme_minimal()+theme(
    panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
    legend.title=element_blank(),
    legend.position="bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.spacing.x = unit(0.2, 'cm'),
    legend.spacing.y = unit(0.2, 'cm'),
    legend.text = element_text(colour="black", size = 8, face = "bold"),
    plot.caption = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size =8, face = "italic"),
    text=element_text(family="Times", face="bold", size=8),
    #text = element_text(family=windowsFont("Times"),size = 8,face = "bold"),
    #axis.text.x = element_text(angle = 90,hjust=0.5,vjust=0.5),
    #axis.title = element_text(size = 8,face = "bold", colour="black"),
    panel.spacing = unit(.75, "lines"))+    
   labs(x=NULL,y="Value of output-based allocations and residual carbon charges ($/MWh)",
       #title="Electricity Generation and GHG Emissions by Province (2020)",
       #subtitle=paste("2022 National Inventory (2020 emissions)",sep=""),
       #caption=str_wrap("Source: Environment and Climate Change Canada 2022 National Inventory. Graph by @andrew_leach.",width = 180),
       NULL
  )

ggsave("images/ccir_ggppa_ctf.png",width = 7.5,height=4.5,dpi=300,bg="white")




#adding on graph with Ben's data
mark<-read_excel("bens_data.xlsx",sheet="Sheet4") %>% as_tibble()%>%clean_names()

mark<-mark %>% mutate(
  cat=factor(cat,levels = unique(cat))
)

ggplot(mark)+
  geom_col(aes(cat,val))


#"actual"                                                    "legislated_milestones"                                    
  
  #getting national communication info
  library(tabulizer)
download.file("https://www.aeso.ca/assets/listedfiles/AESO-2017-Long-term-Outlook.pdf",destfile="aeso_2017_lto.pdf",mode="wb")
f <- "aeso_2017_lto.pdf"

# extract tables from only second page
aeso_table<-data.frame(extract_tables(f, pages = 30)[1],stringsAsFactors = F)
aeso_table %>% write_csv("aeso_table.csv")

download.file("https://www.aeso.ca/assets/Uploads/AESO-2019-LTO-updated-10-17-19.pdf",destfile="aeso_2019_lto.pdf",mode="wb")
f <- "aeso_2019_lto_t.pdf"

# extract tables from only second page
aeso_table<-data.frame(extract_tables(f, pages = 1)[1],stringsAsFactors = F)
aeso_table %>% write_csv("aeso_table_2019.csv")

download.file("https://www.aeso.ca/assets/Tariff-2021-BR-Application/Appendix-K-AESO-2021-Long-term-Outlook.pdf",destfile="aeso_2021_lto.pdf",mode="wb")
f <- "aeso_2021_lto.pdf"

# extract tables from only second page
aeso_table<-data.frame(extract_tables(f, pages = 27)[1],stringsAsFactors = F)
aeso_table %>% write_csv("aeso_table_2021.csv")


npri_rep<-read_excel("../NPRI/NPRI_Report.xlsx")
npri_rel<-read_excel("../NPRI/npri_releases.xlsx")

npri_rep<-npri_rep %>% clean_names()
npri_rel<-npri_rel %>% clean_names()

merged <- npri_rel %>%
  left_join(npri_rep,c("npri_id_no_inrp"="npri_id",
                             "reporting_year_annee"="report_year"))


power<- merged%>% filter(grepl("ectric",naics_title_titre_code_scian),province=="AB")%>% group_by(npri_id_no_inrp,reporting_year_annee,facility_name_installation)%>% 
  summarize(numof_employees=unique(numof_employees))



sands<- merged%>% filter(grepl("il sands",naics_title_titre_code_scian),province=="AB")%>% group_by(npri_id_no_inrp,reporting_year_annee,facility_name_installation)%>% 
  summarize(numof_employees=unique(numof_employees))

suncor<- merged%>% filter(grepl("Suncor",company_name_denomination_sociale_de_lentreprise),province=="AB")%>% group_by(npri_id_no_inrp,reporting_year_annee,facility_name_installation)%>% 
  summarize(numof_employees=unique(numof_employees))


fh<- merged%>% filter(grepl("ort",facility_name_installation),province=="AB")%>% group_by(npri_id_no_inrp,reporting_year_annee,facility_name_installation)%>% 
  summarize(numof_employees=unique(numof_employees))



cscc<-read_csv("../book/cscc.csv")%>%clean_names()%>%
  filter(iso3 %in% c("CAN","WLD","USA"))

#test<-
  
  cscc %>% select(-n)%>%filter(dmgfuncpar=="bootstrap",ssp=="SSP2",climate=="uncertain")%>%
  pivot_wider(names_from=iso3,values_from=c(x16_70_percent,x50_percent,x83_30_percent))%>%
  group_by(climate)%>%
  arrange(x50_percent_WLD)%>%
  mutate(discount=ifelse(is.na(dr),"Ramsey rule time preference","Fixed (3%) discount rate"),
         rcp=stringr::str_to_upper(rcp),
         run=factor(run),
         run=fct_recode(run,
                        "BHM long-run (5-lag) , pooled model"="bhm_lr",
                        "BHM long-run, income-dependent model"="bhm_richpoor_lr",
                        "BHM short-run (no lag), pooled model"="bhm_sr",
                        "BHM short-run, income-dependent model"="bhm_richpoor_sr",
                        "Dell, Jones & Olken (2012) alternative impact function"="djo" ),
         
         )%>%
  mutate(n_can=rcp)%>%
    
    
  #arrange(x50_percent_USA)%>%
  #mutate(n_usa=row_number())%>%
  mutate(us_diff=x50_percent_USA-x50_percent_CAN)%>%
  ggplot()+
  geom_point(aes(n_can,x50_percent_CAN,fill="Canada",color="Canada"),size=2.5,shape=21)+
  #geom_errorbar(aes(n_can,ymin=x16_70_percent_CAN,ymax=x83_30_percent_CAN,color="Canada"),width=.5,linewidth=.45)+
    
    #geom_errorbar(aes(n_can,ymin=x16_70_percent_WLD,ymax=x83_30_percent_WLD,color="WLD"),size=.5)+
    geom_point(aes(n_can,x50_percent_WLD,fill="World",color="World"),size=2.5,shape=21)+
    
    #geom_errorbar(aes(n_can,ymin=x16_70_percent_USA,ymax=x83_30_percent_USA,color="USA"),width=.5,linewidth=.45)+
    geom_point(aes(n_can,x50_percent_USA,fill="USA",color="USA"),size=2.5,shape=21)+
    facet_grid(rows=vars(discount),cols = vars(run),labeller = label_wrap_gen(width=30)) +
    #facet_grid(rows=vars(discount),cols = vars(run),labeller =label_parsed(labels, multi_line = TRUE))+
    scale_fill_manual("",values=c("white","#002868","lightblue"))+
    scale_color_manual("",values=c("red","red","darkblue"))+
    theme_minimal()+
    #theme(axis.text.x = element_text(angle=90))
    labs(x=NULL,y="Country-level social cost of carbon ($/tonne)",
         #title="Average weekly earnings by industry, monthly, unadjusted for seasonality",
         #subtitle=paste("2022 National Inventory (2020 emissions)",sep=""),
         caption=str_wrap("Source: Ricke, K., Drouet, L., Caldeira, K. et al. Country-level social cost of carbon. Nature Climate Change 8, 895–900 (2018)",width = 180),
         NULL
    )
    NULL
  ggsave("../book/images/cscc_can_us.png",width = 9.5,height=6.5,dpi=220,bg="white")
  

