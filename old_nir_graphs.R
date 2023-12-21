
ctf_graph<-  
  ggplot(cdn_data%>%filter(!grepl('Additional', scenario))%>%filter(!grepl("NIR",scenario)),aes(x=year))+
  
  geom_line(data=filter(cdn_data,grepl('2022 Reference', scenario)),aes(year,emissions),color="black",lty="11",size=1.45)+
  geom_line(data=filter(cdn_data,grepl('NIR', scenario)),aes(year,emissions),color="black",size=1.45)+
  #scale_linetype_manual("",values=c("solid","31"))+
  geom_point(data=targets,aes(Year,target),size=3,colour=palette[9])+
  geom_point(aes(2000,588.6),size=3,colour=palette[9])+ #rio target
  scale_color_manual("",values=palette[-1])+
  annotate("text",x=1990+(2019-1990)/2,y=790,label="National Inventory Emissions (1990-2021)",
           color="black",fontface="bold",hjust=0.5,family= theme_get()$text[["family"]], 
           size= theme_get()$text[["size"]]/scale)+
  #Rio 
  annotate("text",x=2000,y=588.6-y_shift,label="Rio Target\n(1990 levels by 2000)",
           colour=palette[9],fontface="bold",hjust=0.5,vjust=1,family= theme_get()$text[["family"]], 
           size= theme_get()$text[["size"]]/scale)+
  #kyoto
  #geom_point(aes(2010,565),size=5,colour=palette[9])+ #kyoto target
  geom_errorbarh(aes(xmin=2008,y=588.6*.94,xmax=2012),height=40,size=1.2,colour=palette[9])+
  annotate("text",x=2012.5,y=588.6*.94,label="Kyoto Target (6% below 1990 levels, 2008-12)",
           colour=palette[9],fontface="bold",hjust=0,family= theme_get()$text[["family"]], 
           size= theme_get()$text[["size"]]/scale)+
  #copenhagen
  annotate("text",x=2012,y=732.218*.83,label="Copenhagen Target\n(17% below 2005 levels by 2020)\n",
           colour=palette[9],fontface="bold",hjust=0.5,vjust=0.5,family= theme_get()$text[["family"]], 
           size= theme_get()$text[["size"]]/scale)+
  #paris
  annotate("text",x=2029,y=732.218*.7,label="Paris Target (30% below 2005 levels by 2030)",
           colour=palette[9],fontface="bold",hjust=1,family= theme_get()$text[["family"]], 
           size= theme_get()$text[["size"]]/scale)+
  #glasgow
  #geom_point(aes(2030,423),size=5,colour=palette[9])+ #glasgow target
  geom_errorbar(aes(x=2030,ymax=732.218*.6,ymin=732.218*.55),width=1.2,size=1.2,colour=palette[9])+
  annotate("text",x=2029,y=732.218*.575,label="Glasgow Target (40-45% below 2005 levels by 2030)",
           colour=palette[9],fontface="bold",hjust=1,family= theme_get()$text[["family"]], 
           size= theme_get()$text[["size"]]/scale)+
  #2050
  annotate("text",x=2049,y=-15,label="2050 Net Zero Goal",
           colour=palette[9],fontface="bold",hjust=1,
           family= theme_get()$text[["family"]], 
           size= theme_get()$text[["size"]]/scale)+
  
  geom_hline(yintercept=0,size=1)+
  #scale_y_continuous(limit=c(-10,825),expand = c(0,0))+
  scale_x_continuous(limit=c(1990,2052),breaks=seq(1990,2050,10),expand = c(0,0))+
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
    text=element_text(family="Times", face="bold", size=9),
    #text = element_text(family=windowsFont("Times"),size = 8,face = "bold"),
    #axis.text.x = element_text(angle = 90,hjust=0.5,vjust=0.5),
    #axis.title = element_text(size = 8,face = "bold", colour="black"),
    panel.spacing = unit(.75, "lines"))+    
  theme(legend.position = "none")+
  labs(x="",y=expression('Annual Emissions '*'(MtCO'[2]*'e)'),
  )+
  coord_cartesian(clip = 'off')+
  annotate("text",x=2036,y=625,label="2022 ECCC\nReference Case (2020-2035)",color="black",fontface="bold",hjust=0,vjust=0.5,
           family= theme_get()$text[["family"]], 
           size= theme_get()$text[["size"]]/scale)
ggsave(filename = "images/emissions_and_targets_ctf.png",plot=ctf_graph,bg="white",dpi=300,width=7.5,height=4.5)
