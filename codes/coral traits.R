library(tidyverse)
library(patchwork)
library(leaflet)
library(ggmap)
library(egg)

theme_set(theme_bw())
theme_update(
  panel.grid = element_blank(),
  strip.background = element_rect(fill = 'orange')
)

#To use ggmap...
register_stadiamaps("8edd83d3-efff-409c-9e5f-fac579653f94", write = TRUE)



setwd("C:/Users/mihir/Documents/coral_popgen")


#Filter the dominant species.families (more than 100 observations) and their geographic 
#distributions
trdat=read.csv("ctdb_1.1.1_data.csv")%>%
      as_tibble()%>%
      mutate(family=str_split_i(specie_name," ",1))%>%
      filter(family %in% c("Acropora","Porites","Pocillopora",
                       "Stylophora"))

splist=trdat%>%
      count(specie_name)%>%
      filter(n>100)%>%
      pull(specie_name)

trdat=trdat%>%
      filter(specie_name %in% splist)

trlist=unique(trdat$trait_name)
      

#Plot Geographic distributions of species

trdat%>%
        select(specie_name,latitude,longitude)%>%
        ggplot(aes(longitude,latitude,col=specie_name))+
        geom_point(show.legend = FALSE)+
        facet_wrap(vars(family))

#Select symbiodinium related traits
dat=trdat%>%
  filter(trait_name %in% 
           c("Symbiodinium subclade"))%>%
  select(specie_name,
         location_name,
         trait_name,
         value)

dat%>%
  group_by(specie_name,location_name,trait_name)%>%
  count(value)%>%
  ungroup()%>%
  select(-trait_name)%>%
  rename("subclade"=value)%>%
  rename("frequency"=n)%>%
  ggplot(aes(fill=subclade,x=specie_name,y=frequency))+
  geom_bar(position="stack", stat="identity")+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90))


###################################################################
# Analyse Craig's Fst data

#load files
path1="D:/Project files/coral/data/Mihir"



#cluster data
clusdat=read.table(paste0(path1,"/","Qmatrix-K6-BestCE.txt"))%>%
        as_tibble()

names(clusdat)[1:6]=c("1","2","3","4","5","6")

clusdat_raw=clusdat%>%
  pivot_longer(cols=1:6,names_to = "clusters",values_to = "abun")

#Location coordinates
locs=tibble(Site=unique(clusdat$Site),
            lat=c(27.36,16.87,27.20,22.20,22.26,22.03,23.41,20.14,16.50,26.26,24.00),
            lon=c(35.66,41.79,50.00,38.51,39.00,38.45,58.31,40.26,42.10,36.43,37.90))%>%
    arrange(desc(lat))

clusdat=clusdat_raw%>%inner_join(locs)%>%
        filter(lon<45)
locs=locs%>%filter(lon<45)

rx=range(locs$lon)
ry=range(locs$lat)

#Get the geographical map
map=get_stadiamap( bbox = c(left =rx[1]-2, bottom = ry[1]-2, right = rx[2]+2, top = ry[2]+2), 
                   zoom = 6, maptype = "stamen_toner_lite")


mainmap=ggmap(map)+
  geom_point(data=locs,color="red",size=2)

#setwd("D:/Project files/coral/Presentations/figs")

mainmap
tiff("basemap.tiff", width = 4, height = 4, units = "in", res = 300)
dev.off()


finmap=mainmap


#Create inset maps of genotype frequeny for each location
for (i in c(1,3,5,7,9)){
  dat=clusdat%>%
        filter(Site==locs$Site[i])%>%
        mutate(clusters=as.numeric(clusters))%>%
        group_by(clusters)%>%
        summarize(freq=mean(abun),
                  sd=sd(abun))%>%
        ungroup()%>%
        ggplot(aes(x=clusters,y=freq))+
        geom_line()+
        geom_ribbon(aes(ymin=freq-sd,ymax=freq+sd),alpha=0.2)+
        theme(axis.title = element_blank(), 
              axis.text = element_blank(), 
              axis.ticks = element_blank())
  
  xs=(locs$lon[i]-rx[1]+2)/(diff(rx)+4)
  ys=(locs$lat[i]-ry[1]+2)/(diff(ry)+4)
  
  finmap=finmap+inset_element(dat,xs,ys,xs+0.2,ys+0.1)
  
  #assign(paste0("map",i),dat)
  
#left = 32, bottom = 10, right = 60, top = 35

}

finmap
tiff("genomap.tiff", width = 4, height = 4, units = "in", res = 300)
dev.off()

clusdat%>%
  mutate(clusters=as.numeric(clusters))%>%
  group_by(Site,clusters)%>%
  summarize(freq=mean(abun),
            sd=sd(abun))%>%
  ungroup()%>%
  ggplot(aes(x=clusters,y=freq))+
  geom_point()+
  geom_line()+
  geom_ribbon(aes(ymin=freq-sd,ymax=freq+sd),alpha=0.2)+
  facet_wrap(~Site)

clusdat%>%
  mutate(clusters=as.numeric(clusters))%>%
  ggplot(aes(x=clusters,y=abun,col=Ind))+
  geom_line()+
  theme( legend.position = "none" )+
  facet_wrap(~Site)


files=list.files(path1)[grep(".FsT.weir",list.files(path1))]

fstdat=tibble()

for(i in 1:length(files)){
  
  dat=read.table(paste0(path1,"/",files[i]))%>%as_tibble()
  
  dat=dat[-1,]
  
  dat=dat%>%mutate(loc=files[i]%>%str_remove("_FsT.weir.fst"))
  
  fstdat=fstdat%>%bind_rows(dat)

  
}

fstdat=fstdat[-1]

names(fstdat)=c("ind","fst","loc")



