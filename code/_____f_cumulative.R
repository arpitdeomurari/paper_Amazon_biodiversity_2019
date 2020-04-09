# load cumulative fire data
my_fire_type = "cumu"
df_burn <- read.csv("MODIS_burntArea/99_summary/final_fire_spei_year_eco.csv")
df_burn$year <- as.numeric(df_burn$year)
df_burn_az <- subset(df_burn,fire_type==my_fire_type & id_country=="amazon")
df_active <- read.csv("MODIS_activefire/99_summary/final_fire_spei_year_eco.csv")
df_active$year <- as.numeric(df_burn$year)
df_active_az <- subset(df_active,fire_type==my_fire_type & id_country=="amazon")
fire <- data.frame(year=df_active_az$year,
                   N_km_af=df_active_az$N_km,
                   N_km_ba=df_burn_az$N_km)
plot(df_active_az$year,df_active_az$N_km)
points(df_burn_az$year,df_burn_az$N_km,col="red")


# load plant spp num
plant_af <- read.csv("ranges/plant_num_burntArea_activeFire.csv")
plant_af <- subset(plant_af,country=="amazon"& fire=="activeFire")
plant_ba <- read.csv("ranges/plant_num_burntArea_activeFire.csv")
plant_ba <- subset(plant_ba,country=="amazon"& fire=="burntArea")
names(plant_af)[1] <- "sppN_af"
names(plant_ba)[1] <- "sppN_ba"
plant <- merge(plant_af,plant_ba,by=names(plant_ba)[-c(1,5)])
plant$sppN_max = apply(plant[,c(4,6)],1,max)
plant$sppN_min = apply(plant[,c(4,6)],1,min)

####### plot for plants
my_factor= 70
ggplot(fire, aes(year)) +
  # for fire numbers
  geom_ribbon( aes(ymin=N_km_ba,ymax=N_km_af), fill="black", alpha="0.5")+
  geom_line(aes(y=N_km_ba),col="black")+
  geom_line(aes(y=N_km_af),col="black")+
  
  # for species numbers
  geom_ribbon(data=subset(plant,pct!="0"),
              aes(x=year,
                  ymin=sppN_min*my_factor ,
                  ymax=sppN_max*my_factor,
                  fill=factor(pct) ),
              size = 2) +
  geom_line(data=subset(plant,pct!="0"),aes(y=sppN_min*my_factor, color=factor(pct)  ) )+
  geom_line(data=subset(plant,pct!="0"),aes(y=sppN_max*my_factor, color=factor(pct)  ) )+
  scale_y_continuous("area", sec.axis = sec_axis(~./my_factor,name = "spp number")) +
  scale_x_continuous("year", breaks = 2002:2019) +
  ggtitle("combined plot")


# load vertebrate, active fire
vert_af <- read.csv("ranges/vert_num_activefire.csv")
vert_af <- subset(vert_af,country=="amazon")
vert_ba <- read.csv("ranges/vert_num_burntArea.csv")
vert_ba <- subset(vert_ba,country=="amazon")
names(vert_af)[1] <- "sppN_af"
names(vert_ba)[1] <- "sppN_ba"
vert <- merge(vert_af,vert_ba,by=names(vert_af)[-1])
vert$sppN_max = apply(vert[,c(5,6)],1,max)
vert$sppN_min = apply(vert[,c(5,6)],1,min)
vert$sppN_max - vert$sppN_min


####### plot for vertebrates
my_factor= 350   # change this to rescale the y axis
ggplot(fire, aes(year)) +
  # for fire numbers
  geom_ribbon( aes(ymin=N_km_ba,ymax=N_km_af), fill="black", alpha="0.5")+
  geom_line(aes(y=N_km_ba),col="black")+
  geom_line(aes(y=N_km_af),col="black")+
  
  # for species numbers
  geom_ribbon(data=subset(vert,pct!="0"),
              aes(x=year,
                  ymin=sppN_min*my_factor ,
                  ymax=sppN_max*my_factor,
                  fill=factor(pct) ),
              size = 2) +
  geom_line(data=subset(vert,pct!="0"),aes(y=sppN_min*my_factor, color=factor(pct)  ) )+
  geom_line(data=subset(vert,pct!="0"),aes(y=sppN_max*my_factor, color=factor(pct)  ) )+
  scale_y_continuous("area", sec.axis = sec_axis(~./my_factor,name = "spp number")) +
  scale_x_continuous("year", breaks = 2002:2019) +
  ggtitle("combined plot")

