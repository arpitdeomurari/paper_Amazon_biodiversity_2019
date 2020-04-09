library(data.table)
library(raster)
library(ggplot2)

#### calculate residuals for each month, based on 2009-2018.
final <- read.csv("MODIS_burntArea/99_summary/final_fire_spei09_month_eco.csv")

# OR
final <- read.csv("MODIS_activefire/99_summary/final_fire_spei09_month_eco.csv")


final <- subset(final,fire_type=="new" & year.y!=2002 )
head(final)
library(ggplot2)
policy_threshold = 2008
d2 <- subset(final,  year.y>policy_threshold & year.y <=2018)
d_all <- final
library(ggrepel)
df_special <- final
df_special$policy <- NA
df_special$policy[df_special$year.y<=policy_threshold] <-"no"
df_special$policy[df_special$year.y>policy_threshold] <-"yes"
df_special$policy[df_special$year.y==2019] <-"new"
ggplot(df_special, aes(my_mean,N_km,label=year.y,color=policy))+
  geom_point()+
  geom_smooth(method="lm",se = F)+ 
  geom_label_repel()+
  scale_color_manual(values=c("blue","red3","black"),guide=FALSE)+
  facet_wrap(~month,scales = "free")+
  labs(x="Drought condition",y=bquote('Burnt area ('~km^2*')'))+
  theme_bw()

#do a loop to get residuals in each month
df_residual <- vector()
for(this_month in unique(d2$month)){
  d2_month <- subset(d2,month==this_month)
  d_all_month <- subset(d_all,month==this_month)
  model_month <- lm(data=d2_month,N~my_mean)
  one_summary <- summary(model_month)
  one_r <- one_summary$r.squared
  one_p <- one_summary$coefficients[2,4]
  d_all_month$N_predict <- predict(model_month,d_all_month)
  d_all_month$N_residual <-  d_all_month$N - d_all_month$N_predict
  df_residual <- rbind(df_residual,d_all_month)
}

df_residual2 <- data.table(df_residual)
df_residual2$policy <- "no"
df_residual2$policy[df_residual2$year.y<=2008] <- "no"
df_residual2$policy[df_residual2$year.y>=2009] <- "yes"
df_residual2$policy[df_residual2$year.y==2019] <- "new"

df_residual_mean <- df_residual2[,.(N_residual_mean= mean(N_residual),
                                    N_residual_sd= sd(N_residual)),
                                 by=.(month,policy)]
df_residual$N_residual_km <- df_residual$N_residual *(0.4633127^2)
df_residual_mean$N_residual_mean_km <- df_residual_mean$N_residual_mean*(0.4633127^2)
ggplot(df_residual_mean,aes(month,N_residual_mean_km ))+
  geom_line(data=subset(df_residual_mean,policy!="yes"),
            aes(group=policy,col=policy),size=1  )+ 
  scale_color_manual(values = c("blue","red3",NA))+
  geom_line(data=subset(df_residual_mean,policy=="yes"),
            aes(month,N_residual_mean_km ,group=1),
            linetype="dashed",size=1  )+
  geom_line(data=subset(df_residual,year.y<=2008),aes(month,N_residual_km,group=year.y) ,col="red" ,alpha=0.2)+
  geom_line(data=subset(df_residual,year.y>=2009 &year.y<=2018 ),aes(month,N_residual_km,group=year.y) ,col="gray60",alpha=0.3 )+
  theme_bw()+xlab("Month")+ylab(bquote('Burnt area not explained by drought ('~km^2*')'))



