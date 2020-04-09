library(raster)
library(foreach)
library(doParallel)

rasterOptions(tmpdir="d:/fx_temp/")
lcs <- stack(list.files("data_organized/3_landcover/output_all_merged/",pattern="landcover_A.*.tif",full.names = T))
catf <- function(...,file="z_log2.txt"){
  cat(...,file=file,append=TRUE)
}
myyears <- 2001:2019
mymonth <- c(paste0(0,1:9),10,11,12)
yearmonth <- expand.grid(mymonth,myyears)
yearmonth$yearmonth <- paste0(yearmonth$Var2,yearmonth$Var1)
files <- paste0("data_organized/12_activefire/0_raw/BA",yearmonth$yearmonth,".tif")

i=1
i=171
cup_num=6
cl<-makeCluster(cup_num)
registerDoParallel(cl)
foreach(i= 1:length(files),
       .packages=c("raster"),
       .export=c("catf")   )   %dopar%  {
              catf(paste(i,"mask\n"))
              ama <- shapefile("data_organized/0_amazonBasin/amazonBasin.shp")
              one_raster <- raster(files[i])
              one_raster_ama <- crop(one_raster,extent(ama))
              one_raster_ama <- mask(one_raster_ama,ama)
              
              one_path <- paste0("data_organized/12_activefire/3_maskAmazon/",yearmonth$yearmonth[i],".tif")
              writeRaster(one_raster_ama,one_path ,overwrite=TRUE  )
}              
stopCluster(cl)


for( i in c(2001:2019)){
  ba <- stack(list.files("data_organized/12_activefire/3_maskAmazon/",pattern=as.character(i),full.names = T))
  
  ba_year <- ( ba[[1]]==1 | ba[[2]]==1 | ba[[3]]==1 | ba[[4]]==1 | ba[[5]]==1 | ba[[6]]==1 | ba[[7]]==1| ba[[8]]==1| ba[[9]]==1| ba[[10]]==1| ba[[11]]==1| ba[[12]]==1 )
  one_path <- paste0("data_organized/12_activefire/3_maskAmazon/",i,"_year.tif")
  writeRaster(ba_year,one_path ,overwrite=TRUE  )
}


rasterOptions(tmpdir="d:/fx_temp/")
catf <- function(...,file="z_log2.txt"){
  cat(...,file=file,append=TRUE) }
lcs <- stack(list.files("data_organized/3_landcover/output_all_merged/",pattern="landcover_A.*.tif",full.names = T))
cup_num=4
cl<-makeCluster(cup_num)
registerDoParallel(cl)
foreach(i= c(2001:2019)  ,
        .packages=c("raster"),
        .export=c("catf")   )   %dopar%  {
  ba_year <- raster(paste0("data_organized/12_activefire/3_maskAmazon/",i,"_year.tif"))
  one_raster_ama_prj  <- projectRaster(from=ba_year,to=lcs[[1]],method="ngb")
  one_path <- paste0("data_organized/12_activefire/4_maskAmazon_prj/",i,"_year.tif")
  writeRaster(one_raster_ama_prj,one_path ,overwrite=TRUE  )
}
stopCluster(cl)

library(raster)
i=2017
for(i in c(2002:2019) ){
  print(i)
  one_fire <- raster(paste0("data_organized/12_activefire/4_maskAmazon_prj/",i,"_year.tif"))
  one_lc <- raster(paste0("data_organized/3_landcover/output_all_merged/landcover_A",i-1,".tif"))
  where_fire <- which(values(one_fire)==1)
  values(one_fire)[where_fire] <- values(one_lc)[where_fire]
  writeRaster(one_fire,paste0("data_organized/12_activefire/5_maskAmazon_prj_landCoverValue/",as.character(i),"_year.tif")  ,overwrite=TRUE )
  one_freq <- table(values(one_lc)[where_fire])
  one_freq <- data.frame(one_freq)
  one_freq$year <- i
  write.csv(one_freq,paste0("data_organized/12_activefire/5_maskAmazon_prj_landCoverValue/",as.character(i),"_year.csv")   )
  gc()
}


library(foreach)
library(doParallel)
library(raster)
rasterOptions(tmpdir="d:/fx_temp/")
catf <- function(...,file="z_log2.txt"){
  cat(...,file=file,append=TRUE) }
lcs <- stack(list.files("data_organized/3_landcover/output_all_merged/",pattern="landcover_A.*.tif",full.names = T))
ama <- shapefile("data_organized/0_amazonBasin/amazonBasin.shp")
ama_prj <- spTransform(ama,crs(lcs[[1]]))
plot(lcs[[1]])
plot(ama_prj,add=T)
lcs_ama_template <- crop(lcs[[1]],extent(ama_prj))
plot(lcs_ama_template)
plot(ama_prj,add=T)

library(stringr)
all_month <- str_pad(1:12, 2, pad = "0")

i=2001;j="01"
for(i in 2001:2019){
  print(i)
cup_num=2
cl<-makeCluster(cup_num)
registerDoParallel(cl)
foreach(j = all_month  ,
       .packages=c("raster"),
       .export=c("catf")   )   %dopar%  {
          rasterOptions(tmpdir="d:/fx_temp/")
          this_label <- paste0(i,j)
          catf(this_label)
          print(this_label)
        ba_year <- raster(paste0("data_organized/12_activefire/3_maskAmazon/",this_label,".tif"))
          one_raster_ama_prj  <- projectRaster(from=ba_year,
                                               #to=lcs[[1]],
                                               to=lcs_ama_template,
                                               method="ngb")
          one_path <- paste0("data_organized/12_activefire/4_maskAmazon_prj_quick/",this_label,".tif")
          writeRaster(one_raster_ama_prj,one_path ,overwrite=TRUE  )
        }
stopCluster(cl)
}

library(raster)
library(stringr)
all_month <- str_pad(1:12, 2, pad = "0")
lc_temp <- raster(paste0("data_organized/3_landcover/output_all_merged/landcover_A",2018,".tif"))
ama <- shapefile("data_organized/0_amazonBasin/amazonBasin.shp")
ama_prj <- spTransform(ama,crs(lc_temp))

i=2002;j="01"
for(i in c(2002:2019) ){
  one_lc_whole <- raster(paste0("data_organized/3_landcover/output_all_merged/landcover_A",i-1,".tif"))
  one_lc <- crop(one_lc_whole,extent(ama_prj))
  for( j in all_month){
  print(i)
    this_label <- paste0(i,j)
  one_fire <- raster(paste0("data_organized/12_activefire/4_maskAmazon_prj_quick/",this_label,".tif"))
  where_fire <- which(values(one_fire)==1)
  values(one_fire)[where_fire] <- values(one_lc)[where_fire]
  writeRaster(one_fire,paste0("data_organized/12_activefire/5_maskAmazon_prj_landCoverValue/",this_label,".tif")  ,overwrite=TRUE )
  one_freq <- table(values(one_lc)[where_fire])
  one_freq <- data.frame(one_freq)
  one_freq$year <- i
  write.csv(one_freq,paste0("data_organized/12_activefire/5_maskAmazon_prj_landCoverValue/",this_label,".csv")   )
  gc()
  }
}
