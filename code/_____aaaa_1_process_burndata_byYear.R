library(raster)
library(foreach)
library(doParallel)
rasterOptions(tmpdir="d:/fx_temp/")

## in each window (05/06/07), there are 12 months, merge them to yearly layer
mywindow <- c("05","06","07")
myyears <- 2001:2019
path_downloaded_pieces <- "data_organized/12_burntArea/1_burndata/Win"
i=19
one_window = "05"
for(one_window in mywindow  ){
  print(one_window)
  
  cup_num=6
  cl<-makeCluster(cup_num)
  registerDoParallel(cl)
  foreach(i=1:length(myyears),
          .packages=c("raster") )   %dopar%  {
  #for(i in 1:length(myyears)){
    print(myyears[i])
    files <- list.files(paste0( path_downloaded_pieces,
                                one_window,"/",myyears[i]),
                        pattern="burndate",
                        full.names = T)
    print(length(files))
    
    if(length(files)==12){
      ba <- stack(files)
      ba_year <- ( ba[[1]]>0 | ba[[2]]>0 | ba[[3]]>0 | ba[[4]]>0 | ba[[5]]>0 | ba[[6]]>0 | ba[[7]]>0| ba[[8]]>0| ba[[9]]>0| ba[[10]]>0| ba[[11]]>0| ba[[12]]>0 )
      
    } else if (length(files)==6){
      ba <- stack(files)
      ba_year <- ( ba[[1]]>0 | ba[[2]]>0 | ba[[3]]>0 | ba[[4]]>0 | ba[[5]]>0 | ba[[6]]>0)
    }else {
      print("error, check if there is enough data")
    }
    saveRDS(ba_year,file=paste0("data_organized/12_burntArea/2_burndata_out/1_mergeMonth/",myyears[i],"_","win",one_window,"annual.rds")   )
          }
  stopCluster(cl)
}


## merge the threee windows for by year
################### merge by window
files5 <- list.files("data_organized/12_burntArea/2_burndata_out/1_mergeMonth/",pattern="win05",full.names = T)
files6 <- list.files("data_organized/12_burntArea/2_burndata_out/1_mergeMonth/",pattern="win06",full.names = T)
files7 <- list.files("data_organized/12_burntArea/2_burndata_out/1_mergeMonth/",pattern="win07",full.names = T)


cup_num=6
cl<-makeCluster(cup_num)
registerDoParallel(cl)
foreach(i=1:length(files5),
        .packages=c("raster") )   %dopar%  {
  one_raster5 <- readRDS(files5[i])
  one_raster6 <- readRDS(files6[i])
  one_raster7 <- readRDS(files7[i])
  
  one_raster <- mosaic(one_raster5, one_raster6,one_raster7,
                       fun="max",tolerance =0.5)
  
  one_path <- gsub("win06","winMERGED567",files6[i])
  one_path <- gsub("1_mergeMonth","2_mergeWindow",one_path)
  one_path <- gsub(".rds",".tif",one_path)
  writeRaster(one_raster,one_path ,overwrite=TRUE  )
}
stopCluster(cl)


# overlap between fire + landcover
library(raster)
fires <- stack(list.files("data_organized/12_burntArea/2_burndata_out/2_mergeWindow/",pattern="winMERGED567annual.tif",full.names = T))
names(fires)
ama <- shapefile("data_organized/0_amazonBasin/amazonBasin.shp")
fires_ama <- crop(fires,extent(ama))
fires_ama <- mask(fires_ama,ama)
plot(fires_ama[[1:2]])
writeRaster(fires_ama,
            filename =paste0("data_organized/12_burntArea/2_burndata_out/3_maskAmazon/",names(fires_ama),".tif"),
            bylayer=TRUE,overwrite=TRUE)

library(raster)
lcs <- stack(list.files("data_organized/3_landcover/output_all_merged/",pattern="landcover_A.*.tif",full.names = T))
files <- list.files("data_organized/12_burntArea/2_burndata_out/3_maskAmazon/",full.names = T,pattern="annual.tif")

for(i in  18:length(files) ){
  print(i)
          one_layer <- raster(files[i])
          one_layer_prj  <- projectRaster(from=one_layer,to=lcs[[1]],method="ngb")
          writeRaster(one_layer_prj,filename =gsub("3_maskAmazon","4_maskAmazon_prj",files[i])  ,overwrite=TRUE)
}


