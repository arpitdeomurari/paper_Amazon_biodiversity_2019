library(gdalUtils)
allyear <- paste0("A",2001:2018)
y = 18
for(y in 1:length(allyear)){
  this_year <- allyear[y]
  files <- dir("data_organized/3_landcover/download_all/",pattern = paste0(this_year,".*.hdf"),full.names = T)
  #print(length(files))
  files_out <- gsub(".hdf",".tif",files)
  files_out <- gsub("download_all/","output_all/",files_out)
  i <- 1
  library(foreach)
  library(doParallel)
  cl<-makeCluster(6)
  registerDoParallel(cl)
  foreach(i = 1:length(files),
          .packages=c("gdalUtils","raster") ) %dopar% {
    sds <- get_subdatasets(files[i])
    gdal_translate(sds[1], dst_dataset = files_out[i])
  }
  stopCluster(cl)
  files_tif <- dir("data_organized/3_landcover/output_all/",pattern = paste0(this_year,".*.tif"),full.names = T)
  i=1
  all_ly <- list()
  for(i in 1:length(files_tif)){
    one_ly <- raster(files_tif[i])
    all_ly[i] <- one_ly
  }
  #all_one <- mosaic(all_ly[[1]],all_ly[[2]],fun=max)
  names(all_ly)[1:2] <- c('x', 'y')
  all_ly$fun <- max
  all_ly$na.rm <- TRUE
  all_one <- do.call(mosaic, all_ly)
  plot(all_one)
  #table(values(all_one))
  writeRaster(all_one,filename = paste0("data_organized/3_landcover/output_all_merged/landcover_",this_year,".tif"),overwrite=TRUE  )
}

### look at land cover within Amazon
library(raster)
lccc <- stack(list.files("data_organized/3_landcover/output_all_merged/",full.names=T,pattern="landcover"))
ama <- shapefile("data_organized/0_amazonBasin/amazonBasin.shp")
ama_prj <- spTransform(ama,crs(lccc[[1]]))

lccc_ama <- crop(lccc,extent(ama_prj))
lccc_ama <- mask(lccc_ama,ama_prj)
writeRaster(lccc_ama,bylayer=TRUE,
            paste0("data_organized/3_landcover/output_all_merged_ama/",
                   names(lccc_ama),
                   ".tif"))
j=1
for(j in 1 : nlayers(lccc_ama)  ){
  print(j)
  temp1 <- values(lccc_ama[[j]])
  dt <- data.table(value=temp1)
  one_freq <- dt[,.N,by=value]
  names(one_freq)[2] <- names(lccc_ama)[j]
  if(j==1){
    all_freq=one_freq
  } else{
    all_freq <- merge(all_freq,one_freq,by="value")
  }
}
saveRDS(all_freq,file="data_organized/3_landcover/output_all_merged_ama/all_freq.rds")

