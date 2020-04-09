########################
library(raster)
fires_ama_prj <- stack(list.files("data_organized/12_burntArea/2_burndata_out/4_maskAmazon_prj/",full.names = T,pattern="tif"))

lcs <- stack(list.files("data_organized/3_landcover/output_all_merged/",pattern="landcover_A.*.tif",full.names = T))

for(i in 2:nlayers(fires_ama_prj)){
  print(i)
  one_fire <- fires_ama_prj[[i]]
  one_lc <- lcs[[i-1]]
  
  #values(one_lc)
  where_fire <- which(values(one_fire)==1)
  values(one_fire)[where_fire] <- values(one_lc)[where_fire]
  writeRaster(one_fire,paste0("data_organized/12_burntArea/2_burndata_out/5_maskAmazon_prj_landCoverValue/",names(fires_ama_prj[[i]]),".tif")  ,overwrite=TRUE )
  
  one_freq <- table(values(one_lc)[where_fire])
  one_freq <- data.frame(one_freq)
  names(one_freq)[2] <- names(fires_ama_prj[[i]])
  write.csv(one_freq,paste0("data_organized/12_burntArea/2_burndata_out/5_maskAmazon_prj_landCoverValue/",names(fires_ama_prj[[i]]),".csv")   )
  gc()
}
