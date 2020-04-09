library(raster)

lcs <- stack(list.files("data_organized/3_landcover/output_all_merged/",pattern="landcover_A.*.tif",full.names = T))

i=2002
for(i in 2002:2019){
  print(i)
  fires_ama_prj <- stack(list.files("data_organized/12_burntArea/2_burndata_out_byMonth/4_maskAmazon_prj/",full.names = T,pattern=as.character(i)))
  print(names(fires_ama_prj))
  
  one_lc <- lcs[[i-2000-1]]
  print(names(one_lc))
  
  for(j in 1:nlayers(fires_ama_prj)){
    one_fire <- fires_ama_prj[[j]]
    print(names(one_fire))
    
    #values(one_lc)
    where_fire <- which(values(one_fire)>0)#here the value is the date of fire
    values(one_fire)[where_fire] <- values(one_lc)[where_fire]
    writeRaster(one_fire,paste0("data_organized/12_burntArea/2_burndata_out_byMonth/5_maskAmazon_prj_landCoverValue/",names(fires_ama_prj[[j]]),".tif")  ,overwrite=TRUE )
    
    one_freq <- table(values(one_lc)[where_fire])
    one_freq <- data.frame(one_freq)
    names(one_freq)[2] <- names(fires_ama_prj[[j]])
    write.csv(one_freq,paste0("data_organized/12_burntArea/2_burndata_out_byMonth/5_maskAmazon_prj_landCoverValue/",names(fires_ama_prj[[j]]),".csv")   )
    gc()
  }
  
}

