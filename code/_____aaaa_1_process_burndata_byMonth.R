library(raster)
library(foreach)
library(doParallel)
rasterOptions(tmpdir="d:/fx_temp/")
lcs <- stack(list.files("data_organized/3_landcover/output_all_merged/",pattern="landcover_A.*.tif",full.names = T))
path_downloaded_pieces <- "data_organized/12_burntArea/1_burndata/Win"

catf <- function(...,file="z_log1.txt"){
  cat(...,file=file,append=TRUE)
}

myyears <- 2001:2019
mymonth <- c(paste0(0,1:9),10,11,12)
i=2001
for(i in myyears){
    temp_05 <- list.files(paste0( path_downloaded_pieces,
                                    "05","/",i,"/"),
                            pattern="burndate",
                            full.names = T)
    temp_06 <- list.files(paste0( path_downloaded_pieces,
                                  "06","/",i,"/"),
                          pattern="burndate",
                          full.names = T)
    temp_07 <- list.files(paste0( path_downloaded_pieces,
                                  "07","/",i,"/"),
                          pattern="burndate",
                          full.names = T)

    if( length(temp_05) == 12 & 
        length(temp_06) == 12 & 
        length(temp_07)  ){
      
      cup_num=6
      cl<-makeCluster(cup_num)
      registerDoParallel(cl)
      foreach(j= 1:12,
             .packages=c("raster"),
             .export=c("catf")   )   %dopar%  {
      #for(j in 1:12){ # for 12 month
        print(paste(i,j))
        catf(paste(i,j,"\n"))
        
        one_raster5 <- raster(temp_05[j])
        one_raster6 <- raster(temp_06[j])
        one_raster7 <- raster(temp_07[j])
        
        catf(paste(i,j,"mosic\n"))
        one_raster <- mosaic(one_raster5, one_raster6,one_raster7,
                             fun="max",tolerance =0.5)
        
        one_path <- paste0("data_organized/12_burntArea/12_burntArea/2_burndata_out_byMonth/2_mergeWindow/",i,mymonth[j],"_winMERGED567.tif")
        writeRaster(one_raster,one_path ,overwrite=TRUE  )
        
        catf(paste(i,j,"mask\n"))
        ama <- shapefile("data_organized/0_amazonBasin/amazonBasin.shp")
        one_raster_ama <- crop(one_raster,extent(ama))
        one_raster_ama <- mask(one_raster_ama,ama)
        
        one_path <- paste0("data_organized/12_burntArea/2_burndata_out_byMonth/3_maskAmazon/",i,mymonth[j],"_winMERGED567.tif")
        writeRaster(one_raster_ama,one_path ,overwrite=TRUE  )
        
        
        catf(paste(i,j,"project\n"))
        one_raster_ama_prj  <- projectRaster(from=one_raster_ama,to=lcs[[1]],method="ngb")
        one_path <- paste0("data_organized/12_burntArea/2_burndata_out_byMonth/4_maskAmazon_prj/",i,mymonth[j],"_winMERGED567.tif")
        writeRaster(one_raster_ama_prj,one_path ,overwrite=TRUE  )
        
      } # end of month loop
      stopCluster(cl)
    }# end of if
}

