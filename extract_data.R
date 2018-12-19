library(sp)
library(raster)
library(gdalUtils)
library(RColorBrewer)

# ## C6 one tile multi-year
# args <- commandArgs()
# print(args)
# 
# lat <- as.numeric(substr(args[3],1,10))
# lon <- as.numeric(substr(args[3],11,20))
# print(lat)
# print(lon)

dat <- read.csv('/projectnb/modislc/users/mkmoon/coffefarm/data/coordi.csv')

for(ss in 1:5){
  
  lat <- -dat[ss,2]
  lon <- -dat[ss,3]
  
    
  source('~/R/Codes/latlong2MODIStile.R', echo=TRUE)
  sam <- latlong2tile(lat,lon)
  
  tile <- paste('h',sprintf("%02d",sam[1]),'v',sprintf("%02d",sam[2]),sep='')
  print(tile)
  
  # Shapefile for the site location
  lon <- as.numeric(lon)
  lat <- as.numeric(lat)
  geog_crs  <- CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")
  site_geog <- data.frame(lon,lat)
  xy        <- site_geog[,c(1,2)]
  pcpt      <- SpatialPointsDataFrame(coords=xy,data=site_geog,proj4string = geog_crs)
  sinu_crs  <- '+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs'
  pcpt      <- spTransform(pcpt,sinu_crs)
  
  ## Pheno-metric
  # C6
  phenomet <- matrix(NA,16,4)
  for(yy in 1:16){
    year <- 2000 + yy
    for(vari in c(2,5,6,8)){  
      path <- paste('/projectnb/modislc/users/mkmoon/LCD_C6/product/',year,sep='')
      sstr <- paste('*',tile,'*',sep='')
      file <- list.files(path=path,pattern=glob2rx(sstr),full.names=T)
      sub <- get_subdatasets(file)
      bb <- raster(sub[vari]) 
      
      e <- extent((bb@extent@xmin+((sam[4])-1)*463.3127),
                  (bb@extent@xmin+((sam[4])+4)*463.3127),
                  (bb@extent@ymax-((sam[3])+3)*463.3127),
                  (bb@extent@ymax-((sam[3])-2)*463.3127))    
      bb_sub <- crop(bb,e)
      temp <- bb_sub[(ceiling((bb_sub@extent@ymax - pcpt@bbox[2])/463.3127)-1):
                       (ceiling((bb_sub@extent@ymax - pcpt@bbox[2])/463.3127)+1),
                     (ceiling((pcpt@bbox[1] - bb_sub@extent@xmin)/463.3127)-1):
                       (ceiling((pcpt@bbox[1] - bb_sub@extent@xmin)/463.3127)+1)]
      temp[temp>30000] <- NA
      
      if(vari==2){
        phenomet[yy,1] <- as.Date(mean(temp,na.rm=T),origin='1970-1-1')  
      }else if(vari==5){
        phenomet[yy,2] <- as.Date(mean(temp,na.rm=T),origin='1970-1-1')
      }else if(vari==6){
        phenomet[yy,3] <- as.Date(mean(temp,na.rm=T),origin='1970-1-1')  
      }else{
        phenomet[yy,4] <- as.Date(mean(temp,na.rm=T),origin='1970-1-1')
      }
    }  
    print(yy)
  }
  
  colnames(phenomet) <- c('greenup','maturity','senescence','dormancy') 
  rownames(phenomet) <- 2001:2016
  
  # save data
  setwd('/projectnb/modislc/users/mkmoon/coffefarm/data/')
  save(phenomet,file=paste(dat[ss,1],'_phenometric',sep=''))  
}

####################################################
setwd('/projectnb/modislc/users/mkmoon/coffefarm/data/')
load('Fazenda Um_phenometric')
as.Date(phenomet[,1],origin='1970-1-1')
