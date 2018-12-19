latlong2tile <- function(lat,long){
  
  path <-'/usr3/graduate/mkmoon/Codes/tilemap3_r4_0'
  
  if(length(lat)==1){
    
    out <- system(paste(path,'/tilemap3_linux is_h h fwd tp ',lat,' ',long,sep=''),intern=T)
        
    s1 <- sub('.*horiz tile ','',out)
    s2 <- sub(' line.*','',s1)
    h_tile <- as.numeric(sub(' .*','',s2))
    
    s1 <- sub('.*vert tile ','',out)
    s2 <- sub(' horiz.*','',s1)
    v_tile <- as.numeric(sub(' .*','',s2))
      
    s1 <- sub('.*line ','',out)
    s2 <- sub(' samp.* ','',s1)
    line <- as.numeric(sub(' .*','',s2))
    samp <- as.numeric(sub('.* ','',s2))
    
    tile_out <- cbind(h_tile,v_tile,line,samp)
    colnames(tile_out) <- c('h_tile','v_tile','line','samp')
    
    return(tile_out)  
  
  }else{
    tile_out <- matrix(NA,length(lat),4)
    colnames(tile_out) <- c('h_tile','v_tile','line','samp')
    
    for(i in 1:length(lat)){
      
      la <- lat[i]
      lo <- long[i]
      
      out <- system(paste(path,'/tilemap3_linux is_h h fwd tp ',la,' ',lo,sep=''),intern=T)
            
      s1 <- sub('.*horiz tile ','',out)
      s2 <- sub(' line.*','',s1)
      h_tile <- as.numeric(sub(' .*','',s2))
      
      s1 <- sub('.*vert tile ','',out)
      s2 <- sub(' horiz.*','',s1)
      v_tile <- as.numeric(sub(' .*','',s2))
      
      s1 <- sub('.*line ','',out)
      s2 <- sub(' samp.* ','',s1)
      line <- as.numeric(sub(' .*','',s2))
      samp <- as.numeric(sub('.* ','',s2))
      
      tile_out[i,] <- c(h_tile,v_tile,line,samp)
    } 
    
    return(tile_out)
    
  }
}