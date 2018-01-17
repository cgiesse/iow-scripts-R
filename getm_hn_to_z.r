getm_hn_to_z <- function(hn,z_dim_pos){
  
  #Computes the z-axis values out of the GETM adaptive coordinates cell
  #thicknesses 'hn'. Works for 1-, 2-, 3- and 4-dimensional 'hn'-arrays.
  #The position of the z-coordinate dimension in hn must be specified
  #('z_dim_pos'). In the resulting array 'z' the z-coordinate is the last dimension.
  #
  #usage example:
  #  source("getm_hn_to_z.r")
  #  z <- getm_hn_to_z(hn,3)
  
  n_dim <- length(dim(hn))
  if ((z_dim_pos > n_dim) || (z_dim_pos < 1)){
    print("ERROR: No valid value for z_dim_pos.")
    return()
  }
  # set z dimension as last dimension of hn array
  hn <- aperm(hn,unique(c(1:n_dim,z_dim_pos),fromLast=TRUE)) 
  z <- array(0, dim=dim(hn))
  
  if (n_dim==1){
    z[dim(hn)[n_dim]] <- -hn[dim(hn)[n_dim]]
    for (i in (dim(hn)[n_dim]-1):1){
      z[i] <- z[i+1]-hn[i]
    }
  } else if (n_dim==2){
    z[,dim(hn)[n_dim]] <- -hn[,dim(hn)[n_dim]]
    for (i in (dim(hn)[n_dim]-1):1){
      z[,i] <- z[,i+1]-hn[,i]
    }
  } else if (n_dim==3){
    z[,,dim(hn)[n_dim]] <- -hn[,,dim(hn)[n_dim]]
    for (i in (dim(hn)[n_dim]-1):1){
      z[,,i] <- z[,,i+1]-hn[,,i]
    }
  } else if (n_dim==4){
    z[,,,dim(hn)[n_dim]] <- -hn[,,,dim(hn)[n_dim]]
    for (i in (dim(hn)[n_dim]-1):1){
      z[,,,i] <- z[,,,i+1]-hn[,,,i]
    }
  } else{
    print("ERROR: hn needs to be 1-, 2-, 3- or 4-dimensional.")
    return()
  }
  return(z)
}
