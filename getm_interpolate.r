getm_interpolate <- function(zax,z,variable_z,z_dim_pos){
  
  #Interpolates some variable 'variable_z' from a varying grid 'z' (e.g. computed from hn
  #with function getm_hn_to_z()) on a fixed grid with z-axis 'zax'.
  #IMPORTANT: The position of the z-coordinate dimension in 'variable_z' must be specified
  #('z_dim_pos'). However, in 'z' the z-coordinate must be at last position, which is
  #fulfilled automatically when computing 'z' via function getm_hn_to_z().
  #
  #usage example:
  #  source("getm_interpolate.r")
  #  z <- getm_hn_to_z(hn,2)
  #  zax <- -250:0
  #  salt_zax <- getm_interpolate(zax,z,salt,2)
  
  #some dimension checks and adjustments
  if (length(dim(variable_z))!=length(dim(z))){
    print("ERROR: z and variable_z have different numbers of dimensions.")
    return()
  }
  n_dim <- length(dim(variable_z))
  
  if ((z_dim_pos > n_dim) || (z_dim_pos < 1)){
    print("ERROR: No valid value for z_dim_pos.")
    return()
  }
  
  # set z dimension as last dimension of variable_z and z array
  variable_z <- aperm(variable_z,unique(c(1:n_dim,z_dim_pos),fromLast=TRUE))
 
  if (!(all(dim(variable_z)==dim(z)))){
    print("ERROR: z and variable_z have different dimensions.")
    return()
  }
  
  #linear interpolation
  variable_zax <- array(NA,dim=c(dim(variable_z)[1:(n_dim-1)],length(zax)))
  if (n_dim==1){
    tryCatch(variable_zax <- approx(z, variable_z, xout=zax, method="linear", yleft=NA, rule=2)$y, error=function(e) NA)
  } else if (n_dim==2){
    for (i in 1:(dim(variable_z)[1])){
      tryCatch(variable_zax[i,] <- approx(z[i,], variable_z[i,], xout=zax, method="linear", yleft=NA, rule=2)$y, error=function(e) NA)
    }
  } else if (n_dim==3){
    for (i in 1:(dim(variable_z)[1])){
      for (j in 1:(dim(variable_z)[2])){
        tryCatch(variable_zax[i,j,] <- approx(z[i,j,], variable_z[i,j,], xout=zax, method="linear", yleft=NA, rule=2)$y, error=function(e) NA)
      }
    }
  } else if (n_dim==4){
    for (i in 1:(dim(variable_z)[1])){
      for (j in 1:(dim(variable_z)[2])){
        for (k in 1:(dim(variable_z)[3])){
          tryCatch(variable_zax[i,j,k,] <- approx(z[i,j,k,], variable_z[i,j,k,], xout=zax, method="linear", yleft=NA, rule=2)$y, error=function(e) NA)
        }
      }
    }
  } else{
    print("ERROR: variable_z needs to be 1-, 2-, 3- or 4-dimensional.")
    return()
  }
  return(variable_zax)
}
