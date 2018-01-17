read_netcdf <- function(filename,var_input,var_output=var_input){
  
  #Function reads variables with names specified in vector 'var_input' from
  #netcdf-file 'filename' into a list. The names of the list variables can be
  #specified in the optional argument 'var_output' (default: var_input=var_output,
  #i.e. same names as in netcdf file)
  #
  #usage example:
  #  source("read_netcdf.r")
  #  var_input <- c("SALTF","HI","time")
  #  var_output <- c("saltflux","ice","time")
  #  filename <- "ice_day_1986.nc"
  #  var_list <- read_netcdf(filename,var_input,var_ouput)
  #
  #  #if individual variables preferred, delete list afterwards
  #  for (i in 1:length(var_input)){
  #    assign(var_output[i],var_list[[i]])
  #  }
  #  rm(var_list)
  
  library(ncdf4)
  
  #check if input and output variable lists have same size
  if (length(var_input)!=length(var_output)){
    print("ERROR: Equal number of input and output variable names required.")
  }
  else{
    #open netcdf file and read variables into dataframe
    ncfile <- nc_open(filename)
    for (i in 1:length(var_input)){
      assign(var_output[i],ncvar_get(ncfile,var_input[i]))
    }
    l <- mget(var_output)
    names(l) <- var_output
    return(l)
  }
}
