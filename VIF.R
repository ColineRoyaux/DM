##############################################################################
# Compute Variable Inflation Factors of a raster stack VIFs - after B. Leroy #
##############################################################################

##Dependency
library(terra) #1.7-71
library(usdm) #2.1-7

#Empty environment
rm(list = ls())
##Parameters entry
### Environmental Raster stack
rast_filename <- "" ## Path to environmental raster stack, can be the output of "Resampling_NAsync_stack.R"

#Selected rasters
s_rast <- c() ## If you want to select only some rasters from the stack, list their names here #If you want to use all rasters from the stack, leave the list empty

#output parameters
out_dir <- "" ## Enter path to directory where to save outputs
out_filename <- paste0(out_dir, Sys.Date(),"_seasprec_", gsub("^.+/([^/]+)\\.[a-z0-9]+$", "\\1", rast_filename), ".tif") ## Filename with today's date included, one can modify the function if necessary

##Steps
dir.create(out_dir, recursive = TRUE)
if(any(list.files(out_dir, full.names = TRUE) == out_filename)){stop(paste0(out_filename, " already exists, please change output name"))} 

cat(capture.output(ls.str()), sep = "\n", file = paste0(out_dir, Sys.Date(), "VIF_init_param.txt")) #Get initial parameters
#Load environmental stack
env_stack <- terra::rast(rast_filename)

#VIF computation
if(length(s_rast) > 0){
  vifs <- usdm::vif(env_stack[[s_rast]])
  
  ### Save subsetted stack
  dir.create(out_dir) 
  writeRaster(env_stack[[s_rast]], out_filename, overwrite = T)
  
}else{
  vifs <- usdm::vif(env_stack)
}
# If VIF> 10 it's bad
vifs

