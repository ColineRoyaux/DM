#########################################################
# Environmental variables collinearity - after B. Leroy #
#########################################################

##Dependency
library(terra) #1.7-71
library(virtualspecies) #1.6

##Empty environment
rm(list = ls())

##Parameters entry
# Env Raster stack
rast_filename <- "" ##Path for environmental raster

# Cutoff value
cutoff <- 0.5 ##Threshold value for delimiting the correlated variables

#Correlation method
corr_m <- "spearman"

#output parameters
out_dir <- "" ##Path to save outputs

##Steps
dir.create(out_dir) 
out_filename <- paste0(out_dir, Sys.Date(),"_coll_", gsub("^.+/([^/]+)\\.[a-z0-9]+$", "\\1", rast_filename), ".png")
cat(capture.output(ls.str()), sep = "\n", file = paste0(out_dir, Sys.Date(), "_collinearity_env_rasters_init_param.txt")) #Get initial parameters
#Load env stack
env_stack <- terra::rast(rast_filename)

#Collinearity plot
png(out_filename, width = 960, height = 480) 
virtualspecies::removeCollinearity(raster::stack(env_stack), plot = T,
                                   multicollinearity.cutoff = cutoff,
                                   method = corr_m)
dev.off()
