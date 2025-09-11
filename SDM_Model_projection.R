#####################################
# Model projection - after B. Leroy #
#####################################

##Dependency
library(biomod2) #4.2-4
library(terra) #1.6-53

##Parameters entry
rm(list = ls())
### Models R data 
mod_file <- "" ## Path to models data, output .rds of SDM_Model_calibration_new.R

### Projection stack
proj_rast <- "" ## Path to environmental stack where models should be projected, could be the stack used for modelisation or future scenarios for example

### Were the variables scaled ?
sca <- FALSE

### Enter directory where to save outputs
out_dir <- ""

##Steps 
cat(capture.output(ls.str()), sep = "\n", file = paste0(out_dir, "Model_projection_init_param.txt")) #Get initial parameters
###Load data
mod <- readRDS(mod_file)
projection_stack <- terra::rast(proj_rast)

if(sca){
  projection_stack <- scale(projection_stack)
}

###Projection 

proj <- lapply(names(mod), function(sp){
  cat(paste("---- ", Sys.time(), "Projection:", sp, "----"))
  projection_runs <- tryCatch(biomod2::BIOMOD_Projection(bm.mod = mod[[sp]]$models, # Calibrated models
                                                         proj.name = "a", # Name of the projection
                                                         new.env = projection_stack[[mod[[sp]]$models@expl.var.names]], # Environmental projection stack
                                                         models.chosen = "all", # Models to project
                                                         metric.binary = "TSS", # Which metric to use to transform probabilities into pres-abs ? 
                                                         metric.filter = NULL, # Which metric to use to filter with a threshold under which probability is forced to 0 ?
                                                         #nb.cpu = 4, # Parallelisation, doesn't work on windows
                                                         build.clamping.mask = TRUE), # Clamping mask permits to illustrate areas where predictions are out of values used during model calibration, so when the model is extrapolating
                              error = function(e){})

  saveRDS(projection_runs, file = paste0(out_dir, sp, "/", Sys.Date(), "_projection_runs.RDS"))
  
  # 8.Project ensemble model
  proj_em <- tryCatch(biomod2::BIOMOD_EnsembleForecasting(bm.em = mod[[sp]]$ensemble, # Project on ensemble model
                                                          bm.proj = projection_runs, # individual models projection runs
                                                          metric.binary = "TSS"), # Which metric to use to transform probabilities into pres-abs ? 
                      error = function(e){})
  
  saveRDS(proj_em, file = paste0(out_dir, sp, "/", Sys.Date(), "_projection_em.RDS"))
  cat(paste("---- ", Sys.time(), "Projection:", sp, "finished ----\n"))
  print(warnings())
  
  return(list(proj_run = projection_runs, proj_em = proj_em))
})
