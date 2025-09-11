#################################################
# Model evaluation - ROC & TSS - after B. Leroy #
#################################################

## Dependency
library(biomod2) #4.2-2
library(ggplot2) #3.4.0
library(plyr) #1.8.8

## Parameters entry
rm(list = ls())

### Models R data 
mod_file <- "" ## Path to models data, output .rds of SDM_Model_calibration_new.R

### Enter directory where to save outputs
out_dir <- ""

##Steps
cat(capture.output(ls.str()), sep = "\n", file = paste0(out_dir, "eval_init_param.txt")) #Get initial parameters
###Load models data
mod <- readRDS(mod_file)

### retrieve ROC and TSS evals
evals_allsp <- plyr::rbind.fill(lapply(names(mod), function(sp){
  cat(paste("----", Sys.time(), sp, " evaluation initialised ----\n", sep = " "))

  model_runs <- mod[[sp]]$models
  
  return(tryCatch(data.frame(Species = sp, 
                    model_runs@models.evaluation@val), error = function(e){}))
}))

ggplot(evals_allsp, aes(y = validation, x = algo)) +
  geom_point() +
  facet_grid(Species ~ metric.eval)

p <- ggplot(evals_allsp) + 
  geom_boxplot(aes(x = algo, y = validation, col = Species)) + 
  facet_grid(metric.eval ~ ., scales = "free")

ggsave(paste0(out_dir, Sys.Date(), "_evals.png"), plot = p, height = 10, width = 10, dpi = 600)
 