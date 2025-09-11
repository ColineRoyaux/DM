#########################################################################################################################################
# Compute ensemble model manually - Generate presence probability, uncertainty and penalised presence probability maps - after B. Leroy #
#########################################################################################################################################

## Dependencies
library(biomod2) #4.2-4
library(terra) #1.7-71
library(tidyterra) #0.6.0
library(viridis) #0.6.5
library(grDevices) #4.2.2
library(colorspace) #2.1-0

## parameters entry
rm(list = ls())
### Models R data 
mod_file <- list("occurrences.1.all" = "",  
                 "occurrences.2.all" = "") ## List paths to models data, output .rds of SDM_Model_calibration_new.R

### Model unit (species, taxa, assemblages, etc.) coordinates
sp_file <- "" ## Path to presence (absence) table
sp_col <- "" ## Colname for model units (species, taxa, assemblages, etc.)

### Cut map according to following GIS data : NULL if none
stack_cut <- "" ## Path to file, can be raster or vector, to cut according to specific territory boundaries (country, regions, etc.) one can use GADM shapefiles

### Enter directory where to save outputs
out_dir <- ""

## Steps
dir.create(out_dir, recursive = TRUE)
cat(capture.output(ls.str()), sep = "\n", file = paste0(out_dir, "manual_EM_map_init_param.txt")) #Get initial parameters
### Load models data
mod <- lapply(names(mod_file), function(f) readRDS(mod_file[[f]])[[f]] )
names(mod) <- names(mod_file)

### generate BASELINE stacks

mod_stack <- lapply(names(mod), function(sp) 
{
  cat(paste("----", Sys.time(), sp, "stack creation initialised ----\n", sep = " "))
  
  model_runs <- mod[[sp]]$models
  input_data <- get(load(model_runs@formated.input.data@link))

  sp_coords <- input_data@coord[which(input_data@data.species == 1), ] # Presence Coordinates for maps

  cat(paste("-- Creating continuous maps...\n", sep = " "))
  
  list_stacks <- grep("/proj_.+$", list.dirs(paste0(model_runs@dir.name, "/", sp), full.names = TRUE, recursive = FALSE), value = TRUE)
  
  if(length(list_stacks) > 1){
    stop("I don't know how to handle multiple projections for now, you have to set up this script for multiple projections, see Boris script 05")
  }else{
      
    cur_stack <- terra::rast(grep(paste0(sp, ".tif$"), list.files(list_stacks, full.names = TRUE), value = TRUE))
    
    if(!is.null(stack_cut)){
      if(grep(".shp$", stack_cut)) {
        v_cut <- terra::vect(stack_cut)
        r_cut <- terra::rasterize(v_cut, cur_stack)
      }else{
        r_cut <- terra::rast(stack_cut)
      }
      cur_stack <- mask(cur_stack, r_cut)
    }

    # Ensemble model : probabilities
    cur_em <- mean(cur_stack)
    return(cur_em)
    
  }
})

mods_stack <- rast(mod_stack)
names(mods_stack) <- names(mod)

## Binary map of the most probable 
bin_stack <- as.factor(which.max(mods_stack))

prob_stack <- do.call(merge,lapply(na.omit(unique(values(bin_stack))), function(sp){
  bin_stack[bin_stack != sp] <- NA
  m_stack <- mask(mods_stack[[sp]], bin_stack)
  return(m_stack)
}))

coltab(prob_stack) <- data.frame(value = sort(na.omit(unique(values(prob_stack)))),
                                 col = unlist(lapply(sort(na.omit(unique(values(prob_stack)))), function(v) adjustcolor("black", alpha.f = 1-(v/1000)))))

plot(prob_stack)
st_full <- c(bin_stack, prob_stack)
names(st_full) <- c("bin", "prob")

## !!!!!!!!!! NOT GENERAL !!!!!!!!!
#Creating a color table for transparent uncertainty

tiff(res = 600, height = 20, width = 30, units = "cm",
       filename = paste0(out_dir, 
                         Sys.Date(), "_comm_mod_plot.tiff"))
plot(bin_stack, col = c("#8d33a3FF", "#5bafc6FF"))
plot(prob_stack, add = TRUE)
dev.off()


### Add locations

tab_sp <- read.table(sp_file, 
                       sep = "\t", dec = ".", header = TRUE)
tab_sp$color <- tab_sp[, sp_col]
tab_sp[tab_sp$color == 1, "color"] <- "#8d33a3FF"
tab_sp[tab_sp$color == 2, "color"] <- "#5bafc6FF"
tab_sp[tab_sp$color == 3, "color"] <- "#7AD151FF"
tab_sp[tab_sp$color == 4, "color"] <- "#FDE725FF"


tiff(res = 600, height = 20, width = 30, units = "cm",
     filename = paste0(out_dir, 
                       Sys.Date(), "_comm_mod_plot_loc.tiff"))
plot(bin_stack, col = c("#8d33a3FF", "#5bafc6FF"))
plot(prob_stack, add = TRUE)
points(tab_sp$x_longitude, tab_sp$y_latitude, pch = 23, cex = 1.5,
       col = tab_sp$color, bg = lighten(tab_sp$color, amount = 0.5))
dev.off()
