####################################################
# Resampling environmental stacks - after B. Leroy #
####################################################

## Dependency
library(terra) #1.6-53 or 1.7-71
rm(list = ls())

## Parameters entry
### Envrionmental data : Check Coordinate Reference Systems are the same !!!!!!!!

#Data that don't need transformation (used as such in the model)
file_list_no_transf <- c(lapply(c( ## List of folder(s) containing the files if there are any
                        ), function(lf){gtools::mixedsort(list.files(lf, full.names = TRUE))}),
			## Continue here with the list of single files if there is any
) 

names(file_list_no_transf) <- c(## List of the names to give to each untransformed layer (each folder or single file given in the preceding parameter)
                                )
#How to resample these files if needed
nt_resamp_mode <- c( ##List of resampling strategy for each untransformed layer examples : mode, mean, max, bilinear
                    )
if(length(file_list_no_transf) != length(nt_resamp_mode)){stop("Set enough resampling mode")}
  
#Categorical data
file_list_cat <- c(lapply(c( ## List of folder(s) containing the files if there are any
                           )
                  , function(lf){gtools::mixedsort(list.files(lf, full.names = TRUE))}), 
                  ## Continue here with the list of single files if there is any
                  )

names(file_list_cat) <- c( ## List of the names to give to each categorical layer (each folder or single file given in the preceding parameter)
                          )
#How to resample these files if needed
cat_resamp_mode <- c( ##List of resampling strategy for each categorical layer examples : mode, mean, max, bilinear
                     )
if(length(file_list_cat) != length(cat_resamp_mode)){stop("Set enough resampling mode")}

#Other data
rast_other_data <- c( ##List of folder(s) or single files with any special alterations you would like to make example : using the sum of several listed raster layers
                     ) 

names(rast_other_data) <- c( ## List of the names to give to each special layer (each folder or single file given in the preceding parameter)
                            )
#How to resample these files if needed
other_resamp_mode <- c( ##List of resampling strategy for each special layer examples : mode, mean, max, bilinear
                       )
if(length(rast_other_data) != length(other_resamp_mode)){stop("Set enough resampling mode")}

### Cut env rast according to following stack : NULL if none
stack_cut <- "" ## Path to a raster to cut the environmental raster, this raster won't be included in the environmental stack but will be used to remove values according to the shape of this raster for example to remove the environmental values on the sea when studying terrestrial animals
crs_cut <- "" ## Coordinates Reference System identifier of stack_cut

### Enter directory where to save outputs
out_dir_plot <- ""

## Steps
dir.create(out_dir_plot, recursive = TRUE)
cat(capture.output(ls.str()), sep = "\n", file = paste0(out_dir_plot, Sys.Date(), "_Resamp_NAsync_init_param.txt")) #Get initial parameters

resamp_mode <- list(nt_resamp_mode, cat_resamp_mode, other_resamp_mode)
### Load all rasters individually

all_rast_list <- c(lapply(list(file_list_no_transf, file_list_cat), function(type){
  lapply(type, function(lis){ #For each types of rasters : categorical and not to be transformed
    lapply(lis, function(fil){ #For each file
      tryCatch(terra::rast(fil), error = function(e) {})
    })
  })
}), list(list(list(rast_other_data))))
names(all_rast_list) <- c("no_transf", "cat", "other")

#Removing empty elements created when files aren't rasters
all_rast_list <- lapply(all_rast_list, 
             function(type){
               lapply(type,
                      function(lis){ 
                        lis[lengths(lis) > 0]
                      })
              })

all_rast <- lapply(names(all_rast_list), function(ntype){
  #Categorical data must be implemented as a 0-1 variable for each categories, otherwise the model will infer that values are ordinate when
  #they are representing unsorted categories
  if(ntype == "cat"){ 
    cat_r <- lapply(names(all_rast_list[[ntype]]), function(nlis){
      cat_list <- unique(na.omit(terra::values(unlist(all_rast_list[[ntype]])[[nlis]])))
      if(length(cat_list) > 2){
        c_rast <- gtools::mixedsort(cat_list)
        cat_rast <- unlist(lapply(c_rast, 
                                 function(x){
                                   ra <- unlist(all_rast_list[[ntype]])[[nlis]] == x
                                   values(ra) <- as.integer(values(ra))
                                   return(ra)
                                 }))
        
        names(cat_rast) <- as.character(c_rast)
        return(cat_rast)
      }else{
        return(unlist(all_rast_list[[ntype]])[nlis])
      }
    })
    names(cat_r) <- names(all_rast_list[[ntype]])
    return(cat_r)
  }else{
    return(all_rast_list[[ntype]])
  }
})
names(all_rast) <- c("no_transf", "cat", "other")

all_rast_u <- unlist(all_rast) #unlisted version

### List of resampling modes
all_resamp_u <- unlist(lapply(1:length(all_rast), function(ntype){
  if(length(all_rast[[ntype]] > 0)){
    lapply(1:length(all_rast[[ntype]]), function(nlis){
      return(rep(resamp_mode[[ntype]][nlis], length(all_rast[[ntype]][[nlis]])))
    })
  }else{
    return(NULL)
  }
}))

### Check resolution of each raster and resample data
rast_res <- lapply(all_rast_u, res) #List resolution

n_rast_base <- unique(gsub("^(.*)[1-9]$", "\\1", 
                           names(which(round(unlist(rast_res), 10) == round(max(unlist(rast_res)), 10))
                                 ))) #Get names of raster(s) that have the less quality resolution
#!!!!!! I wanted to assign the raster with an integer extent but maybe no layers will have
#integer extent so I just drop it and use the first raster listed
rast_base <- all_rast_u[n_rast_base][[1]]

#!!!!!! Should check if extents are approx the same here, will be included in the script later

if(length(all_rast_u) > 1){ #If more than one raster
  ext_is_equal <- unlist(lapply(
    all_rast_u[-which(names(all_rast_u) == n_rast_base[1])], #exclude the raster used as base to avoid doublons
                                function(r){
                                  ext(r) == ext(rast_base)
                                  })) #Are the other rasters of same extent ?
  
  if(any(!ext_is_equal)){#If at least one raster is different
    num_rast_resamp <- grep(n_rast_base[1], names(all_rast_u), invert = TRUE) #Which rasters aren't the base raster chosen at the beginning
    rast_base_resamp <- unlist(lapply(num_rast_resamp, function(nr){ #Resample other rasters according to base raster with their resampling mode assigned at the beginning of the script
      m <- all_resamp_u[nr]
      return(resample(all_rast_u[[nr]], rast_base, method = m))
    })) #resample the other rasters
  }
}

stack_base <- rast(unlist(list(rast_base, rast_base_resamp)))
names(stack_base) <- c(n_rast_base[1], names(all_rast_u)[num_rast_resamp])

### Check the differences of extent between layers VERY LONG !!!! Remove comments on the following lines if interested
# sb_visu <- stack_base
# values(sb_visu)[!is.na(values(sb_visu))] <- 1 #1 when there is data
# values(sb_visu)[is.na(values(sb_visu))] <- 0 #0 when there isn't
# 
# l_comb <- as.list(as.data.frame(combn(1:terra::nlyr(sb_visu), m = 2))) #Single combinations of rasters
# nu <- lapply(l_comb, function(l){
#   rast_comp <- sb_visu[[l[1]]] - sb_visu[[l[2]]]
#   l_nozero <- length(which(values(rast_comp) != 0))
#     cat(names(sb_visu[[l[1]]]), names(sb_visu[[l[2]]]), "\n",
#         l_nozero, "\n")
#     if(l_nozero > 0){
#       png(paste0(out_dir_plot, "supp_", names(sb_visu[[l[1]]]), "-", names(sb_visu[[l[2]]]), ".png"), width = 600, height = 600)
#       plot(rast_comp, col = c("red","white","purple"), main = paste(names(sb_visu[[l[1]]]), " - ", names(sb_visu[[l[2]]])))
#       dev.off()
#     }
#     return("")
# })
# dev.off()

### NA synchronisation
sb_sync <- mask(stack_base, app(stack_base, fun = sum))

#How many pixels were suppressed
cat("Pixels transformed in NAs: \n")
nu <- lapply(1:nlyr(stack_base), function(n){
  cnt <- length(which(is.na(values(sb_sync[[n]])))) - length(which(is.na(values(stack_base[[n]]))))
  cat("\n- ", names(stack_base)[n], ": n = ", cnt, " ; ", cnt/length(which(is.na(values(stack_base[[n]]))))*100, "%")
})

### Cut rasters according to the given layer

if(!is.null(stack_cut)){
  if(grepl(".shp$", stack_cut)) {
    v_cut <- terra::vect(stack_cut)
    r_cut <- terra::rasterize(v_cut, sb_sync)
  }else{
    r_cut <- terra::rast(stack_cut)
    if(crs(crs_cut) != crs(sb_sync)){
      terra::crs(r_cut) <- crs_cut
      r_cut <- terra::project(r_cut, sb_sync, method = "max")
    }
    
  }
  sb_sync <- terra::mask(sb_sync, r_cut)
}

### Save stack

writeRaster(sb_sync, paste0(out_dir_plot, Sys.Date(), "_stack_env.tif"), overwrite = T)

#Empty env
rm(list = ls())
