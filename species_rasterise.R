##############################################
# Rasterise occurrence data - after B. Leroy #
##############################################

##Dependency
library(terra) #1.7-71
library(openxlsx) #4.2.5.1
library(tidyr) #1.3.1

##Parameters entry
rm(list = ls())
### Presence (absence) dataframe file
tab_filename <- "" ## Path to presence (absence) data file
sheetname <- NULL ## If tab_filename is a xlsx file, give the name of the sheet to the presence (absence) table # NULL if not
loc_sheet <- NULL ## If coordinates are in a different sheet, give the name of the sheet to the coordinates table # NULL if not
ID_loc <- NULL ## If location names and presence (absence) table are linked through samples or coordinates are in a different sheet, give the name of the column containing location names # NULL if not
samp_sheet <- NULL ## If location names and presence (absence) table are linked through a samples table, give the name of the sheet to the samples table # NULL if not
ID_samp <- NULL ## If location names and presence (absence) table are linked through samples table, give the name of the column containing the samples names # NULL if not
sort_col_samp <- NULL ## If one column from the sample file is to be used to separate presence files, give the name of the column # NULL if not
x_col <- "" ## Name of the longitude (x) column
y_col <- "" ## Name of the latitude (y) column
PA_col <- "" ## Name of the occurence (presence and/or absence) column
div_col <- "" ## Name of the column used to separate rasterized presence(-absence) output files => often "species" #NULL if you don't want to split
True_abs <- FALSE ## Fill all localities without presence or absence with an absence?
crs_PA <- "" ## Coordinate reference system of occurrence coordinates in the EPSG standard => put "epsg:4326" for WGS84

### Raster stack
rast_filename <- "" ## Path to the raster used for the rasterisation of occurrences. Can be the output of "Resampling_NAsync_stack.R" for example
crs_stack <- "" ## Coordinate reference system of the raster file in the EPSG standard => put "epsg:4326" for WGS84
r_fun <- sum  ## What function do you want to use for the rasterisation => sum , mean , mode , ...

### Enter directory where to save outputs
out_dir <- ""

##Steps
###Rename and create output directory
out_dir <- paste0(out_dir, "rast_sp_", Sys.Date(), "/")
dir.create(out_dir, recursive = TRUE)
cat(capture.output(ls.str()), sep = "\n", file = paste0(out_dir, "species_rasterise_init_param.txt")) #Get initial parameters

### Read files
if(grepl("\\.xlsx$", tab_filename)){
  tab <- openxlsx::read.xlsx(tab_filename, sheetname, na.strings = "")
}else{
  tab <- read.table(tab_filename, header = TRUE, sep = "\t", dec = ".")
}
#Get true NAs
tab[is.na(tab)] <- ""
tab[apply(tab, c(1, 2), function(x) grepl("NA", x))] <- NA

#Other eventual files
if(!is.null(loc_sheet)){ #If location table
  tab_loc <- openxlsx::read.xlsx(tab_filename, loc_sheet, na.strings = "")
  #Get true NAs
  tab_loc[is.na(tab_loc)] <- ""
  tab_loc[apply(tab_loc, c(1, 2), function(x) grepl("NA", x))] <- NA
  
  if(!is.null(samp_sheet)){ #If samples table
    tab_samp <- openxlsx::read.xlsx(tab_filename, samp_sheet, na.strings = "")
    #Get true NAs
    tab_samp[is.na(tab_samp)] <- ""
    tab_samp[apply(tab_samp, c(1, 2), function(x) grepl("NA", x))] <- NA
    
    tab[, c(ID_loc, sort_col_samp)] <- tab_samp[match(tab[, ID_samp], tab_samp[, ID_samp]), c(ID_loc, sort_col_samp)]
  }
  tab[, c(x_col, y_col)] <- tab_loc[match(tab[, ID_loc], tab_loc[, ID_loc]), c(x_col, y_col)]
}

#Set column types
tab[, x_col] <- as.numeric(tab[, x_col])
tab[, y_col] <- as.numeric(tab[, y_col])

if(is.null(PA_col)){
  PA_col <- "Pres_abs"
  tab[, PA_col] <- TRUE
}else{
  tab[, PA_col] <- as.numeric(as.logical(tab[, PA_col]))
}

#Fill localities with no information with absence
if(True_abs){
tab_wide <- tidyr::spread(tab, key = div_col, value = PA_col)
tab <- tidyr::gather(tab_wide, key = !!div_col, value = !!PA_col, as.character(unique(tab[, div_col])))
tab[is.na(tab[, PA_col]), PA_col] <- FALSE
}

#Load environmental stack
rast_stack <- terra::rast(rast_filename)
crs(rast_stack) <- crs_stack

### Rasterization

if(is.null(div_col)){div <- "none"}else{div <- na.omit(unique(tab[, div_col]))}

poub <- lapply(div, function(d){
  if(d == "none"){
    tab_su <- tab
    d <- PA_col
  }else{
    tab_su <- tab[grep(d, tab[, div_col]), ]
  }
  if(is.null(sort_col_samp)){so <- "all"}else{so <- na.omit(unique(tab_su[, sort_col_samp]))}
  
  tab_su <- tab_su[which(apply(!is.na(tab_su[, c(x_col, y_col)]), 1, any)), ] #Remove lines with NA in X or Y columns
  
  if(any(so == "")){stop("Empty fields found in the Presence-Absence column or in the sorting column")}
  
  lapply(so, function(s){
    if(s != "all"){
      tab_sub <- tab_su[grep(s, tab_su[, sort_col_samp]), ]
    }else{
      tab_sub <- tab_su
    }
    
    ####Change tabular occurrence in spatialized vector
    occ <- vect(tab_sub,
                geom = c(x_col, y_col),
                crs = crs_PA)
    
    if(crs_PA != crs_stack){
      occ_p <- terra::project(occ,
                              crs_stack)
    }else{
      occ_p <- occ
    }
    
    #dev.off()
    ####Transform pres_abs field into sums per raster pixel
    PA_env <- terra::rasterize(x = occ_p, 
                        y = rast_stack,
                        field = PA_col,
                        fun = r_fun)
    PA_env[PA_env > 1] <- 1
    names(PA_env) <- d
    
    full_stack <- c(PA_env, rast_stack)
    
    ####Remove species data that isn't on environmental extent
    coorXY <- xyFromCell(rast_stack, 1:ncell(rast_stack)) #Get coordinates of all pixels
    PA_env_df <- values(full_stack)
    
    cat("#", d, s,"#\n\n")
    if(any(is.na(PA_env_df[, names(rast_stack)[1]]) & !is.na(PA_env_df[, d])))
    {
      cat("Some points are in pixels without environmental values :", length(which(is.na(PA_env_df[, names(rast_stack)[1]]) & !is.na(PA_env_df[, d]))), "pixels\n\n")
    }
    
    #Removing these pixels
    NApix <- which(is.na(PA_env_df[, names(rast_stack)[1]]))
    if(length(NApix) > 0){
      coorXY <- coorXY[-NApix, ]
      PA_env_df <- PA_env_df[-NApix, ]
    }

    # Number of cells in the ens
    cat("Number of pixels of presence:",
        "\n - Initial: ", length(which(values(occ_p[, PA_col]) == 1)),
        "\n - After rasterisation: ", length(which(PA_env_df[, 1] == 1)), "\n\n")
    cat("Number of pixels of absence:",
        "\n - Initial: ", length(which(values(occ_p[, PA_col]) == 0)),
        "\n - After rasterisation: ", length(which(PA_env_df[, 1] == 0)), "\n\n\n")
    #### Get rasterized occurences on disk
    P_points <- data.frame(
      # Get coordinates where there is presence and absence
      coorXY[which(!is.na(PA_env_df[, d])), ],
      # get Pres-abs column
      Observed = PA_env_df[which(!is.na(PA_env_df[, d])), d]) 
    
    #Save occurrence table
    write.table(P_points, file = paste0(out_dir, "occurrences_", d, "_", s, ".tabular"), sep = "\t", dec = ".", 
                quote = FALSE, row.names = FALSE)
    
  })
  
  })
