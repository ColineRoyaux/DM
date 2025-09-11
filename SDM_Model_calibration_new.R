######################################
# Model calibration - after B. Leroy #
######################################

##Dependency
library(biomod2) #4.2-5
library(terra) #1.7-71
library(ggplot2) #3.5.0

##Parameters entry
rm(list = ls())
### Environmental variables stack
env_rast <- "" ## path to environmental stack, can be the output of Resampling_NAsync_stack.R 

### Rasterized presence directory
occ_dir <- "" ## Path to directory containing rasterized occurences, can be the outputs of species_rasterise.R

### Variables selection
var_sel <- list("occurrences_1_all" = NULL,
                "occurrences_2_all" = NULL,
                "occurrences_3_all" = NULL,
                "occurrences_4_all" = NULL) ## List of variables selected for each unit to modelise (species, taxa, assemblage,etc.)
		   ## For each element name of the list put the name of each file in the rasterized occcurences directory (occ_dir) without the extension, then =, then c() and list the names of variables to use for each unit
		   ## NULL if no selection needed

### Choose model algorithms to run 
choose_mod <- c(#"GLM",
                # "CTA",
                # "GAM",
                # "ANN",
                # "MARS",
                # "GBM",
                # "SRE",
                # "FDA",
                # "MAXENT",
                # "XGBOOST",
                # "RF"
				) 

### Personnalised parametrisation 
down_param <- TRUE ## Down-sampled models ? Useful if presence and absences or backgrounds are unbalanced

# No other options for now for fine tuning, if you want to tune your models, modify the code in ##Steps and ###

### Cross-Validation parametrisation
cur_block_dir <- NULL ## Path to directory where manual Cross-Validation tables are stored #set NULL if no manual blockCV computed with SDM_block_CV_prep.R

# If you want to calibrate the cross-validation using biomod2, set cur_block_dir to NULL and the following arguments are cross-validation calibration parameters
CV_runs <- 5 ## number of cross-validation runs
perc_CV <- 0.8 ## % of data kept to calibrate cross-validation, between 0 and 1 
CV_strat <- "" ## Strategy to select cross-validation calibration data, can be "random", "kfold", "block", "strat" or "env"

#### Generate pseudo absence? PA parametrization
run_PA <- FALSE
nb_PA <- 1000 ## How many pseudo-absence ? 1000 is an absolute minimum
runs_PA <- 3 ## How many psuedo-absence runs ? 2 is an absolute minimum, if low pseudo-absence enhance runs and reverse
pa_strat <- "" ## Strategy to select pseudo-absences, can be "random", "sre" or "disk

### Center-scale variables? Help to homogenize order of magnitude
sca <- FALSE

### Enter directory where to save outputs
out_dir <- ""

##Steps
###Format out_dir and create dir
out_dir <- paste0(out_dir, Sys.Date())
dir.create(paste0(out_dir, "_models"), recursive = TRUE)

cat(capture.output(ls.str()), sep = "\n", file = paste0(out_dir, "_models/Model_calibration_init_param.txt")) #Get initial parameters

###Load files
#baseline environmental raster
baseline <- terra::rast(env_rast)

if(sca){
  baseline <- terra::scale(baseline)
}

#Each occurrence table
files_occ <- grep(".txt$", list.files(occ_dir, full.name = TRUE), invert = TRUE, value = TRUE)
tab_occ <- lapply(files_occ , read.table, header = TRUE, sep = "\t", dec = ".")
names(tab_occ) <- gsub("^.+/([^/]+)\\.[a-z]+$", "\\1", files_occ)

#Manually calibrated blockCV
if(!is.null(cur_block_dir)){
  cur_block_files <- grep("_blocks.tabular$", list.files(cur_block_dir, full.names = TRUE), value = TRUE)
  cur_blocks <- lapply(cur_block_files, function(nf){ #Loading generated blockCV fold tables
    tab <- as.matrix(read.table(nf,
               header = TRUE,
               sep = "\t",
               dec = "."))
    
    #Formating colnames for block CV table, it must be in format "_allData_RUN1", "_allData_RUN2", "_allData_RUN3" or "_allData_allRun"
    if(all(grepl("^(all)?R[Uu][Nn][0-9]?$", colnames(tab)))){ #if column names are "RUN1", "RUN2", "RUN3" or "allRun"
      colnames(tab) <- paste0("_allData_", colnames(tab)) #Only paste "_allData_" at the begining
    }else if(all(grepl("^_allData_(all)?R[Uu][Nn][0-9]?$", colnames(tab)))){
      #Do nothing if columns are in the right format
    }else{ #If columns are in another format
      warning("Block Cross-Validation manual table had column names in a wrong format, 
              it was changed into \"_allData_RUN1\", \"_allData_RUN2\", \"_allData_RUN3\", ... or \"_allData_allRun\"")
      if(ncol(tab) == 1){ #If only one column
        colnames(tab) <- "_allData_allRun"
      }else{
        colnames(tab) <- paste0("_allData_RUN", 1:ncol(tab))
      }
    }
    return(tab)
  })
                            
  
  names(cur_blocks) <- gsub("^.+/([^/]+)_blocks\\.tabular$", "\\1", cur_block_files)
  CV_runs <- 1
  perc_CV <- NULL
  CV_strat <- "user.defined"
}else{
  cur_blocks <- NULL
}

if(all(unlist(lapply(var_sel, is.null))) || length(var_sel) != length(tab_occ)){
  var_sel <- NULL
}

### Model calibration
mod <- lapply(names(tab_occ), function(sp){
  cat("\n######\n", sp, "\n######\n")
  #Occurrence locations
  P_points <- tab_occ[[sp]]
  
  if(sum(P_points$Observed) > 5){
    
    # 1. Pseudo-absence parametrization
    if(!run_PA)
    {
      runs_PA <- 0
      nb_PA <- 0
      pa_strat <- NULL
      PA_table <- NULL
      #!!!!!! Add manual PA generation => PA table in script 08b.
    }else if(any(P_points$Observed == 0)){
      P_points <- P_points[P_points$Observed != 0, ]
    }
    
    # 2. Filter environmental data for the modelled group
    if(is.null(var_sel)){
      env_data <- baseline
    }else{
      env_data <- baseline[[ var_sel[[sp]] ]]
    }
    
    # !!!!! TODO 2. Environmental data are in a raster or a matrix ?
  
    # if(sp_list$env.data.type[i] == "Spatial") #raster
    # { 
    #   calib_env_data <- baseline[[cur_vars]]
    # } else  { #Matrix
    #   calib_env_data <- P_points[, cur_vars]
    # }
    # 
    
    # 2. Format occurrence for Biomod2
    # One object with coordinates
    coorxy <- P_points[, c("x", "y")]
    # One object with presence-absence
    P_points <- P_points[, "Observed"]
    
    # 4. initialize Biomod
    run_data <- biomod2::BIOMOD_FormatingData(resp.name = sp, # Occurrence entity name (species, community, ...)
                                     resp.var = P_points, # response variable: occurrence
                                     expl.var = env_data, # Explicative variables: environmental stack
                                     dir.name = paste0(out_dir, "_models"), #Output directory
                                     resp.xy = coorxy, # occurrence coordinates
                                     PA.nb.rep = runs_PA, # pseudoabsence runs
                                     PA.nb.absences = nb_PA, # pseudoabsences quantity
                                     PA.strategy = pa_strat) # selection strategy for pseudo-absence
    
    #5. Calibrate models
    # !!!!!!!!!!!!!! Quite specific, must be modified by the user !!! 
    if(is.null(cur_blocks[[sp]])){
      cur_blocks[[sp]] <- biomod2::bm_CrossValidation(
        bm.format = run_data,
        strategy = CV_strat,
        nb.rep = CV_runs,
        perc = perc_CV,
        do.full.models = FALSE
      )
    }

    
    if(down_param){
      opt_st <- "user.defined"
      
      NPA <- lapply(colnames(cur_blocks[[sp]]), function(rCV){
        prNum <- length(which(run_data@data.species[which(cur_blocks[[sp]][, rCV])] == 1))
        
        if(run_PA){
          absNum <- length(which(is.na(run_data@data.species[which(cur_blocks[[sp]][, rCV])])))
        }else{
          absNum <- length(which(run_data@data.species[which(cur_blocks[[sp]][, rCV])] == 0))
        }
        
        
        rf_size <- min(c(prNum, absNum))
        RF_options <- list(ntree = 1000,
                        sampsize = c("0" = rf_size, "1" = rf_size),
                        replace = TRUE)
        
        if(prNum > absNum){
          wt <- ifelse(P_points == 1, 1, prNum/absNum)
        }else{
          wt <- ifelse(P_points == 0, 1, absNum/prNum)
        }
        
        GBM_options <- list(interaction.depth = 5,
                          n.trees = 500,
                          shrinkage = 0.001,
                          bag.fraction = 0.75,
                          cv.folds = 5,
                          weights = wt)
        XGBOOST_options <- list(nrounds = 10000,
                             eta = 0.001,
                             max_depth = 5,
                             subsample = 0.75,
                             gamma = 0,
                             colsample_bytree = 0.8,
                             min_child_weight = 1,
                             weight = wt,
                             verbose = 0)
        
        return(list("RF" = RF_options, "GBM" = GBM_options, "XGBOOST" = XGBOOST_options))
      })
      names(NPA) <- colnames(cur_blocks[[sp]])
        
      RF_options <- lapply(colnames(cur_blocks[[sp]]), function(x){NPA[[x]]$RF})
      names(RF_options) <- colnames(cur_blocks[[sp]])
      GBM_options <- lapply(colnames(cur_blocks[[sp]]), function(x){NPA[[x]]$GBM})
      names(GBM_options) <- colnames(cur_blocks[[sp]])
      XGBOOST_options <- lapply(colnames(cur_blocks[[sp]]), function(x){NPA[[x]]$XGBOOST})
      names(XGBOOST_options) <- colnames(cur_blocks[[sp]])
      
      mod_options  <- biomod2::bm_ModelingOptions(
        data.type = "binary",
        models = c("RF", "GBM", "XGBOOST"),
        strategy = "user.defined",
        user.base = "default",
        user.val = list(
          GBM.binary.gbm.gbm = GBM_options,
          RF.binary.randomForest.randomForest = RF_options,
          XGBOOST.binary.xgboost.xgboost = XGBOOST_options
        ),
        bm.format = run_data,
        calib.lines = cur_blocks[[sp]]
      )
      
    }else{
      opt_st <- "default"
       mod_options <- NULL
    }
  
    model_runs <- biomod2::BIOMOD_Modeling(bm.format = run_data, # Initialized data
                                           modeling.id = "1", # Remove if you want all runs to be in separate folders
                                           models = choose_mod,
                                           OPT.strategy = opt_st,
                                           OPT.user = mod_options, # Modelisation options
                                           CV.strategy = "user.defined",
                                           CV.user.table = cur_blocks[[sp]],
                                           weights = NULL, # Weight of observations to input manually
                                           prevalence = 0.5, # Weight prevalence between presence and pseudo-absences
                                           var.import = 10, # Randomization runs of variable importance
                                           metric.eval = c("TSS", "ROC"), # evaluation metrics
                                           CV.do.full.models = FALSE,
                                           #nb.cpu = 4, # Parallelisation, doesn't work on windows
                                           do.progress = TRUE)
  
    mod_imp <- biomod2::get_variables_importance(model_runs)
    
    mod_imp$expl.var <- reorder(mod_imp$expl.var,
                                mod_imp$var.imp,
                                median,
                                na.rm = TRUE)
    
    p <- ggplot(mod_imp, aes(y = expl.var, x = var.imp)) +
      geom_boxplot(aes(col = algo)) + geom_jitter(alpha = .2, aes(col = algo)) + 
      theme_bw() + ggtitle(sp) + scale_color_brewer(palette = "Set2")
    
    
    ggplot2::ggsave(paste0(out_dir, "_models/variable_importance_", sp, ".png"), p, width = 2100, height = 2100, units = "px")

    # 6. Prepare ensemble model

    em_runs <- biomod2::BIOMOD_EnsembleModeling(model_runs, # Individual calibrated models
                                       models.chosen = 'all', # Manula filter of models to use for EM
                                       em.by = 'all', # What must be included in the EM?
                                       metric.select = 'TSS', # Which evaluation metric to use to filter "bad" models?
                                       # or ponder contribution of models in the EM?
                                       metric.select.thresh = 0.6, # Filtration threshold for "bad" models
                                       metric.eval = c("TSS", "ROC"), # evaluation metrics for the EM ?
                                       var.import = 1, # Randomization runs of variable importance
                                       em.algo = c('EMmean', 'EMcv', 'EMci', 'EMmedian', 'EMwmean'), # which confidence interval to compute ?
                                       EMci.alpha = 0.05, # confidence interval threshold
                                       EMwmean.decay = 'proportional') # Weight models technique
                                       #nb.cpu = 4), # Parallelisation, doesn't work on windows

    em_imp <- biomod2::get_variables_importance(em_runs)
    
  }else{
    cat("Impossible to compute models on less than 6 presences")
    run_data <- NULL
    model_runs <- NULL
    mod_imp <- NULL
    em_runs <- NULL
    em_imp <- NULL
  }

  return(list(form = run_data, models = model_runs, var_mod = mod_imp, ensemble = em_runs, var_em = em_imp, env_data = env_rast))
})

names(mod) <- unlist(lapply(mod, function(n){
  tryCatch(n$form@sp.name, error = function(e){"none"})
}))

saveRDS(mod[names(mod) != "none"], file = paste0(out_dir, "_models/model_list_info.RDS"))
