#####################################################
# Model evaluation - Jaccard index - after B. Leroy #
#####################################################

##!!!! Only works with presence-absence models do not use with presence-only

## Dependencies
library(terra) #1.7-71
library(biomod2) #4.2-4
library(ggplot2) #3.5.0
library(reshape2) #1.4.4

## parameters entry
rm(list = ls())                             
### Models R data 
mod_file <- "" ## Path to models data, output .rds of SDM_Model_calibration_new.R

### Enter directory where to save outputs
out_dir <- "D:/R_sdm/SDM_community/2024-07-25_models/"

## Steps
cat(capture.output(ls.str()), sep = "\n", file = paste0(out_dir, "jaccard_init_param.txt")) #Get initial parameters
### Load models data
mod <- readRDS(mod_file)

### Compute Jaccard index
#Use first model of the list to create an array for all other models
model_runs <- mod[[1]]$models 
allsp_jaccard <- array(dim = c(length(mod),
                             length(unique(model_runs@models.evaluation@val$algo)),
                             length(unique(model_runs@models.evaluation@val$run)),
                             length(unique(c("allData", unique(model_runs@models.evaluation@val$PA))))),
                     dimnames = list(species = names(mod),
                                     model = unique(model_runs@models.evaluation@val$algo),
                                     cv.run = unique(model_runs@models.evaluation@val$run),
                                     pa.run = unique(c("allData", unique(model_runs@models.evaluation@val$PA)))))


# Creation of an array for optimal presence-absence conversion cutoffs so they can be used in the models
allsp_cutoffs <- allsp_jaccard


jaccard_list <- list()

for (sp in dimnames(allsp_jaccard)$species)
{
  cat(paste("----", Sys.time(), sp, " evaluation initialised ----\n", sep = " "))

  model_runs <- mod[[sp]]$models
  input_data <- get(load(model_runs@formated.input.data@link))
  calib_lines <- get(load(model_runs@calib.lines@link))
  model_preds <- get(load(model_runs@models.prediction@link))
  
  for (pa in unique(model_runs@models.evaluation@val$PA))
  {
    
    for (cv in unique(model_runs@models.evaluation@val$run))
    {
      obs_data <- input_data@data.species[which(!calib_lines[, 
                                                             paste0("_", pa, 
                                                                    "_", cv)])]
      obs_data[is.na(obs_data)] <- 0 # /!\ Transforming pseudo-absences into absences
      
      cur_calib_lines <- calib_lines[which(!is.na(calib_lines[, 
                                                              paste0("_", pa, 
                                                              "_", cv)])),
                                     paste0("_", pa, 
                                            "_", cv)]
      cur_eval <- which(!cur_calib_lines)
      
      for (md in unique(model_runs@models.evaluation@val$algo))
      {
        # On évalue uniquement les lignes qui n'ont pas servi à la calibration
        cur_preds <- model_preds[which(model_preds$points %in% cur_eval & # Evaluation lines 
                                         model_preds$algo == md & # Algorithms
                                         model_preds$run == cv & # run cv
                                         model_preds$PA == pa), ] # run PA
        
        if(!any(is.na(cur_preds))) # If there are NAs, it means the model failed so no Jaccard computation
        {
          # Computing Jaccard for all treshold between 0 and 1
          
          Sys.time()
          jaccard_test <- as.data.frame(do.call(rbind, lapply(seq(0, 1000, by = 1), function(cutoff){
            pred_pa <- cur_preds$pred
            pred_pa[pred_pa < cutoff] <- 0
            pred_pa[pred_pa >= cutoff] <- 1
            TP <- length(which(obs_data == 1 & pred_pa == 1))
            FN <- length(which(obs_data == 1 & pred_pa == 0))
            FP <- length(which(obs_data == 0 & pred_pa == 1))
            jaccard <- TP / (TP + FP + FN)
            return(data.frame(cutoff = cutoff,
                              TP = TP,
                              FN = FN,
                              FP = FP,
                              jaccard = jaccard))
          })))
          Sys.time()
          
          jaccard_list[[sp]][[paste0(pa, "_", cv, "_", md)]] <- jaccard_test
          
          # Saving the treshold for highest Jaccard index
          # Using the mean value of the best treshold if it is attained several times
          allsp_cutoffs[sp, md, cv, pa] <- mean(jaccard_test$cutoff[
            which(jaccard_test$jaccard == max(jaccard_test$jaccard))])
          
          # Jaccard is extracted at best mean treshold
          jaccard_b_tresh <- jaccard_test$jaccard[
            which(jaccard_test$cutoff == round(allsp_cutoffs[sp, md, cv, pa]))]
          
          if(length(jaccard_b_tresh) != 0){ allsp_jaccard[sp, md, cv, pa] <- jaccard_b_tresh }
          
        } else
        {
          jaccard_list[[sp]][[paste0(pa, "_", cv, "_", md)]] <- NA
          
          allsp_cutoffs[sp, md, cv, pa] <- NA 
          
          allsp_jaccard[sp, md, cv, pa] <- NA
        }
      }
    }
  }
}

## Save files

saveRDS(allsp_cutoffs, file = paste0(out_dir, "jaccard_cutoffs.RDS"))
saveRDS(allsp_jaccard, file = paste0(out_dir, "jaccard_evals.RDS"))
saveRDS(jaccard_list, file = paste0(out_dir, "jaccard_tests.RDS"))

## Plot

ggjaccard <- reshape2::melt(allsp_jaccard)

p <- ggplot(ggjaccard, aes(x = model, y = value, col = pa.run, shape = cv.run)) +
  geom_point() + facet_wrap (~ species) +
  scale_shape_manual(values=1:nlevels(ggjaccard$cv.run))

ggsave(paste0(out_dir, "jaccard_plot.png"), p, dpi = 600, width = 7, height = 10)

