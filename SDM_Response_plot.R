##################################
# Response plot - after B. Leroy #
##################################

## Dependencies
library(biomod2) #4.2-4
library(ggplot2) #3.5.0
library(PTXQC) #1.1.1

##Parameters entry
rm(list = ls())
### Models R data 
mod_file <- "" ## Path to models data, output .rds of SDM_Model_calibration_new.R

### Enter directory where to save outputs
out_dir <- ""

### Change variable labels ? #NULL if not
n_var_names <- c() ## List of original variable names as element name, then =, then a new name for display on plots. Ex : c("cont.Precip" = "Annual precipitation", "cat.soil" = "Soil type")

## Steps
cat(capture.output(ls.str()), sep = "\n", file = paste0(out_dir, "resp_plot_init_param.txt")) #Get initial parameters
### Load models data
mod <- readRDS(mod_file)

### Compute Response plots

lapply(names(mod), function(sp) 
{

  # Load model_runs data
  model_runs <- mod[[sp]]$models
  input_data <- get(load(model_runs@formated.input.data@link))
  
  #Create tab for max and min env data values at presence locations
  tab <- as.data.frame(cbind("pres" = mod[[sp]]$form@data.species, mod[[sp]]$form@data.env.var))
  tab_p <- tab[tab$pres == 1, ]
  tab_pres <- data.frame(Variable = colnames(tab_p), max = apply(tab_p, 2, max), min = apply(tab_p, 2, min))[-1,]

  # Variables used for calibration
  cur_vars <- model_runs@expl.var.names
  
  # Response curve computation 
  resp <- bm_PlotResponseCurves(bm.out = model_runs,
                                fixed.var = "mean",
                                data_species = input_data@data.species, 
  )$tab

  resp$pred.name <- gsub(paste0("^(.+)_(", paste(unique(mod[[sp]]$var_mod$algo), collapse = "|"),")$"), "\\2_\\1", resp$pred.name)
  
  colnames(resp) <- c("Index", "Variable", "Var.value", "Model", "Response")
  
  resp$Model <- gsub(PTXQC::LCSn(unique(as.character(resp$Model))), "", resp$Model)

  resp$Variable <- factor(resp$Variable, levels = cur_vars)
  n_var_names <- n_var_names[levels(resp$Variable)]
  
  p <- ggplot(resp, aes(x = Var.value, y = Response))+
    geom_line(alpha = 0.4, aes(group = Model, col = Model)) +
    stat_smooth(resp, mapping = aes(x = Var.value, y = Response)) +
    facet_wrap(~Variable, scales = "free_x", labeller = labeller(Variable = n_var_names)) +
    theme_bw() +
    ylim(0, 1.1) +
    xlab("Variable value")

  # Add environmental limits of presence data to visualize where extrapolation starts
  p <- p + geom_vline(data = tab_pres, mapping = aes(xintercept = max), color = "red", linewidth = 1.5) +
    geom_vline(data = tab_pres, mapping = aes(xintercept = min), color = "red", linewidth = 1.5)

  png(paste0(out_dir, "response_plot_", sp, ".png"), width = 30, height = 30, res = 600, unit = "cm")

  print(p)

  dev.off()
})
