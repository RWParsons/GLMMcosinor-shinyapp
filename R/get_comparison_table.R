get_comparison_table <- function(ref_level, 
                                 ref_comp, 
                                 group_name,
                                 components,
                                 cc_obj, 
                                 choose_comparison){
  
  # create empty dataframes fort he two tables. 
  comp_table <- data.frame() #dataframe for group comparison
  counter = 0 #this will be used to track the number of rows in comp_table
  
  comp_table2 <- data.frame() #dataframe for component comparison
  counter2 = 0 #this will be used to track the number of rows in comp_table2
  
  # get the levels within the specified group from the cglmm object 
  levels <- cc_obj$group_stats[[group_name]]
  if(!is.null(group_name)){
    comparison_levels <- levels[levels != ref_level]
  }
  
  # compare the parameter estimates for amplitude and acrophase 
  for (param in c("amp","acr")) {

    if(choose_comparison %in% c("group", "both group and component")) {  
      for (i in 1:components) {
        
        for (j in 1:length(comparison_levels)) {
          counter = counter + 1
          comp_output_param<- test_cosinor_levels(cc_obj,group_name, param = param,
                                                  comparison_A = ref_level,
                                                  comparison_B = comparison_levels[j], 
                                                  component_index = i)
          
          #If n_components = 1 in cc_obj, then the coefficients output will have no component suffix, so
          #so, capturing the reference value from this output must be adjusted 
          if(components == 1){
            ref_full <- round(cc_obj$coefficients[paste0(group_name,ref_level,":",param)], digits = 5)
          } else {
            ref_full <- round(cc_obj$coefficients[paste0(group_name,ref_level,":",param,i)], digits = 5)
          }
          comp_table[counter,1] <- paste0("[",paste0(group_name,ref_level,":",param,i),"] = ",ref_full)
          comp_table[counter,2] <- paste0("[",group_name,"=",comparison_levels[j],"]:", param,i," = ",round(comp_output_param$ind.test$conf.int[1], digits = 5) + ref_full)
          comp_table[counter,3:5] <- comp_output_param$ind.test$conf.int
          
          comp_table[counter,6] <- comp_output_param$ind.test$p.value
          
          colnames(comp_table) = c("reference est.",'comparison est.',"difference","lower CI", "upper CI", "p-value")
        }
      }
    }
    
    if(components > 1 && choose_comparison %in% c("component", "both group and component")) {
      #for this, should just be able to append to existing table
      ref_comp <- as.numeric(ref_comp)
      levels <- cc_obj$group_stats[[group_name]]
      comparison_components <- seq(components)[-ref_comp]
      
      for (i in 1:length(levels)) {
        for (j in 1:length(comparison_components)) {
          counter2 = counter2 + 1
          comp_output_param <-test_cosinor_components(cc_obj,group_name, param = param,
                                                      comparison_A = ref_comp,
                                                      comparison_B = comparison_components[j],
                                                      level_index = as.integer(levels[i]))
          if(is.null(group_name)){
            ref_full <- round(cc_obj$coefficients[paste0(param,ref_comp)], digits = 5)
            comp_table2[counter2,1] <- paste0("[",paste0(param,ref_comp),"] = ",ref_full)
            
            comp_table2[counter2,2] <- paste0(param, comparison_components[j]," = ",round(comp_output_param$ind.test$conf.int[1], digits = 5) + ref_full)
          } else {
            ref_full <- round(cc_obj$coefficients[paste0(group_name,levels[i],":",param,ref_comp)], digits = 5)
            comp_table2[counter2,1] <- paste0("[",paste0(group_name,levels[i],":",param,ref_comp),"] = ",ref_full)
            comp_table2[counter2,2] <- paste0("[",group_name,"=",levels[i],"]:", param,comparison_components[j]," = ",round(comp_output_param$ind.test$conf.int[1], digits = 5) + ref_full)
          }
          
          comp_table2[counter2,3:5] <- comp_output_param$ind.test$conf.int
          comp_table2[counter2,6] <- comp_output_param$ind.test$p.value
          colnames(comp_table2) = c("reference est.",'comparison est.',"difference","lower CI", "upper CI", "p-value")
          
          
        }
      }
    }
    
  } 
  
  return(list(comp_table, comp_table2))
  
}
