# Function for saving visualizations


save_my_plot <- 
  function(plot, desired_filename){
    
    if(!dir.exists("visualizations/")){
      
      dir.create("visualizations/")
      
    }
      filepath <- stringr::str_glue("visualizations/{desired_filename}.png")
      
      ggplot2::ggsave(filepath, width = 8.5, height = 11 )
    }

