# Library -----------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, stringr, lubridate, swmmr)

# Functions ---------------------------------------------------------------


read_GI_plan <- function(GI_plan_path){
  # Purpose: 
  #     Read GI implementation plan
  # Input: 
  #     GI_plan_path = file path of GI_plan
  # Output: 
  #     A tibble of GI_plan parameters
  
  out <- read_csv(GI_plan_path, 
           col_types = cols(
             per_area_rep = col_double(),
             imp_area_rep = col_double(),
             width_adj = col_double(),
             .default = col_character()))
  
  # If relative path is used, it is changed to full path
  inflow_path <- out$inflow_path
  for (i in seq_along(inflow_path)){
    inflow_path[[i]] <- str_replace(inflow_path[[i]], "^\\.", getwd())
  }
  
  out$inflow_path <- inflow_path
  
  out
}

read_infows <- function(GI_plan, inp){
  # Purpose: 
  #     read inflows for each subcatchments, inflows for the same outlet is added
  #     inflows to nodes and to subcatchments are processed independently 
  # Input: 
  #     GI_plan = GI plan read by read_GI_plan
  #     inp = SWMM inp tibble read by swmmr::read_inp()
  # Output: 
  #     A list of to_nodes and to_subcatchment tibbles
  #       where, col1 = outlet index, 
  #       and col2 = inflow hydrograph in tibble (aligned with other hydrographs, and sum of all sources)
  
  subcatchments <- inp$subcatchments %>%
    select(Name, Outlet)
  
  # Fill the missing outlet values in GI_plan
  GI_plan <- GI_plan %>%
    left_join(subcatchments, by = c("subcatchment_name" = "Name")) %>%
    mutate(outlet_missing = outlet == 0,
           outlet = replace(outlet, outlet_missing, Outlet[outlet_missing])) %>%
    select(-Outlet, -outlet_missing)
  
  # read inflows from inflow_path
  read_infow <- function(inflow_path){
    # Purpose: read inflow from inflow_path
    # Input: inflow_path = file path of inflow
    # Output: inflow_df
    read_csv(inflow_path,
             col_types = cols(datetime = col_character(),
                              flow = col_double())) %>%
      mutate(datetime = ymd_hms(datetime)) %>%
      arrange(datetime)
  }
  
  GI_plan <- GI_plan %>%
    mutate(inflows = map(inflow_path, read_infow))
  
  # separate flows that are to_nodes and to_subcatchments 
  to_nodes <- GI_plan %>%
    filter(!(outlet %in% subcatchments$Name))
  
  to_subcatchments <- GI_plan %>%
    filter(outlet %in% subcatchments$Name)
  
  # Align inflows
  align_inflow <- function(flow_df_sub){
    # Purpose: Create regular interval df for all flows, NA is replaced with 0
    # input: 
    #     flow_df_sub = to_nodes or to_subcatchments
    # output:
    #     Aligned flow_df_sub
    
    # Get time series of all flow_df
    combined_ts <- flow_df_sub$inflows %>%
      lapply(function(x) x$datetime %>% sort()) %>%
      do.call(what = c)
    
    # Create complete ref_ts with regular intervals
    ref_ts <- tibble(
      datetime = seq(from = range(combined_ts)[1], to = range(combined_ts)[2],
                     by = combined_ts[2] - combined_ts[1])
    )
    
    # Algin flow_df
    fill_flow_ts <- function(flow_df, ref_ts){
      # Purpose: 
      #     Align individual flow_df according to ref_ts, fill NA with 0
      # Input:
      #     flow_df = individual flow df
      #     ref_ts = reference long time series
      # Output:
      #     aligned individual flow_df
      ref_ts %>%
        left_join(flow_df, by = "datetime") %>%
        mutate(flow = replace(flow, is.na(flow), 0)) %>%
        arrange(datetime)
    }
    
    # Apply fill_flow_ts to individual flow_df, return
    flow_df_sub %>%
      mutate(inflows = map(inflows, fill_flow_ts, ref_ts = ref_ts)) 
  }
  
  if (nrow(to_nodes) > 0){
    to_nodes <- to_nodes %>%
      align_inflow
  }
  
  if (nrow(to_subcatchments) > 0){
    to_subcatchments <- to_subcatchments %>%
      align_inflow
  }
  
  
  # Join the inflows send to the same node or subcatchment
  sum_multi_inflow <- function(flow_df_sub){
    # Purpose: 
    #     sum inflow drained to the same distination
    # Input: 
    #     flow_df_sub, i.e., to_nodes or to_subcatchments
    # Output: 
    #     summed flow_df_sub with inflows drained to the same node/subcatchment
    flow_df_sub %>%
      group_by(outlet) %>%
      summarise(inflows = list(inflows)) %>%
      mutate(inflows = map(inflows,
                           function(x)
                             x %>%
                             bind_rows() %>%
                             group_by(datetime) %>%
                             summarise(flow = sum(flow))))
  }
  
  
  if (nrow(to_nodes) > 0){
    to_nodes <- to_nodes %>%
      sum_multi_inflow()
  }
  
  if (nrow(to_subcatchments) > 0){
    to_subcatchments <- to_subcatchments %>%
      sum_multi_inflow()
  }
  
  # Output
  list(to_nodes = to_nodes,
       to_subcatchments = to_subcatchments)
}

write_routing_interface_file <- function(GI_plan, inp, routing_interface_path = NULL, flow_unit = "CFS") {
  # Purpose: 
  #     write runoff interface file
  # Input:
  #     GI_plan = GI plan read by read_GI_plan
  #     inp = SWMM inp tibble read by swmmr::read_inp()
  #     routing_interface_path = file path of resulting interface file 
  #     flow_unit = flow unit
  # Output:
  #     T or F, whether the input file has been created
  
  # Get inflow tibble for each node
  to_nodes <- read_infows(GI_plan, inp)$to_nodes
  
  # Prepare interface file headings
  node_names <- to_nodes["outlet"] %>% unlist() %>% unique()
  num_lines <- 7 + length(node_names) + sum(sapply(to_nodes$inflows, nrow))
  lines <- vector("character", length = num_lines)
  
  lines[[1]] <- "SWMM5 Inflows Interface File"
  lines[[2]] <- "Inflows rate to nodes"
  lines[[3]] <-  difftime(to_nodes$inflows[[1]]$datetime[2],
                          to_nodes$inflows[[1]]$datetime[1],
                          units = "secs") %>%
    as.character() # Time resolution, in second
  lines[[4]] <- "1" # The number of variables stored in the file
  lines[[5]] <- paste0("FLOW ", flow_unit) # The name and units of each variable
  lines[[6]] <-  length(node_names) # The number of nodes with recorded inflow data
  
  # write unique node names
  for (i in seq_along(node_names)) {
    line_ind <- 6 + i
    lines[[line_ind]] <- node_names[[i]]
  }
  
  # write colnames
  line_ind <- line_ind + 1
  lines[[line_ind]] <- "Node Year Mon Day Hr Min Sec Flow"
  
  # format flow time series, and assign to lines
  flow_lines <- to_nodes$inflows %>%
    bind_rows() %>%
    mutate(outlet = rep(to_nodes$outlet, times = sapply(to_nodes$inflows, nrow)),
           outlet = factor(outlet, levels = node_names)) %>%
    arrange(datetime, outlet) %>%
    mutate(lines = paste(outlet, as.character(datetime, format = "%Y %m %d %H %M %S"), flow)) %>%
    pull(lines)
  
  line_ind <- (line_ind + 1):num_lines
  lines[line_ind] <- flow_lines
  
  # Save routing interface file
  if (is.null(routing_interface_path)){
    routing_interface_path <- paste0(getwd(), "/inflow.txt")
  }
  
  fileConn <- file(routing_interface_path)
  writeLines(lines, fileConn)
  close(fileConn)
  
  # Check if file created
  ifelse(file.exists(routing_interface_path), T, F)
}

modify_inp <- function(GI_plan, inp, routing_interface_path = NULL){
  # Purpose:
  #     Modify inp according to GI_plan
  # Input:
  #     GI_plan = GI_plan read by read_GI_plan
  #     inp = SWMM input read by read_GI_plan
  #     routing_interface_path = file path of resulting interface file
  # Output:
  #     inp = modified inp
  
  # update file tab
  update_files_tab <- function(inp, routing_interface_path = NULL){
    # Purpose: add 'USE INFLOWS  "routing_interface_path"' under [FILES] to inp
    # Input:
    #     inp = SWMM input file read by swmmr::read_inp
    # Output:
    #     inp = modified inp with USE INFLOWS added
    if (is.null(routing_interface_path)){
      routing_interface_path <- paste0("\"", normalizePath(getwd()), "\\inflow.txt\"")
    }
    
    # If relative path is used, it is changed to full
    routing_interface_path <- str_replace(routing_interface_path, "^\\.", getwd())
    
    new_row <- tibble(Verb = "USE",
                      Parameter = "INFLOWS",
                      Path = routing_interface_path)
    if (any(str_detect(names(inp), "files"))){
      # If [FILES] tab presents, check if USE INFLOWS presented
      action <- inp$files %>%
        mutate(action = paste(Verb, Parameter, sep = " ")) %>%
        pull(action)
      if (!any(action == "USE INFLOWS")) {
        inp$files <- inp$files %>%
          bind_rows(new_row)
      } else {
        warning("USE INFLOWS already presented in SWMM inp!")
      }
    } else {
      # If [FILES] tab not present, add this tab at the 3rd place
      inp <- append(inp, list(new_row), after = 2)
      names(inp)[3] <- "files"
    }
    class(inp) <- "inp"
    inp
  }
  
  inp <- inp %>% update_files_tab(routing_interface_path = routing_interface_path)
  
  # adjust catchment proporities   
  last_none_zero <- function(x){
    # Purpose: Return the last none zero element of x
    # Input: x, a vector of integers
    # Output: the last none-zero element
    # Detail: the last element is used as the value to modify inp
    x[ifelse(any(x != 0), max(which(x != 0)), 1)]
  }
  
  na_to_zero <- function(x){
    # Purpose: replace NA with 0
    # Input: x = a vector of numbers
    # Output: NA replaced x
    # Detail: subcatchment that remains unchanged receives change paras of NA, change NA to 0
    replace(x, is.na(x), 0)
  }
  
  subcatchment_adj <- GI_plan %>%
    group_by(subcatchment_name) %>%
    summarise(per_area_rep = sum(per_area_rep),
              imp_area_rep = sum(imp_area_rep),
              width_adj = last_none_zero(width_adj))
  
  inp$subcatchments <- inp$subcatchments %>% 
    left_join(subcatchment_adj, by = c("Name" = "subcatchment_name")) %>%
    mutate(per_area_rep = na_to_zero(per_area_rep),
           imp_area_rep = na_to_zero(imp_area_rep),
           width_adj =  na_to_zero(width_adj),
           rep_ratio = 1 - (per_area_rep + imp_area_rep)/Area, # Area_after_GI/Area_current
           Perc_Imperv = (Area*Perc_Imperv/100 - imp_area_rep)/(Area * rep_ratio) * 100,
           Area = Area * rep_ratio,
           Width = replace(Width, width_adj != 0, width_adj[width_adj != 0])) %>%
    select(Name:Snowpack)
  
  
  # adjust other parameters with name tab_variableName if necessary
  adjust_other_variable <- function(side_plan, inp){
    # Purpose:
    #     adjust variables other than those presented in [SUBCATCHMENTS]
    # Input:
    #     side_plan = tibble of the side effects of implementing GI
    #     inp = SWMM input read by read_inp
    # Output:
    #     inp = modified inp
    
    # Iterate for each side effect variable
    for (side_var_ind in 2:ncol(side_plan)){
      
      # df = tibble of subcatchment name and one side effect variable
      df <- side_plan[, c(1, side_var_ind)] %>%
        .[complete.cases(.),]
      
      # indentify the tabs and varaible names in inp
      tab_var <- str_split(names(df)[2], "_", simplify = T) %>% as.vector()
      tab <- tab_var[1] %>% tolower()
      var <- tab_var[2] %>% tolower()
      
      tabs <- sapply(names(inp), tolower) %>% unname()
      tab_ind <- which(tabs == tab)
      
      target_df <- inp[[tab_ind]]
      vars <- colnames(target_df) %>% tolower()
      var_ind <- which(vars == var)
      
      # change target_df for each modification in GI_plan
      # if multiple changes are made to a single subcatchment, the last one counts
      for (i in 1:nrow(df)){
        if (!all(is.na(target_df[[var_ind]]))){
          # If all the value in current inp is NA, class is set as "character"
          # new value is then convert into this class to enbale assignment in tibble
          target_class <- class(target_df[[var_ind]])
        } else {
          target_class <- "character"
        }
        new_value <- unlist(lapply(df[[i,2]], paste0('as.',target_class)))
        
        subcatch_ind <- which(target_df$Subcatchment == df$subcatchment_name[i])
        target_df[[subcatch_ind, var_ind]] <- new_value
      }
      inp[[tab_ind]] <- target_df
    }
    
    inp
  }
  
  if (length(names(GI_plan)) > 6){
    side_plan <- GI_plan[,c(-1:-2, -4:-6)]
    
    inp <- adjust_other_variable(side_plan, inp)
  }
  
  # return
  class(inp) <- "inp"
  inp
}
