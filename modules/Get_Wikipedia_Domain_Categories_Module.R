### VERIFY ATTACHMENT OF REQUIRED PACKAGES ###
required_packages <- c("dplyr", "stringr", "WikipediR")
for(pack in required_packages){
  if(!pack %in% (.packages())){
    library(pack, character.only = TRUE)
  }
}
rm(required_packages, pack)

### FUNCTION FOR RECURSIVE MATCHING INSIDE A NESTED LIST ###
unnest_list <- function(x, name) { 
  pos <- match(name, names(x)) 
  if (!is.na(pos)) 
    return(x[[pos]]) 
  
  for (el in x) { 
    if (class(el) == "list") { 
      out <- unnest_list(el, name) 
      if (!is.null(out)) return(out) 
    } 
  } 
} 

### FUNCTION FOR GETTING SUBCATEGORIES FROM A SINGLE CATEGORY ###
get_subcats <- function(category, language = "en"){
  #browser()
  subcats_out <- pages_in_category(language = language, project = "wikipedia", categories = category,
                                   type = "subcat", limit = 500)
  subcats_list <- unnest_list(subcats_out, "categorymembers")
  subcats_df <- bind_rows(subcats_list)
  subcats <- str_extract(string = subcats_df$title, pattern = "(?<=:).*$")
  return(subcats)
}

### FUNCTION FOR ADDING CATEGORY-SUBCATEGORIES EDGES TO EDGELIST ###
add_subcats_to_edgelist <- function(category, subcats){
  edge_list <- data.frame(super=character(), sub=character(), stringsAsFactors = FALSE)  
  for(subcat in subcats){
    temp <- list(super=category, sub=subcat)
    edge_list <- rbind(edge_list, temp, stringsAsFactors = FALSE)
  }
  return(edge_list)
}

### FUNCTION FOR DRILLING DOWN DEGREES FROM DOMAIN CATEGORY ###
drill_down_domain <- function(domain, blacklist_subcats, blacklist_substrings, degrees, language){
  edge_list <- data.frame(super=character(), sub=character(), stringsAsFactors = FALSE)
  subcats <- c()
  subcats <- c(subcats, domain)
  subcat_log <- c()
  subcats_counter <- 0
  degree_counter <- 0
  
  while(degree_counter <= degrees){
    
    subcats_next_degree <- c()
    
    for(subcat in subcats){
      subcat_log <- c(subcat_log, subcat)
      
      if(length(subcats) == 1){
        degree_counter = degree_counter + 1
      }
      
      subcats_counter <- subcats_counter + 1
      if(subcats_counter %% 50 == 0){
        message(paste0("Processed Subcats:\n", paste(subcat_log, collapse = "\n"),"\r",appendLF=TRUE))
        message(paste0("Total Processed Subcats: ",subcats_counter),"\r",appendLF=TRUE)
        message(paste0("Total Remaining Subcats: ",length(subcats)),"\r",appendLF=TRUE)
        message(paste0("Ellapsed Time: ",(Sys.time() - t1)),"\r",appendLF=TRUE)
        flush.console()
        subcat_log <- c()
      }
      #browser()
      subcats_temp <- get_subcats(subcats[1], language)
      
      if(length(subcats_next_degree) == 0 && length(subcats_temp) == 0 && degree_counter == 1){
        edge_list_temp <- add_subcats_to_edgelist(category = domain, subcats = domain)
        edge_list <- rbind(edge_list, edge_list_temp, stringsAsFactors = FALSE)
        return(edge_list)
      }
      else if(length(subcats_temp) == 0){
        subcats <- subcats[-1]
        next
      }
      
      subcats_temp <- subcats_temp[!grepl(pattern = paste(blacklist_substrings, collapse = "|"), subcats_temp, ignore.case = TRUE)]
      subcats_temp <- subcats_temp[!subcats_temp %in% blacklist_subcats]
      subcats_temp <- subcats_temp[!subcats_temp %in% edge_list$super]
      
      edge_list_temp <- add_subcats_to_edgelist(category = subcats[1], subcats = subcats_temp)
      edge_list <- rbind(edge_list, edge_list_temp, stringsAsFactors = FALSE)
      
      subcats_next_degree <- c(subcats_next_degree, subcats_temp)
      subcats <- subcats[-1]
      
    }
    subcats <- c(subcats, subcats_next_degree)
  }
  return(edge_list)
}
