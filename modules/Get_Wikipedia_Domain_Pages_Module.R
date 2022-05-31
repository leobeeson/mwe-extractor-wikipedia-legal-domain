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

### FUNCTION FOR GETTING PAGES FROM A SINGLE CATEGORY###
get_pages <- function(category, language = "en"){
  pages <- ""
  page_fetching <- tryCatch(
    {
    pages_out <- pages_in_category(language = language, project = "wikipedia", categories = category, type = "page", limit = 500)
    pages_list <- unnest_list(pages_out, "categorymembers")
    pages_df <- bind_rows(pages_list)
    pages <- pages_df$title[pages_df$ns == 0]
    },
    error = function(e) e
  ) 
  if(!inherits(page_fetching, "error") & class(pages) == "character"){
    return(pages)
  } else {
    pages <- ""
    return(pages)
  }
}


### FUNCTION FOR GETTING ALL PAGES FROM ALL DOMAIN SUBCATS ###
get_pages_from_all_cats <- function(selected_cats, blacklist_substrings_pages, language){  
  edge_list_pages <- data.frame(category=character(), page=character(), stringsAsFactors = FALSE)
  counter <- 0
  for(s in selected_cats){
    selected_pages <- get_pages(s, language)
    selected_pages <- selected_pages[!grepl(pattern = paste(blacklist_substrings_pages, collapse = "|"), selected_pages, ignore.case = TRUE)]
    edge_list_pages_temp <- data.frame(category=rep(s, length(selected_pages)), page=selected_pages, stringsAsFactors = FALSE)
    edge_list_pages <- rbind(edge_list_pages, edge_list_pages_temp)
    
    counter <- counter + 1
    if(counter %% 100 == 0){
      message(paste0("Processed cats: ",counter),"\r",appendLF=TRUE)
      message(paste0("Unprocessed cats: ",(length(selected_cats) - counter)),"\r",appendLF=TRUE)
      message(paste0("Ellapsed Time: ",(Sys.time() - t1)),"\r",appendLF=TRUE)
      flush.console()
    }
  }
  return(edge_list_pages)
}
