### VERIFY ATTACHMENT OF REQUIRED PACKAGES ###
required_packages <- c("dplyr", "stringr", "WikipediR")
for(pack in required_packages){
  if(!pack %in% (.packages())){
    library(pack, character.only = TRUE)
  }
}
rm(required_packages, pack)

get_page_links <- function(page_titles){
  links_domain <- data.frame(page=character(), links=character(), backlinks=character(), stringsAsFactors = FALSE)
  counter <- 0
  
  for(p in page_titles){
    link_fetching <- tryCatch(
      {
      links <- page_links(language = "en", project = "wikipedia", page = p, limit = 500, namespaces = c(0,14))
      links <- unname(unlist(lapply(X = links[[2]][[1]][[1]][[4]], function(x) x[names(x) == "title"])))
      },
      error = function(e) e
    )
    if(!inherits(link_fetching, "error") & class(links) == "character"){
      links <- paste(links, collapse = "|")
    } else {
      links <- ""
    }
    
    backlink_fetching <- tryCatch(
      {
      backlinks <- page_backlinks(language = "en", project = "wikipedia", page = p, limit = 500, namespaces = c(0,14))
      backlinks <- unname(unlist(lapply(X =  backlinks[[2]][[1]], function(x) x[names(x) == "title"])))
      },
      error = function(e) e
    )
    if(!inherits(backlink_fetching, "error") & class(backlinks) == "character"){
      backlinks <- paste(backlinks, collapse = "|")
    } else {
      backlinks <- ""
    }
    
    links_domain_temp <- data.frame(page=p, links=links, backlinks=backlinks, stringsAsFactors = FALSE)
    links_domain <- rbind(links_domain, links_domain_temp)
    
    counter <- counter + 1
    if(counter %% 1000 == 0){
      message(paste0("Processed pages: ",counter),"\r",appendLF=TRUE)
      message(paste0("Unprocessed pages: ",(length(page_titles) - counter)),"\r",appendLF=TRUE)
      message(paste0("Ellapsed Time: ",(Sys.time() - t1)),"\r",appendLF=TRUE)
      flush.console()
    }
  }
  return(links_domain)
}
