### VERIFY ATTACHMENT OF REQUIRED PACKAGES ###
required_packages <- c("dplyr", "stringr", "WikipediR", "rvest")
for(pack in required_packages){
  if(!pack %in% (.packages())){
    library(pack, character.only = TRUE)
  }
}
rm(required_packages, pack)

### FUNCTION FOR GETTING TEXT IN <P> TAGS FROM ALL SELECTED PAGES ###
get_texts <- function(selected_pages, language = "en"){
  texts_domain <- data.frame(page=character(), text=character(), stringsAsFactors = FALSE)
  counter <- 0
  #browser()
  for(p in selected_pages){
    content_fetching <- tryCatch(
      {
      content_temp <- page_content(page_name = p, language = language, project = "wikipedia", as_wikitext = FALSE)
      content_html <- read_html(content_temp$parse$text$`*`)
      content_para <- html_nodes(x = content_html, css = "p")
      content_para_text <- html_text(content_para)
      content_li <- html_nodes(x = content_html, css = "li")
      content_li_text <- html_text(content_li)
      content_text <- c(content_para_text, content_li_text)
      },
      error = function(e) e
    )
    if(!inherits(content_fetching, "error") && class(content_text) == "character"){
      content_text <- paste(content_text, collapse = "|")
    } else {
      content_text <- ""
    }
    pages_text_df_temp <- data.frame(page=p, text=content_text, stringsAsFactors = FALSE)
    texts_domain <- rbind(texts_domain, pages_text_df_temp)
    
    counter <- counter + 1
    if(counter %% 100 == 0){
      message(paste0("Processed pages: ",counter),"\r",appendLF=TRUE)
      message(paste0("Unprocessed pages: ",(length(selected_pages) - counter)),"\r",appendLF=TRUE)
      message(paste0("Ellapsed Time: ",(Sys.time() - t1)),"\r",appendLF=TRUE)
      flush.console()
    }
  }
  return(texts_domain)
}
