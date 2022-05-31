if("quanteda" %in% installed.packages() == FALSE){
  install.packages("quanteda", repos="http://cran.us.r-project.org")
}
library(quanteda)
if("dplyr" %in% installed.packages() == FALSE){
  install.packages("dplyr", repos="http://cran.us.r-project.org")
}
library(dplyr)

if("stringr" %in% installed.packages() == FALSE){
  install.packages("stringr", repos="http://cran.us.r-project.org")
}
library(stringr)

if("WikipediR" %in% installed.packages() == FALSE){
  install.packages("WikipediR", repos="http://cran.us.r-project.org")
}
library(WikipediR)

source("modules/Get_Wikipedia_Domain_Categories_Module.R")
source("modules/Get_Wikipedia_Domain_Pages_Module.R")
source("modules/Get_Wikipedia_Domain_Texts_Module.R")
source("resources/99_white_and_blacklists.R")

############################################
##### CONFIGURE THREADING FOR QUANTEDA #####
############################################
quanteda_options(threads = (RcppParallel::defaultNumThreads() - 1))
cat(paste0("Quanteda using ", quanteda_options("threads"), " parallel processing threads.\nLeaving 1 available for you to browse for cat pictures while you wait..."))


##### PREPARE TAXONOMY DATA #####
# Initialize time stamps; they get reassigned a real time stamp when calling a function:
t0 <- Sys.time()
t1 <- Sys.time()

get_wikipedia_corpora <- function(domain, degrees, 
                                  blacklist_subcats = c("xxxxxxx", "yyyyyyy"), 
                                  blacklist_substrings = c("xxxxxxx", "yyyyyyy"), 
                                  blacklist_substrings_pages = c("xxxxxxx", "yyyyyyy"), 
                                  parent_category,
                                  language = "en"){
  
  assign(x = "t0", value = Sys.time(), envir = .GlobalEnv)
  assign(x = "t1", value = Sys.time(), envir = .GlobalEnv)
  
  
  # Get subcats from domain:
  
  edge_list_domain <- drill_down_domain(domain, blacklist_subcats, blacklist_substrings, degrees, language)
  
  message(paste0("TOTAL ELLAPSED TIME FROM DRILL DOWN DOMAIN: ",(Sys.time() - t1)),"\r",appendLF=TRUE)
  flush.console()
  
  if(!dir.exists("resources")){
    dir.create("resources")
  }
  saveRDS(edge_list_domain, file = paste0("resources/", tolower(domain), "_domain_categories_degree_", degrees, ".RDS"))
  
  message(paste0("TOTAL SUPER CATEGORIES: ",length(unique(edge_list_domain$super))),"\r",appendLF=TRUE)
  message(paste0("TOTAL SUBCATEGORIES: ",length(unique(edge_list_domain$sub))),"\r",appendLF=TRUE)
  message(paste0("USING ",length(unique(edge_list_domain$sub))," TOTAL SUBCATEGORIES"),"\r",appendLF=TRUE)
  flush.console()
  
  ##### GET PAGES FROM DOMAIN #####
  t1 <- Sys.time()
  
  subcats_domain <- unique(c(edge_list_domain$super, edge_list_domain$sub))
  pages_domain <- get_pages_from_all_cats(subcats_domain, blacklist_substrings_pages, language)
  
  message(paste0("TOTAL ELLAPSED TIME FROM GET PAGES FROM ALL SUBCATEGORIES: ",(Sys.time() - t1)),"\r",appendLF=TRUE)
  flush.console()
  
  saveRDS(pages_domain, file = paste0("resources/", str_replace_all(tolower(domain), " ", "_"), "_domain_pages_degree_", degrees, ".RDS"))
  pages_domain_vector <- unique(pages_domain$page)
  
  ##### GET TEXTS FROM PAGES #####
  t1 <- Sys.time()
  
  texts_domain <- get_texts(pages_domain_vector, language)
  message(paste0("TOTAL ELLAPSED TIME FROM GET ALL TEXTS: ",(Sys.time() - t1)),"\r",appendLF=TRUE)
  flush.console()
  
  texts_domain <- left_join(pages_domain, texts_domain, by = "page")
  texts_domain <- left_join(texts_domain, edge_list_domain, by =c("category" = "sub"))
  texts_domain$super[is.na(texts_domain$super)] <- parent_category
  
  saveRDS(texts_domain, file = paste0("resources/", str_replace_all(tolower(domain), " ", "_"), "_domain_texts_degree_", degrees, ".RDS"))
  
  if(!dir.exists("outputs")){
    dir.create("outputs")
  }
  #write.csv(texts_domain, file = paste0("outputs/", str_replace_all(tolower(domain), " ", "_"), "_domain_texts_degree_", degrees, ".csv"))
  
  message(paste0("TOTAL ELLAPSED TIME FROM GET WIKIPEDIA CORPUS FOR ", toupper(domain), ": ",(Sys.time() - t0)),"\r",appendLF=TRUE)
  flush.console()
}

positive_domain_set <- c("United Kingdom company law", "English contract law", "English property law", "English tort law")
negative_domain_set <- c()
domain_set <- c(positive_domain_set, negative_domain_set)
domain_set_degrees <- rep(c(0, 0), c(length(positive_domain_set), length(negative_domain_set)))
blacklist_subcats <- c("Construction documents", "Technical drawings", "Event management", "Software engineering costs",
                       "Software development", "Extreme programming", "Maturity models", "Project management education",
                       "Project Management Institute", "Projectors (business)", "GitHub", "Concept- and mind-mapping software for Linux",
                       "Concept- and mind-mapping software programmed in Java", "Distributed bug tracking systems", "Help desk software",
                       "Project hosting websites")
negative_blacklist_substrings <- c("fictional", "of India") #, "list", 
                                   #"by continent", "by country", "by nationality", "by etnicity", "by language", "by region", "by area", "by place", "by location", 
                                   #"by gdp", "by population", "people in", "works about", "books", " v\\. ", "profession", "Statisticians")
negative_blacklist_substrings_pages <- negative_blacklist_substrings
parent_category <- "UK Law"

t3 <- Sys.time()
for(i in 1:length(domain_set)){
  get_wikipedia_corpora(domain = domain_set[i], 
                        degrees = domain_set_degrees[i], 
                        blacklist_subcats = blacklist_subcats,
                        blacklist_substrings = negative_blacklist_substrings, 
                        blacklist_substrings_pages = negative_blacklist_substrings_pages,
                        parent_category = parent_category)   
}
t4 <- Sys.time()
t4 - t3 # Time difference of 5.550328 mins




















##### LOAD CORPUS: ----
taxonomy_corpus_df <- readRDS(file = "resources/project_management_domain_texts_degree_4.RDS")

taxonomy_corpus_df <- taxonomy_corpus_df %>% 
  distinct(page, .keep_all = TRUE)

# Remove
# taxonomy_corpus_df$text <- taxonomy_corpus_df$text %>%
#   str_replace_all(pattern = "\\|Marketing\\|Marketing management\\|Distribution\\|Pricing\\|Retail\\|Service\\|Activation\\|Brand licensing\\|Brand management\\|Co-creation\\|Corporate identity\\|Dominance\\|Effectiveness\\|Ethics\\|Mystery shopping\\|Promotion\\|Research\\|Segmentation\\|Strategy\\|Account-based marketing\\|Digital marketing\\|Product marketing\\|Social marketing\\|Influencer marketing\\|Attribution\\|Advertising\\|Branding\\|Corporate anniversary\\|Direct marketing\\|Loyalty marketing\\|Mobile marketing\\|On-hold messaging\\|Personal selling\\|Premiums\\|Prizes\\|Product placement\\|Propaganda\\|Publicity\\|Sales promotion\\|Sex in advertising\\|Underwriting spot\\|Behavioral targeting\\|Brand ambassador\\|Broadcasting\\|Display advertising\\|Drip marketing\\|In-game advertising\\|Mobile advertising\\|Native advertising\\|New media\\|Online advertising\\|Out-of-home advertising\\|Point of sale\\|Printing\\|Product demonstration\\|Promotional merchandise\\|Publication\\|Visual merchandising\\|Web banner\\|Word-of-mouth\\|v\\|t\\|e\\|", 
#                   replacement = "")

for (i in 1:length(taxonomy_corpus_df$text)) {
  temp_text <- taxonomy_corpus_df$text[i]
  temp_text_sub <- str_sub(temp_text, start = 1, end = str_locate(temp_text,"(\\^)")[1])
  taxonomy_corpus_df$text[i] <- temp_text_sub 
  }

#check:
#str_extract_all(string = taxonomy_corpus_df$text[1:100], pattern = "\\|\\w+.*\\|") 

##### LOAD PROCESSED MWE:
#mwe_obj_processed <- readRDS(file = "outputs/mwe_df_marketing_taxonomy_degree1_processed.RDS")

##### DETECT WORDS WITH AMPERSAND:
compound_ampersands <- str_extract_all(string = taxonomy_corpus_df$text, pattern = "\\w+\\s{1}&\\s{1}\\w+") %>%
  unlist() %>% # elapsed: 6.84 secs
  tolower() %>%
  unique() %>% 
  .[!is.na(.)]

whitelist_phrases <- c(whitelist_phrases, compound_ampersands)
rm(compound_ampersands)

##### FUNCTION FOR TOKENISING AND COMPOUNDING KEY PHRASES:
# compound_corpus <- function(df, text_field, docid_field, 
#                             blacklist_phrases, whitelist_phrases, blacklist_words, stopwords, substrings_to_filter_out){
# Create corpus object:
corpus_obj <- corpus(x = taxonomy_corpus_df, docid_field = "page", text_field = "text")
# Tokenize:
tokens_obj <- tokens_tolower(tokens(x = corpus_obj, 
                                    split_hyphens = FALSE, 
                                    remove_punct = FALSE,
                                    remove_numbers = FALSE,
                                    remove_symbols = FALSE))
# Remove blacklisted phrases: 
tokens_obj <- tokens_compound(tokens_obj, 
                              pattern = phrase(blacklist_phrases), 
                              concatenator = "_", 
                              join = FALSE)
# Remove tokens with "_": 
tokens_obj <- tokens_select(tokens_obj, 
                            pattern = "_", 
                            selection = "remove", 
                            valuetype = "regex",
                            case_insensitive = TRUE, 
                            padding = TRUE)
# Compound whitelisted phrases:
tokens_obj <- tokens_compound(tokens_obj, 
                              pattern = phrase(whitelist_phrases), 
                              concatenator = "_", valuetype = "fixed",
                              join = FALSE)
length(unique(types(tokens_obj))) 
# Filter tokens with acceptable characters:
tokens_obj <- tokens_select(tokens_obj, 
                            pattern = "^[A-Za-zšœÀ-ÖØ-öø-ÿ&_-]+$", # removed digits (0-9)
                            valuetype = "regex", 
                            selection = "keep", 
                            padding = TRUE)
length(unique(types(tokens_obj))) # 18,478
# Remove one-letter words:
tokens_obj <- tokens_select(tokens_obj, 
                            pattern = "(^&|\\w{2,})", 
                            valuetype = "regex", 
                            selection = "keep", 
                            padding = TRUE)
length(unique(types(tokens_obj)))
# Remove blacklisted words:
tokens_obj <- tokens_select(tokens_obj, 
                            pattern = c(stopwords, blacklist_words), 
                            selection = "remove", 
                            case_insensitive = TRUE, 
                            padding = TRUE)


domain_specific_stop_words <- c(
  "citation", "reference", "\\balso", "\\bnotes", "issn", "isbn", "retrieved", "wayback", "archived", "accessed", "cs1", "http", "(\\d+(px|em))", "(\\w{4,}\\d{1,2})", "\\b\\w{1}\\d{1,3}", # bibliographical/wikipedia/web terms
  "january", "february", "march", "april", "may", "june", "july", "august", "september", "october", "november", "december", "month", "week", "century", "\\bdate", # time references
  "(\\d+(st|nd|rd|th|s))", "previous", "\\blast\\b", "\\bpast\b", "\\bago\\b",  "since\\b", "\\brecent", "brief", "throughout", # time references
  "please", "\\btell\\b", "written", "ipsos", "confidential", "proprietary", "\\bdisclosed", "reproduced", "formally", "about", "\\bhow\\b", "when", "(\\blet us\\b)", # survey text
  "\\bcan\\b", "\\bcould\\b", "\\bmay\\b", "\\bmight\\b", "\\bshall\\b", "\\bshould\\b", "\\bwill\\b", "\\bwould\\b", "\\bmust\\b", "\\bought\\b" # auxiliary verbs - we want to focus more on nouns/entities for this use case.
)

# Filter tokens obj with domain-specific stopwords:

tokens_obj <- tokens_select(tokens_obj, 
                            pattern = domain_specific_stop_words, 
                            selection = "remove", 
                            valuetype = "regex", 
                            padding = TRUE)
length(unique(types(tokens_obj))) # 18,193

##### Estimate MWE:
if(!dir.exists("mwe")){
  dir.create("mwe")
}
system.time(
  mwe_obj <- textstat_collocations(x = tokens_obj, size = c(2:5), min_count = 10) # elapsed: 792.91 secs 
)
saveRDS(object = mwe_obj, file = "mwe/mwe_wikipedia_project_management_size_2-5_min_count_10.RDS")
#mwe_obj <- readRDS(file = "mwe/mwe_geography_jti_size_2-3_min_count_50_min_doc_10.RDS")
length(mwe_obj$collocation) # [1] 37,491


