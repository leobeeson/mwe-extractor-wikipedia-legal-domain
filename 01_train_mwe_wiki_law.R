if("quanteda" %in% installed.packages() == FALSE){
  install.packages("quanteda", repos="http://cran.us.r-project.org")
}
library(quanteda)
if("quanteda.textstats" %in% installed.packages() == FALSE){
  install.packages("quanteda.textstats", repos="http://cran.us.r-project.org")
}
library(quanteda.textstats)
if("nsyllable" %in% installed.packages() == FALSE){
  install.packages("nsyllable", repos="http://cran.us.r-project.org")
}
library(nsyllable)
if("stopwords" %in% installed.packages() == FALSE){
  install.packages("stopwords", repos="http://cran.us.r-project.org")
}
library(stopwords)
if("dplyr" %in% installed.packages() == FALSE){
  install.packages("dplyr", repos="http://cran.us.r-project.org")
}
library(dplyr)
if("stringr" %in% installed.packages() == FALSE){
  install.packages("stringr", repos="http://cran.us.r-project.org")
}
library(stringr)


##### CONFIGURE THREADING FOR QUANTEDA:
quanteda_options(threads = (RcppParallel::defaultNumThreads() - 1))
cat(paste0("Quanteda using ", quanteda_options("threads"), " parallel processing threads.\nLeaving 1 available for you to browse for cat pictures while you wait..."))

##### Get Wikipedia law corpus:
filepath_to_corpora <- "resources/"
domain_texts_file_names <- list.files(filepath_to_corpora, pattern = "domain_texts")
domain_texts_df <- data.frame(category=character(), page=character(), text=character(), super=character(), stringsAsFactors = FALSE)

for(dom_texts in domain_texts_file_names){
  
  domain <- dom_texts %>%
    str_split(pattern = "_domain_texts", n = 2) %>%
    .[[1]] %>%
    .[1]
  
  texts_df <- readRDS(paste0(filepath_to_corpora, dom_texts))
  # names(texts_df) <- c("page", "text")
  # texts_df$category <- domain
  
  domain_texts_df <- rbind(domain_texts_df, texts_df)
}

domain_texts_df$text <- str_replace_all(domain_texts_df$text, pattern = fixed("|"), replacement = " \n zuluzulu ")
saveRDS(domain_texts_df, file = "resources/law_domain_all_texts.RDS")
rm(list=setdiff(ls(), "domain_texts_df"))


# CREATE CORPUS:
system.time(
  corpus_obj <- corpus(x = domain_texts_df, text_field = "text")
) # elapsed: 2.11
rm(domain_texts_df)


# TOKENIZE:
system.time(
  tokens_obj <- tokens_tolower(tokens(x = corpus_obj, 
                                                remove_hyphens = FALSE, 
                                                remove_punct = FALSE,
                                                remove_numbers = FALSE,
                                                remove_symbols = FALSE))
  ) # elapsed: 5.69 secs
length(unique(types(tokens_obj))) # [1] 42,123
rm(corpus_obj)


# REMOVE TOKENS WITH "_":
system.time(
  tokens_obj <- tokens_select(tokens_obj, pattern = "_", selection = "remove", valuetype = "regex",
                                        case_insensitive = TRUE, padding = TRUE)
  ) # elapsed: 0.15 secs
length(unique(attr(tokens_obj, "types"))) # [1] 42.093


# KEEP TOKENS WITH ACCEPTABLE CHARACTERS:
system.time(
  tokens_obj <- tokens_select(tokens_obj, 
                                        pattern = "^[A-Za-zšœÀ-ÖØ-öø-ÿ0-9&_-]{2,20}$",
                                        valuetype = "regex", 
                                        selection = "keep", 
                                        padding = TRUE)
  ) # elapsed: 0.30 secs
length(unique(types(tokens_obj))) # [1] 38,629

#grep(pattern = "zuluzulu", x = types(tokens_obj), value = TRUE)

# REMOVE TOKENS WITH ONLY 1 LETTER:
system.time(
  tokens_obj <- tokens_select(tokens_obj, 
                                        pattern = "(^&|\\w{2,})", 
                                        valuetype = "regex", 
                                        selection = "keep", 
                                        padding = TRUE)
  ) # elapsed: 0.22 secs     
length(unique(types(tokens_obj))) # [1] 38,602
#grep(pattern = "^[0-9-]+$", x = attr(tokens_obj, "types"), value = TRUE)
#unique(types(tokens_obj))[1001:2000]


# REMOVE TOKENS WITH ONLY NUMBERS:
system.time(
  tokens_obj <- tokens_select(tokens_obj, 
                                        pattern = "^[0-9-]+$", 
                                        valuetype = "regex", 
                                        selection = "remove", 
                                        padding = TRUE)
  ) # elapsed: 0.17 secs     
length(unique(types(tokens_obj))) # [1] 34,984


# CREATE TOKENS OBJECT WITH TOP FEATURES:
# Get features with frequency > 20:
system.time(
  dfm_feats <- dfm(tokens_obj)
) # elapsed: 0.40 secs
system.time(
  dfm_trimmed <- dfm_trim(dfm_feats, min_termfreq = 10, min_docfreq = 3) # [1] 
) # elapsed: 0.03 secs
feats <- featnames(dfm_trimmed)
feats <- feats[feats != ""]
rm(list=setdiff(ls(), c("tokens_obj", "feats")))

# Filter tokens obj with selected features:
system.time(
  tokens_obj_for_mwe <- tokens_select(tokens_obj, pattern = feats, selection = "keep", padding = TRUE)
) # elapsed: 0.20 secs
length(types(tokens_obj_for_mwe)) # 8,249

# REMOVE ZULUZULU:
system.time(
  tokens_obj_for_mwe <- tokens_select(tokens_obj_for_mwe, 
                                      pattern = "zuluzulu", 
                                      valuetype = "fixed", 
                                      selection = "remove", 
                                      padding = TRUE)
) # elapsed: 0.09 secs     
length(unique(types(tokens_obj_for_mwe))) # [1] 8,248
#grep(pattern = "zuluzulu", x = types(tokens_obj_for_mwe), value = TRUE)

# Define patterns for domain-specific stopwords:
domain_specific_stop_words <- c(
  "wikipedia", "citation", "reference", "\\balso", "\\bnotes", "issn", "isbn", "retrieved", "wayback", "archived", "accessed", "cs1", "http", "(\\d+(px|em))", "(\\w{4,}\\d{1,2})", "\\b\\w{1}\\d{1,3}", # bibliographical/wikipedia/web terms
  "january", "february", "march", "april", "may", "june", "july", "august", "september", "october", "november", "december", "month", "week", "century", "\\bdate", # time references
  "(\\d+(st|nd|rd|th|s))", "previous", "\\blast\\b", "\\bpast\b", "\\bago\\b",  "since\\b", "\\brecent", "brief", "throughout", # time references
  "please", "\\btell\\b", "written", "ipsos", "confidential", "proprietary", "\\bdisclosed", "reproduced", "formally", "about", "\\bhow\\b", "when", "(\\blet us\\b)", # survey text
  "\\bcan\\b", "\\bcould\\b", "\\bmay\\b", "\\bmight\\b", "\\bshall\\b", "\\bshould\\b", "\\bwill\\b", "\\bwould\\b", "\\bmust\\b", "\\bought\\b" # auxiliary verbs - we want to focus more on nouns/entities for this use case.
)

# Filter tokens obj with domain-specific stopwords:
system.time(
  tokens_obj_for_mwe <- tokens_select(tokens_obj_for_mwe, pattern = domain_specific_stop_words, selection = "remove", valuetype = "regex", padding = TRUE)
) # elapsed: 0.14 secs
length(types(tokens_obj_for_mwe)) # 3,852

if(!dir.exists("resources")){
  dir.create("resources")
}
#saveRDS(tokens_obj_for_mwe, file = "resources/tokens_obj_for_mwe.RDS")
#tokens_obj_for_mwe <- readRDS(file = "resources/tokens_obj_for_mwe.RDS")
rm(list=setdiff(ls(), c("tokens_obj_for_mwe", "domain_specific_stop_words")))
gc()


##### Estimate MWE:
if(!dir.exists("mwe")){
  dir.create("mwe")
}
system.time(
  mwe_obj <- textstat_collocations(x = tokens_obj_for_mwe, size = c(2:4), min_count = 10) # elapsed: 352.89 secs 
  ) 
saveRDS(object = mwe_obj, file = "mwe/mwe_wikipedia_law_size_2-4_min_count_10_min_doc_10.RDS")
#mwe_obj <- readRDS(file = "mwe/mwe_wikipedia_law_size_2-4_min_count_10_min_doc_10.RDS")
length(mwe_obj$collocation) # [1] 23,082


###########################
##### PRE-PROCESS MWE #####
###########################

# Initialize set of key phrases to keep which would otherwise be removed by the cleaning of the list of MWE:
key_phrases_exceptions <- c()

################################################
##### REMOVE MWE GRAMMATICALLY SUSPICIOUS  #####
################################################

# Count hyphen as additional token:
mwe_obj$hyphens <- str_count(mwe_obj$collocation, pattern = fixed("-"))
mwe_obj$ntokens <- mwe_obj$length + mwe_obj$hyphens

# By number of characters:
max_chars <- 40
mwe_obj$nchar <- nchar(mwe_obj$collocation)
mwe_obj <- mwe_obj %>%
  filter(nchar <= max_chars)
length(mwe_obj$collocation) # [1] 23,079

# By ratio of characters to tokens:
max_chars_per_tok <- 14
mwe_obj$nchars_per_tok <- (mwe_obj$nchar - mwe_obj$ntokens + 1) / mwe_obj$ntokens # remove whitespaces from nchar
mwe_obj <- mwe_obj %>%
  filter(nchars_per_tok <= max_chars_per_tok)
length(mwe_obj$collocation) # [1] 23,079

# By ratio of types to tokens:
min_types_per_tok <- 1
mwe_obj$ntypes <- ntype(mwe_obj$collocation)
mwe_obj$types_per_tok <- mwe_obj$ntypes / mwe_obj$length
length(mwe_obj$collocation) # [1] 23,079

# Capture exceptions:
key_phrases_exceptions <- union(x = key_phrases_exceptions, y = c("business to business", "customer to customer", "one to one", "face to face", "peer to peer"))
mwe_obj <- mwe_obj %>%
  filter(types_per_tok >= min_types_per_tok)
length(mwe_obj$collocation) # [1] 22,792

# By ratio of syllables to tokens:
max_sylls_per_tok <- 6
mwe_obj$nsyllables <- nsyllable(mwe_obj$collocation)
mwe_obj$sylls_per_tok <- mwe_obj$nsyllables / mwe_obj$ntokens
mwe_obj <- mwe_obj %>%
  filter(sylls_per_tok <= max_sylls_per_tok)
length(mwe_obj$collocation) # [1] 22,791

#########################################################
##### REMOVE MWE WITH LEADING OR TRAILING STOPWORDS #####
#########################################################
stopwords <- c(stopwords("en"))

sw_to_exclude <- c("")
if(exists("sw_to_exclude")){
  stopwords <- setdiff(stopwords, sw_to_exclude)
}

# ATTENTION: This is for words you don't want leading or trailing a MWE, but which is acceptable inside the MWE:
sw_to_include <- c()
if(exists("sw_to_include")){
  stopwords <- c(stopwords, sw_to_include)
}

#### HELPER FUNCTION FOR FILTERING FOR STOPWORDS #####
match_sw <- function(x, stopwords){
  x %in% stopwords
}

# Remove leading stopwords
mwe_obj$leading_sw <- sapply(X = word(string = mwe_obj$collocation, start = 1L), FUN = match_sw, stopwords)

# Remove trailing stopwords
mwe_obj$trailing_sw <- sapply(X = word(string = mwe_obj$collocation, start = -1L),FUN = match_sw, stopwords)

# Keep MWEs without leading or trailing stopwords
mwe_obj <- mwe_obj %>%
  filter(leading_sw == FALSE & trailing_sw == FALSE)
length(mwe_obj$collocation) # 4,048


################################################################
##### REMOVE DOMAIN SPECIFIC WORDS ANYWHERE INSIDE THE MWE #####
################################################################

domain_specific_stop_words <- c(domain_specific_stop_words,
                                c("university press", "external links", "\bet al\\b", "even though", "de la", "multiple names", "wide(?! web)", "per year",
                                  "commonly used", "examples include", "different\\b", "(?<!search )term(\\b|s)")
                                )



#grep("(boost to provide)", mwe_obj_processed$collocation, value = TRUE)
system.time(
  mwe_obj <- mwe_obj[!grepl(pattern = paste(domain_specific_stop_words, collapse = "|"), x = mwe_obj$collocation, perl = TRUE), ]
  ) # elapsed: 0.01 secs
length(mwe_obj$collocation) # [1] 3,994


##### SAVE PROCESSED MWE:
mwe_obj_processed <- mwe_obj
#rm(mwe_obj)
saveRDS(object = mwe_obj_processed, file = "mwe/mwe_wikipedia_law_processed_size_2-4_min_count_10_min_doc_10.RDS")
#mwe_obj_processed <- readRDS(file = "mwe/mwe_geography_processed_size_2-3_min_count_50_min_doc_10.RDS")
#write.csv(mwe_obj_processed[,1:6], file = "mwe/mwe_obj_processed.RDS, fileEncoding = "UTF-8")

