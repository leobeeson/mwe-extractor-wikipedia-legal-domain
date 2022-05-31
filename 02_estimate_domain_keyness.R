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


############################################
##### CONFIGURE THREADING FOR QUANTEDA #####
############################################
quanteda_options(threads = (RcppParallel::defaultNumThreads() - 1))
cat(paste0("Quanteda using ", quanteda_options("threads"), " parallel processing threads.\nLeaving 1 available for you to browse for cat pictures while you wait..."))


##### Get negative corpora:
corpora_directory_filepath <- "negative_corpora/"
domain_texts_file_names <- list.files(corpora_directory_filepath, pattern = "domain_texts")

negative_corpora_classes <- c("automotive_industry", "babycare", "biology", "climate", "culture", "dairy", "detergents", "drinks", "economics", "ethnicity", 
                              "foods", "language", "law", "military", "music", 
                              "personal_hygiene", "physics", "religion", "sports", "tobacco",
                              "financial_services", "forms_of_government", "government", "human_rights", "national_security", "politics", "united_nations")

domain_texts_df <- data.frame(page=character(), text=character(), category=character(), stringsAsFactors = FALSE)

system.time(
  for(dom_texts in domain_texts_file_names){
    
    domain <- dom_texts %>%
      str_split(pattern = "_domain_texts", n = 2) %>%
      .[[1]] %>%
      .[1]
    
    if(!domain %in% negative_corpora_classes){
      next
    }
    
    texts_df <- readRDS(paste0(corpora_directory_filepath, dom_texts))
    names(texts_df) <- c("page", "text")
    texts_df$category <- domain
    
    domain_texts_df <- rbind(domain_texts_df, texts_df)
  }
) # elapsed: 2.14

domain_texts_df$super <- "negative"

domain_texts_df <- domain_texts_df[,c("category", "page", "text", "super")]

rm(list=setdiff(ls(), c("domain_texts_df")))

##### Get Wikipedia law data:
law_domain_texts <- readRDS(file = "resources/law_domain_all_texts.RDS")

corpus_df <- rbind(law_domain_texts, domain_texts_df)
corpus_df$text <- str_replace_all(corpus_df$text, pattern = fixed("|"), replacement = " \n zuluzulu ")
rm(law_domain_texts, domain_texts_df)


############################
###### TOKENIZE CORPUS #####
############################

# CREATE CORPUS:
system.time(
  corpus_obj <- corpus(x = corpus_df, text_field = "text")
) # elapsed: 2.57 secs
rm(corpus_df)


# TOKENIZE:
system.time(
  tokens_obj <- tokens_tolower(tokens(x = corpus_obj, 
                                      remove_hyphens = FALSE, 
                                      remove_punct = FALSE,
                                      remove_numbers = FALSE,
                                      remove_symbols = FALSE))
) # elapsed:  299.11  secs
# saveRDS(tokens_obj, file = "resources/tokens_obj_with_negative_corpora_for_keyness.RDS")
# tokens_obj <- readRDS(file = "resources/tokens_obj_with_negative_corpora_for_keyness.RDS")
length(unique(types(tokens_obj))) # [1] 506,139
rm(corpus_obj)

# REMOVE ZULUZULU:
system.time(
  tokens_obj <- tokens_select(tokens_obj, 
                                      pattern = "zuluzulu", 
                                      valuetype = "fixed", 
                                      selection = "remove", 
                                      padding = TRUE)
) # elapsed: 5.29 secs     
length(unique(types(tokens_obj))) # 506,138
#grep(pattern = "zuluzulu", x = types(tokens_obj), value = TRUE)

######################################
##### EXTRACT KEY PHRASE KEYNESS #####
######################################

#stopwords <- c(stopwords("english"), stopwords("french"), stopwords("portuguese"), stopwords("spanish"))

# Get MWE for domains:
# mwe_keyness_law <- readRDS("mwe/mwe_wikipedia_law_processed_size_2-3_min_count_30_min_doc_10.RDS")
mwe_keyness_law <- readRDS("mwe/mwe_wikipedia_law_processed_size_2-4_min_count_10_min_doc_10.RDS")
mwe_features <- mwe_keyness_law$collocation
mwe_features_compounded <- mwe_features %>%
  str_replace_all(" ", "_") %>%
  unique()

rm(mwe_keyness_law)

# Tokenise:
system.time(
  toks <- tokens_obj %>%
    tokens_compound(pattern = phrase(mwe_features), join = TRUE) %>%
    tokens_select(pattern = mwe_features_compounded, selection = "keep", valuetype = "fixed", padding = TRUE)
) # elapsed: 11.34 secs
rm(tokens_obj, mwe_features, mwe_features_compounded)

if(!dir.exists("keyness")){
  dir.create("keyness")
}

categories <- unique(docvars(toks, "super"))
categories <- categories[categories != "negative"]
categories <- categories[categories != "UK Law"]


topfeats_specific_stopwords <- c("")
topfeats_specific_stopwords_regex <- c("observed_variables", "latent_class", "spice_girls", "microsoft_office", "little_britain",
                                       "green_bank", "free_range")

normalize <- function(x){
  (x-min(x))/(max(x)-min(x))
}

# mls_topic_taxonomy <- read.csv("../../RB/RB_MLS_topics/resources/MLS_topic_taxonomy.csv", header = TRUE, col.names = c("category", "parent_category"),
#                                stringsAsFactors = FALSE)

mwe_keyness_law <- data.frame(feature=character(), chi2=numeric(), p=numeric(), n_target=numeric(), n_reference=numeric(), 
                                    topic=character(), topic_parent=character(), length=numeric(), client=character(), ontology=character(),
                                    stringsAsFactors = FALSE)

keyness_dfm <- toks %>%
  dfm() %>%
  dfm_remove(pattern = topfeats_specific_stopwords) %>%
  dfm_remove(pattern = topfeats_specific_stopwords_regex, valuetype = "regex") 

#i <- 11
#categories[i]
# Select category keyness:
system.time(
  for(i in 1:length(categories)){
    
    keyness <- keyness_dfm %>%
      dfm_group(groups = super) %>%
      textstat_keyness(target = categories[i]) %>%
      mutate(normalized_chi2=normalize(chi2)) %>%
      filter(normalized_chi2 > 0.35 & n_target > 40) 
      
    
    if(nrow(keyness) == 0){
      next
    }
    topic_cat <- tolower(str_replace_all(categories[i], " ", "_"))
    # topic_parent <- mls_topic_taxonomy$parent_category[tolower(mls_topic_taxonomy$category) == topic_cat]
    topic_parent <- "UK Law"
    length <- str_count(keyness$feature, pattern = "_") + 1
    client <- "Client XYZ"
    ontology <- "Law"
      
    keyness$topic <- topic_cat
    keyness$topic_parent <- topic_parent
    keyness$length <- length
    keyness$ontology <- ontology
    
    saveRDS(keyness, file = paste0("keyness/mwe_keyness_law_topics_",categories[i],".RDS"))
    
    mwe_keyness_law <- rbind(mwe_keyness_law, keyness)
  }
) # elapsed: 2890.525 secs

saveRDS(mwe_keyness_law, file = "keyness/mwe_keyness_law_topics_size_2-4_min_count_10_min_doc_10.RDS")
 #"mwe/mwe_wikipedia_law_processed_size_2-4_min_count_10_min_doc_10.RDS"

write.csv(mwe_keyness_law, file = "outputs/mwe_keyness_law_topics_size_2-4_min_count_10_min_doc_10.csv")
