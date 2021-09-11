

# Do not run this, only needed to be run once at the start... plus it won't
# work unless behind the UoA network

#query scopus via R
library(rscopus)

rscopus::set_api_key("f9e12315185ecf1650e7d60c0ab897a9")
scopus_query <- scopus_search(query = "EXACTSRCTITLE ( \"Freshwater Invertebrate Biology\"  OR
                       \"Journal of the North American Benthological Society\"  OR  \"Freshwater Science\")",
                       view = "COMPLETE", count = 25, max_count = 2500)

scopus_results <- gen_entries_to_df(scopus_query$entries)

# save data
df_all <- saveRDS("./data/scopus_api_2021_09_10.rds")



# load and tidy data -----------------------------------------------------------
library(janitor)
library(tidyverse)
library(lubridate) # Date formats

df_all <- readRDS("./data/raw/scopus_api_2021_09_10.rds")

# colnames
df_all <- lapply(df_all, clean_names)

# Freshwater Invertebrate Biology not included - will have to add
table(df_all$df$prism_publication_name)

# metadata
df_meta <- df_all$df |> 
  mutate(prism_cover_date = as_date(prism_cover_date)) |>
  mutate_at(vars(prism_issn:prism_issue_identifier, citedby_count, 
                 author_count_limit:author_count, source_id, entry_number),
            as.numeric)
  

# author affiliations
df_affil <- df_all$affiliation |> 
  mutate_at(vars(afid), as.numeric)

# Author details
df_auth <- df_all$author |> 
  mutate_at(vars(seq, authid, afid, entry_number), as.numeric)

# add article title to df_auth and df_affil based on entry_number
df_tit <- df_meta |>
  select(entry_number, dc_title)

# df_affil
df_affil <- df_affil |>
  left_join(df_tit) |>
  select(entry_number, dc_title, everything())

# df_auth
df_auth <- df_auth |>
  left_join(df_tit) |>
  select(entry_number, dc_title, everything())


# import .bib  database --------------------------------------------------------
# pull relevant Freshwater Invertebrate Biology data which is missing from rscopus

df_man_db <- read.csv(file = "./data/raw/df_manual_db.csv")

df_man_db <- df_man_db %>%
  filter(Volume < 13)

# add unique entry number
df_man_db$entry_number <- (max(df_affil$entry_number) + 1):(max(df_affil$entry_number) + nrow(df_man_db))

# Meta data --------------------------------------------------------------------

# select and rename columns
df_man_db_meta <- df_man_db %>%
  select(entry_number, Journal, Doc_type, Title, Keywords, Times_cited, Authors,
         Abstract, Year, Volume, Issue) %>%
  rename("prism_publication_name" = "Journal",
         "subtype_description" = "Doc_type",
         "dc_title" = "Title",
         "authkeywords" = "Keywords",
         "citedby_count" = "Times_cited",
         "dc_creator" = "Authors",
         "dc_description" = "Abstract",
         "prism_cover_display_date" = "Year",
         "prism_volume" = "Volume",
         "prism_issue_identifier" = "Issue")

# get first name in right format
df_man_db_meta <- df_man_db_meta %>%
  mutate(author_count_total = str_count(dc_creator, ";") + 1,
         author_count = author_count_total,
         dc_creator = sub(";.*", "", dc_creator)) |>
  separate(dc_creator, c("last", "first"), sep = ",") |>
  mutate(first = gsub("[^A-Z]+", "", first),
         first = gsub(" ", ".",
                      str_trim(gsub("([A-Z])([A-Z])",
                                    "\\1 \\2 \\3", first))),
         first = paste(first, ".", sep = "")) |>
  unite("dc_creator", last:first, sep = " ")

df_meta2 <- plyr::rbind.fill(df_meta, df_man_db_meta)

# add year column and tidy author names
df_meta2 <- df_meta2 %>%
  mutate(year_date = as.numeric(str_sub(prism_cover_display_date,-4,-1)),
         dc_title = tolower(dc_title),
         authkeywords = tolower(authkeywords),
         authkeywords =gsub("\\|", ";",  authkeywords),
         authkeywords =gsub(" ; ", ";",  authkeywords),
         dc_creator = tolower(dc_creator),
         dc_creator = substr(dc_creator, 1, regexpr("\\.", dc_creator)-1))


# drop useless cols
df_meta2 <- df_meta2 %>%
  select(entry_number, year_date, dc_title, dc_creator, prism_publication_name,
         prism_volume:prism_page_range, prism_doi:citedby_count,
         subtype_description, author_count, authkeywords, fund_acr,
         fund_sponsor, openaccess_flag)

#save
write.csv(df_meta2, file = "./data/created/df_meta_updated_v1.csv", row.names = F)


# authors data --------------------------------------------------------------------

# relevant data from .bibs
df_man_db_auth <- df_man_db %>%
  select(entry_number, Title, Authors) %>%
  rename("dc_title" = "Title")


# separate first and last names and tidy
n_col2 <- max(str_count(df_man_db_auth$Authors, ";") + 1)
vars <- paste("a", 1:n_col2, sep = "")

df_man_db_auth2 <- df_man_db_auth |>
  separate(Authors, vars, sep = ";") |>
  pivot_longer(cols = starts_with("a"), names_to = "lets", values_to = "auths2") |>
  separate(auths2, c("last", "first"), sep = ",") |>
  filter(!is.na(last)) |>
  mutate(first = gsub("[^A-Z]+", "", first),
         first = gsub(" ", ".",
                      str_trim(gsub("([A-Z])([A-Z])",
                                    "\\1 \\2 \\3", first))),
         first = paste(first, ".", sep = "")) |>
  unite("authname", last:first, sep = " ") |>
  mutate(authname = str_trim(authname))
  
# join together 
df_auth3 <- plyr::rbind.fill(df_auth, df_man_db_auth2)

# add affiliation data 
# get columns to add 
df_affil2 <- df_affil %>%
  select(afid, affilname, affiliation_city, affiliation_country) %>%
  distinct()

# join
df_auth4 <- df_auth3 %>%
  left_join(df_affil2) %>%
  mutate(dc_title = tolower(dc_title),
         authname = tolower(authname),
         authname = substr(authname, 1, regexpr("\\.", authname)-1))


# add year col
df_yr <- df_meta2 %>%
  select(entry_number, year_date)

df_auth4 <- df_auth4 %>%
  left_join(df_yr)

# select keeper cols
df_auth4 <- df_auth4 %>%
  select(entry_number, year_date, dc_title, authid, authname:initials,
         afid, affilname:affiliation_country)
   

# fill in some missing countries       
df_country <- df_auth4 %>%
  select(authname, affiliation_country) %>%
  count(authname, affiliation_country) %>%
  arrange(authname, -n) %>%
  group_by(authname) %>%
  top_n(1) %>%
  na.omit() %>%
  select(-n)

xx <- df_auth4 %>%
  filter(is.na(affiliation_country)) %>%
  select(-affiliation_country) %>%
  left_join(df_country)

df_auth4 <- df_auth4 %>%
  filter(!is.na(affiliation_country)) %>%
  rbind(xx)

# save
write.csv(df_auth4, file = "./data/created/df_auth_updated_v1.csv", row.names = F)







