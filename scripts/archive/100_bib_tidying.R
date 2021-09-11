
# tidy up American SFS bibliometric data


# helper functions -------------------------------------------------------------

# capitalise first letter of word
firstup <- function(x) {
  y <- word(x, 1)
  z <- tolower(word(x, 1))
  substr(z, 1, 1) <- toupper(substr(z, 1, 1))
  sub(y, z, x)
}

# replace non-english letters
replace_accents <- function(string)
{
  unwanted_array = list('Š'='S', 'š'='s', 'Ž'='Z', 'ž'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E', 'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U', 'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ā'='a', 'ç'='c', 'č'='c', 'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ō'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o', 'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ü'='u', 'ū'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y')
  
  repl_string <- chartr(old = paste(names(unwanted_array), collapse = ''), 
                        new = paste(unwanted_array, collapse = ''), string)
  
  return(repl_string)
}


# tidy up author names and initials 
fun_tidy_author_names <- function(db){
  
  # requires a column called Author_style, I added this manually to the 
  # database as JSTOR and WOS resulted in two different sets of author name 
  # formatting.
  
  # select relevant columns
  db_sub <- db %>%
    select(ID, Author_style, Authors) %>%
    mutate(Authors = if_else(Author_style == 2, gsub(',', ';', Authors), Authors)) %>%
    select(-Author_style)
  
  
  
  # remove non English letters - not ideal but the function below doesn't work 
  # if there are non-English letters ("([A-Z][\\w-]+)")
  df2 <- db_sub %>%
    filter(stringi::stri_enc_isascii(Authors) == TRUE)
  
  
  # separate author names - get max number of authors for single paper
  n_col <- max(str_count(db_sub$Authors, ";") + 1)
  vars <- paste("a", 1:n_col, sep = "")
  
  # 1 author per row
  df2b <- df2 %>%
    separate(Authors, vars, sep = ";") %>%
    pivot_longer(cols = -ID, names_to = "lets", values_to = "author") %>%
    na.omit() %>%
    mutate(author = trimws(author), 
           author = gsub(',', '', author),
           author = sub("\\b([A-Z]\\S+)", "\\1,", author, perl = TRUE))
  
  # separate first and last names
  n_col2 <- max(str_count(df2b$author, ",") + 1)
  vars2 <- paste("b", 1:n_col2, sep = "")
  
  # first name starting with uppercase, rest lowercase 
  df3 <- df2b %>%
    separate(author, vars2, sep = ",") %>%
    mutate(b2 = sapply(str_extract_all(b2, '[A-Z]'), paste, collapse= ''),
           b1 = sapply(b1, firstup))
  
  # rejoin names
  df4 <- df3 %>%
    unite("author", b1:b2, sep = " ") %>%
    pivot_wider(names_from = "lets", values_from = "author") %>%
    unite("Authors", starts_with("a"), sep = "; ", remove = TRUE, na.rm = TRUE)
  
  # add back to db
  db_final <- db %>%
    select(-Authors) %>%
    left_join(df4, by = "ID") %>%
    select(ID:Author_style, Authors, everything())
  
  return(db_final)
  
}


# remove dups = George
rm.dup <- function(x){
  
  fStr <- stringr::str_split(x, ", ", simplify = TRUE)
  dStr <- fStr[!stringi::stri_duplicated(fStr)]
  
  dStr <- paste(dStr, collapse = ", ")
  dStr <- stringr::str_remove(dStr, "\\b, NA\\b|\\bNA, \\b")
  dStr
}



parse.countries <- function(countries, db) { 
  
  # PART 1 - extract country from address
  
  # countries <- read_csv('countries.csv')  # from google with a few additions
  countries.flat <- toupper(stringr::str_c(countries$name, collapse = "|"))
  
  #  Make a data-frame from biblioData with the countries matched for each paper
  #  Assume countries in <<affiliation>> column
  
  ctry.au <- db %>%
    mutate(id = row_number()) %>%
    tidyr::separate_rows(Addresses, sep = ';\\s*') %>%
    group_by(id) %>%
    summarise(match = toString(str_extract(toupper(Addresses), countries.flat)), 
              aff = toString(Addresses))
  
  # add countries to OG db
  db$Countries <- ctry.au$match
  
  # fix country names - states that class with countries, check these manually
  db$Countries <- gsub("UNITED STATES", "USA", db$Countries)
  db$Countries <- gsub("GEORGIA", "USA", db$Countries)
  db$Countries <- gsub("JERSEY", "USA", db$Countries)
  
  
  # PART 2 where mulit authors are from same institute, add their country
  
  # split data that has multi author countries 
  tt <- db %>%
    filter(Address_style == 1)
  
  # separate author names - get max number of authors for single paper
  n_col <- max(str_count(tt$Countries, ",") + 1)
  vars <- paste("a", 1:n_col, sep = "")
  
  # 1 author per row, use fill to replace NA with country
  tt2 <- tt %>%
    select(ID, Countries) %>%
    separate(Countries, vars, sep = ",") %>%
    pivot_longer(cols = -ID, names_to = "lets", values_to = "Countries2") %>%
    mutate(Countries2 = trimws(Countries2),
           Countries2 = ifelse(Countries2 == "NA", NA, Countries2)) %>%
    group_by(ID) %>%
    fill(Countries2, .direction = 'up')
  
  # countries back to wide and single column
  tt3 <- tt2 %>%
    pivot_wider(names_from = "lets", values_from = "Countries2") %>%
    unite("Countries", starts_with("a"), sep = "; ", remove = TRUE, na.rm = TRUE)
  
  # join with rest of data (papers with insufficient address info)
  tt4 <- tt %>%
    select(-Countries) %>%
    left_join(tt3, by = "ID", keep = FALSE) %>%
    select(colnames(db))
  
  # tidy
  xx2 <- db %>%
    filter(Address_style == 2) %>%
    rbind(tt4) %>%
    arrange(ID)
  
  # number of unique countries/paper
  xx2$n_country <- str_count(sapply(xx2$Countries, rm.dup), ",") + 1
  # how many authors and countries were extracted per paper, should be equal!
  xx2$n_country_ids <- str_count(xx2$Countries, ",") + 1
  xx2$n_author <- str_count(xx2$Authors, ";") + 1
  
  return(xx2)
}  



# extract known Author countries
fun_pull_known_author_addresses <- function(tidied_db){
  
  zz <- tidied_db %>%
    na.omit(Addresses) %>%
    filter(n_country_ids == n_author) %>%
    select(ID, Year, Authors, Countries)
  
  # separate author names - get max number of authors for single paper
  n_col <- max(str_count(zz$Countries, ",") + 1)
  vars <- paste("a", 1:n_col, sep = "")
  
  
  n_col2 <- max(str_count(zz$Authors, ",") + 1)
  vars2 <- paste("b", 1:n_col, sep = "")
  
  # separate countries
  zz2 <- zz %>%
    filter(Countries != "NA") %>%
    separate(Countries, vars, sep = ",") %>%
    pivot_longer(cols = -c(ID, Year, Authors), names_to = "lets", 
                 values_to = "Countries2") %>%
    na.omit() %>%
    mutate(Countries2 = trimws(Countries2)) %>%
    select(ID, Year, Countries2)
  
  # separate authors
  zz3 <- zz %>%
    filter(Countries != "NA") %>%
    separate(Authors, vars2, sep = ";") %>%
    pivot_longer(cols = starts_with("b"), names_to = "lets2", 
                 values_to = "Authors2") %>%
    na.omit() %>%
    mutate(Authors2 = trimws(Authors2)) %>%
    select(Authors2)
  
  # combine
  zz4 <- cbind(zz2, zz3)
  
  return(zz4)
  
}

kk <- tidied_db %>%
  filter(n_country_ids > n_author) %>%
  filter(!is.na(Addresses))



colnames(tidied_db)
kk$Countries2 <- paste(kk$Countries, str_dup(";NA", kk$n_author - 1))


kk2 <- kk %>%
  select(Authors, n_country_ids, n_author, Countries, Countries2)


unique(db$Journal)

# Analysis ---------------------------------------------------------------------

# packages
library(tidyverse)
library(gsheet) # import from Google sheet

# Import from Google sheets 
db <- gsheet::gsheet2tbl('https://docs.google.com/spreadsheets/d/13OtfyWIqS8XKhuBXzboJIyLzFFUp702ESCmk91Q204Q/edit?usp=sharing')
db <- as_tibble(db)

# replace non-english letters
db$Authors <- replace_accents(db$Authors)

# tidy author names
db2 <- fun_tidy_author_names(db)


# load country list
countries <- read_csv("./data/countries.csv")  # from google with a few additions

# add author countries to db 
tidied_db <- parse.countries(countries, db2)

write.csv(tidied_db, file = "./data/database_v1.csv", row.names = F)


# extract all known countries for authors
ss <- fun_pull_known_author_addresses(tidied_db)
 
ss <- ss %>%
  distinct(Countries2, Authors2, .keep_all = T) %>%
  arrange(Authors2)


# list of unique authors where we know the country
# if an author has multiple countries, use most recent
xx <- ss %>%
  group_by(Authors2) %>%
  arrange(-Year) %>%
  slice(1) %>%
  select(Authors2, Countries2)


# get list of all authors


# separate countries
# separate author names - get max number of authors for single paper
n_col <- max(str_count(tidied_db$Authors, ";") + 1, na.rm = T)
vars <- paste("a", 1:n_col, sep = "")

# 1 author per row
dat_authors <- tidied_db %>%
  select(ID, Year, Title, Authors) %>%
  separate(Authors, vars, sep = ";") %>%
  pivot_longer(cols = -c(ID, Year, Title), names_to = "lets", values_to = "author") %>%
  na.omit() %>%
  mutate(author = trimws(author), 
         author = gsub(',', '', author))


zz <- dat_authors %>%
  left_join(xx, by = c("author" = "Authors2"))



# get list of all missing countries by author
dat_auth_countries <- tidied_db %>%
  select(ID, Year, Title, Authors) %>%
  separate(Authors, vars, sep = ";") %>%
  pivot_longer(cols = -c(ID, Year, Title), names_to = "lets", values_to = "author") %>%
  na.omit() %>%
  mutate(author = trimws(author), 
         author = gsub(',', '', author)) %>%
  group_by(author) %>%
  arrange(-Year) %>%
  slice(1)  %>%
  left_join(xx, by = c("author" = "Authors2")) %>%
  arrange(Year, Title)

write.csv(dat_auth_countries, file = "./data/dat_auth_countries.csv", row.names = F)







