library(tidyverse)
library(bibliometrix)

#################################
# IMPORT WOS BIB FILES AND TIDY #
#################################

fun_tidy_multi_wos_bibs <- function(pth = "./bibs"){
  
  # load bib files 
  f <- list.files("./bibs", pattern = "\\.bib$")
  f <- paste(pth, "/", f, sep = "")
  
  dat <- mapply(bibliometrix::convert2df, file = f,  MoreArgs = list(dbsource = "wos", format = "bibtex"))
  
  # retain columns common across bibs
  common_cols <- Reduce(intersect, lapply(dat, names))
  dat2 <- lapply(dat, `[`, common_cols)
  dat2 <- bind_rows(dat2)

    # drop NA col(s)
  dat2 <- dat2[ , colSums(is.na(dat2)) < nrow(dat2)]
  
  # only include articles, reviews and notes 
  dat3 <- dat2 %>%
    filter(DT %in% c("ARTICLE", "ARTICLE; EARLY ACCESS", "REVIEW", "NOTE"))
  
  # Fix instances where a name has different number of initials 
  # e.g. SMITH C and SMITH CD both become SMITH C - sometimes not appropriate!

  n_col <- max(str_count(dat3$AU, pattern = ";") + 1)
  
  vars <- paste("a", 1:n_col, sep = "")
  
  
  xx <- dat3 %>%
    select(SR_FULL, AU) %>%
    separate(AU, vars, sep = ";")
    
  xx <- pivot_longer(xx, cols = -SR_FULL, names_to = "lets", values_to = "author") %>%
    na.omit() %>% 
    group_by(grp = str_remove(author, "(?<= [A-Z])[A-Z]$")) %>% 
    mutate(author = author[which.min(nchar(author))]) %>%
    ungroup %>%
    select(-grp) %>%
    pivot_wider(names_from = "lets", values_from = "author", 
                values_fill = NA, values_fn = max)
  
  xx2 <- unite(xx, "AU", starts_with("a"), sep = ";", remove = TRUE, na.rm = TRUE)
  
  dat4 <- dat3 %>%
    select(-AU) %>%
    left_join(xx2)
  
  # drop unwanted columns and rename remaining columns
  kp <- c("DE", "C1", "CR", "AB", "PA", "da", "DI", "SO", "month", "NR", 
          "orcid.numbers", "PP", "PU", "TC", "TI", "DT", "VL", "PY", "DB", 
          "AU_UN", "AU1_UN", "SR_FULL", "AU")
  
  nms <- c("AuthorKeywords", "AuthorAddress",
           "CitedRefs",  "Abstract", "PubAdress",
           "DateSearched", "Doi", "PublicationName", "Month",
           "CitedRefCount", "OrcidIds", "Pages", 
           "Publisher", "TimesCitedWoS", "Title", "DocType",
           "Volume", "YearPublished",  "DataBase",
           "AuthorAddresses", "LeadInst", "Ref", "Author")
  
  dat5 <- select(dat4, all_of(kp))
  colnames(dat5) <- nms
  
  # add ID column 
  dat4$ID <- 1:nrow(dat4)
  dat5$ID <- 1:nrow(dat5)
  
  # save the biblio database (original colnames) and the tidied version
  res <- list(biblioDB = dat4, biblioDF = dat5)
  res
}



# load and tidy wos bib files - ignore warning
dat <- fun_tidy_multi_wos_bibs(pth = "./bibs")
saveRDS(dat, "data/biblio_tidy.rds")



