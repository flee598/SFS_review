

# country cooccurance/through time --------------------------------------------
source("./scripts/1_biblio_tidying.R")
source("./scripts/0_georges_country_code.R")


# load and tidy wos bib files - ignore warning
dat <- fun_tidy_multi_wos_bibs(pth = "./bibs")
countries <- read_csv('data/countries.csv')  # from google with a few additions


# seperate df and db objects
biblioDB <- dat[[1]]
biblioDF <- dat[[2]]

biblioDF$AuthorCountries <- parse.countries(countries, biblioDF)

time_dat <- biblioDF %>%
  select(YearPublished, AuthorCountries)

time_dat$AuthorCountries <- gsub("NA, ", "", time_dat$AuthorCountries)
time_dat$USA <- grepl("USA", time_dat$AuthorCountries, fixed = TRUE)
time_dat$NumCountries <- str_count(time_dat$AuthorCountries, '\\,') + 1


# get 
xx <- time_dat %>%
  mutate(ID = 1:nrow(.)) %>%
  tidytext::unnest_tokens(word, AuthorCountries) %>%
  dplyr::group_by(YearPublished, ID) %>% 
  mutate(NumNonUSA = sum(word != "usa")) %>%
  dplyr::distinct(ID, .keep_all = TRUE) %>%
  select(-word)


xx2 <- xx %>%
  select(ID, everything()) %>%
  dplyr::group_by(YearPublished) %>%
  mutate(n = n()) %>% 
  dplyr::summarise(NumUSAOnly = sum(USA == TRUE & NumCountries == 1),
            USA_only = NumUSAOnly/n,
            NumUSAPlus = sum(USA == TRUE & NumCountries > 1),
            USA_plus_int = NumUSAPlus/n,
            NumIntOnly = sum(USA == FALSE),
            Int_only = NumIntOnly/n) %>%
  dplyr::distinct(YearPublished, .keep_all = TRUE)
  

xx3 <- xx2 %>%
  dplyr::select(YearPublished, USA_only, USA_plus_int, Int_only) %>%
  pivot_longer(cols = USA_only:Int_only, names_to = "AuthorLocation", 
               values_to = "values")

# international collaborations
p1 <- ggplot(xx3, aes(YearPublished, values, colour = AuthorLocation)) +
  geom_line() +
  geom_point() +
  #geom_smooth(method = "lm") +
  ylab("Proportion of studies") +
  xlab("Year")


xx4 <- xx %>%
  dplyr::group_by(YearPublished) %>%
  summarise(median_c = median(NumCountries),
            mean_c = mean(NumCountries)) %>%
  pivot_longer(cols = median_c:mean_c, names_to = "Num_countries", 
               values_to = "values")


p2 <- ggplot(xx4, aes(YearPublished, values, colour = Num_countries)) +
  geom_line() +
  geom_point() +
  ylab("Number of countries/study")


p1 / p2


ggsave("figures/countries_through_time.png", width = 20, height = 20, units = "cm")









