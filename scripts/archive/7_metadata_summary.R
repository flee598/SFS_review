
source("./scripts/1_biblio_tidying.R")

# required packages
library(tidyverse)

# load and tidy wos bib files - ignore warning
dat <- fun_tidy_multi_wos_bibs(pth = "./bibs")

# seperate df and db objects
biblioDB <- dat[[1]]
biblioDF <- dat[[2]]

# drop double ;;
biblioDF$AuthorKeywords <- gsub(";;", ";", biblioDF$AuthorKeywords)

# Metadata through time (ripped off from George's article)
datTime <- biblioDF %>%
  mutate(NumAuth = lengths(gregexpr(";", biblioDF$Author)) + 1,
         TitleLength = lengths(gregexpr(" ", biblioDF$Title)) + 1,
         AbstractLength = lengths(gregexpr(" ", biblioDF$Abstract)) + 1) %>%
  group_by(YearPublished) %>%
  summarise(numArticles = n(),
            NumAuth = mean(NumAuth),
            TitleLength = mean(TitleLength),
            AbstractLength = mean(AbstractLength))

datTime2 <- pivot_longer(datTime, names_to = "var", values_to = "value", -YearPublished)

ggplot(datTime2, aes(YearPublished, value)) +
  geom_line() +
  geom_smooth() +
  facet_wrap(~var, scales = "free")


ggsave("figures/metadata_through_time.png", width = 20, height = 15, units = "cm")



