
library(gsheet)
library(tidyverse)
library(patchwork)

# get data from google sheets
df_meta <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/13OtfyWIqS8XKhuBXzboJIyLzFFUp702ESCmk91Q204Q/edit#gid=1641925563")
df_auth <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/13OtfyWIqS8XKhuBXzboJIyLzFFUp702ESCmk91Q204Q/edit#gid=1222222462")



# Corpus overview

# article types - still lots of info missing -----------------------------------
p1 <- ggplot(df_meta, aes(fct_infreq(subtype_description))) +
  geom_bar() +
  scale_x_discrete(na.translate = FALSE, guide = guide_axis(n.dodge = 2)) +
  labs(x = "Article type", y = "Count") +
  theme_bw()


# nuber through time
df_meta_sum <- df_meta |>
  group_by(year_date) |>
  summarise(n_articles = n(),
            mean = mean(author_count, na.rm = T),
            median = median(author_count, na.rm = T)) |>
  pivot_longer(cols = mean:median, names_to = "Metric", values_to = "value")


p2 <- ggplot(df_meta_sum, aes(year_date, n_articles)) +
  geom_line() +
  scale_y_continuous(limits = c(0, 130)) +
  geom_smooth(fill = "black", colour = "black") +
  labs(x = "Year", y = "Articles") +
  theme_bw()


# number of authors / article
p3 <- ggplot(df_meta_sum, aes(year_date, value, group = Metric, colour = Metric, fill = Metric)) +
  geom_line() +
  geom_smooth() +
  scale_y_continuous(limits = c(0,5)) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Year", y = "Authors") +
  theme_bw() +
  theme(legend.position = "bottom")


# most cite papers -------------------------------------------------------------
p4 <- df_meta |>
  arrange(desc(citedby_count)) |> 
  slice(1:10) |>
  mutate(dc_title = fct_reorder(dc_title, citedby_count)) |> 
  ggplot(aes(y = dc_title, x = citedby_count)) +
  geom_col() +
  scale_y_discrete(labels = scales::wrap_format(40)) +
  labs(x = "Citations", y = NULL) +
  theme_bw()
  

# authors - international vs USA -----------------------------------------------
df_count <- df_auth |>
  filter(!is.na(affiliation_country)) |>
  group_by(year_date, entry_number) |>
  summarise(has_usa = any(affiliation_country == "United States"),
            no_usa = !any(affiliation_country == "United States"),
            n_countries = length(unique(affiliation_country)))

# calc summary stats 
df_count <- df_count |>
  group_by(year_date) |>
  summarise(tot = n(),
            count_US_ony = sum(has_usa & n_countries == 1),
            no_usa = sum(no_usa),
            usa_plus_int = tot - count_US_ony - no_usa,
            count_US_ony2 = count_US_ony/tot,
            no_usa2 = no_usa/tot,
            usa_plus_int2 = usa_plus_int/tot) |>
  select(year_date, count_US_ony2:usa_plus_int2) |>
  pivot_longer(cols = count_US_ony2:usa_plus_int2, names_to = "Author_location",
               values_to = "values")

# recode vars
df_count <- df_count |>
  mutate(Author_location =  recode(Author_location,
                                   "count_US_ony2" = "USA only",
                                   "no_usa2" = "International only",
                                   "usa_plus_int2" = "USA + International"))
  
# plot
p5 <- ggplot(df_count, aes(year_date, values, colour = Author_location,
                           group = Author_location, fill = Author_location)) +
  geom_line() +
  geom_point() +
  geom_smooth() +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Year", y = "Prop. studies", colour = "Author location", fill = "Author location") +
  theme_bw() +
  theme(legend.position = "bottom")
  

# authors - most papers --------------------------------------------------------

p6 <- df_auth |> 
  select(entry_number, year_date, authname) |>
  distinct(entry_number, authname) %>%
  count(authname) |>
  arrange(-n) |>
  top_n(20) |>
  mutate(authname = fct_reorder(authname, n)) |> 
  ggplot(aes(authname, n)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL, y = "Articles") +
  theme_bw()


# all plots together
p7 <- (p2 + p1) / (p3 + p5)
ggsave(file = "./figures/p1_meta_summary.pdf", plot = p7, width = 18, height = 16, units = "cm")
ggsave(file = "./figures/p1_meta_summary.png", plot = p7, width = 18, height = 16, units = "cm", dpi = 300)


# most pubs and citations
p8 <- p4 / p6 + 
  plot_layout(heights = c(2, 1.2))
ggsave(file = "./figures/p2_cites_and_papers.pdf", plot = p8, width = 18, height = 22, units = "cm")
ggsave(file = "./figures/p2_cites_and_papers.png", plot = p8, width = 18, height = 22, units = "cm", dpi = 300)






