pacman::p_load("quanteda","lexicon","dplyr","ggplot2","tidyr","quanteda.textstats","ggrepel")

# Read in the data
df <- readr::read_csv("datasets/hertie_papers.csv")
head(df,3)

# Summarise and plot by year
annual_pubs <- df %>% count(publication_year) 
ggplot(annual_pubs, aes(publication_year, n)) +
  geom_line()

# Create document feature matrixc
df <- df %>% filter(!is.na(abstract))
dfmat <- df$abstract %>%
  tokens(remove_punc=TRUE) %>%
  tokens_remove(pattern=stopwords("en")) %>%
  tokens_wordstem("english") %>%
  dfm()

# Plot text feature frequency
tfreq <- dfmat %>% textstat_frequency() %>% head(20)
tfreq$feature <- factor(tfreq$feature, levels=tfreq$feature)
ggplot(tfreq, aes(x=frequency, y=feature)) +
  geom_col()

# frequency by year
ytfreq <- dfmat %>% 
  textstat_frequency(groups=df$publication_year) 

# filter european and climat features
ytfreq$group <- as.numeric(ytfreq$group)
interesting_features <- ytfreq %>%
  filter(feature %in% c("european","climat"))

# Plot two features over time
ggplot(
  interesting_features, 
  aes(x=group, y=frequency, colour=feature)
) +
  geom_point() +
  geom_line() + 
  theme_bw()

# Binary time variable
df$era <- ifelse(df$publication_year<2017, "Pre", "Post")

# Create dataframe with rows as features and columns as groups
ytfreq <- dfmat %>% textstat_frequency(groups=df$era) %>%
  pivot_wider(id_cols=feature, names_from=group, values_from=frequency)

# Plot scattertext
ggplot(ytfreq, aes(x=Post, y=Pre)) +
  geom_point() + 
  coord_fixed()

# Pivot dimensions
dfmat %>% textstat_frequency(groups=df$era) %>%
  pivot_wider(id_cols=feature, names_from=group, values_from=frequency) %>%
  head()

dfmat %>% textstat_frequency(groups=df$era) %>%
  pivot_wider(id_cols=feature, names_from=group, values_from=frequency) %>%
  pivot_longer(cols=Post:Pre, names_to="group") %>%
  head()

# proportions
ytfreq <- dfmat %>%
  textstat_frequency(groups=df$era) %>%
  filter(docfreq>10) %>%
  group_by(group) %>%
  mutate(prop=docfreq/n()) %>%
  ungroup() %>%
  pivot_wider(
    id_cols=feature, 
    names_from=group, 
    values_from=prop
  )

# Change in frequency from one era to the next
ytfreq$change <- log(ytfreq$Post / ytfreq$Pre)
max_change <- max(abs(ytfreq$change), na.rm=TRUE)

# colour coded scattertext
p <- ggplot(ytfreq, aes(x=Post, y=Pre, fill=change)) +
  geom_point(color="grey", shape=21) + 
  coord_fixed() + 
  scale_fill_gradientn(
    colors = c("#4575b4","white","#d73027"),
    values = scales::rescale(c(max_change*-1,0,max_change)),
    limits = c(max_change*-1,max_change)
  ) + 
  theme_bw()
p

# select labels

labels <- ytfreq %>% rowwise() %>% 
  mutate(max_value = max(Post,Pre)) %>%
  filter(
    (abs(change)>0.3 & max_value>0.12)
  )

# plot labels
p + geom_label_repel(
  data=labels, 
  aes(label=feature), 
  min.segment.length = 0
)
