library(tidyverse)
library(tidyquant)
library(broom)
library(umap)
library(ggrepel)

# STOCK PRICES
sp_500_prices_tbl <- read_rds("C:/Users/rueta/Desktop/Data_Science_ML/data/sp_500_prices_tbl.rds")
sp_500_prices_tbl

# SECTOR INFORMATION
sp_500_index_tbl <- read_rds("C:/Users/rueta/Desktop/Data_Science_ML/data/sp_500_index_tbl.rds")
sp_500_index_tbl


# Step 1 - Convert stock prices to a standardized format (daily returns)

sp_500_daily_returns_tbl <- sp_500_prices_tbl  %>%
  select(symbol, date, adjusted) %>% 
  filter(lubridate::year(date) == 2018 | lubridate::year(date) == 2019| lubridate::year(date) == 2020)%>%
  group_by(symbol) %>%
  mutate(lag = lag(adjusted,1)) %>%
  ungroup() %>% na.omit()%>%
  mutate(diff=adjusted-lag)%>%
  mutate(pct_return=diff/lag) %>%
  select(symbol, date, pct_return)%>%
  arrange(symbol)

# Step 2 - Convert to User-Item Format

stock_date_matrix_tbl <- sp_500_daily_returns_tbl %>%
  pivot_wider(names_from = date, values_from = pct_return, values_fill = 0) %>%
  ungroup()


# Step 3 - Perform K-Means Clustering

kmeans_obj <- stock_date_matrix_tbl %>%
  select(-symbol) %>%
  kmeans(centers = 4, nstart = 20)

kmeans_obj$cluster
kmeans_obj$size

kmeans_obj %>% glance()



# Step 4 - Find the optimal value of K

kmeans_mapper <- function(center = 3) {
  stock_date_matrix_tbl %>%
    select(-symbol) %>%
    kmeans(centers = center, nstart = 20)
}

3 %>% kmeans_mapper() %>% glance()


kmeans_mapped_tbl <- tibble(centers = 1:30) %>%
  mutate(k_means = centers %>% map(kmeans_mapper)) %>%
  mutate(glance  = k_means %>% map(glance))

kmeans_mapped_tbl %>%
  unnest(glance) %>%
  select(centers, tot.withinss)


# Skree Plot ----

kmeans_mapped_tbl %>%
  unnest(glance) %>%
  select(centers, tot.withinss) %>%
  
  # Visualization
  ggplot(aes(centers, tot.withinss)) +
  geom_point(color = "#2DC6D6", size = 4) +
  geom_line(color = "#2DC6D6", size = 1) +
  # Add labels (which are repelled a little)
  ggrepel::geom_label_repel(aes(label = centers), color = "#2DC6D6") + 
  
  # Formatting
  labs(title = "Skree Plot",
       subtitle = "Measures the distance each of the customer are from the closes K-Means center",
       caption = "Conclusion: Based on the Scree Plot, we select 3 clusters to segment the customer base.")


# Step 5 - Apply UMAP

umap_results <- stock_date_matrix_tbl %>%
  select(-symbol) %>%
  umap()

umap_results_tbl <- umap_results$layout %>%
  as_tibble(.name_repair = "unique") %>% # argument is required to set names in the next step
  set_names(c("x", "y")) %>%
  bind_cols(
    stock_date_matrix_tbl %>% select(symbol)
  )

umap_results_tbl %>%
  ggplot(aes(x, y)) +
  geom_point(alpha = .5) + labs(title = "UMAP Projection")+
  geom_label_repel(aes(label = symbol), size = 3)+
  theme_tq()

# Step 6 - Combine K-Means and UMAP


kmeans_obj <- kmeans_mapped_tbl %>%
  pull(k_means) %>%
  pluck(10)

# Convert it to a tibble with broom
kmeans_clusters_tbl <- kmeans_obj %>% 
  augment(stock_date_matrix_tbl) %>%
  # Select the data we need
  select(symbol, .cluster)


# Bind data together
join_tbl1 <- umap_results_tbl %>%
  left_join(kmeans_clusters_tbl)

umap_kmeans_results_tbl <- sp_500_index_tbl %>%  
  select(symbol, company, sector) %>%  
  left_join(join_tbl1)


#Visualize the combined K-Means and UMAP results

umap_kmeans_results_tbl %>%
  
  ggplot(aes(x, y, color = .cluster))+
  
  # Geometries
  geom_point(alpha = 0.5) +
  geom_label_repel(aes(label = symbol), size = 2, fill = "#282A36")






























