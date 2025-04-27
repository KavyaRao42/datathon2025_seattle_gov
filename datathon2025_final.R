# Libraries
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(leaflet)
library(htmlwidgets)

# Load in data
final_data <- read.csv('Customer_Service_Requests_20250426.csv')

# Understand the scale of the data
head(final_data)
colnames(final_data)
nrow(final_data)
colSums(is.na(final_data))

# Clean data by removing rows with missing values
final_data_clean <- final_data %>%
  filter(!is.na(Latitude) & !is.na(Longitude) & !is.na(Council.District))

nrow(final_data_clean)
summary(final_data_clean)

# Convert Created.Date column to POSIXct to work with smaller time frames more easily
final_data_clean$created_date <- as.POSIXct(final_data_clean$Created.Date, format = "%m/%d/%Y %I:%M:%S %p")

head(final_data_clean$created_date)

# Create an additional column for time-based analysis
final_data_clean$year <- year(final_data_clean$created_date)

# Basic visualization of neighborhoods by number of service requests
# Count number of requests per neighborhood
neighborhood_counts <- final_data_clean %>%
  filter(!is.na(Neighborhood)) %>%
  group_by(Neighborhood) %>%
  summarise(request_count = n()) %>%
  arrange(desc(request_count))

ggplot(neighborhood_counts, aes(x = reorder(Neighborhood, request_count), y = request_count)) +
  geom_bar(stat = "identity", fill = "#0072B2") +
  coord_flip() +
  labs(title = "Service Requests by Neighborhood in Seattle",
       x = "Neighborhood",
       y = "Number of Requests") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        panel.grid.major.y = element_blank())

# Visualization of top 10 neighborhoods with the highest number of service requests
top_nhoods <- neighborhood_counts %>% slice_max(request_count, n = 10)

ggplot(top_nhoods, aes(x = reorder(Neighborhood, request_count), y = request_count)) +
  geom_bar(stat = "identity", fill = "#0072B2") +
  coord_flip() +
  labs(title = "Top 10 Neighborhoods by Service Requests",
       x = "Neighborhood",
       y = "Number of Requests") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.y = element_text(size = 10))

# Create a metric to rank neighborhoods by performance
# Components:
# 1. Need index (based on volume and % of open requests)
# 2. Year-over-year growth trend (indicates worsening or improving trends)

# Prepare the data
metric_data <- final_data_clean %>%
  filter(!is.na(Neighborhood), !is.na(Status))

# Calculate the raw Need Index for each neighborhood
need_index_df <- metric_data %>%
  group_by(Neighborhood) %>%
  summarise(
    total_requests = n(),
    open_requests = sum(Status != "Closed", na.rm = TRUE),
    closed_requests = sum(Status == "Closed", na.rm = TRUE),
    resolution_rate = closed_requests / total_requests
  ) %>%
  mutate(
    raw_need_index = (open_requests / total_requests) * total_requests
  )

# Calculate year-over-year (YoY) growth for each neighborhood (this tracks the change in total requests compared to the previous year)
# Count requests by year per neighborhood
yearly_counts <- metric_data %>%
  group_by(Neighborhood, year) %>%
  summarise(requests_in_year = n(), .groups = "drop")

# Compute the YoY growth
yoy_growth_df <- yearly_counts %>%
  arrange(Neighborhood, year) %>%
  group_by(Neighborhood) %>%
  mutate(
    yoy_growth = (requests_in_year - lag(requests_in_year)) / lag(requests_in_year)
  ) %>%
  filter(year == max(year))

# Handle NA values
yoy_growth_df$yoy_growth[is.na(yoy_growth_df$yoy_growth)] <- 0

# Combine Need Index and YoY Growth into a Final Metric
# Join the two components by Neighborhood
final_metric_df <- need_index_df %>%
  inner_join(yoy_growth_df, by = "Neighborhood") %>%
  
  # Normalize (rescale to 0–1) both metrics for fair comparison
  mutate(
    scaled_need_index = scales::rescale(raw_need_index, to = c(0, 1)),
    scaled_yoy_growth = scales::rescale(yoy_growth, to = c(0, 1)),
    
    # Final score: equally weighted sum of both scaled metrics
    final_score = scaled_need_index + scaled_yoy_growth
  ) %>%
  
  # Sort by neighborhoods most in need of attention
  arrange(desc(final_score))

# View the top 10 neighborhoods that need the most attention
top_metric_nhoods <- final_metric_df %>%
  select(Neighborhood, total_requests, open_requests, resolution_rate,
         raw_need_index, yoy_growth, final_score) %>%
  arrange(desc(final_score)) %>%
  head(10)

print(top_metric_nhoods)

# Interactive map visualization of metric
# Get the latitude/longitude center for each neighborhood (rough centroid), using the average location of all requests in each neighborhood
nhood_locations <- final_data_clean %>%
  filter(!is.na(Neighborhood)) %>%
  group_by(Neighborhood) %>%
  summarise(
    lat = mean(Latitude, na.rm = TRUE),
    lng = mean(Longitude, na.rm = TRUE)
  )

# Merge final metric data with location data
map_data <- final_metric_df %>%
  inner_join(nhood_locations, by = "Neighborhood")

# Create color palette based on final score
pal <- colorNumeric(palette = "YlOrRd", domain = map_data$final_score)

# Build the interactive map
map <- leaflet(map_data) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%  # nice clean background
  addCircleMarkers(
    lng = ~lng,
    lat = ~lat,
    radius = ~scales::rescale(final_score, to = c(5, 15)),  # bigger = more need
    color = ~pal(final_score),
    stroke = FALSE,
    fillOpacity = 0.8,
    label = ~paste0(
      "<strong>", Neighborhood, "</strong><br>",
      "Need Index: ", round(raw_need_index, 1), "<br>",
      "YoY Growth: ", round(yoy_growth * 100, 1), "%<br>",
      "Final Score: ", round(final_score, 2)
    ) %>% lapply(htmltools::HTML),
    popup = ~paste0("Requests: ", total_requests, "<br>Open: ", open_requests)
  ) %>%
  addLegend("bottomright", pal = pal, values = ~final_score,
            title = "Neighborhood Attention Score",
            opacity = 1)

# Save the leaflet map as an HTML file
saveWidget(map, "neighborhood_attention_map.html", selfcontained = TRUE)