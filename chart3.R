library("ggplot2")
library("dplyr")
library("scales")
library("stringr")
data = read.csv("/Users/macbookpro/Documents/a3-spl-checkouts-JayCai001/2020-2022-Checkouts-Data.csv")


# Viewing different book material types 
filtered_data <- data %>% group_by(MaterialType) %>% summarise(checkouts = sum(Checkouts))

# Sort the data from highest to lowest checkouts
sorted_data <- filtered_data[order(-filtered_data$checkouts),]

# Grabbing only top 5 material type books
top_5_data <- sorted_data[1:5,]

# Computing the position of labels
top_5_data <- top_5_data %>%
  arrange(desc(MaterialType)) %>%
  mutate(prop = checkouts / sum(top_5_data$checkouts) * 100) %>%
  mutate(ypos = cumsum(prop) - 0.5 * prop)

# Sorting data in descending order
sorted_data <- top_5_data[order(-top_5_data$checkouts),]

# Make the checkout numbers separated with commas
sorted_data$checkouts <- format(sorted_data$checkouts, big.mark = ",", scientific = FALSE)

sorted_data <- sorted_data %>%
  mutate(legend = paste0(MaterialType, ": ", checkouts))

# Plot
material_plot <- ggplot(data = sorted_data, aes(x = "",
                                               y = prop,
                                               fill = legend)) +
  geom_bar(width = 1, stat = "identity", color = "#E2E5DE") +
  coord_polar("y", start = 0) +
  geom_text(aes(y = ypos, label = paste(round(prop, digits = 1), "%")), color = "white", size = 2.5) +
  theme_void() +
  labs(title = "Top 5 most checked out material type between 2020-2022") +
  scale_fill_discrete(breaks = c(sorted_data$legend)) +
  guides(fill = guide_legend(title = "Number of checkouts"))

material_plot

