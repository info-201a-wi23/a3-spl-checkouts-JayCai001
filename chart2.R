library("ggplot2")
library("dplyr")
library("scales")
library("stringr")
data = read.csv("/Users/macbookpro/Documents/a3-spl-checkouts-JayCai001/2020-2022-Checkouts-Data.csv")

# Filtering most checked out authors between 2020-2022.
creator_checkouts = data %>%
  group_by(Creator) %>%
  summarise(total_checkouts = sum(Checkouts, na.rm = TRUE))


# Sort the data from highest to lowest
author_sorted_data <- creator_checkouts[order(-creator_checkouts$total_checkouts),]


# Grabbing the top 10 authors

top_10_author_data <- author_sorted_data[2:11,]

# Creating a histogram

author_plot <- ggplot(top_10_author_data, aes(fill=Creator,
                                              x = total_checkouts,
                                              y = reorder(Creator, +total_checkouts),
                                              )) +
  geom_col() +
  labs(title = "Top 10 checkout by authors in 2020-2022",
       x = "Authors",
       y = "Total checkouts") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.8)) +
  scale_fill_discrete(breaks = c(top_10_author_data$Creator)) +
  guides(fill = guide_legend(title = "Authors"))
author_plot

