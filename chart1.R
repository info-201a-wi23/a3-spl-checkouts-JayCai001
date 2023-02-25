library("ggplot2")
library("dplyr")
library("scales")
library("stringr")
data = read.csv("/Users/macbookpro/Documents/a3-spl-checkouts-JayCai001/2020-2022-Checkouts-Data.csv")

# Rise of COVID affecting checkouts between digtal and physical books.
data_in_2020 = data %>% filter(CheckoutYear == "2020")

# First get the checkouts for hardcover
digital_vs_hardcover <- data_in_2020 %>% filter(UsageClass %in% "Physical") %>% group_by(CheckoutMonth) %>% summarise(hardcover_checkout_per_month = sum(Checkouts))

# Then mutate the checkouts for digital
digital_vs_hardcover <- digital_vs_hardcover %>% mutate(digital_checkout_per_month = data_in_2020 %>% filter(UsageClass %in% "Digital") %>% group_by(CheckoutMonth) %>% summarise(digital_checkout_per_month = sum(Checkouts)))


digital_vs_hardcover_plot <- ggplot(data = digital_vs_hardcover, aes(x = CheckoutMonth)) +
  geom_line(data = digital_vs_hardcover, aes(y = hardcover_checkout_per_month, color = "Hard Cover")) +
  geom_line(data = digital_vs_hardcover, aes(y = digital_checkout_per_month$digital_checkout_per_month, color = "Digital")) +
  scale_x_continuous(breaks = seq(1, 12, 1)) +
  scale_y_continuous(labels = label_number_si()) +
  scale_color_manual(values=c("#CC6666", "#9999CC")) +
  labs(title = "2020 data of digital vs hardcover checkouts",
       color="Digital vs HardCover",
       x = "Months",
       y = "Number of checkouts")

digital_vs_hardcover_plot

