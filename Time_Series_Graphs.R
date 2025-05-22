##Libraries for analysis
library(patchwork)
library(dplyr)
library(MASS)
library(openxlsx)
library(ggplot2)

##Load in each data table to be combined
Y0005 <- read.csv("RTRA0426565_lfsstat4digNAICS.csv")
Y0610 <- read.csv("RTRA7467734_lfsstat4digNAICS.csv")
Y1115 <- read.csv("RTRA1062132_lfsstat4digNAICS.csv")
Y1620 <- read.csv("RTRA3503286_lfsstat4digNAICS.csv")
Y2124 <- read.csv("RTRA0917880_lfsstat4digNAICS_truncated.csv")
label_table <- read.xlsx("industry_mapping_2025_with_stokes_agg.xlsx")

##Data table is messy, some rows are just summaries of others. Remove all
##rows that contain NA values to create full data set.
Y0005_c <- Y0005[complete.cases(Y0005),] 
Y0610_c <- Y0610[complete.cases(Y0610),]
Y1115_c <- Y1115[complete.cases(Y1115),]
Y1620_c <- Y1620[complete.cases(Y1620),]
Y2124_c <- Y2124[complete.cases(Y2124),]

##Combine complete data tables
data_base <- rbind(Y0005_c, Y0610_c, Y1115_c, Y1620_c, Y2124_c)

##Be sure to remove all NA's/empty values as some were still appearing
data_base <- data_base %>%
  filter(if_all(everything(), ~ !is.na(.) & . != ""))

##Upon inspection of data, a value of 5211 exists in NAICS_5,
##a value that should not exist in our data.
data_base[data_base$NAICS_5 == 5211 & data_base$X_COUNT_ != 0, ]

##Format table of information to be joined
label_table$naics_5 <- as.integer(label_table$naics_5)

##Join tables to add information
full_data <- data_base %>%
  left_join(label_table, by = c("NAICS_5" = "naics_5"))

##Check if any other incorrect data besides 5211 NAICS_5 code
unmatched <- full_data %>%
  filter(is.na(lmo_ind_code))
unmatched[unmatched$NAICS_5 != 5211, ]

##No other incorrect results!

##Remove data that contains 5211 NAICS_5 code, and any data from 2024. 
##We remove 2024 data because this is a yearly time series, and we don't
##have all data from the year yet. Seasonal trends may effect results and 
##produce skewed results.
full_data <- full_data[full_data$NAICS_5 != 5211 & full_data$SYEAR != 2024, ]

##Find how many people were surveyed each year
yearly_totals <- full_data %>%
  group_by(SYEAR) %>%
  summarise(total_people = sum(X_COUNT_, na.rm = TRUE))

##Don't need all these columns for analysis
analysis_data <- full_data[,1:8]

##Take months out of the equation, this is a yearly time series. We group
##by year and sum by count each month to produce yearly totals.
analysis_data_y <- analysis_data %>%
  group_by(SYEAR, LF_STAT, lmo_ind_code, aggregate_industry, lmo_detailed_industry) %>%  
  summarise(X_COUNT_ = sum(X_COUNT_), .groups = "drop")

##Join analysis with yearly totals
analysis_data_y <- analysis_data_y %>%
  left_join(yearly_totals, by = c("SYEAR" = "SYEAR"))

##Create new column, the percentage of people who were surveyed that fall into
##each category.
analysis_data_y$X_PCT <- analysis_data_y$X_COUNT_/analysis_data_y$total_people

##Inspect our new data
head(analysis_data_y)

##Only want to plot employed data for our purposes
plot_data <- analysis_data_y %>%
  filter(LF_STAT == "Employed")

##Create plot of proportion of surveyed population employed for each industry
plots <- plot_data %>%
  split(.$aggregate_industry) %>%
  lapply(function(df) {
    ggplot(df, aes(x = SYEAR, y = X_PCT, color = lmo_detailed_industry, group = lmo_ind_code)) +
      geom_line() +
      labs(
        title = unique(df$aggregate_industry),
        x = "Year",
        y = "Proportion of Population",
        color = "Industry"
      ) +
      theme_minimal() +
      theme(
        legend.position = "right",
        legend.justification = c("left", "top"),
        legend.box.just = "left",
        legend.margin = margin(l = 10, unit = "pt"),
        plot.margin = margin(5, 20, 5, 5)
      )
  })

##Combine plots together to create one pdf
final_plot <- wrap_plots(plots, ncol = 2) +
  plot_annotation(title = "Proportion of Surveyed Population Employed in Each Industry by Year (LFS)",
                  theme = theme(plot.title = element_text(size = 24, hjust = 0.5, margin = margin(b = 10)))
)

##Save plot as png to be viewed
ggsave("DANIEL_POLSON_timegraph.pdf", final_plot, width = 18, height = 24, dpi = 300)
