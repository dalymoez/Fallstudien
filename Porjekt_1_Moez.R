install.packages("tidyverse")
install.packages("ggplot2")
install.packages("ineq")
library("tidyverse")
library("ggplot2")
library("ineq")


#### Aufgabe 4 ####
#vorbereitung der Daten:
kennzahlen <- read.csv("Kennzahlen_Laender.csv")
emission_data <- daten_final3
emission_data <- subset(emission_data,year>1989)
emission_data_cleaned <- na.omit(emission_data)

# Datensatz nur für das Jahr 2021 erstellen
data2021 <- subset(emission_data_cleaned, year==2021)

#First, I want to see how homogenous the CO2 emissions for each sector is (graphically)

data2021non_sectorial <- data2021[data2021$Sector != "Fuel combustion - sectoral approach",]
data2021non_sectorial <- subset(data2021non_sectorial,Country!="European Union - 27 countries (from 2020)" )
agg_data2021non_sectorial<- aggregate(Carbon.dioxide ~ Country, data = data2021non_sectorial, FUN = sum)
# Calculate the mean of the 'CO2' column
mean_value <- mean(agg_data2021non_sectorial$Carbon.dioxide)
# Create the ggplot
p <- ggplot(agg_data2021non_sectorial, aes(x = Country, y = Carbon.dioxide - mean_value, fill = Carbon.dioxide >= mean_value)) +
  geom_bar(stat = "identity", position = "identity") +
  scale_fill_manual(values = c("pink", "lightblue"), guide = FALSE) +
  labs(x = "Sectors", y = "CO2 Ausstoss") +
  ggtitle("Unterschied in Ausstoss von CO2 je Land") +
  theme_minimal()

# Add a horizontal line at y = 0
p <- p + geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 1)

# Rotate x-axis labels for better readability (optional)
p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot
print(p)

sum(data2021non_sectorial$Carbon.dioxide[data2021non_sectorial$Sector=="Fuel combustion in energy industries"])

#Grafically it is obvious that the emission of CO2 is very disproportionate to the sectors.
#Let's look into it more in details using precise statistical methods : The Gini Index

#I am tired of writing the full name each time, I shall rename the aggregated data frame
df <- agg_data2021non_sectorial



# Calculate the total sum of values
total_sum <- sum(df$Carbon.dioxide)

# Sort the data frame by values in descending order
df <- df[order(-df$Carbon.dioxide), ]

# Calculate the cumulative sum of values and cumulative percentage
df$CumulativeValue <- cumsum(df$Carbon.dioxide)
df$CumulativePercentage <- df$CumulativeValue / total_sum

# Calculate the Gini index using the ineq package
gini_index <- ineq(df$Carbon.dioxide, type= "Gini")
# Calculate the Lorenz curve
lorenz_curve <- Lc(df$Carbon.dioxide)
# plot(lorenz_curve) : I find this one unattractive
plot(lorenz_curve)
# 
# df <- df[order(df$Carbon.dioxide), ]
# 
# # Calculate cumulative share of categories and cumulative share of value
# df$CumulativeShareCategories <- cumsum(1:length(df$Carbon.dioxide)) / length(df$Carbon.dioxide)
# df$CumulativeShareValue <- cumsum(df$Carbon.dioxide) / sum(df$Carbon.dioxide)
# 
# # Create a Lorenz curve plot using ggplot2
# p2 <- ggplot(data = df, aes(x = CumulativeShareCategories, y = CumulativeShareValue)) +
#   geom_line() +
#   geom_abline(intercept = 0, slope = 1/15 , linetype = "dashed", color = "red") +  # Add the diagonal line
#   labs(x = "Cumulative Share of sectors", y = "Cumulative Share of CO2 emissions") +
#   ggtitle("Lorenz Curve")+
#   theme(
#     plot.title = element_text(size = 15, hjust = 0.5),  # Set title size and center it
#     panel.border = element_rect(color = "black", fill = NA, size = 1)  # Add a box
#   )
# print(p2)
# 
# 
# p2 <- ggplot(data = df, aes(x = CumulativeShareCategories, y = CumulativeShareValue)) +
#   geom_line() +
#   geom_abline(intercept = 0, slope = 1/8, linetype = "dashed", color = "red") +
#   labs(x = "Cumulative Share of sectors", y = "Cumulative Share of CO2 emissions") +
#   ggtitle("Lorenz Curve") +
#   theme(
#     plot.title = element_text(size = 16, hjust = 0.5),
#     panel.border = element_rect(color = "black", fill = NA, size = 1),
#   ) +
#   scale_x_continuous(expand = expansion(add = c(0, 0.02))) +  # Expand x-axis limits
#   scale_y_continuous(expand = expansion(add = c(0, 0.02))) +  # Expand y-axis limits
#   coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))  # Set the axis limits
# 
# print(p2)

### Aufgabe 5 ###





countries <- c("France","Germany","Austria","Netherlands")
data_fr <- subset(emission_data_cleaned, Country == "France")
data_de <- subset(emission_data_cleaned, Country == "Germany")
data_au <- subset(emission_data_cleaned, Country == "Austria")
data_ne <- subset(emission_data_cleaned, Country == "Netherlands")


# Category names
sector_names <- unique(data_fr$Sector)
data_fr$sectornames <- sector_names
data_de$sectornames <- sector_names
data_au$sectornames <- sector_names
data_ne$sectornames <- sector_names

# Create a plot for each category
p_fr <- ggplot(data_fr, aes(x = year, y = Carbon.dioxide)) +
  geom_line() +
  facet_wrap(sectornames ~ ., scales = "free_y", switch = 'y') +
  labs(title = "CO2 emission over time by sector",
       x = "Year",
       y = "CO2 emission")

p_de <- ggplot(data_de, aes(x = year, y = Carbon.dioxide)) +
  geom_line() +
  facet_wrap(sectornames ~ ., scales = "free_y", switch = 'y') +
  labs(title = "CO2 emission over time by sector",
       x = "Year",
       y = "CO2 emission")

p_au <- ggplot(data_au, aes(x = year, y = Carbon.dioxide)) +
  geom_line() +
  facet_wrap(sectornames ~ ., scales = "free_y", switch = 'y') +
  labs(title = "CO2 emission over time by sector",
       x = "Year",
       y = "CO2 emission")

p_ne <- ggplot(data_ne, aes(x = year, y = Carbon.dioxide)) +
  geom_line() +
  facet_wrap(sectornames ~ ., scales = "free_y", switch = 'y') +
  labs(title = "CO2 emission over time by sector",
       x = "Year",
       y = "CO2 emission")

#rtiugheruzhe
