# Install required packages
install.packages(c("dplyr", "lubridate", "ggplot2", "zoo"))
# Load libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(zoo)
# Define the URL for the data
urls = c("https://stats.bis.org/api/v2/data/dataflow/BIS/WS_EER/1.0/D.N.B.AT+AU+BE+CA+CH+CL+CO+CZ+DE+DK+EE+ES+FI+FR+GB+GR+HU+IE+IL+IS+IT+JP+KR+LT+LU+LV+MX+NL+NO+NZ+PL+PT+SE+SI+SK+TR+US?startPeriod=2012-01-01&endPeriod=2024-10-29&format=csv")
# Initialize a list to store the data
data = list() 
# Loop through the URLs and read the data
for (i in 1:length(urls)) { data[[i]] <- read.csv(urls[i])
}
# Combine the data frames into one
combined_data <- do.call(rbind, data) 
# Ensure date is in the correct format
combined_data$date <- as.Date(combined_data$TIME_PERIOD)
# Interpolate missing values for the exchange rates
combined_data$exchange_rate <- na.approx(combined_data$OBS_VALUE, na.rm = FALSE)
# Calculate monthly standard deviations of exchange rates
monthly_exchange <- combined_data %>% group_by(country = TITLE_TS, month = floor_date(date, "month")) %>% summarize(std_dev_xr = sd(exchange_rate, na.rm = TRUE)) %>% ungroup()
View(monthly_exchange)
#Load monthly CPI inflation data
inflation_data <-read.csv("C:/Users/suuser/Desktop/CPI_OECD.csv", sep=";")
View(inflation_data)
#In the date column chracter values is converted as date
inflation_data$Date <- dmy(inflation_data$Date)
# In the CPI column chracter values is converted as numeric
inflation_data$CPI <- as.numeric(inflation_data$CPI)
# Find indices of NA values in the CPI column
na_indices <- which(is.na(inflation_data$CPI)) 
# View the rows with NA values
na_rows <- inflation_data[na_indices, ]
print(na_rows)
sum(is.na(inflation_data$CPI))
colnames(monthly_exchange)[colnames(monthly_exchange) == "country"] <- "Country"
colnames(monthly_exchange)[colnames(monthly_exchange) == "month"] <- "Date"

# Load necessary library
library(dplyr)

# Updated cleaning function to keep text up to the first break
clean_country_name <- function(country) {
  
  # Keep everything before the first occurrence of ' - '
  country <- sub(" -.*", "", monthly_exchange$Country)  # Remove everything after and including ' - '
  
  # Return cleaned country name
  return(country)
}
clean_date_name <- function(Date) { 
  # Keep everything before the first occurrence of 
  Date <- sub("2024-10-01", "", monthly_exchange$Date)  # Remove everything after and including 
  # Return cleaned Date name
  return(Date)
}

# Apply the cleaning function to both data frames
monthly_exchange$Country <- clean_country_name(monthly_exchange$Country)
monthly_exchange$Date <- clean_date_name(monthly_exchange$Date)
combined_data <- merge(inflation_data, monthly_exchange, by = c("Country", "Date"), all = FALSE)
sum(is.na(combined_data$CPI))
# Load necessary libraries
library(dplyr) 
# Step 1: Calculate global max and min values for normalization
max_xr <- max(combined_data$std_dev_xr, na.rm = TRUE)
min_xr <- min(combined_data$std_dev_xr, na.rm = TRUE) 
max_inf <- max(combined_data$CPI, na.rm = TRUE)
min_inf <- min(combined_data$CPI, na.rm = TRUE) 
# Step 2: Normalize std_dev_xr and CPI using the provided formula
combined_data <- combined_data %>%
  mutate(normalized_xr = (max_xr - std_dev_xr) / (max_xr - min_xr), normalized_inf = (max_inf - CPI) / (max_inf - min_inf))
# Step 3: Calculate the financial uncertainty index using geometric average
combined_data <- combined_data %>%mutate(financial_uncertainty_index = sqrt(normalized_xr * normalized_inf))
# Step 4: Check the resulting data frame
head(combined_data)
install.packages("ConvergenceClubs")
library(ConvergenceClubs)
library(dplyr)
library(tidyverse)
library(plm)
library(ggplot2)
library(sp)
library(scales)
panel_data <- combined_data %>% select(Country, Date, financial_uncertainty_index) %>% pivot_wider(names_from = Country, values_from = financial_uncertainty_index)  # Replace spread with pivot_wider
panel_data_trans<- as.data.frame(t(panel_data))
install.packages("openxlsx")
library(openxlsx)
write.xlsx(panel_data_trans, "panel_data_transpose.xlsx")
#Burada tarih sat??r??n?? ????kar??yoruz. Dosyay?? annex olarak kaydediyoruz. Annex dosyas??n?? R???a y??kl??yoruz.
clubs <-findClubs(annex, dataCols=2:154, unit_names = 1, refCol=154,time_trim = 0.333, HACmethod = "FQSB",cstar = 0, cstar_method = 'incremental', cstar_increment = 0.1) 
mclubs <- mergeClubs(clubs, mergeMethod = c("PS", "vLT"), threshold = -1.65, mergeDivergent = FALSE, estar = -1.65) 
summary(mclubs)
plot(clubs,legend=TRUE,cex=1, plot_args=list(lwd=0.5,lty=1, type='o',xmarks=seq(1,13),xlabs=seq(2012,2024), xlabs_dir=0, xlab='', ylab=''), legend_args=list(cex=1.2, max_length_labels=6, y.intersp=0.205, lwd=1))
install.packages("rworldmap")
library(rworldmap)
club.data=read.csv("C:/Users/suuser/Desktop/clubs.csv", sep=";")
colnames(club.data)[colnames(club.data) == "X...country"] <- "country"
convergencemap<- joinCountryData2Map(club.data, joinCode = "ISO3", nameJoinColumn = "country")
countrynames <- convergencemap$NAME
countrycodes <- convergencemap$ADM0_A3
deneme <- data.frame(countrycodes,countrynames)
deneme
plot <- mapCountryData(convergencemap
                       , nameColumnToPlot="convergencenumber"
                       , oceanCol = "azure2"
                       , catMethod="categorical"
                       , missingCountryCol = gray(.8)
                       , mapRegion="World"
                       , colourPalette=c("orange",
                                         "cadetblue3",
                                         "burlywood4",
                                         "brown",
                                         "blue")
                       , mapTitle = "Title of this Map")




