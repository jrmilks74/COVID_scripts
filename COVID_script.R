library(forecast)
library(ggplot2)
library(dplyr)
library(lubridate)
#Get the raw data from John Hopkins University COVID dashboard via GitHub
COVID <- read.csv("csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", header = TRUE)

#Process the data (sum up the individual province data for Australia, Canada, and China, change the Congo names to official country names)
COVID <- COVID[-c(42, 43, 102, 103, 105, 119:129, 160, 172, 191:194, 217, 253:262, 269),]
COVID <- COVID[,-c(1,3,4)]
COVID[2:313] <- lapply(COVID[2:313], as.numeric)
COVID <- aggregate(.~Country.Region, COVID, sum)
COVID <- t(COVID)
colnames(COVID) <- COVID[1,]
COVID <- COVID[-1,]
colnames(COVID)[colnames(COVID) == "Congo (Brazzaville)"] <- "Congo"
colnames(COVID)[colnames(COVID) == "Congo (Kinshasa)"] <- "DR Congo"
colnames(COVID)[colnames(COVID) == "Cote d'Ivoire"] <- "Côte d'Ivoire"
colnames(COVID)[colnames(COVID) == "Burma"] <- "Myanmar"
colnames(COVID)[colnames(COVID) == "Korea, South"] <- "South Korea"
colnames(COVID)[colnames(COVID) == "Saint Kitts and Nevis"] <- "Saint Kitts & Nevis"
colnames(COVID)[colnames(COVID) == "Sao Tome and Principe"] <- "Sao Tome & Principe"
colnames(COVID)[colnames(COVID) == "Taiwan*"] <- "Taiwan"
COVID <- COVID[,order(colnames(COVID))]
class(COVID) <- "numeric"
COVID_df <- as.data.frame(COVID)
COVID_df$Time <- seq.Date(as.Date("2020-01-22"), origin = "1970-01-01", by = "day", length.out = nrow(COVID_df))

#Get the daily infections per country
daily_infections <- diff(COVID)
daily_infections_ma <- ma(daily_infections, 7)

daily_infections_df <- as.data.frame(daily_infections)
daily_infections_df$Time <- seq.Date(as.Date("2020-01-22"), origin = "1970-01-01", by = "day", length.out = nrow(daily_infections_df))
colnames(daily_infections_df) <- colnames(COVID_df)

daily_infections_ma_df <- as.data.frame(daily_infections_ma)
daily_infections_ma_df$Time <- seq.Date(as.Date("2020-01-22"), origin = "1970-01-01", by = "day", length.out = nrow(daily_infections_ma_df))
colnames(daily_infections_ma_df) <- colnames(COVID_df)

#Plot the US pandemic trend and the daily infections
plot(US~Time, data = COVID_df, type = "l", lwd = 1.5, xlab = "Time", ylab = "Total positive cases", main = "US COVID-19 pandemic since January 2020")
abline(v = as.Date("2020-11-26"), lwd = 1.5, col = "red", lty = 2)
text(x = as.Date("2020-11-21"), y = 5000000, "Thanksgiving", col = "red", srt = 90)
abline(v = as.Date("2020-12-25"), lwd = 1.5, col = "green", lty = 2)
text(x = as.Date("2020-12-20"), y = 5000000, "Christmas", col = "green", srt = 90)

infections_per_month <- daily_infections_df %>% group_by(month = floor_date(Time, "month")) %>% summarize(US = sum(US))
ggplot(data = infections_per_month, aes(x = month, y = US)) +
        theme_bw() +
        geom_bar(stat = "identity", col = "red", fill = "red") +
        labs(title = "Total COVID infections per month",
             subtitle = "US",
             x = "Time",
             y = "Infections")

plot(US~Time, data = daily_infections_ma_df, type = "l", lwd = 1.5, xlab = "Time", ylab = "Number of positive cases", main = "COVID-19 cases per day in the US, 7-day MA")
abline(v = as.Date("2020-11-26"), lwd = 1.5, col = "red", lty = 2)
text(x = as.Date("2020-11-21"), y = 50000, "Thanksgiving", col = "red", srt = 90)
abline(v = as.Date("2020-12-25"), lwd = 1.5, col = "green", lty = 2)
text(x = as.Date("2020-12-20"), y = 50000, "Christmas", col = "green", srt = 90)

#Import country population data from https://www.kaggle.com/tanuprabhu/population-by-country-2020, filter it out to just those countries in the COVID data, then extract a vector of world population numbers
population_by_country_2020 <- read.csv("~/Desktop/COVID-19/population_by_country_2020.csv", header = T)
colnames(population_by_country_2020)[colnames(population_by_country_2020) == "Country..or.dependency."] <- "Country"
colnames(population_by_country_2020)[colnames(population_by_country_2020) == "Population..2020."] <- "Population" 
population_by_country_2020[3, 1] <- "US"
population_by_country_2020[86, 1] <- "Czechia"
country_match <- match(colnames(COVID), population_by_country_2020[, 1])
population_2020 <- population_by_country_2020[country_match, ]
population_2020 <- population_2020[complete.cases(population_2020[, 2]) ,]

#Calculate per capita data
infections_per_capita <- t((t(COVID)/population_2020[, 2])*100000)
daily_infections_per_capita <- t((t(daily_infections)/population_2020[, 2])*100000)

#Plot the overall infections per 100,000 population
ipc_df <- data.frame(infections_per_capita)
ipc_df$Time <- seq.Date(as.Date("2020-01-22"), by = "day", origin = "1970-01-01", length.out = nrow(ipc_df))

ggplot(ipc_df, aes(x = Time)) +
        theme_bw() +
        geom_line(aes(y = US, colour = "US")) +
        geom_line(aes(y = Canada, colour = "Canada")) +
        geom_line(aes(y = Germany, colour = "Germany")) +
        geom_line(aes(y = France, colour = "France")) +
        geom_line(aes(y = United.Kingdom, colour = "UK")) +
        geom_line(aes(y = Sweden, colour = "Sweden")) +
        geom_line(aes(y = Belgium, colour = "Belgium")) +
        geom_line(aes(y = Italy, colour = "Italy")) +
        scale_colour_manual("",
                            breaks = c("US", "Canada", "Germany", "France", "UK", "Sweden", "Belgium", "Italy"),
                            values = c("black", "blue", "green", "salmon", "red", "yellow", "purple", "orange")) +
        scale_x_date(date_breaks = "2 months",
                     date_minor_breaks = "1 month") +
        labs(title = "COVID infections per 100,000 population",
             x = "Time",
             y = "Infections per capita")

#Plot the seven-day moving average of the daily infection rate per 100,000 population
dipc_df <- data.frame(daily_infections_per_capita)
dipc.ma <- ma(daily_infections_per_capita, order = 7, centre = TRUE)
colnames(dipc.ma) <- colnames(COVID)
dipc_ma_df <- data.frame(dipc.ma)
dipc_ma_df$Time <- seq.Date(as.Date("2020-01-23"), by = "day", origin = "1970-01-01", length.out = nrow(dipc_ma_df))

ggplot(dipc_ma_df, aes(x = Time)) +
        theme_bw() +
        geom_line(aes(y = US, colour = "US")) +
        geom_line(aes(y = Canada, colour = "Canada")) +
        geom_line(aes(y = Germany, colour = "Germany")) +
        geom_line(aes(y = France, colour = "France")) +
        geom_line(aes(y = United.Kingdom, colour = "UK")) +
        geom_line(aes(y = Sweden, colour = "Sweden")) +
        geom_line(aes(y = Belgium, colour = "Belgium")) +
        geom_line(aes(y = Italy, colour = "Italy")) +
        scale_colour_manual("",
                            breaks = c("US", "Canada", "Germany", "France", "UK", "Sweden", "Belgium", "Italy"),
                            values = c("black", "blue", "green", "salmon", "red", "yellow", "purple", "orange")) +
        scale_x_date(date_breaks = "2 months",
                     date_minor_breaks = "1 month") +
        labs(title = "New COVID infections per day per 100,000 population",
             subtitle = "7-day moving average",
             x = "Time",
             y = "Infections per capita")

#Get the raw data from John Hopkins University COVID dashboard via GitHub
deaths <- read.csv("csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", header = TRUE)

#Process the data (sum up the individual province data for Australia, Canada, and China, change the Congo names to official country names)
deaths <- deaths[-c(42, 43, 102, 103, 105, 119:129, 160, 172, 191:194, 217, 253:262, 269),]
deaths <- deaths[,-c(1,3,4)]
deaths[2:313] <- lapply(deaths[2:313], as.numeric)
deaths <- aggregate(.~Country.Region, deaths, sum)
deaths <- t(deaths)
colnames(deaths) <- deaths[1,]
colnames(deaths)[colnames(deaths) == "Congo (Brazzaville)"] <- "Congo"
colnames(deaths)[colnames(deaths) == "Congo (Kinshasa)"] <- "DR Congo"
colnames(deaths)[colnames(deaths) == "Cote d'Ivoire"] <- "Côte d'Ivoire"
colnames(deaths)[colnames(deaths) == "Burma"] <- "Myanmar"
colnames(deaths)[colnames(deaths) == "Korea, South"] <- "South Korea"
colnames(deaths)[colnames(deaths) == "Saint Kitts and Nevis"] <- "Saint Kitts & Nevis"
colnames(deaths)[colnames(deaths) == "Sao Tome and Principe"] <- "Sao Tome & Principe"
colnames(deaths)[colnames(deaths) == "Taiwan*"] <- "Taiwan"
deaths <- deaths[-1,]
deaths <- deaths[,order(colnames(deaths))]
class(deaths) <- "numeric"
deaths_df <- as.data.frame(deaths)
deaths_df$Time <- seq.Date(as.Date("2020-01-22"), origin = "1970-01-01", by = "day", length.out = nrow(deaths_df))

#Get the daily infections per country
daily_deaths <- diff(deaths)
daily_deaths_ma <- ma(daily_deaths, 7)

daily_deaths_df <- as.data.frame(daily_deaths)
daily_deaths_df$Time <- seq.Date(as.Date("2020-01-22"), origin = "1970-01-01", by = "day", length.out = nrow(daily_deaths_df))
colnames(daily_deaths_df) <- colnames(COVID_df)

daily_deaths_ma_df <- as.data.frame(daily_deaths_ma)
daily_deaths_ma_df$Time <- seq.Date(as.Date("2020-01-22"), origin = "1970-01-01", by = "day", length.out = nrow(daily_deaths_ma_df))
colnames(daily_deaths_ma_df) <- colnames(deaths_df)

#Plot the US pandemic trend and the daily deaths
plot(US~Time, data = deaths_df, type = "l", lwd = 1.5, xlab = "Time", ylab = "Total deaths", main = "US COVID-19 deaths since January 2020")
abline(v = as.Date("2020-11-26"), lwd = 1.5, col = "red", lty = 2)
text(x = as.Date("2020-11-21"), y = 50000, "Thanksgiving", col = "red", srt = 90)
abline(v = as.Date("2020-12-25"), lwd = 1.5, col = "green", lty = 2)
text(x = as.Date("2020-12-20"), y = 50000, "Christmas", col = "green", srt = 90)

deaths_per_month <- daily_deaths_df %>% group_by(month = floor_date(Time, "month")) %>% summarize(US = sum(US))
ggplot(data = deaths_per_month, aes(x = month, y = US)) +
        theme_bw() +
        geom_bar(stat = "identity", col = "red", fill = "red") +
        labs(title = "Total COVID deaths per month",
             subtitle = "US",
             x = "Time",
             y = "Deaths")

plot(US~Time, data = daily_deaths_ma_df, type = "l", lwd = 1.5, xlab = "Time", ylab = "Number of deaths", main = "COVID-19 deaths per day in the US, 7-day MA")
abline(v = as.Date("2020-11-26"), lwd = 1.5, col = "red", lty = 2)
text(x = as.Date("2020-11-21"), y = 500, "Thanksgiving", col = "red", srt = 90)
abline(v = as.Date("2020-12-25"), lwd = 1.5, col = "green", lty = 2)
text(x = as.Date("2020-12-20"), y = 500, "Christmas", col = "green", srt = 90)

#Match country population data with country death data
country_match <- match(colnames(deaths), population_by_country_2020[, 1])
population_2020 <- population_by_country_2020[country_match, ]
population_2020 <- population_2020[complete.cases(population_2020[, 2]) ,]

#Calculate per capita data
deaths_per_capita <- t((t(deaths)/population_2020[, 2])*100000)
daily_deaths_per_capita <- t((t(daily_deaths)/population_2020[, 2])*100000)

#Plot the overall infections per 100,000 population
dpc_df <- data.frame(deaths_per_capita)
dpc_df$Time <- seq.Date(as.Date("2020-01-22"), by = "day", origin = "1970-01-01", length.out = nrow(dpc_df))

ggplot(dpc_df, aes(x = Time)) +
        theme_bw() +
        geom_line(aes(y = US, colour = "US")) +
        geom_line(aes(y = Canada, colour = "Canada")) +
        geom_line(aes(y = Germany, colour = "Germany")) +
        geom_line(aes(y = France, colour = "France")) +
        geom_line(aes(y = United.Kingdom, colour = "UK")) +
        geom_line(aes(y = Sweden, colour = "Sweden")) +
        geom_line(aes(y = Belgium, colour = "Belgium")) +
        geom_line(aes(y = Italy, colour = "Italy")) +
        scale_colour_manual("",
                            breaks = c("US", "Canada", "Germany", "France", "UK", "Sweden", "Belgium", "Italy"),
                            values = c("black", "blue", "green", "salmon", "red", "yellow", "purple", "orange")) +
        scale_x_date(date_breaks = "2 months",
                     date_minor_breaks = "1 month") +
        labs(title = "COVID deaths per 100,000 population",
             x = "Time",
             y = "Deaths per capita")

#Plot the seven-day moving average of the daily infection rate per 100,000 population
ddpc_df <- data.frame(daily_deaths_per_capita)
library(forecast)
ddpc.ma <- ma(daily_deaths_per_capita, order = 7, centre = TRUE)
colnames(ddpc.ma) <- colnames(deaths)
ddpc_ma_df <- data.frame(ddpc.ma)
ddpc_ma_df$Time <- seq.Date(as.Date("2020-01-23"), by = "day", origin = "1970-01-01", length.out = nrow(ddpc_ma_df))

ggplot(ddpc_ma_df, aes(x = Time)) +
        theme_bw() +
        geom_line(aes(y = US, colour = "US")) +
        geom_line(aes(y = Canada, colour = "Canada")) +
        geom_line(aes(y = Germany, colour = "Germany")) +
        geom_line(aes(y = France, colour = "France")) +
        geom_line(aes(y = United.Kingdom, colour = "UK")) +
        geom_line(aes(y = Sweden, colour = "Sweden")) +
        geom_line(aes(y = Belgium, colour = "Belgium")) +
        geom_line(aes(y = Italy, colour = "Italy")) +
        scale_colour_manual("",
                            breaks = c("US", "Canada", "Germany", "France", "UK", "Sweden", "Belgium", "Italy"),
                            values = c("black", "blue", "green", "salmon", "red", "yellow", "purple", "orange")) +
        scale_x_date(date_breaks = "2 months",
                     date_minor_breaks = "1 month") +
        labs(title = "COVID deaths per day per 100,000 population",
             subtitle = "7-day moving average",
             x = "Time",
             y = "Deaths per capita")


#Nordic countries
ggplot(ipc_df, aes(x = Time)) +
        theme_bw() +
        geom_line(aes(y = Sweden, colour = "Sweden")) +
        geom_line(aes(y = Norway, colour = "Norway")) +
        geom_line(aes(y = Denmark, colour = "Denmark")) +
        geom_line(aes(y = Finland, colour = "Finland")) +
        geom_line(aes(y = Iceland, colour = "Iceland")) +
        scale_colour_manual("",
                            breaks = c("Sweden", "Norway", "Denmark", "Finland", "Iceland"),
                            values = c("black", "red", "green", "orange", "blue")) +
        scale_x_date(date_breaks = "2 months",
                     date_minor_breaks = "1 month") +
        labs(x = "Time",
              y = "Infections per capita",
             title = "COVID infections per 100,000",
             subtitle = "Nordic Countries")


ggplot(dipc_ma_df, aes(x = Time)) +
        theme_bw() +
        geom_line(aes(y = Sweden, colour = "Sweden")) +
        geom_line(aes(y = Norway, colour = "Norway")) +
        geom_line(aes(y = Denmark, colour = "Denmark")) +
        geom_line(aes(y = Finland, colour = "Finland")) +
        geom_line(aes(y = Iceland, colour = "Iceland")) +
        scale_colour_manual("",
                            breaks = c("Sweden", "Norway", "Denmark", "Finland", "Iceland"),
                            values = c("black", "red", "green", "orange", "blue")) +
        scale_x_date(date_breaks = "2 months",
                     date_minor_breaks = "1 month") +
        labs(x = "Time",
             y = "Daily infections per capita",
             title = "New COVID cases per day per 100,000 7 day MA",
             subtitle = "Scandinavia")


ggplot(dpc_df, aes(x = Time)) +
        theme_bw() +
        geom_line(aes(y = Sweden, colour = "Sweden")) +
        geom_line(aes(y = Norway, colour = "Norway")) +
        geom_line(aes(y = Denmark, colour = "Denmark")) +
        geom_line(aes(y = Finland, colour = "Finland")) +
        geom_line(aes(y = Iceland, colour = "Iceland")) +
        scale_colour_manual("",
                            breaks = c("Sweden", "Norway", "Denmark", "Finland", "Iceland"),
                            values = c("black", "red", "green", "orange", "blue")) +
        scale_x_date(date_breaks = "2 months",
                     date_minor_breaks = "1 month") +
        labs(x = "Time",
             y = "Daily infections per capita",
             title = "COVID deaths per 100,000",
             subtitle = "Scandinavia")


ggplot(ddpc_ma_df, aes(x = Time)) +
        theme_bw() +
        geom_line(aes(y = Sweden, colour = "Sweden")) +
        geom_line(aes(y = Norway, colour = "Norway")) +
        geom_line(aes(y = Denmark, colour = "Denmark")) +
        geom_line(aes(y = Finland, colour = "Finland")) +
        geom_line(aes(y = Iceland, colour = "Iceland")) +
        scale_colour_manual("",
                            breaks = c("Sweden", "Norway", "Denmark", "Finland", "Iceland"),
                            values = c("black", "red", "green", "orange", "blue")) +
        scale_x_date(date_breaks = "2 months",
                     date_minor_breaks = "1 month") +
        labs(x = "Time",
             y = "Daily infections per capita",
             title = "New COVID deaths per day per 100,000 7 day MA",
             subtitle = "Scandinavia")


#North America
ggplot(ipc_df, aes(x = Time)) +
        theme_bw() +
        geom_line(aes(y = US, colour = "US")) +
        geom_line(aes(y = Canada, colour = "Canada")) +
        geom_line(aes(y = Mexico, colour = "Mexico")) +
        scale_colour_manual("",
                            breaks = c("US", "Canada", "Mexico"),
                            values = c("red", "blue", "green")) +
        scale_x_date(date_breaks = "2 months",
                     date_minor_breaks = "1 month") +
        labs(title = "COVID infections per 100,000 population",
             subtitle = "North America",
             x = "Time",
             y = "Infections per capita")

ggplot(dipc_ma_df, aes(x = Time)) +
        theme_bw() +
        geom_line(aes(y = US, colour = "US")) +
        geom_line(aes(y = Canada, colour = "Canada")) +
        geom_line(aes(y = Mexico, colour = "Mexico")) +
        scale_colour_manual("",
                            breaks = c("US", "Canada", "Mexico"),
                            values = c("red", "blue", "green")) +
        scale_x_date(date_breaks = "2 months",
                     date_minor_breaks = "1 month") +
        labs(title = "New COVID infections per day per 100,000 population",
             subtitle = "North America",
             x = "Time",
             y = "Infections per capita")

ggplot(dpc_df, aes(x = Time)) +
        theme_bw() +
        geom_line(aes(y = US, colour = "US")) +
        geom_line(aes(y = Canada, colour = "Canada")) +
        geom_line(aes(y = Mexico, colour = "Mexico")) +
        scale_colour_manual("",
                            breaks = c("US", "Canada", "Mexico"),
                            values = c("red", "blue", "green")) +
        scale_x_date(date_breaks = "2 months",
                     date_minor_breaks = "1 month") +
        labs(title = "COVID deaths per 100,000 population",
             subtitle = "North America",
             x = "Time",
             y = "Deaths per capita")

ggplot(ddpc_ma_df, aes(x = Time)) +
        theme_bw() +
        geom_line(aes(y = US, colour = "US")) +
        geom_line(aes(y = Canada, colour = "Canada")) +
        geom_line(aes(y = Mexico, colour = "Mexico")) +
        scale_colour_manual("",
                            breaks = c("US", "Canada", "Mexico"),
                            values = c("red", "blue", "green")) +
        scale_x_date(date_breaks = "2 months",
                     date_minor_breaks = "1 month") +
        labs(title = "New COVID deaths per day per 100,000 population",
             subtitle = "North America",
             x = "Time",
             y = "Deaths per capita")
