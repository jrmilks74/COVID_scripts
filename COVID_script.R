library(forecast)
#Get the raw data from John Hopkins University COVID dashboard via GitHub
COVID <- read.csv("csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", header = TRUE)

#Process the data (sum up the individual province data for Australia, Canada, and China, change the Congo names to official country names)
COVID <- COVID[-c(42, 43, 100, 101, 103, 117:126, 157, 169, 188:191, 214, 249:258, 265),]
COVID <- t(COVID)
names <- COVID[2,]
colnames(COVID) <- names
colnames(COVID)[colnames(COVID) == "Congo (Brazzaville)"] <- "Congo"
colnames(COVID)[colnames(COVID) == "Congo (Kinshasa)"] <- "DR Congo"
colnames(COVID)[colnames(COVID) == "Cote d'Ivoire"] <- "Côte d'Ivoire"
colnames(COVID)[colnames(COVID) == "Burma"] <- "Myanmar"
colnames(COVID)[colnames(COVID) == "Korea, South"] <- "South Korea"
colnames(COVID)[colnames(COVID) == "Saint Kitts and Nevis"] <- "Saint Kitts & Nevis"
colnames(COVID)[colnames(COVID) == "Sao Tome and Principe"] <- "Sao Tome & Principe"
colnames(COVID)[colnames(COVID) == "Taiwan*"] <- "Taiwan"
COVID <- COVID[-c(1,2,3,4),]
class(COVID) <- "numeric"
Australia <- rowSums(COVID[, 9:16])
Canada <- rowSums(COVID[, 40:51])
China <- rowSums(COVID[, 55:87])
COVID <- cbind(COVID, c(Australia),c(Canada), c(China))
COVID <- COVID[,-c(9:16, 40:51, 55:87)]
colnames(COVID)[183:185] <- c("Australia", "Canada", "China")
COVID <- COVID[,order(colnames(COVID))]
COVID <- COVID[, -1]
COVID_df <- as.data.frame(COVID)
COVID_df$Time <- seq.Date(as.Date("2020-01-22"), origin = "1970-01-01", by = "day", length.out = nrow(COVID_df))

#Get the daily infections per country
daily_infections <- diff(COVID)
daily_infections_ma <- ma(daily_infections, 7)

daily_infections_ma_df <- as.data.frame(daily_infections_ma)
daily_infections_ma_df$Time <- seq.Date(as.Date("2022-01-22"), origin = "1970-01-01", by = "day", length.out = nrow(daily_infections_ma_df))
colnames(daily_infections_ma_df) <- colnames(COVID_df)

#Plot the US pandemic trend and the daily infections
plot(US~Time, data = COVID_df, type = "l", lwd = 1.5, xlab = "Time", ylab = "Total positive cases", main = "US COVID-19 pandemic since January 2020")

plot(US~Time, data = daily_infections_ma_df, type = "l", lwd = 1.5, xlab = "Time", ylab = "Number of positive cases", main = "COVID-19 cases per day in the US, 7-day MA")

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

plot(US~Time, data = ipc_df, type = "l", xlab = "Time", ylab = "Positive cases per capita", main = "COVID infections per 100,000 people", lwd = 1.5, ylim = c(0, 4500))
points(Canada~Time, data = ipc_df, type = "l", col = "blue", lwd = 1.5)
points(France~Time, data = ipc_df, type = "l", col = "green", lwd = 1.5)
points(United.Kingdom~Time, data = ipc_df, type = "l", col = "red", lwd = 1.5)
points(Russia~Time, data = ipc_df, type = "l", col = "brown", lwd = 1.5)
points(Sweden~Time, data = ipc_df, type = "l", col = "gold", lwd = 1.5)
points(Belgium~Time, data = ipc_df, type = "l", col = "purple", lwd = 1.5)
points(Italy~Time, data = ipc_df, type = "l", col = "orange", lwd = 1.5)
points(Iceland~Time, data = ipc_df, type = "l", col = "pink", lwd = 1.5)
legend("topleft", legend = c("US", "Canada", "France", "UK", "Russia", "Sweden", "Belgium", "Italy", "Iceland"), col = c("black", "blue", "green", "red", "brown", "gold", "purple", "orange", "pink"), lwd = 1.5)

#Plot the seven-day moving average of the daily infection rate per 100,000 population
dipc_df <- data.frame(daily_infections_per_capita)
library(forecast)
dipc.ma <- ma(daily_infections_per_capita, order = 7, centre = TRUE)
colnames(dipc.ma) <- colnames(COVID)
dipc_ma_df <- data.frame(dipc.ma)
dipc_ma_df$Time <- seq.Date(as.Date("2020-01-23"), by = "day", origin = "1970-01-01", length.out = nrow(dipc_ma_df))

plot(US~Time, data = dipc_ma_df, type = "l", xlab = "Time", ylab = "Daily Infections", main = "COVID infections per day per 100,000 people, 7-day MA", lwd = 1.5, ylim = c(0,155))
points(Canada~Time, data = dipc_ma_df, type = "l", col = "blue", lwd = 1.5)
points(France~Time, data = dipc_ma_df, type = "l", col = "green", lwd = 1.5)
points(United.Kingdom~Time, data = dipc_ma_df, type = "l", col = "red", lwd = 1.5)
points(Russia~Time, data = dipc_ma_df, type = "l", col = "brown", lwd = 1.5)
points(Sweden~Time, data = dipc_ma_df, type = "l", col = "gold", lwd = 1.5)
points(Belgium~Time, data = dipc_ma_df, type = "l", col = "purple", lwd = 1.5)
points(Italy~Time, data = dipc_ma_df, type = "l", col = "orange", lwd = 1.5)
points(Iceland~Time, data = dipc_ma_df, type = "l", col = "pink", lwd = 1.5)
legend("topleft", legend = c("US", "Canada", "France", "UK", "Russia", "Sweden", "Belgium", "Italy", "Iceland"), col = c("black", "blue", "green", "red", "brown", "gold", "purple", "orange", "pink"), lwd = 1.5)