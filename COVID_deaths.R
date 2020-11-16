library(forecast)
#Get the raw data from John Hopkins University COVID dashboard via GitHub
deaths <- read.csv("csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", header = TRUE)

#Process the data (sum up the individual province data for Australia, Canada, and China, change the Congo names to official country names)
deaths <- deaths[-c(42, 43, 100, 101, 103, 117:126, 157, 169, 188:191, 214, 249:258, 265),]
deaths <- t(deaths)
names <- deaths[2,]
colnames(deaths) <- names
colnames(deaths)[colnames(deaths) == "Congo (Brazzaville)"] <- "Congo"
colnames(deaths)[colnames(deaths) == "Congo (Kinshasa)"] <- "DR Congo"
colnames(deaths)[colnames(deaths) == "Cote d'Ivoire"] <- "Côte d'Ivoire"
colnames(deaths)[colnames(deaths) == "Burma"] <- "Myanmar"
colnames(deaths)[colnames(deaths) == "Korea, South"] <- "South Korea"
colnames(deaths)[colnames(deaths) == "Saint Kitts and Nevis"] <- "Saint Kitts & Nevis"
colnames(deaths)[colnames(deaths) == "Sao Tome and Principe"] <- "Sao Tome & Principe"
colnames(deaths)[colnames(deaths) == "Taiwan*"] <- "Taiwan"
deaths <- deaths[-c(1,2,3,4),]
class(deaths) <- "numeric"
Australia <- rowSums(deaths[, 9:16])
Canada <- rowSums(deaths[, 40:51])
China <- rowSums(deaths[, 55:87])
deaths <- cbind(deaths, c(Australia),c(Canada), c(China))
deaths <- deaths[,-c(9:16, 40:51, 55:87)]
colnames(deaths)[183:185] <- c("Australia", "Canada", "China")
deaths <- deaths[,order(colnames(deaths))]
deaths <- deaths[, -1]
deaths_df <- as.data.frame(deaths)
deaths_df$Time <- seq.Date(as.Date("2020-01-22"), origin = "1970-01-01", by = "day", length.out = nrow(deaths_df))

#Get the daily infections per country
daily_deaths <- diff(deaths)
daily_deaths_ma <- ma(daily_deaths, 7)

daily_deaths_ma_df <- as.data.frame(daily_deaths_ma)
daily_deaths_ma_df$Time <- seq.Date(as.Date("2022-01-22"), origin = "1970-01-01", by = "day", length.out = nrow(daily_deaths_ma_df))
colnames(daily_deaths_ma_df) <- colnames(deaths_df)

#Plot the US pandemic trend and the daily infections
plot(US~Time, data = deaths_df, type = "l", lwd = 1.5, xlab = "Time", ylab = "Total deaths", main = "US COVID-19 deaths since January 2020")

plot(US~Time, data = daily_deaths_ma_df, type = "l", lwd = 1.5, xlab = "Time", ylab = "Number of deaths", main = "COVID-19 deaths per day in the US, 7-day MA")

#Import country population data from https://www.kaggle.com/tanuprabhu/population-by-country-2020, filter it out to just those countries in the COVID data, then extract a vector of world population numbers
population_by_country_2020 <- read.csv("~/Desktop/COVID-19/population_by_country_2020.csv", header = T)
colnames(population_by_country_2020)[colnames(population_by_country_2020) == "Country..or.dependency."] <- "Country"
colnames(population_by_country_2020)[colnames(population_by_country_2020) == "Population..2020."] <- "Population" 
population_by_country_2020[3, 1] <- "US"
population_by_country_2020[86, 1] <- "Czechia"
country_match <- match(colnames(deaths), population_by_country_2020[, 1])
population_2020 <- population_by_country_2020[country_match, ]
population_2020 <- population_2020[complete.cases(population_2020[, 2]) ,]

#Calculate per capita data
deaths_per_capita <- t((t(deaths)/population_2020[, 2])*100000)
daily_deaths_per_capita <- t((t(daily_deaths)/population_2020[, 2])*100000)

#Plot the overall infections per 100,000 population
dpc_df <- data.frame(deaths_per_capita)
dpc_df$Time <- seq.Date(as.Date("2020-01-22"), by = "day", origin = "1970-01-01", length.out = nrow(dpc_df))

plot(US~Time, data = dpc_df, type = "l", xlab = "Time", ylab = "Deaths per capita", main = "COVID deaths per 100,000 people", lwd = 1.5, ylim = c(0, 122))
points(Canada~Time, data = dpc_df, type = "l", col = "blue", lwd = 1.5)
points(France~Time, data = dpc_df, type = "l", col = "green", lwd = 1.5)
points(United.Kingdom~Time, data = dpc_df, type = "l", col = "red", lwd = 1.5)
points(Russia~Time, data = dpc_df, type = "l", col = "brown", lwd = 1.5)
points(Sweden~Time, data = dpc_df, type = "l", col = "gold", lwd = 1.5)
points(Belgium~Time, data = dpc_df, type = "l", col = "purple", lwd = 1.5)
points(Italy~Time, data = dpc_df, type = "l", col = "orange", lwd = 1.5)
points(Iceland~Time, data = dpc_df, type = "l", col = "pink", lwd = 1.5)
legend("topleft", legend = c("US", "Canada", "France", "UK", "Russia", "Sweden", "Belgium", "Italy", "Iceland"), col = c("black", "blue", "green", "red", "brown", "gold", "purple", "orange", "pink"), lwd = 1.5)

#Plot the seven-day moving average of the daily infection rate per 100,000 population
ddpc_df <- data.frame(daily_deaths_per_capita)
library(forecast)
ddpc.ma <- ma(daily_deaths_per_capita, order = 7, centre = TRUE)
colnames(ddpc.ma) <- colnames(deaths)
ddpc_ma_df <- data.frame(ddpc.ma)
ddpc_ma_df$Time <- seq.Date(as.Date("2020-01-23"), by = "day", origin = "1970-01-01", length.out = nrow(ddpc_ma_df))

plot(US~Time, data = ddpc_ma_df, type = "l", xlab = "Time", ylab = "Daily deaths", main = "COVID deaths per day per 100,000 people, 7-day MA", lwd = 1.5, ylim = c(0, 3))
points(Canada~Time, data = ddpc_ma_df, type = "l", col = "blue", lwd = 1.5)
points(France~Time, data = ddpc_ma_df, type = "l", col = "green", lwd = 1.5)
points(United.Kingdom~Time, data = ddpc_ma_df, type = "l", col = "red", lwd = 1.5)
points(Russia~Time, data = ddpc_ma_df, type = "l", col = "brown", lwd = 1.5)
points(Sweden~Time, data = ddpc_ma_df, type = "l", col = "gold", lwd = 1.5)
points(Belgium~Time, data = ddpc_ma_df, type = "l", col = "purple", lwd = 1.5)
points(Italy~Time, data = ddpc_ma_df, type = "l", col = "orange", lwd = 1.5)
points(Iceland~Time, data = ddpc_ma_df, type = "l", col = "pink", lwd = 1.5)
legend("topleft", legend = c("US", "Canada", "France", "UK", "Russia", "Sweden", "Belgium", "Italy", "Iceland"), col = c("black", "blue", "green", "red", "brown", "gold", "purple", "orange", "pink"), lwd = 1.5)