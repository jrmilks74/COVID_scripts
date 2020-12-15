library(forecast)
library(ggplot2)
#Get the raw data from John Hopkins University COVID dashboard via GitHub
COVID <- read.csv("csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", header = TRUE)

#Process the data (sum up the individual province data for Australia, Canada, and China, change the Congo names to official country names)
COVID <- COVID[-c(42, 43, 102, 103, 105, 119:128, 159, 171, 190:193, 216, 252:261, 268),]
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

daily_infections_ma_df <- as.data.frame(daily_infections_ma)
daily_infections_ma_df$Time <- seq.Date(as.Date("2020-01-22"), origin = "1970-01-01", by = "day", length.out = nrow(daily_infections_ma_df))
colnames(daily_infections_ma_df) <- colnames(COVID_df)

#Plot the US pandemic trend and the daily infections
plot(US~Time, data = COVID_df, type = "l", lwd = 1.5, xlab = "Time", ylab = "Total positive cases", main = "US COVID-19 pandemic since January 2020")
abline(v = as.Date("2020-11-26"), lwd = 1.5, col = "red", lty = 2)
text(x = as.Date("2020-11-21"), y = 5000000, "Thanksgiving", col = "red", srt = 90)
plot(US~Time, data = daily_infections_ma_df, type = "l", lwd = 1.5, xlab = "Time", ylab = "Number of positive cases", main = "COVID-19 cases per day in the US, 7-day MA")
abline(v = as.Date("2020-11-26"), lwd = 1.5, col = "red", lty = 2)
text(x = as.Date("2020-11-21"), y = 50000, "Thanksgiving", col = "red", srt = 90)

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

plot(US~Time, data = ipc_df, type = "l", xlab = "Time", ylab = "Positive cases per capita", main = "COVID infections per 100,000 people", lwd = 1.5, ylim = c(0, 5300))
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

#Get the raw data from John Hopkins University COVID dashboard via GitHub
deaths <- read.csv("csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", header = TRUE)

#Process the data (sum up the individual province data for Australia, Canada, and China, change the Congo names to official country names)
deaths <- deaths[-c(42, 43, 102, 103, 105, 119:128, 159, 171, 190:193, 216, 252:261, 268),]
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

daily_deaths_ma_df <- as.data.frame(daily_deaths_ma)
daily_deaths_ma_df$Time <- seq.Date(as.Date("2020-01-22"), origin = "1970-01-01", by = "day", length.out = nrow(daily_deaths_ma_df))
colnames(daily_deaths_ma_df) <- colnames(deaths_df)

#Plot the US pandemic trend and the daily deaths
plot(US~Time, data = deaths_df, type = "l", lwd = 1.5, xlab = "Time", ylab = "Total deaths", main = "US COVID-19 deaths since January 2020")
abline(v = as.Date("2020-11-26"), lwd = 1.5, col = "red", lty = 2)
text(x = as.Date("2020-11-21"), y = 50000, "Thanksgiving", col = "red", srt = 90)

plot(US~Time, data = daily_deaths_ma_df, type = "l", lwd = 1.5, xlab = "Time", ylab = "Number of deaths", main = "COVID-19 deaths per day in the US, 7-day MA")
abline(v = as.Date("2020-11-26"), lwd = 1.5, col = "red", lty = 2)
text(x = as.Date("2020-11-21"), y = 500, "Thanksgiving", col = "red", srt = 90)

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

plot(US~Time, data = dpc_df, type = "l", xlab = "Time", ylab = "Deaths per capita", main = "COVID deaths per 100,000 people", lwd = 1.5, ylim = c(0, 160))
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
legend("top", legend = c("US", "Canada", "France", "UK", "Russia", "Sweden", "Belgium", "Italy", "Iceland"), col = c("black", "blue", "green", "red", "brown", "gold", "purple", "orange", "pink"), lwd = 1.5)

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
        xlab("Time") +
        ylab("Infections per capita") +
        ggtitle("COVID infections per 100,000 in Nordic Countries")


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
        xlab("Time") +
        ylab("Daily infections per capita") +
        ggtitle("Daily COVID infections per 100,000 in Nordic Countries")


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
        xlab("Time") +
        ylab("Deaths per capita") +
        ggtitle("COVID deaths per 100,000 in Nordic Countries")


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
        xlab("Time") +
        ylab("Daily deaths per capita") +
        ggtitle("Daily COVID deaths per 100,000 in Nordic Countries")


#North America
plot(US~Time, data = ipc_df, ylab = "Total cases per capita", main = "COVID cases per 100,000 in North America", col = "red", type = "l", lwd = 1.5)
points(Canada~Time, data = ipc_df, col = "blue", type = "l", lwd = 1.5)
points(Mexico~Time, data = ipc_df, col = "green", type = "l", lwd = 1.5)
abline(v = as.Date("2020-11-26"), lwd = 1.5, col = "black", lty = 2)
text(x = as.Date("2020-11-21"), y = 2000, "Thanksgiving", col = "black", srt = 90)
legend("topleft", c("US", "Canada", "Mexico"), col = c("red", "blue", "green"), lwd = 2)

plot(US~Time, data = dipc_ma_df, ylab = "Daily cases per capita", main = "Daily COVID cases per 100,000 in North America", col = "red", type = "l", lwd = 1.5)
points(Canada~Time, data = dipc_ma_df, col = "blue", type = "l", lwd = 1.5)
points(Mexico~Time, data = dipc_ma_df, col = "green", type = "l", lwd = 1.5)
abline(v = as.Date("2020-11-26"), lwd = 1.5, col = "black", lty = 2)
text(x = as.Date("2020-11-21"), y = 30, "Thanksgiving", col = "black", srt = 90)
legend("topleft", c("US", "Canada", "Mexico"), col = c("red", "blue", "green"), lwd = 2)

plot(US~Time, data = dpc_df, ylab = "Total deaths per capita", main = "COVID deaths per 100,000 in North America", col = "red", type = "l", lwd = 1.5)
points(Canada~Time, data = dpc_df, col = "blue", type = "l", lwd = 1.5)
points(Mexico~Time, data = dpc_df, col = "green", type = "l", lwd = 1.5)
abline(v = as.Date("2020-11-26"), lwd = 1.5, col = "black", lty = 2)
text(x = as.Date("2020-11-20"), y = 50, "Thanksgiving", col = "black", srt = 90)
legend("topleft", c("US", "Canada", "Mexico"), col = c("red", "blue", "green"), lwd = 2)

plot(US~Time, data = ddpc_ma_df, ylab = "Daily deaths per capita", main = "Daily COVID deaths per 100,000 in North America", col = "red", type = "l", lwd = 1.5)
points(Canada~Time, data = ddpc_ma_df, col = "blue", type = "l", lwd = 1.5)
points(Mexico~Time, data = ddpc_ma_df, col = "green", type = "l", lwd = 1.5)
abline(v = as.Date("2020-11-26"), lwd = 1.5, col = "black", lty = 2)
text(x = as.Date("2020-11-20"), y = 0.10, "Thanksgiving", col = "black", srt = 90)
legend("topleft", c("US", "Canada", "Mexico"), col = c("red", "blue", "green"), lwd = 2)

#COVID numbers by state
US <- read.csv("~/Desktop/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
US.deaths <- read.csv("csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv", header = TRUE)

Ohio <- subset(US, Province_State == "Ohio")
Montgomery <- subset(Ohio, Admin2 == "Montgomery")
Ohio_sum <- Ohio[, -c(1:11)]
Ohio_sum <- as.matrix(Ohio_sum)
Ohio_sum <- colSums(Ohio_sum)
Ohio_sum <- as.data.frame(Ohio_sum)
names(Ohio_sum) <- "Cases"
Ohio_sum$Time <- seq.Date(as.Date("2020-01-22"), by = "day", origin = "1970-01-01", length.out = nrow(Ohio_sum))
Ohio_daily <- diff(Ohio_sum$Cases)
Ohio_time <- seq.Date(as.Date("2020-01-23"), by = "day", origin = "1970-01-01", length.out = length(Ohio_daily))
Ohio_daily <- data.frame(Time = Ohio_time, daily = Ohio_daily)
Ohio_daily$ma <- ma(Ohio_daily$daily, 7)
plot(ma~Time, data = Ohio_daily, ylab = "COVID-19 cases per day", main = "COVID-19 cases per day in Ohio, USA", type = "l", lwd = 1.5, xaxt = "n")
ticks.at <- seq(min(Ohio_daily$Time), max(Ohio_daily$Time), by = "months")
ticks.lab <- format(ticks.at, format = "%b")
Axis(Ohio_daily$Time, at = ticks.at, side = 1, labels = ticks.lab, las = 2)
abline(v = as.Date("2020-11-26"), lwd = 1.5, col = "red", lty = 2)
text(x = as.Date("2020-10-30"), y = 3000, "Thanksgiving", col = "red")


Montgomery <- Montgomery[, -c(1:11)]
Montgomery <- as.matrix(Montgomery)
Montgomery <- t(Montgomery)
Montgomery <- as.data.frame(Montgomery)
names(Montgomery) <- "cases"
Montgomery$Time <- seq.Date(as.Date("2020-01-22"), by = "day", origin = "1970-01-01", length.out = length(Montgomery$cases))
Montgomery_daily <- diff(Montgomery$cases)
Time <- seq.Date(as.Date("2020-01-23"), by = "day", origin = "1970-01-01", length.out = length(Montgomery_daily))
Montgomery_daily <- data.frame(cases = Montgomery_daily, time = Time)
Montgomery_daily$ma <- ma(Montgomery_daily$cases, 7)
plot(ma~time, data = Montgomery_daily, xlab = "Time", ylab = "COVID-19 cases per day", main = "COVID-19 cases per day in Montgomery County, Ohio", type = "l", lwd = 1.5, col = "red", xaxt = "n")
ticks.at <- seq(min(Montgomery_daily$time), max(Montgomery_daily$time), by = "months")
ticks.lab <- format(ticks.at, format = "%b")
Axis(Montgomery_daily$time, at = ticks.at, side = 1, labels = ticks.lab, las = 2)

Ohio.deaths <- subset(US.deaths, Province_State == "Ohio")
Montgomery.deaths <- subset(US.deaths, Province_State == "Ohio" & Admin2 == "Montgomery")
Ohio.deaths <- Ohio.deaths[, -c(1:12)]
Ohio.deaths <- as.matrix(Ohio.deaths)
Ohio.deaths <- colSums(Ohio.deaths)
Ohio.deaths <- as.data.frame(Ohio.deaths)
names(Ohio.deaths) <- "Deaths"
Ohio.deaths$Time <- seq.Date(as.Date("2020-01-22"), by = "day", origin = "1970-01-01", length.out = nrow(Ohio.deaths))
Ohio_deaths_daily <- diff(Ohio.deaths$Deaths)
Ohio_deaths_time <- seq.Date(as.Date("2020-01-23"), by = "day", origin = "1970-01-01", length.out = length(Ohio_deaths_daily))
Ohio_deaths_daily <- data.frame(Time = Ohio_deaths_time, daily = Ohio_deaths_daily)
Ohio_deaths_daily$ma <- ma(Ohio_deaths_daily$daily, 7)
plot(ma~Time, data = Ohio_deaths_daily, ylab = "COVID-19 deaths per day", main = "COVID-19 deaths per day in Ohio, USA", type = "l", col = "red", lwd = 1.5, xaxt = "n")
ticks.at <- seq(min(Ohio_deaths_daily$Time), max(Ohio_deaths_daily$Time), by = "months")
ticks.lab <- format(ticks.at, format = "%b")
Axis(Ohio_deaths_daily$Time, at = ticks.at, side = 1, labels = ticks.lab, las = 2)

Montgomery.deaths <- Montgomery.deaths[, -c(1:12)]
Montgomery.deaths <- as.matrix(Montgomery.deaths)
Montgomery.deaths <- t(Montgomery.deaths)
Montgomery.deaths <- as.data.frame(Montgomery.deaths)
names(Montgomery.deaths) <- "Deaths"
Montgomery.deaths$Time <- seq.Date(as.Date("2020-01-23"), by = "day", origin = "1970-01-01", length.out = length(Montgomery.deaths$Deaths))
Montgomery_deaths_daily <- diff(Montgomery.deaths$Deaths)
names(Montgomery_deaths_daily) <- "Deaths"
Time <- seq.Date(as.Date("2020-01-23"), by = "day", origin = "1970-01-01", length.out = length(Montgomery_deaths_daily))
Montgomery_deaths_daily <- data.frame(deaths = Montgomery_deaths_daily, time = Time)
Montgomery_deaths_daily$ma <- ma(Montgomery_deaths_daily$deaths, 7)
plot(ma~time, data = Montgomery_deaths_daily, xlab = "Time", ylab = "COVID-19 deaths per day", main = "COVID-19 deaths per day in Montgomery County, Ohio", type = "l", lwd = 1.5, col = "red", xaxt = "n")
ticks.at <- seq(min(Montgomery_deaths_daily$time), max(Montgomery_deaths_daily$time), by = "months")
ticks.lab <- format(ticks.at, format = "%b")
Axis(Montgomery_deaths_daily$time, at = ticks.at, side = 1, labels = ticks.lab, las = 2)
