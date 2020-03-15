library(data.table)
library(lattice)
library(ggplot2)

x <- list.files("COVID-19/csse_covid_19_data/csse_covid_19_daily_reports/", pattern = ".csv", full.names = TRUE)
x <- data.frame(file = x, date = substr(basename(x), 1, 10), stringsAsFactors = FALSE)
x <- split(x$file, x$date)
x <- lapply(x, fread)
x <- rbindlist(x, fill = TRUE, idcol = "date")
x$date <- as.Date(x$date, format = "%m-%d-%Y")
x <- setnames(x, 
              old = c("date", "Country/Region", "Province/State", "Confirmed", "Deaths", "Recovered"),
              new = c("date", "region", "subregion", "confirmed", "death", "recovered"))
x <- subset(x, subregion %in% "Hubei" | 
              region %in% c("Belgium", "France", "Netherlands", "Spain", "Singapore", "Germany", "Switzerland", "Italy"))
x$area <- ifelse(x$subregion %in% "Hubei", x$subregion, x$region)
x <- x[!duplicated(x, by = c("date", "area")), ]
x <- x[, c("date", "area", "confirmed", "death", "recovered")]
subset(x, area %in% "Belgium" & confirmed > 1)

x <- x[order(x$date, x$area, decreasing = TRUE), ]
x <- x[, days_since_case_onset := as.integer(date - min(date[confirmed > 75])), by = list(area)]
x <- x[, newly_confirmed := as.integer(confirmed - shift(confirmed, n = 1, type = "lead")), by = list(area)]
onset <- subset(x, days_since_case_onset == 0, select = c("date", "area", "confirmed"))
onset[order(onset$date), ]

data <- subset(x, days_since_case_onset >= 0 & days_since_case_onset < 30 &
                       area %in% c("Germany", "Hubei", "France", "Belgium", "Singapore", "Netherlands", "Italy"))

p <- ggplot(data, aes(days_since_case_onset, log(confirmed), color=area)) + geom_point() + geom_line()

#p <- xyplot(log(confirmed) ~ days_since_case_onset | "Log(confirmed cases) of Corona since onset of sick person nr 75", 
#       groups = area,
#       data = subset(x, days_since_case_onset >= 0 & 
#                       area %in% c("Germany", "Hubei", "France", "Belgium", "Singapore", "Netherlands", "Italy")), 
#       xlab = "Days since Corona onset (confirmed case 75)", ylab = "Log of number of confirmed cases",
#       auto.key = list(space = "right", lines = TRUE),
#       type = "b", pch = 20, lwd = 2) 

#p <- xyplot(confirmed ~ days_since_case_onset | "Log(confirmed cases) of Corona since onset of sick person nr 75", 
#            groups = area,
#            data = subset(x, days_since_case_onset >= 0 & 
#                                  area %in% c("Germany", "Hubei", "France", "Belgium", "Singapore", "Netherlands", "Italy")), 
#            xlab = "Days since Corona onset (confirmed case 75)", ylab = "Log of number of confirmed cases",
#            auto.key = list(space = "right", lines = TRUE),
#            type = "b", pch = 20, lwd = 2) 

print(p)
