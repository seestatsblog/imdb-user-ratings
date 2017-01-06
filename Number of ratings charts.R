## Import libraries ##
library(zoo)
library(plyr)
library(dplyr)
library(lubridate)
library(reshape2)
library(ggplot2)
library(grid)
library(extrafont)
loadfonts()

## Upload and process data ##
data <- read.csv("ratings.csv", row.names=NULL)
genreList <- unlist(strsplit(paste(data$Genres, sep=", "), ", "))
genres <- genreList[!duplicated(genreList)]
for(i in 1:nrow(data)) {
  for(j in 1:length(genres))
    data[i, genres[j]]<-length(grep(genres[j], data$Genres[i]))
}

data$Month.rated <- as.Date(paste(substr(data$created,5,7),
                                  substr(data$created,21,24),1), format = "%b %Y %d")
data$Year.rated <- as.numeric(format(data$Month.rated, "%Y"))
data$Day.rated <- paste(substr(data$created,1,3))
data$Day.rated[data$Day.rated == "Mon"] <- 1
data$Day.rated[data$Day.rated == "Tue"] <- 2
data$Day.rated[data$Day.rated == "Wed"] <- 3
data$Day.rated[data$Day.rated == "Thu"] <- 4
data$Day.rated[data$Day.rated == "Fri"] <- 5
data$Day.rated[data$Day.rated == "Sat"] <- 6
data$Day.rated[data$Day.rated == "Sun"] <- 7

## Data analysis ##

movies <- data[data$Title.type=="Feature Film",]
tv <- data[data$Title.type %in% c("TV Series","Mini-Series"),]
nMovies <- nrow(movies)
nTv <- nrow(tv)

yearTot <- aggregate(movies$Year, by = list(Year = movies$Year), FUN = "length")
yearTot$x<-as.numeric(yearTot$x)

rateTot <- aggregate(movies$Month.rated, by = list(Month.rated = movies$Month.rated), FUN = "length")
rateTot$x <- as.numeric(rateTot$x)
rateTot$Year.rated <- format(rateTot$Month.rated, "%Y")
rateTot$Month <- month(rateTot$Month.rated)

dayTot <- aggregate(movies$Day.rated[movies$Year.rated >= 2014],
                    by = list(Day.rated = movies$Day.rated[movies$Year.rated >= 2014]), FUN = "length")
dayTot$Day.name <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
dayTot$Max.day <- rep(0, dim(dayTot)[1])
dayTot$Max.day[dayTot$x == max(dayTot$x)] <- 1

## Charts ##

# Chart 1: Number of ratings by month (bar chart)

svg('ratings by year.svg', width = 7.8, height = 5.48)

p1 <- ggplot(data = rateTot, mapping = aes(x = Month.rated, y = x, fill = Year.rated)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#4FC1E9", "#FFCE54", "#48CFAD", "#EC87C0", "#AC92EC", "#FC6E51", "#A0D468", "#ED5565","#5D9CEC")) +
  scale_x_date(breaks = c(as.Date("2012-08-01", format = "%Y-%m-%d"),
                          as.Date("2013-07-01", format = "%Y-%m-%d"),
                          as.Date("2014-07-01", format = "%Y-%m-%d"),
                          as.Date("2015-07-01", format = "%Y-%m-%d"),
                          as.Date("2016-07-01", format = "%Y-%m-%d")), 
               date_labels = "%Y") +
  scale_y_continuous() +
  xlab("") +
  ylab("Number of films rated each month") +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.background = element_blank(),
        plot.margin = margin(10,10,10,20),
        legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 14, margin = margin(t =-10)),
        axis.title.y = element_text(size = 10, margin = margin(0,10,0,0)),
        text = element_text(family = "Segoe UI Light", color = "#434A54")) +
  ggtitle(paste(nMovies, "films rated and counting..."))

gt <- ggplot_gtable(ggplot_build(p1))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)

dev.off()

# Chart 2: Cumulative number of ratings by year (area chart)

chartYearData <- melt(rateTot[rateTot$Year.rated >= 2014,c(2:4)], id.vars = c('Month', 'Year.rated'))
chartYearData <- ddply(chartYearData, .(Year.rated), transform, product = cumsum(value))

svg('cumulative ratings by year.svg', width = 7.8, height = 5.48)

p2 <- ggplot(data = chartYearData, mapping = aes(x = Month, y = product, fill = Year.rated)) +
  geom_line(size = 1, aes(colour = Year.rated)) +
  geom_area(data = chartYearData[chartYearData$Year.rated == 2016,], mapping = aes(x = Month, y = product, fill = Year.rated), alpha = 0.5, position = "identity") +
  geom_text(data = chartYearData[chartYearData$Month == 12,], aes(x =  Month, y = product, label = paste(Year.rated, " total: ", product, sep = "")),
            hjust = -0.1,
            family = "Segoe UI Light",
            color = "#434A54") +
  scale_colour_manual(values = c("#48CFAD", "#EC87C0", "#AC92EC")) +
  scale_fill_manual(values = c("#48CFAD", "#EC87C0", "#AC92EC")) +
  scale_x_continuous(breaks = seq(1,12), labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  scale_y_continuous() +
  xlab("") +
  ylab("Cumulative number of film ratings") +
  ggtitle("Cumulative number of films rated by month in 2014, 2015 and 2016") +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.background = element_blank(),
        plot.margin = margin(30,20,10,20),
        legend.title = element_blank(),
        legend.key = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 12, margin = margin(t =-10)),
        axis.title.y = element_text(size = 10, margin = margin(0,15,0,0)),
        text = element_text(family = "Segoe UI Light", color = "#434A54"))

gt <- ggplot_gtable(ggplot_build(p2))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)

dev.off()

# Chart 3: Number of ratings by day (lollipop chart)

svg('ratings by day.svg', width = 7.8, height = 5.48)

p3 <- ggplot(data = dayTot, mapping = aes(x = Day.rated, y = x, colour = factor(Max.day))) +
  geom_point(size = 22, aes(fill = factor(Max.day))) +
  geom_segment(data = dayTot[dayTot$Max.day == 0,], aes(xend = Day.rated, yend = 0),
               size = 1, linetype = 1, colour = "#CCD1D9") +
  geom_segment(data = dayTot[dayTot$Max.day == 1,], aes(xend = Day.rated, yend = 0),
               size = 1, linetype = 1, colour = "#A0D468") +
  geom_text(data = dayTot, aes(x =  Day.rated, y = x, label = paste(Day.name, x, sep = "\n")),
            size = 4, color = "#FFFFFF", family = "Segoe UI Light") +
  scale_colour_manual(values = c("#CCD1D9","#A0D468")) +
  scale_y_continuous(limits = c(0, 60)) +
  xlab("") +
  ylab("Total number of films rated") +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.background = element_blank(),
        plot.margin = margin(20,10,-10,20),
        plot.title = element_text(margin = margin(0,0,10,0)),
        legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(colour = "#656D78"),
        axis.title.y = element_text(size = 10, color = "#656D78", margin = margin(0,10,0,0)),
        text = element_text(family = "Segoe UI Light", color = "#656D78")) +
  ggtitle("Number of films rated by day from 2014 onwards")

gt <- ggplot_gtable(ggplot_build(p3))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)

dev.off()
