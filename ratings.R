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

data$Decade <- (data$Year %/% 10) *10
data$Old.decades <- NA
data$Old.decades[data$Decade < 2000] <- data$Decade[data$Decade < 2000]
data$Old.decades[data$Decade >= 2000] <- data$Year[data$Decade >= 2000]


## Data analysis ##

movies <- data[data$Title.type=="Feature Film",]
tv <- data[data$Title.type %in% c("TV Series","Mini-Series"),]
nMovies <- nrow(movies)
nTv <- nrow(tv)

yearRating <- aggregate(movies[,c("You.rated", "IMDb.Rating")], by = list(Old.decades = movies$Old.decades), FUN = "mean")
yearRating$Number <- aggregate(movies$Old.decades, by = list(Year = movies$Old.decades), FUN = "length")$x
yearRating$Decade <- (yearRating$Old.decades %/% 10) *10
yearRating$Decade <- as.factor(yearRating$Decade)
yearRating$Old.decades[yearRating$Old.decades < 2000] <- paste(yearRating$Old.decades[yearRating$Old.decades < 2000], "s", sep = "")

## Charts ##

# Chart 1: Ratings bubble chart

svg('ratings bubble.svg', width = 7.8, height = 8.3)

p1 <- ggplot(data = yearRating, mapping = aes(x = IMDb.Rating, y = You.rated)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
  geom_point(data = yearRating, mapping = aes(size = Number, colour = Decade), alpha = 0.5) +
  geom_text(data = yearRating, mapping = aes(label = Old.decades), size = 4, family = "Segoe UI Light", color = "#434A54") +
  annotate("text", label = "In agreement", angle = 45, x = 8, y = 8.1, size = 5, family = "Segoe UI Light", color = "#434A54") +
  scale_x_continuous(limits = c(6.5,8.5)) +
  scale_y_continuous(limits = c(6.5,8.5)) +
  scale_size_area(max_size = 10) +
  scale_colour_manual(values = c("#4FC1E9", "#FFCE54", "#48CFAD", "#EC87C0", "#AC92EC", "#FC6E51", "#A0D468", "#ED5565","#5D9CEC")) +
  xlab("IMDb average rating") +
  ylab("User average rating") +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.background = element_blank(),
        plot.margin = margin(10,10,10,20),
        legend.position = "bottom",
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 10, margin = margin(t =-10)),
        axis.title.x = element_text(size = 10, margin = margin(10,0,0,0)),
        axis.title.y = element_text(size = 10, margin = margin(0,10,0,0)),
        text = element_text(family = "Segoe UI Light", color = "#434A54"),
        aspect.ratio = 1) +
  ggtitle("")

gt <- ggplot_gtable(ggplot_build(p1))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)

dev.off()