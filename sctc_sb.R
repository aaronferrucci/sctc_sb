source("sctc_sb_utils.R")

get_plot <- function(data) {
  xsteps <- 10
  ysteps <- 600
  if (max(data$Age) < 20) {
    xsteps <- 1
    ysteps <- 120
  }
  y_values <- seq(0, max(data$Time), ysteps)

  plot <- ggplot(data, aes(x = Age, y = Time, color = Gender)) +
    scale_y_continuous(breaks = y_values, labels = timestr(y_values)) +
    scale_x_continuous(breaks = seq(0, max(data$Age), xsteps)) +
    geom_point() +
    stat_smooth(formula = y ~ x, se=FALSE)

  return(plot)
}

year <- 2016
the_data <- get_data_from_url(year)
data1k <- the_data[[1]]
data3k <- the_data[[2]]
data10k <- the_data[[3]]
alldata <- the_data[[4]]
max_age <- max(alldata$Age)

get_age_histogram <- function(data, binwidth) {
  plot <- ggplot(data, aes(x = Age, fill = Gender), xlab="Age") +
    geom_histogram(binwidth=binwidth) +
    scale_x_continuous(breaks=seq(0, max(data$Age), binwidth)) +
    geom_vline(xintercept = mean(data$Age))

  return(plot)
}

get_city_histogram <- function(data) {
  data2 <- data
  tab <- table(data2$City)
  city_levels <- names(tab)[order(tab, decreasing = TRUE)]
  data2$City <- factor(data$City, levels = city_levels)

  data2$Race <- factor(data$Race, levels = c("1k", "3k", "10k"))

  plot <- ggplot(data2, aes(x = City, fill = Race), xlab="City") +
    geom_bar() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle("Cities")

  return(plot)
}


