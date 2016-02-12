source("sctc_sb_utils.R")

get_plot <- function(data, max_age) {
  y_values <- seq(0, max(data$Time), 600)
  plot <- ggplot(data, aes(x = Age, y = Time, color = Gender)) +
    scale_y_continuous(breaks = y_values, labels = timestr(y_values)) +
    scale_x_continuous(breaks = seq(0, max_age, 10)) +
    geom_point() +
    stat_smooth(formula = y ~ x)

  return(plot)
}

year <- 2016
the_data <- get_data_from_url(year)
data1k <- the_data[[1]]
data3k <- the_data[[2]]
data10k <- the_data[[3]]
max_age <- max(data3k$Age, data10k$Age)

get_age_histogram <- function(data) {
  plot <- ggplot(data, aes(x = Age, fill = Gender), xlab="Age") +
    geom_histogram(binwidth=5) +
    ggtitle("10k Age histogram") +
    scale_x_continuous(breaks=seq(0, max(data$Age), 10)) +
    geom_vline(xintercept = mean(data$Age))

  return(plot)
}


