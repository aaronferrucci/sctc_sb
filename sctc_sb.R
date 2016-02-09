source("sctc_sb_utils.R")
year <- 2016
the_data <- get_data_from_url(year)
data3k <- the_data[[1]]
data10k <- the_data[[2]]

y_values_3k <- seq(0, max(data3k$Time), 600)

max_age <- max(data3k$Age, data10k$Age)

plot3k <-
  ggplot(data3k, aes(x = Age, y = Time, color = Gender)) +
    scale_y_continuous(breaks = y_values_3k, labels = timestr(y_values_3k)) +
    scale_x_continuous(breaks = seq(0, max_age, 10)) +
    ggtitle("3k") +
    geom_point() +
    stat_smooth(formula = y ~ x)
  
y_values_10k <- seq(0, max(data10k$Time), 600)

plot10k <- ggplot(data10k, aes(x = Age, y = Time, color = Gender)) + 
  scale_y_continuous(breaks = y_values_10k, labels = timestr(y_values_10k)) +
  scale_x_continuous(breaks = seq(0, max_age, 10)) +
  ggtitle("10k") +
  geom_point() +
  stat_smooth(formula = y ~ x)


grid.arrange(plot3k, plot10k, nrow=2, top=paste(year, "SCTC Super Bowl 3k/10k"))
