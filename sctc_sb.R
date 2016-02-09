source("sctc_sb_utils.R")
year <- 2016
the_data <- get_data_from_url(year)
data3k <- the_data[[1]]
data10k <- the_data[[2]]

plot3k <-
  qplot(Age, Time, data=data3k, main="3k", ylim=c(0, NA), col=Gender) +
    geom_point() +
    geom_smooth()
  
  return(plot3k)


plot10k <- ggplot(data10k, aes(x = Age, y = Time, color = Gender)) + 
  ggtitle("10k") +
  geom_point() +
  stat_smooth(formula = y ~ x) +
  xlim(min(data10k$Age) * 0.9, max(data10k$Age) * 1.1) +
  ylim(0, NA)


grid.arrange(plot3k, plot10k, nrow=2, top=paste(year, "SCTC Super Bowl 3k/10k"))
