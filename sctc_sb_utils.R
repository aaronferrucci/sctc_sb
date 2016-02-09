library(stringr)
library(ggplot2)
library(gridExtra)
library(RCurl)
library(XML)

to_seconds_recur <- function(x, val) {
  if (length(x) == 0)
    return(val)

  val <- 60 * val + x[1]
  return(to_seconds_recur(x[-1], val))
}

to_seconds <- function(x) {
  return(to_seconds_recur(as.integer(x), 0))
}

clean_line <- function(line, year) {
  fields <- c(as.character())
  if (year == 2016) {
    starts <- c(1,  6, 12, 37, 39, 41, 58, 66)
    stops <-  c(5, 15, 35, 38, 40, 56, 64, 70)
  } else {
    starts <- c(1,  6, 16, 43, 46, 47, 64, 72)
    stops <-   c(5, 15, 42, 45, 46, 63, 71, 76)
  } 
  
  for (i in 1:length(starts)) {
    start <- starts[i]
    stop <- stops[i]

    str <- substring(line, start, stop)
    str <- str_trim(str)
    fields <- c(fields, str)
  }

  return(fields)
}

clean_data <- function(data) {
  the_names <-
    c("Rank", "Age.Rank", "Name", "Age", "Gender", "City", "Time", "Pace")
  names(data) <- the_names
  data$Age.Rank <- str_trim(data$Age.Rank)
  data$Name <- str_trim(data$Name)
  data$Pace <- str_trim(data$Pace)

  # Remove data with missing age.
  data <- subset(data, !is.na(Age))

  runtime <- 
    sapply(data$Time, FUN=function(x) strsplit(as.character(x), ':'))

  time2 <-
    sapply(runtime, FUN=function(x) to_seconds(unlist(x)))

  data$Time <- time2
  return(data)
}


parseit <- function(data, year) {
  len <- length(data)
  the_names <- 
    c("Rank", "Age.Rank", "Name", "Age", "Gender", "City", "Time", "Pace")
  newdata <- data.frame(stringsAsFactors=FALSE,
    rep(1, len),
    rep("X", len),
    rep("X", len),
    rep(1, len),
    rep("X", len),
    rep("X", len),
    rep("X", len),
    rep("X", len)
  )

  names(newdata) <- the_names

  for (i in 1:len) {
    line <- data[i]
    newrow <- clean_line(line, year)
    newdata[i, ] <- newrow
  }

  # Hacky - set type on int fields.
  newdata$Age <- as.numeric(newdata$Age)
  newdata$Rank <- as.numeric(newdata$Rank)

  return(newdata)
}

discard_blanks <- function(data) {
  blanks <- grepl("^$", data)

  return(data[!blanks])
}

discard_header <- function(data) {
  index <- 1 # index of the _next_ line to be read
  for (line in data) {
    index <- index + 1
    if (grepl("^=", line)) {
      break
    }
  }

  return(data[index:length(data)])
}

read_raw <- function(text) {
  con <- textConnection(text)
  data <- readLines(con)
  close(con)
  
  return(data)
}

get_data_from_url <- function(year) {
  url <- paste0("http://www.santacruztrackclub.com/", year, "-super-bowl-run.html")
  html <- htmlParse(url)
  pres <- getNodeSet(html, "//pre")
  data3k <- xmlValue(pres[[2]])
  data10k <- xmlValue(pres[[3]])

  data3k <- read_raw(data3k)
  data10k <- read_raw(data10k)

  # discard up to and including the header line ("====...")
  data3k <- discard_header(data3k)
  data10k <- discard_header(data10k)

  # ... and discard any remaining blank lines.
  data3k <- discard_blanks(data3k)
  data10k <- discard_blanks(data10k)

  data3k <- parseit(data3k, year)
  data10k <- parseit(data10k, year)

  data3k <- clean_data(data3k)
  data10k <- clean_data(data10k)

  return(list(data3k, data10k))
}

