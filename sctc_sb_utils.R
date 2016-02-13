library(stringr)
library(ggplot2)
library(gridExtra)
library(RCurl)
library(XML)

timestr <- function(seconds) {
  hours <- as.integer(seconds / 3600)
  seconds <- seconds - hours * 3600
  minutes <- as.integer(seconds / 60)
  seconds <- round(seconds - minutes * 60, digits=2)

  minute_prefix <- ifelse(minutes < 10, "0", "")
  minutes <- paste0(minute_prefix, minutes)
  second_prefix <- ifelse(seconds < 10, "0", "")
  seconds <- paste0(second_prefix, seconds)

  time <- paste(hours, minutes, seconds, sep=":")
  return(time)
}


to_seconds_recur <- function(x, val) {
  if (length(x) == 0)
    return(val)

  val <- 60 * val + x[1]
  return(to_seconds_recur(x[-1], val))
}

to_seconds <- function(x) {
  return(to_seconds_recur(as.integer(x), 0))
}

# 1k:
#    1 JIM RATLIFF          10 M SANTA CRUZ         3:26 "
# 3k/10k:
# "    1   1/11   CHRISTOPHER RATLIFF  45 M SANTA CRUZ        37:33  6:03 "

clean_line <- function(line, year, type) {
  fields <- c(as.character())
  if (type == "3k/10k") {
    starts <- c(1,  6, 12, 37, 39, 41, 58, 66)
    stops <-  c(5, 15, 35, 38, 40, 56, 64, 70)
  } else {
    starts <- c(1,  7, 28, 31, 33, 51)
    stops <-  c(6, 27, 29, 31, 50, 56)
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

clean_data <- function(data, type) {
  if (type == "3k/10k") {
    the_names <-
      c("Rank", "Age.Rank", "Name", "Age", "Gender", "City", "Time", "Pace")
  } else {
    the_names <- c("Rank", "Name", "Age", "Gender", "City", "Time")
  }
  names(data) <- the_names
  data$Name <- str_trim(data$Name)

  if (type == "3k/10k") {
    data$Age.Rank <- str_trim(data$Age.Rank)
    data$Pace <- str_trim(data$Pace)
  }

  # Remove data with missing age.
  data <- subset(data, !is.na(Age))

  runtime <- 
    sapply(data$Time, FUN=function(x) strsplit(as.character(x), ':'))

  time2 <-
    sapply(runtime, FUN=function(x) to_seconds(unlist(x)))

  data$Time <- time2
  
  # Single white space only, please.
  data$City <- gsub(" +", " ", data$City)
  # Fix a typo: "Aptos,"
  data$City <- gsub(",", "", data$City)

  # Keep a copy of the city as a character array.
  data$Origin <- data$City

  data$City <- factor(data$City)
  return(data)
}

parseit <- function(data, year, type) {
  len <- length(data)
  if (type == "3k/10k") {
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
  } else {
    the_names <- 
      c("Rank", "Name", "Age", "Gender", "City", "Time")
    newdata <- data.frame(stringsAsFactors=FALSE,
      rep(1, len),
      rep("X", len),
      rep(1, len),
      rep("X", len),
      rep("X", len),
      rep("X", len)
    )
  }

  names(newdata) <- the_names

  for (i in 1:len) {
    line <- data[i]
    newrow <- clean_line(line, year, type)
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
  data3k <- read_raw(data3k)
  data3k <- discard_header(data3k)
  data3k <- parseit(data3k, year, "3k/10k")
  data3k <- discard_blanks(data3k)
  data3k <- clean_data(data3k, "3k/10k")
  
  data10k <- xmlValue(pres[[3]])
  data10k <- read_raw(data10k)
  data10k <- discard_header(data10k)
  data10k <- discard_blanks(data10k)
  data10k <- parseit(data10k, year, "3k/10k")
  data10k <- clean_data(data10k, "3k/10k")

  data1k <- xmlValue(pres[[6]])
  data1k <- read_raw(data1k)
  # discard up to and including the header line ("====...")
  data1k <- discard_header(data1k)
  # ... and discard any remaining blank lines.
  data1k <- discard_blanks(data1k)
  data1k <- parseit(data1k, year, "1k")
  data1k <- clean_data(data1k, "1k")

  # data1k fields are a subset; fill in the missing fields
  data1k$Age.Rank <- paste0(data1k$Rank, "/", nrow(data1k))
  seconds_per_mile <- round(data1k$Time / 0.6214, 0)
  minutes <- as.integer(seconds_per_mile / 60)
  seconds <- seconds_per_mile %% 60
  prefix <- ifelse(seconds < 10, "0", "")
  data1k$Pace <- paste0(minutes, ":", prefix, seconds)

  return(list(data1k, data3k, data10k))
}

get_wide_data <- function(data1k, data3k, data10k) {

}

