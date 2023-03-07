#' @title read_clock_lab_files
#'
#' @param file_names the names of the files to read.
#'   Should be the result of a call to list.files(..., full.names = TRUE)
#'
#' @return a big data.frame
#'
#' @description reads binary files in clocklab format
#'
#' @export
#'
#' @examples
#' f <- file.path(system.file(package = 'actogrammr'), 'testdata')
#' dat <- read_clock_lab_files(file_names = list.files(path = f, full.names = TRUE))

read_clock_files <- function(file_names) {
  
  bird_data <- list()
  for (file_name in file_names) {
    
    bird_data[[file_name]] <- read_clock_file(file_name = file_name)
    
  }
  
  dt <- do.call(what = rbind,
               args = bird_data)
  
  return(dt)
}




#' @title read_clock_lab_file
#'
#' @param file_name the name of the file to read, should represent one mouse.
#'   Should be the result of a call to list.files(..., full.names = TRUE)
#'
#' @return a big data.frame
#'
#' @description reads a binary file in clocklab format
#'
#' @export
#'
#' @examples
#' f <- file.path(system.file(package = 'actogrammr'), 'testdata')
#' dat <- read_clock_lab_file(file_name = list.files(path = f, full.names = TRUE)[1])

read_clock_file <- function(file_name) {
  
  #act <- act_1 <- act_60 <- light <- light_1 <- light_60 <- trash <- 'fake_global_for_CRAN'
  # files <- list.files(path = f)
  # l <- list()
  # for (k in 1:length(files)) {
  # 

    
    data <- read_delim(file_name, "\t", escape_double = FALSE, trim_ws = TRUE, col_names = FALSE, show_col_types = FALSE)
    data <- Filter(function(x)!all(is.na(x)), data)   #removes NA column 61
    data <- na.omit(data)   #removes empty rows between birds
    colnames(data) <- seq(1,60,1)   # numbers columns as minutes  1-60
    data$filename <- file_name
    data$Cage <- as.integer(rep(1:24, each=24))    #names cages 1-24
    data$Hour <- rep(seq(0,23,1), 24)   #assigns hours 0-23
    
    # 
    # data <- read_delim(file_name, "\t", escape_double = FALSE, trim_ws = TRUE, col_names = FALSE, show_col_types = FALSE)
    # data <- Filter(function(x)!all(is.na(x)), data)   #removes NA column 61
    # data <- na.omit(data)   #removes empty rows between birds
    # 
    
    
    
    data <- data %>% gather(Minutes, Hops, 1:60)
    data$Minutes <- as.numeric(data$Minutes)
    
    
    #Assign day/night to the correct hours
    data$Phase <- NA
    
    for (i in 1:nrow(data)) {
      if (data$Hour[i] < 9 | data$Hour[i] >= 21) {
        data$Phase[i] <- "night"
      }
      
      if (data$Hour[i] >= 9 & data$Hour[i] < 21) {
        data$Phase[i] <- "day"
      }
      
      
    }
    
    data$light <- NA
    
    for (i in 1:nrow(data)) {
      if (data$Phase[i] == "night") {
        data$light[i] <- 0
      }
      
      if (data$Phase[i] == "day") {
        data$light[i] <- 60
      }
    }
      
    
    ## re-format date
    name <- paste(file_name) 
    
    #need to change this to be whatever input is
    name <- gsub('C:/Users/Cassa/Documents/GitHub/circr/hop_data/', '', name)
    name <- gsub('\\s', '-', name)
    name <- gsub(',-', '-', name)
    name <- gsub('.txt', '', name)
    data$Date <- name
    data$Date<- as.Date(data$Date, "%B-%d-%Y")
    
    
    
    #Add treatment IDs
    data$treat <- NA
    for (i in 1:nrow(data)){
      
      if (data$Cage[i]== 4 | data$Cage[i] == 6 | data$Cage[i]== 7 | data$Cage[i] == 13 | data$Cage[i] == 16 | data$Cage[i]==18 | data$Cage[i]== 21 | data$Cage[i]== 22 | data$Cage[i]== 23 | data$Cage[i]== 24) {
        data$treat[i] <- "LD"}
      
      if (data$Cage[i]== 1 | data$Cage[i] == 2 | data$Cage[i]== 3 | data$Cage[i] == 8 | data$Cage[i] == 14 | data$Cage[i]== 15 | data$Cage[i]== 20) {
        data$treat[i] <- "LL"}
      
     
      #print(k)    #tracks progress
    }  
  #   l[[k]] <- data
  #   #dat <- data.table::rbindlist(l)    
  # }
  # dat <- data.table::rbindlist(l)
  # #dat <- data
    
   
    
  
  return(data)
}

