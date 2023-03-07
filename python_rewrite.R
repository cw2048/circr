###Code plan###

#Steps:
# 1. Upload hop data into a usable format
# 2. Create Figures
# 3. Run stats

######
# 1. Upload data
######

#Set working Directory
setwd("~/GitHub/circr/")


#Load data file
DATA <- "hop_data"

#Check directory
list.files(DATA, pattern= "*.txt|*.csv")

#Set working directory
setwd(DATA)


#edited Val's pythong notebook to correctly add the hours. Now need to update running code and convert to R. 

#Load Libraries
library(dplyr)


####
#july6=pd.read_csv('pilot_samples/July 06, 2022.txt', header=None, sep='\t')
#print(july6.head())

#count number of rows
#print(len(july6.index))
####


july6<- read.delim("~/GitHub/circr/hop_data/July 06, 2021.txt", header=FALSE)

length(july6)
#61 columns, last one is NAs

#select all but last column
df <- select(july6, (V1:V60))



####Bird IDs



#Separeate cages
cage_1 = df[1:24,]
cage_2 = df[25:48,]
cage_3 = df[49:72,]
cage_4 = df[73:96,]



#have the ability to bin into minutes. 
#cage_1 long, into birds, hour, minute, hops
#cage_1 = every row is an hour and every column is a minute. 
#turn rows into lists and add into a df with bird minute, hour, and date

#traspose
tc_1 <- t(cage_1)
#rownames(tc_1) <- colnames(cage_1)
#colnames(tc_1) <- rownames(cage_1)

##############


#install.packages("devtools")
#devtools::install_github("rcorty/actogrammr")

library(actogrammr)

f <- file.path("~/GitHub/circr/hop_data/")
d <- read_clock_file(file_n = list.files(path = f, full.names = TRUE)[1])


b <- bin_data(data = d, minutes_per_bin = 6)

plot_actogram(data = b, start_date = '2010-01-01')


###############


#Read in file data

data <- july6


#################

#Val's code



files <- list.files(path = "~/GitHub/circr/hop_data/")
l <- list()
for (k in 1:length(files)) {
  
  temp <- read_delim(files[k], "\t", escape_double = FALSE, trim_ws = TRUE, col_names = FALSE, show_col_types = FALSE)
  temp <- Filter(function(x)!all(is.na(x)), temp)   #removes NA column 61
  temp <- na.omit(temp)   #removes empty rows between birds
  colnames(temp) <- seq(1,60,1)   # numbers columns as minutes  1-60
  temp$Cage <- rep(1:24, each=24)    #names cages 1-24
  temp$Hour <- rep(seq(0,23,1), 24)   #assigns hours 0-23
  
  temp <- temp %>% gather(Minutes, Hops, 1:60)
  temp$Minutes <- as.numeric(temp$Minutes)
  

  #Assign day/night to the correct hours
  temp$Phase <- NA

  for (i in 1:nrow(temp)) {
    if (temp$Hour[i] < 9 | temp$Hour[i] >= 21) {
      temp$Phase[i] <- "night"
    }

    if (temp$Hour[i] >= 9 & temp$Hour[i] < 21) {
      temp$Phase[i] <- "day"
    }


  }
}




###################




f <- file.path("~/GitHub/circr/hop_data/")
dat <- read_clock_file(file_name = list.files(path = f, full.names = TRUE)[1])


bn <- bin_dat(data = dat, minutes_per_bin = 5)

plot_actogram(data = bn, start_date = '2010-01-01')

