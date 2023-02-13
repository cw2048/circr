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
rownames(tc_1) <- colnames(cage_1)
colnames(tc_1) <- rownames(cage_1)

barplot(tc_1)