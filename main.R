# Geetika Rathee & Eline van Elburg
# January 13, 2016
# Assignment 8

# Set the proper input and output folder
inFolder <- '/home/user/Projects/AssignmentLesson8/data'
outFolder <- '/home/user/Projects/AssignmentLesson8/output'

# Import necessary libraries
library(raster)

# Load all the data, put it into a brick and give them proper names
gewata_list <- list.files(inFolder, pattern = glob2rx('*Gewata*.rda'), full.names = TRUE)

for(i in 1:length(gewata_list)){
	load(gewata_list[i])
}

# Cut the clouds and water from the VCF file
vcfGewata[vcfGewata > 100] <- NA
alldata <- brick(GewataB1, GewataB2, GewataB3, GewataB4, GewataB5, GewataB7, vcfGewata)
names(alldata) <- c("band1", "band2", "band3", "band4", "band5", "band7", "VCF")

# Extract all data to a dataframe
df <- as.data.frame(getValues(alldata))

# Plot
for(i in 1:6){
	pairs(alldata[[c(i,7)]], maxpixels=10000)
}

