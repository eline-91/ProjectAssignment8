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
plot(alldata)
# Extract all data to a dataframe
df <- as.data.frame(getValues(alldata))

# Make plots for every band against the VCF layer
for(i in 1:6){
	pairs(alldata[[c(i,7)]], maxpixels=10000)
}

# Make the linear model with interactions and without interactions
model <- lm(VCF ~ band1 + band2 + band3 + band4 + band5, data = df)
summary(model)

# Predict the map
vcfMap <- predict(alldata[[1:5]], model = model)

# Change negative values to 0 because VCF can just be between 0 and 100
vcfMap[vcfMap < 0] <- 0
plot(vcfMap)

# Calculate the RMSE 
RMSE <- sqrt(mean((alldata$VCF-vcfMap)^2))
hist(RMSE)

# Load training zones
load(file.path(inFolder,'trainingPoly.rda'))

# Give the classes a code and make a raster of it
trainingPoly@data$Code <- as.numeric(trainingPoly@data$Class)
trainingPoly@data
classes <- rasterize(trainingPoly, RMSE, field='Code')

# Perform zonal statistics to calculate mean RMSE of each zone
stat_mat <- zonal(RMSE, classes, fun = 'mean', na.rm=T)
stat_df <- as.data.frame(stat_mat)
worst_zone <- stat_df[which(stat_df$mean == max(stat_df$mean)), ]
