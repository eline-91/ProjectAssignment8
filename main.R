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
squared_differences <- (alldata$VCF-vcfMap)^2
plot(squared_differences)
mean_squared = cellStats(squared_differences, stat = 'mean')
RMSE = sqrt(mean_squared)
print(paste("The RMSE is:", RMSE))

# Load training zones
load(file.path(inFolder,'trainingPoly.rda'))

# Give the classes a code and make a raster of it
trainingPoly@data$Code <- as.numeric(trainingPoly@data$Class)
trainingPoly@data
classes <- rasterize(trainingPoly, squared_differences, field='Code')

# Perform zonal statistics to calculate RMSE of each zone
mat_mean <- zonal(squared_differences, classes, fun = 'mean', na.rm=T)
sqrt_vector <- sqrt(mat_mean[,2])
stat_df <- as.data.frame(mat_mean)
stat_df$RMSE <- sqrt_vector
stat_df$class <- c("cropland","forest","wetland")

# This is a data frame of the RMSE values per class
stat_df
worst_zone <- stat_df[which(stat_df$RMSE == max(stat_df$RMSE)), ]
worst_zone
