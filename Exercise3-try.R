###TEST E3###
#packages
library(readr)        # to import tabular data (e.g. csv)
library(dplyr)        # to manipulate (tabular) data
library(ggplot2)      # to visualize data
library(sf)           # to handle spatial vector data
library(terra)        # To handle raster data
library(lubridate)    # To handle dates and times
library(SimilarityMeasures)

#TASK 1: SEGMENTATION
caro <- read_delim("caro60.csv",",") 

# Specify a temporal window
# The sampling interval for this dataset is 1 minute. We will use a temporal window (v) of 6 minutes
# positions:
# pos[n-3] to pos[n]
# pos[n-2] to pos[n]
# pos[n-1] to pos[n]
# pos[n] to pos[n+1]
# pos[n] to pos[n+2]
# pos[n] to pos[n+3]

#calculte euclidean distances between points within the temporal window (v)

caro <- caro %>%
  mutate(
    nMinus3 = sqrt((lag(E,3)-E)^2+(lag(N,3)-N)^2),  #distance to pos -3
    nMinus2 = sqrt((lag(E,2)-E)^2+(lag(N,2)-N)^2),  #distance to pos -2
    nMinus1 = sqrt((lag(E,1)-E)^2+(lag(N,1)-N)^2),  #distance to pos -1
    nPlus1 = sqrt((E-lead(E,1))^2+(N-lead(N,1))^2), #distance to pos +1
    nPlus2 = sqrt((E-lead(E,2))^2+(N-lead(N,2))^2), #distance to pos +2
    nPlus3 = sqrt((E-lead(E,3))^2+(N-lead(N,3))^2)  #distance to pos +3
  )

#TASK 2: Specify and apply threshold d
# calculate the mean distance for each row
caro <- caro %>%
  rowwise() %>%
  mutate(
    stepMean = mean(c(nMinus3, nMinus2, nMinus1, nPlus1, nPlus2, nPlus3))
  ) %>%
  ungroup()

# look at stepMean 
summary(caro)
# PLOT!!!! (like the one last time)




# specify stops and moves
caro <- caro %>% 
  ungroup() %>%
  mutate(static = stepMean < mean(stepMean, na.rm = TRUE))



#TASK 3: Visualize segmented trajectories
caro %>% 
  ggplot(aes(y=N, x=E) ) +
  geom_path()+ geom_point(aes(colour=static)) + coord_equal() 

caro_filter= caro  %>%
  flter(!static)

#TASK 4: Segment based analysis
rle_id <- function(vec){
  x <- rle(vec)$lengths
  as.factor(rep(seq_along(x), times=x))
}

caro <- caro %>%
  mutate(segment_id = rle_id(static))

#plot
caro %>%
  group_by(segment_id) %>%
  ggplot(aes(y=N, x=E,col=segment_id ) ) +
  geom_path()+ geom_point() + coord_equal()

# determine duration of segments and remove segments <5
caro <- caro %>%
  group_by(segment_id) %>%
  mutate(
    segm_duration = as.integer(difftime(max(lead(DatetimeUTC)),min(DatetimeUTC), units="mins"))
  )

#######????#############



# TASK 5 - Similarity measures
pedestrian <- read_delim("pedestrian.csv",",") 
str(pedestrian)
pedestrian$TrajID=as.factor(pedestrian$TrajID)

#plot and explore
pedestrian %>%
  ggplot(aes(y=N, x=E)) +
  geom_path()+ geom_point(aes(col=TrajID)) + 
  coord_equal() + 
  facet_wrap(~TrajID,nrow=2) +
  labs(title="Visual comparison of the 6 trajectories", subtitle="Each subplot highlights a trajectory")


# TASK 6 - calculate similarity 
help(package = "SimilarityMeasures")
euc.dist = function(x,y) (sqrt((lead(x)-x)^2+(lead(y)-y)^2))

pedestrian = pedestrian %>%
  mutate(
    dimension= euc.dist(E,N),
  )

#separate trajectories
T1 = pedestrian %>%
  filter(TrajID == 1) 

T26 = pedestrian %>%
  filter(TrajID %in% (2:6)) 

Trajec1=as.matrix(nrow(T1), c(T1$dimension))
Trajec2=as.matrix(nrow(T26),c(T26$dimension))

#compare 1 to trajectories 2-6
DTW(Trajec1,Trajec2)



