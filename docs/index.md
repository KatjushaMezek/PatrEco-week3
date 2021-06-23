---
title: "PaTrEco"
author: "Katjusa Mezek"
date: "5/9/2021"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Patterns and trends in environmental data - Exercise 3

## Packages & data import

```{r echo=T, results='hide',warning = FALSE, message = FALSE}
library(readr)        # to import tabular data (e.g. csv)
library(dplyr)        # to manipulate (tabular) data
library(ggplot2)      # to visualize data
library(sf)           # to handle spatial vector data
library(terra)        # To handle raster data
library(lubridate)    # To handle dates and times
library(SimilarityMeasures)
library(purrr)
library(tidyr)
library(ggpubr)

#caro dataset
caro <- read_delim("caro60.csv",",") 
```

# Task 1 - Segmentation
The sampling interval for this dataset is 1 minute. We will use a temporal window (v) of 6 minutes. The following positions were determined:

pos[n-3] to pos[n]
pos[n-2] to pos[n]
pos[n-1] to pos[n]
pos[n] to pos[n+1]
pos[n] to pos[n+2]
pos[n] to pos[n+3]

```{r echo=T, results='hide',warning = FALSE, message = FALSE}
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

```

# Task 2 - Specify and apply threshold d

```{r echo=T, results='hide',warning = FALSE, message = FALSE}
# calculate the mean distance for each row
caro <- caro %>%
  rowwise() %>%
  mutate(
    stepMean = mean(c(nMinus3, nMinus2, nMinus1, nPlus1, nPlus2, nPlus3))
  ) %>%
  ungroup()

# look at stepMean 
summary(caro)

# specify stops and moves
caro <- caro %>% 
  ungroup() %>%
  mutate(static = stepMean < mean(stepMean, na.rm = TRUE))
```

# Task 3 - Visualize segmented trajectories

```{r echo=T, results='hide',warning = FALSE, message = FALSE}
caro %>% 
  ggplot(aes(y=N, x=E) ) +
  geom_path()+ geom_point(aes(colour=static)) + coord_equal() 

```

# Task 4 - Segment-based analysis

```{r echo=T, results='hide',warning = FALSE, message = FALSE}
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
    segm_duration = as.integer(difftime(max(DatetimeUTC),min(DatetimeUTC), units="mins"))
    ) %>% 
    filter(segm_duration > 5)  

caro  %>%
  filter(static == FALSE) %>%
  ggplot(aes(y=N, x=E,col=segment_id)) +
  geom_path()+ geom_point() + coord_equal()

```

# Task 5 - Similarity measures

```{r echo=T, results='hide',warning = FALSE, message = FALSE}
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

# RESULTS: TO GET THE PLOT FROM TASK 5:
ggplot(pedestrian, aes(E,N)) +                         geom_point(data = dplyr::select(pedestrian,       -TrajID),alpha = 0.1) +   #add dataset without trajID
  geom_point(aes(color = as.factor(TrajID)), size = 2) +     geom_path(aes(color = as.factor(TrajID))) +             facet_wrap(~TrajID,labeller = label_both) +         
  coord_equal() +                                       
  theme_minimal() +
  labs(title = "Visual comparison of the 6 trajectories", subtitle = "Each subplot highlights a trajectory") +
  theme(legend.position = "none")  

```

# Task 6 - Calculate similarity

```{r echo=T, results='hide',warning = FALSE, message = FALSE}
# preprocessing
pedestrians_matrix <- pedestrian %>%
  dplyr::select(E, N) %>%             
  split(pedestrian$TrajID) %>%   
  map(as.matrix) # applying universally the as.matrix conversion function using the purrr:map() function

## DTW
# function
dtw_fun= function(y) {
  dtw = DTW(pedestrians_matrix[[1]],y)
  return(dtw)
}

# apply function
dtw_all = pedestrians_matrix %>%
  map(dtw_fun) %>%
  data.frame() %>%
  pivot_longer(cols=1:6,names_to = "trajectory",values_to = "Value") %>%
  data.frame() 

dtw_all$trajectory=as.factor(dtw_all$trajectory)

# plot DTW
dtw_plot=ggplot(dtw_all, aes(x=trajectory,y=Value, fill=trajectory)) + geom_bar(stat="identity")
dtw_plot

## DEditDist
# function
editdist_fun= function(y) {
  Editdist = EditDist(pedestrians_matrix[[1]],y)
  return(Editdist)
}

#apply function
editdist_all = pedestrians_matrix %>%
  map(editdist_fun) %>%
  data.frame() %>%
  pivot_longer(cols=1:6,names_to = "trajectory",values_to = "Value") %>%
  data.frame() 

editdist_all$trajectory=as.factor(editdist_all$trajectory)

# plot editdist
editdist_plot=ggplot(editdist_all, aes(x=trajectory,y=Value, fill=trajectory)) + geom_bar(stat="identity")
editdist_plot

## Frechet
# function
frechet_fun= function(y) {
  frechet = Frechet(pedestrians_matrix[[1]],y)
  return(frechet)
}

# apply function
frechet_all = pedestrians_matrix %>%
  map(frechet_fun) %>%
  data.frame() %>%
  pivot_longer(cols=1:6,names_to = "trajectory",values_to = "Value") %>%
  data.frame() 

frechet_all$trajectory=as.factor(frechet_all$trajectory)

# plot ferchet
frechet_plot=ggplot(frechet_all, aes(x=trajectory,y=Value, fill=trajectory)) + geom_bar(stat="identity")
frechet_plot

## LCSS
# function
lcss_fun= function(y) {
  lcss = LCSS(pedestrians_matrix[[1]],y, pointSpacing=0.5, pointDistance=10,
  errorMarg=2, returnTrans=FALSE)
  return(lcss)
}

# apply function
lcss_all = pedestrians_matrix %>%
  map(lcss_fun) %>%
  data.frame() %>%
  pivot_longer(cols=1:6,names_to = "trajectory",values_to = "Value") %>%
  data.frame() 

lcss_all$trajectory=as.factor(lcss_all$trajectory)

#plot LCSS
lcss_plot=ggplot(lcss_all, aes(x=trajectory,y=Value, fill=trajectory)) + geom_bar(stat="identity")
lcss_plot

#make plot with all similarity measures
final_plot=ggarrange(dtw_plot, editdist_plot, frechet_plot,lcss_plot, ncol=2, nrow=2, common.legend=TRUE, legend = "right")
annotate_figure(final_plot,
                top="Computed similarities using different measures between trajectory 1 and all other trajectories")

# CORRECT BUT ALSO:
# task 6 ########################################################################

library(SimilarityMeasures)  # for the similarity measure functions

# all functions compare two trajectories (traj1 and traj2). Each trajectory
# must be an numeric matrix of n dimensions. Since our dataset is spatiotemporal
# we need to turn our Datetime column from POSIXct to integer:

pedestrians <- pedestrian %>%
  mutate(Datetime_int = as.integer(DatetimeUTC))


# Next, we make an object for each trajectory only containing the
# coordinates in the three-dimensional space and turn it into a matrix

traj1 <- pedestrians %>%
  filter(TrajID == 1) %>%
  dplyr::select(E, N, Datetime_int) %>%
  as.matrix()


# But instead of repeating these lines 6 times, we turn them into a function.
# (this is still more repetition than necessary, use the purr::map if you know 
# how!)

df_to_traj <- function(df, traj){
  df %>%
    filter(TrajID == traj) %>%
    dplyr::select(E, N, Datetime_int) %>%
    as.matrix()
}

traj2 <- df_to_traj(pedestrians, 2)
traj3 <- df_to_traj(pedestrians, 3)
traj4 <- df_to_traj(pedestrians, 4)
traj5 <- df_to_traj(pedestrians, 5)
traj6 <- df_to_traj(pedestrians, 6)



# Then we can start comparing trajectories with each other

dtw_1_2 <- DTW(traj1, traj2)
dtw_1_3 <- DTW(traj1, traj3)

# ... and so on. Since this also leads to much code repetition, we will 
# demostrate a diffferent approach:

# Instead of creating 6 objects, we can also create a single list containing 6
# elements by using "split" and "purrr::map"

library(purrr)


pedestrians_list <- map(1:6, function(x){
  df_to_traj(pedestrians,x)
})


comparison_df <- map_dfr(2:6, function(x){
  tibble(
    trajID = x,
    DTW = DTW(pedestrians_list[[1]], pedestrians_list[[x]]),
    EditDist = EditDist(pedestrians_list[[1]], pedestrians_list[[x]]),
    Frechet = Frechet(pedestrians_list[[1]], pedestrians_list[[x]]),
    LCSS = LCSS(pedestrians_list[[1]], pedestrians_list[[x]],5,4,4)
  )
})


library(tidyr) # for pivot_longer

comparison_df %>%
  pivot_longer(-trajID) %>%
  ggplot(aes(trajID,value, fill = as.factor(trajID)))+ 
  geom_bar(stat = "identity") +
  facet_wrap(~name,scales = "free") +
  theme(legend.position = "none") +
  labs(x = "Comparison trajectory", y = "Value", title = "Computed similarities using different measures \nbetween trajectory 1 to all other trajectories ")


```
