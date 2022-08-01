library(tidyverse)
library(dplyr)
library(CMRnet)
library(lubridate)
library(gt)
library(igraph)

# Load in the data
GSB_CMR<- read_csv("1_filtered_detections_all.csv", na="NULL")

#Test the max and min data range, this can be used to define boundaries for the network:
GSB_CMR %>% 
  summarise(min = min(Date),
            max = max(Date))

#Mutate values/variables to proper type to run CMRnet
GSB_CMR <- GSB_CMR %>% 
  dplyr::mutate(ID = as.integer(ID)) %>% 
  dplyr::mutate(Loc = as.factor(Loc)) %>% 
  dplyr::mutate(Lat = as.numeric(Lat)) %>% 
  dplyr::mutate(Lon = as.numeric(Lon)) %>% 
  dplyr::select(ID, Loc, Lat, Lon, Date) 

#Construct co-capture networks; need to debug if we want it to work ####
#When setting max date, go one DAY past

mindate <- "2019-07-01"
maxdate <- "2019-08-01"
intwindow <- 3 #length of time (in days) w/in which individuals are considered co-captured
netwindow <- 1 #length of each network window in months
overlap <- 0 #overlap between network windows in months
spacewindow <- 0 #spatial tolerance for defining co-captures

#Create co-capture (social) network:
islanddat <- DynamicNetCreate(
  data        = New_data,
  intwindow   = intwindow,
  mindate     = mindate,
  maxdate     = maxdate,
  netwindow   = netwindow,
  overlap     = overlap,
  spacewindow = spacewindow,
  index       = FALSE)

#Plot social network 
i_nets <- CMRnet::cmr_igraph(islanddat, type = "social")
CMRnet::cmrSocPlot(nets=i_nets,
                   fixed_locs = TRUE, 
                   dynamic = TRUE,
                   rows=1,
                   title("Social"))

#Construct movement  networks ####
#When setting max date, go one DAY past end of the month to ensure the whole month is run
#mindate_19 <- "2019-07-01"
#maxdate_19 <- "2019-11-01"
#mindate_20 <- "2020-07-01"
#maxdate_20 <- "2020-11-01"
mindate_all <- "2020-01-01"
maxdate_all <- "2020-11-01"
intwindow <- 60 #length of time (in days) w/in which individuals are considered co-captured
netwindow <- 1 #length of each network window in months
overlap <- 0 #overlap between network windows in months
spacewindow <- 0 #spatial tolerance for defining co-captures

#Create movement network:

movedat_all <- CMRnet::MoveNetCreate(
  data      = GSB_CMR,
  intwindow = intwindow,
  mindate   = mindate_all,
  maxdate   = maxdate_all,
  netwindow = netwindow,
  overlap   = overlap,
  nextonly  = TRUE,
  index     = FALSE)

#Make an igraph "list" object from the movement network "list"
#m_nets_2019<-CMRnet::cmr_igraph(movedat_2019, type="movement")
#m_nets_2020<-CMRnet::cmr_igraph(movedat_2020, type="movement")

m_nets_all <- CMRnet::cmr_igraph(movedat_all, type = "movement")

#For plotting each networking individually; run cmrMovPlot first for best results 
for(i in 1:length(m_nets_all[[1]])){
  plot(m_nets_all[[1]][[i]],
       layout = layout_with_dh,
       vertex.color = "grey",
       vertex.label.font = 2, 
       vertex.label.color = "black",
       vertex.label.cex = 0.5,
       edge.color="blue",
       edge.arrow.size = 0.09,
       edge.width = 2,
       edge.lty = "dashed",
       edge.curved=0.3,
       frame = TRUE,
       main=paste("September 2020"))
}

#Only for plotting as an igraph:
#Make an matrix of the x,y location coordinates for igraph to interpret
SBI_locs <-  cbind(unique(GSB_CMR$Lat), unique(GSB_CMR$Lon))
#Make a distance object (dist()) out of the matrix, does this change the output?
SBI_dist <- dist(SBI_locs)

#Plot the network using a built in igraph function in CMRnet
CMRnet::cmrMovPlot(nets=m_nets_all,
                   title = "TRUE",
                   fixed_locs=TRUE,
                   location = SBI_dist,
                   dynamic=FALSE,
                   rows=2,
                   edge.arrow.size=0.25,
                   edge.color = "red",
                   )

CMRnet::cmrMovPlot(nets=m_nets_2020,
                   title = "TRUE",
                   fixed_locs=TRUE,
                   location = SBI_dist,
                   dynamic=FALSE,
                   rows=2,
                   edge.arrow.size=0.25,
                   edge.color = "red",
                   )
#dynamic = FALSE prints all networks in one stacked image 

##Note:
# Three objects are created from EACH network:
#1 is the edgelist, #2 are the matrices, #3 is the 3d matrix
#Ex: 
movedat_2019[[1]][,,2] #[2]] is the edgelist, adding [,,1] calls n matrix (for [[2]])
movedat_2020[[1]][,,2]

length(m_nets_2020)
length(m_nets_2020[[1]])
length(m_nets_2020[[2]])

months <- c("July", "August", "September", "October")

#https://rstudio-pubs-static.s3.amazonaws.com/337696_c6b008e0766e46bebf1401bea67f7b10.html
#look for layout options!

#For plotting each networking individually; run cmrMovPlot first for best results 
for(i in 1:length(m_nets_all[[1]])){
  plot(m_nets_all[[1]][[i]],
       layout = layout_with_dh,
       vertex.color = "grey",
       vertex.label.font = 2, 
       vertex.label.color = "black",
       vertex.label.cex = 0.5,
       edge.color="blue",
       edge.arrow.size = 0.09,
       edge.width = 2,
       edge.lty = "dashed",
       edge.curved=0.3,
       frame = TRUE,
       main=paste("September 2019"))
}

#For plotting "Overall" network
par(mar=c(1,1,1,1))
plot(m_nets_all[[2]],
     edge.color = "dodgerblue",
     vertex.color = "red",
     edge.arrow.size = 0.1,
     main = "Overall 2020")

# DEPRECATED: ggnet2 - better visualization? -------------------------------------------
class(m_nets_2019[[2]])

gm_nets <- m_nets_2019[[2]] #I *think* this is showing the network for the entire sampling period
#gm_soc <- i_nets[[2]] #this one is wonky, need to troubleshoot

#pass an igraph object to ggplot2 via ggnet2 in the GGally package
library(GGally)

ggnet2(m_nets_2020[[2]],
       size = "degree",
       node.color = "red",
       label = TRUE,
       arrow.size = 10,
       arrow.gap = 0.03, 
       legend.position = "right") +
  theme(panel.background = element_rect(color = "grey")) +
  ggtitle(label = "GSB Movement Network",
          subtitle = "") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# A little troubleshooting ------------------------------------------------

#if the matrix won't convert to igraph
graph.adjacency(X,mode="directed",diag=FALSE) #for movement networks
graph.adjacency(X,mode="undirected",diag=FALSE) #for social networks
#takes the first matrix from an object
graph.adjacency(cmrnet[[2]][,,1],mode="directed",diag=FALSE)
#removing matrix to look at 
movedat[[2]][,,1]
apply(movedat[[2]],3,sum) #run a function across a group of matrices



