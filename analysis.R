library(rjson)
library(ggplot2)
library(ggsci)
library(randomForest)
library(ROCR)
library(plyr)
library(rayshader)

setwd("C:/Users/kenne/OneDrive/Desktop/Data Science/Analysis/Football")

#Set Pitch Length & Width
pitch_l = 114
pitch_w = 74
country <- c("England","France","Germany","Italy","Spain")

#### Full JSON data files can be downloaded from https://figshare.com/collections/Soccer_match_event_dataset/4415000/5
#### Or skip to curated data files below

### Events Data
# Note: Shots/goals data do not reflect the true number of shots/goals in the match due to tagging inaccuracies
#
# shots <- NULL
# for(j in 1:5){
#   events <- fromJSON(file = paste("events/events_",country[j],".json", sep=""))
#   for(i in 1:length(events)){
#     if (events[[i]][1] != 10) next
#     events_i <- events[[i]]
#     
#     # Check for goal (tag id = 101)
#     goal = sum(as.data.frame(events_i$tags) == 101)
#     
#     events_i <- within(events_i, rm(tags))
#     events_i <- as.data.frame(events_i)
#     events_i <- cbind(events_i, goal, country[j])
#     shots <- rbind(shots, events_i)
#   }
# }
# events <- NULL
# colnames(shots)[16] <- "league"
# shots$positions.y <- 101 - shots$positions.y # reflect horizontally
# head(shots)
#
## Adjust shots dataset to reflect opposite team
#
# for(i in 1:dim(shots)[1]){
#   hometeam = matches$home.teamId[which(matches$wyId==shots$matchId[i])]
#   awayteam = matches$away.teamId[which(matches$wyId==shots$matchId[i])]
#   if(shots$teamId[i] == hometeam){
#     shots$teamId.opp[i] <- awayteam
#   } else {
#     shots$teamId.opp[i] <- hometeam
#   }
# }
# write.csv(shots,"shots.csv", row.names = F)

### Matches Data
#
# matches <- NULL
# for(j in 1:5){
#   m <- fromJSON(file = paste("matches/matches_",country[j],".json", sep=""))
#   for(i in 1:length(m)){
#     m_i <- m[[i]]
#     m_i_home <- as.data.frame(within(m_i$teamsData[[1]], rm(formation)))
#     colnames(m_i_home) <- paste(m_i_home$side, colnames(m_i_home), sep=".")
#     m_i_away <- as.data.frame(within(m_i$teamsData[[2]], rm(formation)))
#     colnames(m_i_away) <- paste(m_i_away$side, colnames(m_i_away), sep=".")
#     
#     m_i <- as.data.frame(within(m_i, rm(teamsData,referees)))
#     m_i <- cbind(m_i, m_i_home, m_i_away, league = country[j])
#     matches <- rbind(matches, m_i)
#   }
# }
# m <- NULL
# matches <- matches[order(matches$wyId),]
# write.csv(matches,"matches.csv", row.names = F)

#### Teams Data
#
# t <- fromJSON(file = "teams.json")
# teams <- NULL
# for(i in 1:length(t)){
#   teams_i <- t[[i]]
#   teams_i <- as.data.frame(teams_i)
#   teams <- rbind(teams, teams_i)
# }
# teams[2] <- lapply(teams[2], as.character)
# teams$name[4] <- "Deportivo Alaves"
# teams$name[11] <- "Atletico Madrid"
# teams$name[18] <- "Leganes"
# teams$name[32] <- "Bayern Munchen"
# teams$name[59] <- "Saint-Etienne"
# teams$name[77] <- "Koln"
# teams$name[83] <- "Deportivo La Coruna"
# teams$name[97] <- "Malaga"
# write.csv(teams,"teams.csv", row.names = F)

#### Players Data
# pp <- fromJSON(file = "players.json")
# players <- NULL
# for(i in 1:length(pp)){
#      players_i <- pp[[i]]
#      if (is.null(players_i$currentTeamId)) next
#      players_i <- as.data.frame(players_i)
#      players <- rbind(players, players_i)
#    }
# write.csv(players,"players.csv", row.names = F)

#### Reading from CSV files (pre-processed data)

shots <- read.csv("shots.csv")
matches <- read.csv("matches.csv")
teams_all <- read.csv("teams.csv")
players <- read.csv("players.csv")

# Set up selected teams
teams <- teams_all[c(68,80,69,78,79,85,32,31,34,16,15,11,37,82,38,54,13,72,12),]
teams$code <- c("MCI","MUN","TOT","LIV","CHE","ARS","BAY","DOR","S04","FCB","RMA","ATM","JUV","NAP","ROM","INT","PSG","MON","LYO")
teams$colpri <- c("#97c1e7","#da020e","#132257","#dd0000","#0000dd","#ef0107","#dc052d","#000000","#004d9d","#a50044","#dbdbdb","#ce3524","#000000","#12a0d7","#970a2c","#0068a8","#e30613","#ed1c24","#da001a")
teams$colsec <- c("#ffffff","#ffe500","#ffffff","#ffffff","#ffffff","#ffffff","#ffffff","#ffff00","#ffffff","#004d98","#ffffff","#ffffff","#ffffff","#ffffff","#ffffff","#ffffff","#004170","#ffffff","#ffffff")



plot_density <- function (shots_XX, title = "", col_primary = "dark grey", col_secondary = "white", density = T, col_lim = 0.0018){
  m <- ggplot(shots_XX, aes(x = positions.x, y = positions.y)) +
    ggtitle(title) +
    xlim(50, 101.5) +
    ylim(0, 100) +
    theme(plot.title = element_text(size=30),
          legend.title = element_blank(),
          legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "#f1faee"),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  
  if(density){
    # Plot kernel density plot
    m <- m +
      stat_density_2d(geom = "polygon", contour = TRUE,
                      aes(fill = after_stat(level)), colour = "dark grey",
                      bins = 10) +
      scale_fill_gradientn(colours = c(col_secondary, col_primary),limits = c(0 ,col_lim), na.value = col_primary)  
  } else{
    # Plot scatterplot
    m <- m +
      geom_point(data = shots_XX, mapping = aes(x = positions.x, y = positions.y), color = col_primary, size = 2.0) 
  }
  
  # Plot football field
  m +
    geom_segment(aes(x = 100, y = ((pitch_w-44)/2)*100/pitch_w, xend = 100 - 18*100/pitch_l, yend = ((pitch_w-44)/2)*100/pitch_w)) +
    geom_segment(aes(x = 100 - 18*100/pitch_l, y = ((pitch_w-44)/2)*100/pitch_w, xend = 100 - 18*100/pitch_l, yend = ((pitch_w-44)/2+44)*100/pitch_w)) +
    geom_segment(aes(x = 100 - 18*100/pitch_l, y = ((pitch_w-44)/2+44)*100/pitch_w, xend = 100, yend = ((pitch_w-44)/2+44)*100/pitch_w)) +
    geom_segment(aes(x = 100, y = ((pitch_w-20)/2)*100/pitch_w, xend = 100 - 6*100/pitch_l, yend = ((pitch_w-20)/2)*100/pitch_w)) +
    geom_segment(aes(x = 100 - 6*100/pitch_l, y = ((pitch_w-20)/2)*100/pitch_w, xend = 100 - 6*100/pitch_l, yend = ((pitch_w-20)/2+20)*100/pitch_w)) +
    geom_segment(aes(x = 100 - 6*100/pitch_l, y = ((pitch_w-20)/2+20)*100/pitch_w, xend = 100, yend = ((pitch_w-20)/2+20)*100/pitch_w)) +
    geom_segment(aes(x = 50, y = 0, xend = 50, yend = 100)) +
    geom_segment(aes(x = 50, y = 100, xend = 100, yend = 100)) +
    geom_segment(aes(x = 100, y = 100, xend = 100, yend = 0)) +
    geom_segment(aes(x = 100, y = 0, xend = 50, yend = 0)) +
    geom_segment(aes(x = 100, y = (pitch_w/2-4)*100/pitch_w, xend = 101.5, yend = (pitch_w/2-4)*100/pitch_w)) +
    geom_segment(aes(x = 101.5, y = (pitch_w/2-4)*100/pitch_w, xend = 101.5, yend = (pitch_w/2+4)*100/pitch_w)) +
    geom_segment(aes(x = 101.5, y = (pitch_w/2+4)*100/pitch_w, xend = 100, yend = (pitch_w/2+4)*100/pitch_w)) +
    geom_point(aes(x = 100-12*100/pitch_l, y = 50), size = 2)
}

# Plot shots
for(i in 1:19){
  shots_XX <- shots[shots$teamId==teams$wyId[i],]
  title <- paste("Shots by",teams$name[i])
  col_primary <- teams$colpri[i]
  col_secondary <- teams$colsec[i]
  
  ## Print plots on R
  print(plot_density(shots_XX, title, col_primary, col_secondary))
  
  ## Save plots to PNG file
  #filename = paste("Shots by",i,teams$code[i],".png")
  #png(filename=filename, width = 700, height = 900)
  #print(plot_density(shots_XX, title, col_primary, col_secondary))
  #dev.off()
}

#Plot shots against team
for(i in 1:19){
  shots_XX <- shots[shots$teamId.opp==teams$wyId[i],]
  title <- paste("Shots against",teams$name[i])
  col_primary <- teams$colpri[i]
  col_secondary <- teams$colsec[i]
  
  ## Print plots on R
  print(plot_density(shots_XX, title, col_primary, col_secondary, T, 0.0028))
  
  ## Save plots to PNG file
  #filename = paste("Shots against",i,teams$code[i],".png")
  #png(filename=filename, width = 700, height = 900)
  #print(plot_density(shots_XX, title, col_primary, col_secondary, T, 0.0028))
  #dev.off()
}

# You can also create density plots and scatter plots of individual players. Examples:

# Tottenham Hotspurs
#table(shots[shots$teamId==1624,]$playerId)
plot_density(shots[shots$playerId==8717,], "Shots by Harry Kane", teams$colpri[3], teams$colsec[3], F)
plot_density(shots[shots$playerId==54,], "Shots by Chirstian Eriksen", teams$colpri[3], teams$colsec[3], F)
plot_density(shots[shots$playerId==14911,], "Shots by Son Heung-Min", teams$colpri[3], teams$colsec[3], F)
plot_density(shots[shots$playerId==13484,], "Shots by Dele Alli", teams$colpri[3], teams$colsec[3], F)

# Real Madrid
plot_density(shots[shots$teamId==675,], "Shots by Real Madrid",teams$colpri[11],teams$colsec[11], F)
sort(table(shots[shots$teamId=="675",]$playerId), decreasing = T)
plot_density(shots[shots$playerId==3322,], "Shots by Cristiano Ronaldo",teams$colpri[11],teams$colsec[11], T)
plot_density(shots[shots$playerId==8278,], "Shots by Gareth Bale",teams$colpri[11],teams$colsec[11], T)
plot_density(shots[shots$playerId==3321,], "Shots by Karim Benzema",teams$colpri[11],teams$colsec[11], T)

# Liverpool
sort(table(shots[shots$teamId=="1612",]$playerId), decreasing = T)
plot_density(shots[shots$playerId==120353,], "Shots by M. Salah","#66c2a5", density = F)
plot_density(shots[shots$playerId==15808,], "Shots by R. Firmino","#fc8e62", density = F)
plot_density(shots[shots$playerId==25747,], "Shots by S. Mane","#6685cc", density = F)

# Liverpool Team Scatterplot
shots_LIV <- shots[shots$teamId==1612,]
shots_LIV$positions.x <- shots[shots$teamId==1612,]$positions.x + rnorm(dim(shots_LIV)[1], 0, 0.15)
shots_LIV$positions.y <- shots[shots$teamId==1612,]$positions.y + rnorm(dim(shots_LIV)[1], 0, 0.15)
plot_density(shots_LIV, "Shots by Liverpool", density = F)

# Liverpool Team scatterplot (top 3 players are colored differently)
col_LIV <- rep("dark grey", dim(shots_LIV)[1])
col_LIV[shots_LIV$playerId==120353] <- "#66c2a5"
col_LIV[shots_LIV$playerId==15808] <- "#fc8e62"
col_LIV[shots_LIV$playerId==25747] <- "#6685cc"
plot_density(shots_LIV, "Shots by Liverpool",col_LIV, density = F)

# Liverpool Team scatterplot (goals colored in red)
col_LIV2 <- rep("dark grey", dim(shots_LIV)[1])
col_LIV2[shots_LIV$goal==1] <- "red"
plot_density(shots_LIV, "Shots by Liverpool",col_LIV2, density = F)



#############################################
## 3D graphs for Liverpool                 ##
#############################################

# You may change the teamId here
shots_LIV <- shots[shots$teamId==1612,]
shots_LIV_count <- NULL

# Count the number of shots made in each x-y coordinate
for(i in 61:100){
  for (j in 1:100){
    x = i
    y = j
    num_shots = 0
    shots_LIV_count <- rbind(shots_LIV_count, cbind(x, y, num_shots))
  }
}

for(i in 1:dim(shots_LIV)[1]){
  x <- shots_LIV$positions.x[i]
  y <- shots_LIV$positions.y[i]
  num_shots = shots_LIV_count[shots_LIV_count[,1] == x & shots_LIV_count[,2] == y,3] + 1
  shots_LIV_count[shots_LIV_count[,1] == x & shots_LIV_count[,2] == y,3] <- num_shots
}

shots_LIV_count <- as.data.frame(shots_LIV_count)


# Set up bivariate histogram by aggregating 4 x-y positions into 1 bin
shots_LIV_Hist2 <- NULL
for(i in seq(from = 61.5, to = 99.5, by = 2)){
  for (j in seq(from = 1.5, to = 99.5, by = 2)){
    x = i
    y = j
    num_shots = shots_LIV_count[shots_LIV_count$x == i - 0.5 & shots_LIV_count$y == j - 0.5,]$num_shots +
      shots_LIV_count[shots_LIV_count$x == i - 0.5 & shots_LIV_count$y == j + 0.5,]$num_shots + 
      shots_LIV_count[shots_LIV_count$x == i + 0.5 & shots_LIV_count$y == j - 0.5,]$num_shots +
      shots_LIV_count[shots_LIV_count$x == i + 0.5 & shots_LIV_count$y == j + 0.5,]$num_shots
    shots_LIV_Hist2 <- rbind(shots_LIV_Hist2, cbind(x, y, num_shots))
  }
}
shots_LIV_Hist2 <- as.data.frame(shots_LIV_Hist2[shots_LIV_Hist2[,3] > 0,])

## 2D Histogram with 3D view
livplot = ggplot(shots_LIV_Hist2, aes(x = x, y = y)) +
  xlim(50, 101.5) +
  ylim(0, 100) +
  theme(legend.title = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#f1faee"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  
  # Plot football field
  geom_segment(aes(x = 100, y = ((pitch_w-44)/2)*100/pitch_w, xend = 100 - 18*100/pitch_l, yend = ((pitch_w-44)/2)*100/pitch_w, color = 0)) +
  geom_segment(aes(x = 100 - 18*100/pitch_l, y = ((pitch_w-44)/2)*100/pitch_w, xend = 100 - 18*100/pitch_l, yend = ((pitch_w-44)/2+44)*100/pitch_w, color = 0)) +
  geom_segment(aes(x = 100 - 18*100/pitch_l, y = ((pitch_w-44)/2+44)*100/pitch_w, xend = 100, yend = ((pitch_w-44)/2+44)*100/pitch_w, color = 0)) +
  geom_segment(aes(x = 100, y = ((pitch_w-20)/2)*100/pitch_w, xend = 100 - 6*100/pitch_l, yend = ((pitch_w-20)/2)*100/pitch_w, color = 0)) +
  geom_segment(aes(x = 100 - 6*100/pitch_l, y = ((pitch_w-20)/2)*100/pitch_w, xend = 100 - 6*100/pitch_l, yend = ((pitch_w-20)/2+20)*100/pitch_w, color = 0)) +
  geom_segment(aes(x = 100 - 6*100/pitch_l, y = ((pitch_w-20)/2+20)*100/pitch_w, xend = 100, yend = ((pitch_w-20)/2+20)*100/pitch_w, color = 0)) +
  geom_segment(aes(x = 50, y = 0, xend = 50, yend = 100, color = 0)) +
  geom_segment(aes(x = 50, y = 100, xend = 100, yend = 100, color = 0)) +
  geom_segment(aes(x = 100, y = 100, xend = 100, yend = 0, color = 0)) +
  geom_segment(aes(x = 100, y = 0, xend = 50, yend = 0, color = 0)) +
  geom_segment(aes(x = 100, y = (pitch_w/2-4)*100/pitch_w, xend = 101.5, yend = (pitch_w/2-4)*100/pitch_w, color = 3)) +
  geom_segment(aes(x = 101.5, y = (pitch_w/2-4)*100/pitch_w, xend = 101.5, yend = (pitch_w/2+4)*100/pitch_w, color = 3)) +
  geom_segment(aes(x = 101.5, y = (pitch_w/2+4)*100/pitch_w, xend = 100, yend = (pitch_w/2+4)*100/pitch_w, color = 3)) +
  geom_point(aes(x = 100-12*100/pitch_l, y = 50,color = 0), size = 2) + 
  
  geom_point(aes(x = x, y = y, color = num_shots)) +
  scale_color_continuous(low = "#ffffff", high = "#dd0000", limits=c(0,9))

# Render plot in 3D. Adjust the camera angle using zoom, phi, and theta.
plot_gg(livplot, width = 3.5, multicore = TRUE, windowsize = c(1400,866),
        zoom = 0.6, phi = 40, theta = 310)
# After adjusting the camera view, use this to create a snapshot.
render_snapshot(clear = TRUE)


## 2D KDE with 3D view
livplot = ggplot(shots_LIV, aes(x = positions.x, y = positions.y)) +
  xlim(50, 101.5) +
  ylim(0, 100) +
  theme(legend.title = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#f1faee"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  
  # Plot football field
  geom_segment(aes(x = 100, y = ((pitch_w-44)/2)*100/pitch_w, xend = 100 - 18*100/pitch_l, yend = ((pitch_w-44)/2)*100/pitch_w, color = 0)) +
  geom_segment(aes(x = 100 - 18*100/pitch_l, y = ((pitch_w-44)/2)*100/pitch_w, xend = 100 - 18*100/pitch_l, yend = ((pitch_w-44)/2+44)*100/pitch_w, color = 0)) +
  geom_segment(aes(x = 100 - 18*100/pitch_l, y = ((pitch_w-44)/2+44)*100/pitch_w, xend = 100, yend = ((pitch_w-44)/2+44)*100/pitch_w, color = 0)) +
  geom_segment(aes(x = 100, y = ((pitch_w-20)/2)*100/pitch_w, xend = 100 - 6*100/pitch_l, yend = ((pitch_w-20)/2)*100/pitch_w, color = 0)) +
  geom_segment(aes(x = 100 - 6*100/pitch_l, y = ((pitch_w-20)/2)*100/pitch_w, xend = 100 - 6*100/pitch_l, yend = ((pitch_w-20)/2+20)*100/pitch_w, color = 0)) +
  geom_segment(aes(x = 100 - 6*100/pitch_l, y = ((pitch_w-20)/2+20)*100/pitch_w, xend = 100, yend = ((pitch_w-20)/2+20)*100/pitch_w, color = 0)) +
  geom_segment(aes(x = 50, y = 0, xend = 50, yend = 100, color = 0)) +
  geom_segment(aes(x = 50, y = 100, xend = 100, yend = 100, color = 0)) +
  geom_segment(aes(x = 100, y = 100, xend = 100, yend = 0, color = 0)) +
  geom_segment(aes(x = 100, y = 0, xend = 50, yend = 0, color = 0)) +
  geom_segment(aes(x = 100, y = (pitch_w/2-4)*100/pitch_w, xend = 101.5, yend = (pitch_w/2-4)*100/pitch_w, color = 3)) +
  geom_segment(aes(x = 101.5, y = (pitch_w/2-4)*100/pitch_w, xend = 101.5, yend = (pitch_w/2+4)*100/pitch_w, color = 3)) +
  geom_segment(aes(x = 101.5, y = (pitch_w/2+4)*100/pitch_w, xend = 100, yend = (pitch_w/2+4)*100/pitch_w, color = 3)) +
  geom_point(aes(x = 100-12*100/pitch_l, y = 50,color = 0), size = 2) + 
  
  #geom_point(aes(x=x,y=y,color=shots)) +
  stat_density_2d(geom = "polygon", contour = TRUE,
                  aes(fill = after_stat(level)), #colour = "dark grey",
                  bins = 50) +
  scale_fill_gradientn(colours = c("#ffffff","#dd0000"),limits = c(0 ,0.0018), na.value = "#ffffff") + 
  
  scale_color_continuous(low = "#ffffff", high = "#dd0000", limits=c(0,9))

# Render plot in 3D. Adjust the camera angle using zoom, phi, and theta.
plot_gg(livplot, width = 3.5, multicore = TRUE, windowsize = c(1400,866),
        zoom = 0.6, phi = 40, theta = 310)
# After adjusting the camera view, use this to create a snapshot.
render_snapshot(clear = TRUE)



#############################################
##   Histogram of shots in a match         ##
#############################################

matches1 <- matches[matches$league=="England",c("wyId","home.teamId")]
colnames(matches1) <- c("wyId","teamId")
matches2 <- matches[matches$league=="England",c("wyId","away.teamId")]
colnames(matches2) <- c("wyId","teamId")
matches_EPL <- rbind(matches1,matches2)
for(i in 1:760){
  matches_EPL$shots[i] <- dim(shots[shots$matchId == matches_EPL$wyId[i] & shots$teamId == matches_EPL$teamId[i],])[1]
}

matches_EPL <- matches_EPL[order(matches_EPL$teamId, matches_EPL$wyId),]
matches_EPL[matches_EPL$teamId==1612,]$shots <- c(14, 23, 18, 7, 35, 23, 17, 19, 12, 16, 15, 21, 16, 14, 12, 23, 14, 21, 14, 22, 18, 19, 16, 21, 14, 9, 16, 21, 14, 14, 13, 16, 10, 20, 9, 20, 10, 22)
matches_EPL[matches_EPL$teamId==1625,]$shots <- c(14, 19, 19, 13, 28, 25, 17, 20, 15, 15, 9, 12, 14, 26, 24, 14, 23, 20, 14, 22, 15, 18, 11, 21, 19, 20, 19, 9 ,13, 17, 18, 20, 17, 19, 19, 15, 19, 13)


# Plot Histogram & Kernel Density Plot of Liverpool's shots across 38 games
ggplot(matches_EPL[matches_EPL$teamId %in% c(1612),], aes(x=shots)) + 
  geom_histogram(colour="black", fill="white", binwidth = 5)

ggplot(matches_EPL[matches_EPL$teamId %in% c(1612),], aes(x=shots)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 5) +
  geom_density(alpha=.2, fill="#FF6666") 


# Plot Kernel Density Plots of Liverpool's and Man City's shots across 38 games
matches_LIV_MCI <- matches_EPL[matches_EPL$teamId %in% c(1612, 1625),]
matches_LIV_MCI$teamId <- factor(matches_LIV_MCI$teamId, levels = c(1612, 1625), labels = c("Liverpool", "Manchester City"))
mu <- ddply(matches_LIV_MCI, "teamId", summarise, grp.mean=mean(shots))

ggplot(matches_LIV_MCI, aes(x=shots, fill=teamId)) +
  geom_density(alpha=0.4) +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=teamId),
             linetype="dashed") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
