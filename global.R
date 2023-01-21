#####Packages
pacotes = c("ggplot2", "dplyr", "plotly", "shiny", "dplyr","DT",
            "tidyverse", "igraph", "data.table", "FNN", "shinycssloaders","tidyverse",
            "scales", "knitr", "kableExtra", "ggfortify", "shinydashboard", "shinythemes", 
            "slickR", "visNetwork", "rsconnect", "reactable", "shinyWidgets", "forcats")

package.check <- lapply(pacotes, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
  }
})


library(ggplot2)
library(dplyr)
library(plotly)
library(shiny)
library(igraph)
library(tidyverse)
library(data.table)
library(DT)
library(stringr)
library(FNN)
library(shinycssloaders)
library(scales)
library(knitr)
library(kableExtra)
library(ggfortify)
library(shinydashboard)
library(shinythemes)
library(slickR)
library(visNetwork)
library(rsconnect)
library(reactable)
library(shinyWidgets)
library(forcats)
library(tidyr)

#########Prepare for Degree Distribution######
dt.fut.22 <- read.csv("finished.data.csv")
dt.fut.22.Id <- read.csv("finished.data.csv",  na.strings=c("","NA"))

#Create an id that allows each player to be represented uniquely
dt.fut.22.Id$NameId <- paste(dt.fut.22.Id$Name, dt.fut.22.Id$VER, sep = " - ")
dt.fut.22.Id$NameId <- paste(dt.fut.22.Id$NameId, paste("(",dt.fut.22.Id$RAT,")", sep = ""), sep = " - ")

#####Player Statistics Table####################################################################################
dt.fut.22 <- data.table(dt.fut.22)
dt.fut.22$POS <- as.factor(dt.fut.22$POS)
dt.fut.22$nationality <- as.factor(dt.fut.22$nationality)
dt.fut.22.player <- dt.fut.22[, c(1:4,9:17,21, 29:31)]

#####Nationality Statistics Table####################################################################################
dt.fut.22.nation <- data.table(table(dt.fut.22$nationality))
dt.fut.22.nation <- dt.fut.22[, list(n_cards = .N,
                                     n_players = .(length(unique(Name))),
                                     mean_rating = mean(RAT),
                                     sd_rating = sd(RAT),
                                     mean_price = mean(Price),
                                     sd_price = sd(Price)), by = "nationality"]
#Top 10 Nationalities Histogram
dt.fut.22.nation.top.10 <- dt.fut.22.nation[order(-n_cards), ]
dt.fut.22.nation.top.10 <- dt.fut.22.nation.top.10[1:10,]

#####League Statistics Table####################################################################################
dt.fut.22.league <- dt.fut.22[, list(n_cards = .N,
                                     n_players = .(length(unique(Name))),
                                     mean_rating = mean(RAT),
                                     sd_rating = sd(RAT),
                                     mean_price = mean(Price),
                                     sd_price = sd(Price)), by = "league_name"]
#Top 10 League Nationalities
dt.fut.22.league.top.10 <- dt.fut.22.league[order(-n_cards), ]
dt.fut.22.league.top.10 <- dt.fut.22.league.top.10[1:10]

#####Attribute Statistics Table####################################################################################
dt.fut.22.att.num <- dt.fut.22[,c("PAC", "SHO", "PAS", "DRI", "DEF", "PHY")]
m.fut.22.att.num <- t(sapply(dt.fut.22.att.num, function(x) c( "Stand dev" = sd(x), 
                                                          "Mean"= mean(x,na.rm=TRUE))))
dt.fut.22.att.num.table <- data.table(m.fut.22.att.num, keep.rownames = TRUE)

dt.fut.22.att.rat <- dt.fut.22[,c("PAC_R", "SHO_R", "PAS_R", "DRI_R", "DEF_R", "PHY_R")]
m.fut.22.att.rat <- t(sapply(dt.fut.22.att.rat, function(x) c( n_very_good = sum(str_count(x, "very good")), 
                         n_good= sum(str_count(x, "good")), n_average = sum(str_count(x, "average")))))
dt.fut.22.att.rat.table <- data.table(m.fut.22.att.rat, keep.rownames = TRUE)
dt.fut.22.att.rat.table$rn <- gsub("_R", "", dt.fut.22.att.rat.table$rn)

dt.fut.22.att <- merge(dt.fut.22.att.num.table, dt.fut.22.att.rat.table, by = "rn")

dt.fut.22.att.freq <- dt.fut.22.Id
dt.fut.22.att.freq$PAC_R <- as.character(dt.fut.22.att.freq$PAC_R)
dt.fut.22.att.freq$PAC_R[is.na(dt.fut.22.att.freq$PAC_R)] <- "None"
dt.fut.22.att.freq$PAC_R <- as.factor(dt.fut.22.att.freq$PAC_R)

dt.fut.22.att.freq$SHO_R <- as.character(dt.fut.22.att.freq$SHO_R)
dt.fut.22.att.freq$SHO_R[is.na(dt.fut.22.att.freq$SHO_R)] <- "None"
dt.fut.22.att.freq$SHO_R <- as.factor(dt.fut.22.att.freq$SHO_R)

dt.fut.22.att.freq$PAS_R <- as.character(dt.fut.22.att.freq$PAS_R)
dt.fut.22.att.freq$PAS_R[is.na(dt.fut.22.att.freq$PAS_R)] <- "None"
dt.fut.22.att.freq$PAS_R <- as.factor(dt.fut.22.att.freq$PAS_R)

dt.fut.22.att.freq$DRI_R <- as.character(dt.fut.22.att.freq$DRI_R)
dt.fut.22.att.freq$DRI_R[is.na(dt.fut.22.att.freq$DRI_R)] <- "None"
dt.fut.22.att.freq$DRI_R <- as.factor(dt.fut.22.att.freq$DRI_R)

dt.fut.22.att.freq$DEF_R <- as.character(dt.fut.22.att.freq$DEF_R)
dt.fut.22.att.freq$DEF_R[is.na(dt.fut.22.att.freq$DEF_R)] <- "None"
dt.fut.22.att.freq$DEF_R <- as.factor(dt.fut.22.att.freq$DEF_R)

dt.fut.22.att.freq$PHY_R <- as.character(dt.fut.22.att.freq$PHY_R)
dt.fut.22.att.freq$PHY_R[is.na(dt.fut.22.att.freq$PHY_R)] <- "None"
dt.fut.22.att.freq$PHY_R <- as.factor(dt.fut.22.att.freq$PHY_R)

dt.fut.22.att.freq$PAC_R <- factor(dt.fut.22.att.freq$PAC_R, levels = c("None", "average pace", "good pace", "very good pace"))
dt.fut.22.att.freq$SHO_R <- factor(dt.fut.22.att.freq$SHO_R, levels = c("None", "average shooting", "good shooting", "very good shooting"))
dt.fut.22.att.freq$PAS_R <- factor(dt.fut.22.att.freq$PAS_R, levels = c("None", "average passing", "good passing", "very good passing"))
dt.fut.22.att.freq$DRI_R <- factor(dt.fut.22.att.freq$DRI_R, levels = c("None", "average dribbling", "good dribbling", "very good dribbling"))
dt.fut.22.att.freq$DEF_R <- factor(dt.fut.22.att.freq$DEF_R, levels = c("None", "average defending", "good defending", "very good defending"))
dt.fut.22.att.freq$PHY_R <- factor(dt.fut.22.att.freq$PHY_R, levels = c("None", "average physical", "good physical", "very good physical"))

######################Creating a Network Data Table
dt.pace <- dt.fut.22.Id [, c("PAC_R","NameId", "nationality", "league_name", "POS", "VER", "Price", "RAT")]
dt.physical <- dt.fut.22.Id [, c("PHY_R","NameId", "nationality", "league_name", "POS", "VER", "Price", "RAT")]
dt.defending <- dt.fut.22.Id [, c("DEF_R","NameId", "nationality", "league_name", "POS", "VER", "Price", "RAT")]
dt.shooting <- dt.fut.22.Id [, c("SHO_R","NameId", "nationality", "league_name", "POS", "VER", "Price", "RAT")]
dt.dribbling <- dt.fut.22.Id [, c("DRI_R","NameId", "nationality", "league_name", "POS", "VER", "Price", "RAT")]
dt.passing <- dt.fut.22.Id [, c("PAS_R","NameId", "nationality", "league_name", "POS", "VER", "Price", "RAT")]

dt.pace <- dt.pace[!is.na(dt.pace$PAC_R),]
dt.physical <- dt.physical[!is.na(dt.physical$PHY_R),]
dt.defending <- dt.defending[!is.na(dt.defending$DEF_R),]
dt.shooting <- dt.shooting[!is.na(dt.shooting$SHO_R),]
dt.dribbling <- dt.dribbling[!is.na(dt.dribbling$DRI_R),]
dt.passing <- dt.passing[!is.na(dt.passing$PAS_R),]

names(dt.pace)[names(dt.pace) == "PAC_R"] <- "attribute"
names(dt.physical)[names(dt.physical) == "PHY_R"] <- "attribute"
names(dt.defending)[names(dt.defending) == "DEF_R"] <- "attribute"
names(dt.shooting)[names(dt.shooting) == "SHO_R"] <- "attribute"
names(dt.dribbling)[names(dt.dribbling) == "DRI_R"] <- "attribute"
names(dt.passing)[names(dt.passing) == "PAS_R"] <- "attribute"

#also create a shrunk dataset for network projection of players on attributes to reduce CPU strain
dt.bipartite.data <- rbind(dt.pace, dt.physical, dt.defending, dt.shooting, dt.dribbling, dt.passing)
dt.bipartite.data.shrunk <- top_n(dt.bipartite.data, 250, RAT)
dt.bipartite.data <- as.data.table(dt.bipartite.data)
dt.bipartite.data.shrunk <- as.data.table(dt.bipartite.data.shrunk)

###########################Create Graph Projections
players.all <- dt.bipartite.data[, list(name=unique(NameId), type = TRUE)]
attributes.all <- dt.bipartite.data[, list(name=unique(attribute), type = FALSE)]
vertices.attributes.all <- rbind(players.all, attributes.all)
g.bipartite.attributes <- graph.data.frame(dt.bipartite.data[, list(NameId, attribute)], 
                                           directed = FALSE, vertices = vertices.attributes.all)
g.attributes <- bipartite_projection(g.bipartite.attributes)$proj1

#Create Shrunk Projection of Players on Attributes
players.all.shrunk <- dt.bipartite.data.shrunk[, list(name=unique(NameId), type = TRUE)]
attributes.all.shrunk <- dt.bipartite.data.shrunk[, list(name=unique(attribute), type = FALSE)]
vertices.attributes.all.shrunk <- rbind(players.all.shrunk, attributes.all.shrunk)
g.bipartite.attributes.shrunk <- graph.data.frame(dt.bipartite.data.shrunk[, list(NameId, attribute)], 
                                                  directed = FALSE, vertices = vertices.attributes.all.shrunk)
g.attributes.players <- bipartite_projection(g.bipartite.attributes.shrunk)$proj2


##########################Calculating Graph Statistics
#degree
V(g.bipartite.attributes)$degree <- degree(g.bipartite.attributes)
V(g.attributes.players)$degree <- degree(g.attributes.players)
V(g.attributes)$degree <- degree(g.attributes)

#closeness
V(g.bipartite.attributes)$closeness <- closeness(g.bipartite.attributes)
V(g.attributes.players)$closeness <- closeness(g.attributes.players)
V(g.attributes)$closeness <- closeness(g.attributes)

#betweenness
V(g.bipartite.attributes)$betweenness <- betweenness(g.bipartite.attributes)
V(g.attributes.players)$betweenness <- betweenness(g.attributes.players)
V(g.attributes)$betweenness <- betweenness(g.attributes)

#eigenvector
V(g.bipartite.attributes)$eigenvector <- evcent(g.bipartite.attributes)$vector
V(g.attributes.players)$eigenvector <- evcent(g.attributes.players)$vector
V(g.attributes)$eigenvector <- evcent(g.attributes)$vector

dt.g.bipartite.attributes <- data.table(get.data.frame(g.bipartite.attributes, "vertices"))
dt.g.bipartite.attributes <- head(dt.g.bipartite.attributes[order(-degree)], 18)

dt.g.attributes.players <- data.table(get.data.frame(g.attributes.players, "vertices"))
dt.g.attributes.players <- head(dt.g.attributes.players[order(-degree)], 20)

dt.g.attributes <- data.table(get.data.frame(g.attributes, "vertices"))
dt.g.attributes <- head(dt.g.attributes[order(-degree)], 20)


##########################################Plot Degree Distribution
dt.bipartite.attributes <- as.data.frame(degree(g.bipartite.attributes))
names(dt.bipartite.attributes)[names(dt.bipartite.attributes) == "degree(g.bipartite.attributes)"] <- "Degree"

# PROJECTED PLAYERS BASED ON ATTRIBUTES
dt.bipartite.attributes.players <- as.data.frame(degree(g.attributes.players))
names(dt.bipartite.attributes.players)[names(dt.bipartite.attributes.players) == "degree(g.attributes.players)"] <- "Degree"

# PROJECTED Attributes BASED ON PLAYERS
dt.bipartite.attributes.players.inv <- as.data.frame(degree(g.attributes))
names(dt.bipartite.attributes.players.inv)[names(dt.bipartite.attributes.players.inv) == "degree(g.attributes)"] <- "Degree"



###########################################Calculate average path length and diameter
g.bp.attributes.apl <- mean_distance(g.bipartite.attributes)
g.att.pl.apl <- mean_distance(g.attributes.players)
g.players.att.apl <- mean_distance(g.attributes)

g.bp.attributes.diameter <- diameter(g.bipartite.attributes, directed = FALSE)
g.att.players.diameter <- diameter(g.attributes.players, directed = FALSE)
g.players.att.diameter <- diameter(g.attributes, directed = FALSE)
