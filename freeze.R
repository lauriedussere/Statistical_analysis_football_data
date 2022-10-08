devtools::install_github("statsbomb/StatsBombR")

'https://statsbomb.com/news/statsbomb-announce-the-release-of-free-statsbomb-360-data-euro-2020-available-now/'

library (StatsBombR)
library(ggplot2)
library(ggmosaic)
library(dplyr)
library(tidyverse)

Comp <- FreeCompetitions()

Matches <- FreeMatches(Comp)

Matches = Matches %>% filter(competition.competition_name=="UEFA Euro")

data360 <- StatsBombFree360Events(MatchesDF = Matches[6,1], Parallel = T)

events <- StatsBombFreeEvents(MatchesDF = Matches[6,1], Parallel = T)

events <- allclean(events)

events <- get.opposingteam(events)

data360 = data360 %>% rename(id = event_uuid)

events = events %>% left_join(data360, by = c("id" = "id"))

events = events %>% rename(match_id = match_id.x) %>% select(-match_id.y)

ffs = events %>%
  group_by(team.name) %>%
  filter(type.name=="Pass") %>%
  select(id, match_id, team.name, OpposingTeam, 
         player.name, type.name,
         location.x, location.y, pass.end_location.x,
         pass.end_location.y, pass.recipient.name, freeze_frame,pass.outcome.name,pass.switch)


ffs = ffs %>% unnest(freeze_frame) %>%
  mutate(ff_location.x = (map(location, 1)), ff_location.y = (map(location, 2))) %>%
  select(-location) %>%
  mutate(ff_location.x = as.numeric(ifelse(ff_location.x == "NULL", NA, ff_location.x)), 
         ff_location.y = as.numeric(ifelse(ff_location.y == "NULL", NA, ff_location.y)))


#All pass Spain Team
France<-filter(ffs, team.name=="France")
#1 pass Spain
France_1<-filter(France, id==c("9e775cbc-3fbf-4882-a4e8-5b806566cc8b"))
France_1$teammate[France_1 $teammate == "TRUE"]<-"France"
France_1$teammate[France_1 $teammate== "FALSE"]<-"Portugal"
#France_1[,"teammate"]=factor(France_1[,"teammate"],labels=c("Spain","Poland"))


## Je vais garder ici que les pass_switch qui sont égales à true 
# switch : a switch is any pass that travels more than 40 yards of the width od the pitch (40 yards=36m) 
France_switch=which(France[,"pass.switch"]=="TRUE")
France_switch=France[France_switch,]

#ici on va étudier une switch imcomplete
France_1<-filter(France_switch, id==c("9d1a381e-3ad3-4839-bd17-12a4bfdc5032"))
France_1$teammate[France_1 $teammate == "TRUE"]<-"France"
France_1$teammate[France_1 $teammate== "FALSE"]<-"Portugal"

#ici on va étudier une switch qui est pas incomplète
France_2<-filter(France_switch, id==c("9bfe6af4-32cb-40f5-834f-9a272c576330"))
France_2$teammate[France_2 $teammate == "TRUE"]<-"France"
France_2$teammate[France_2 $teammate== "FALSE"]<-"Portugal"

for (i in 1:nrow(France_2)){
  if (is.na(France_2[i,"pass.outcome.name"])){
    France_2[i,"pass.outcome.name"]="Not incomplete"
  }
  if (is.na(France_2[i,"pass.switch"])){
    France_2[i,"pass.switch"]="FALSE"
  }
}

pass_player=which(France_1[,"actor"]=="TRUE")
pass_player=France_1[pass_player,]


ggplot(France_1, aes(x=ff_location.x, y=ff_location.y, label= teammate))+
  geom_point(aes(color=teammate))+
  geom_segment(aes(x=location.x ,y=location.y,xend=pass.end_location.x,yend=pass.end_location.y,color=pass.outcome.name))+
  #geom_segment(aes(x=ff_location.x , y=ff_location.y, xend = 65,  yend = 67))
  # geom_text(size=3, vjust=-2)+
  # geom_blank()+ theme_bw()+
  # #point start
  geom_point(aes(x=location.x , y=location.y),color="purple")+
  geom_point(aes(x=unlist(pass_player[,"ff_location.x"]),y=unlist(pass_player[,"ff_location.y"]),label=teammate),size=4)+
  # #point end
  geom_point(aes(x=pass.end_location.x , y=pass.end_location.y), color="green")+
  guides(color = FALSE, size = FALSE)
  # #point pre-start
  # geom_point(aes(x=43.82309 , y=64.30251, color="Red"))+
  # #move actor
  # geom_segment(aes(x=43.82309 , y=64.30251, xend = 42.7,  yend = 67.9, color="green"))+
  # xlab("Expected Pass Ratio") + ylab("Real Pass Ratio")


#autre version pour plotter les passes en fonction de la réussite ou non
ggplot(Spain_1, aes(x=ff_location.x, y=ff_location.y, label= teammate))+
  geom_point(aes(color=teammate))+
  geom_segment(aes(x=location.x ,y=location.y,xend=pass.end_location.x,yend=pass.end_location.y,color=keeper))+
  #geom_segment(aes(x=ff_location.x , y=ff_location.y, xend = 65,  yend = 67))
  # geom_text(size=3, vjust=-2)+
  # geom_blank()+ theme_bw()+
  # #point start
  geom_point(aes(x=location.x , y=location.y))+
  geom_point(aes(x=unlist(pass_player[,"ff_location.x"]),y=unlist(pass_player[,"ff_location.y"])),size=4)+
  # #point end
  geom_point(aes(x=pass.end_location.x , y=pass.end_location.y))
# #point pre-start
# geom_point(aes(x=43.82309 , y=64.30251, color="Red"))+
# #move actor
# geom_segment(aes(x=43.82309 , y=64.30251, xend = 42.7,  yend = 67.9, color="green"))+
# xlab("Expected Pass Ratio") + ylab("Real Pass Ratio")

View(events)
ffs = events %>%
  group_by(team.name) %>%
  filter(type.name=="Pass") %>%
  select(id, match_id, team.name, 
         player.name, position.name, type.name,
         location.x, location.y, pass.end_location.x,under_pressure,pass.height.name,
         pass.end_location.y, pass.recipient.name, freeze_frame,pass.outcome.name,pass.switch,pass.length,pass.cross,pass.miscommunication,pass.aerial_won)


ffs = ffs %>% unnest(freeze_frame) %>%
  mutate(ff_location.x = (map(location, 1)), ff_location.y = (map(location, 2))) %>%
  select(-location) %>%
  mutate(ff_location.x = as.numeric(ifelse(ff_location.x == "NULL", NA, ff_location.x)), 
         ff_location.y = as.numeric(ifelse(ff_location.y == "NULL", NA, ff_location.y)))


#All pass Spain Team
France<-filter(ffs, team.name=="France")

France_passes=France
#on conserve uniquement la première ligne qui conserne une passe
France_passes <-France_passes %>% slice(1) %>% 
  bind_rows( France_passes %>% filter(id!=lag(id)))

#on complète les données manquantes
for (i in 1:nrow(France_passes)){
   if (is.na(France_passes[i,"pass.outcome.name"])){
      France_passes[i,"pass.outcome.name"]="Unknown"
      France_passes[i,"pass.outcome.name"]=factor(France_passes[i,"pass.outcome.name"])
   }
  if (is.na(France_passes[i,"pass.switch"])){
    France_passes[i,"pass.switch"]=FALSE
  }
  if (is.na(France_passes[i,"pass.cross"])){
    France_passes[i,"pass.cross"]=FALSE
  }
  if (is.na(France_passes[i,"pass.miscommunication"])){
    France_passes[i,"pass.miscommunication"]=FALSE
  }
  if (is.na(France_passes[i,"pass.aerial_won"])){
    France_passes[i,"pass.aerial_won"]=FALSE
  } 
  if (is.na(France_passes[i,"under_pressure"])){
    France_passes[i,"under_pressure"]=FALSE
  } 
}

ggplot(data = France_passes) +
  geom_mosaic(aes(x=product(pass.outcome.name), fill= pass.switch)) + 
  scale_alpha_manual(values =c(.7,.9)) +
  ylab("True/false")+
  xlab("Variable")+
  ggtitle("Etude des caractéristiques des passes")


France_passes2=droplevels(France_passes[-which(France_passes$pass.outcome.name == "Unknown"), ] )

ggplot(data = France_passes2) +
  geom_mosaic(aes(x=product(pass.outcome.name), fill= pass.switch)) + 
  scale_alpha_manual(values =c(.7,.9)) +
  ylab("True/false")+
  xlab("Variable")+
  ggtitle("Etude des caractéristiques des passes")

ggplot(data = France_passes2) +
  geom_mosaic(aes(x=product(pass.outcome.name), fill= pass.miscommunication)) + 
  scale_alpha_manual(values =c(.7,.9)) +
  ylab("True/false")+
  xlab("Variable")+
  ggtitle("Etude des caractéristiques des passes")

ggplot(data = France_passes2) +
  geom_mosaic(aes(x=product(pass.outcome.name), fill= pass.cross)) + 
  scale_alpha_manual(values =c(.7,.9)) +
  ylab("True/false")+
  xlab("Variable")+
  ggtitle("Etude des caractéristiques des passes")

ggplot(data = France_passes2) +
  geom_mosaic(aes(x=product(pass.outcome.name), fill= under_pressure)) + 
  scale_alpha_manual(values =c(.7,.9)) +
  ylab("True/false")+
  xlab("Variable")+
  ggtitle("Etude des caractéristiques des passes")

ggplot(data = France_passes2) +
  geom_mosaic(aes(x=product(pass.outcome.name), fill= pass.height.name)) + 
  scale_alpha_manual(values =c(.7,.9)) +
  ylab("True/false")+
  xlab("Variable")+
  ggtitle("Etude des caractéristiques des passes")


ggplot(France_passes2, aes(x = "", y = pass.length, fill = under_pressure)) +
  geom_col() +
  coord_polar(theta = "y")

View(France_passes2)


