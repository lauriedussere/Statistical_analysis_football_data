# Statistical analysis of football data

Nowadays, football clubs often use statistical tools to evaluate the performance of team players but also to determine the strategies for future matches.
The goal of this project is to identify new performance indicators for football players and football teams.

We used data from the website https://statsbomb.com/ which gives a free access to football data concerning all matches from a competition (Euro 2020). Data decsribe all actions which happenned during a match. The actions are characterized by several variables such as : 
- type of the actions = pass, shot, ball receipt...
- time at which the action occurs 
- players involved in the action
- characteristics of the action : for example if it is a pass we know the angle, the length, if it is completed or not...
- freeze frame : it is a picture of an exact moment of a match, which allows us to know the exact position of the player in possession of the ball but also the position of all the other players that are on the camera range at time of the action. 

More precisely, our work focused on studying the passes and identifying the ones that break lines of players. By identifying the line breaking passes, we were able to evaluate the performance of each player. To evaluate the performance we took into account the number of lines he broke during a match (and more generally a competition) but also by counting the number of players eliminated by his passes. 
 
