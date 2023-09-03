# 
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("tidyr")

library(dplyr)
library(ggplot2)
library(tidyr)

#Importing the data--------------------------------------------------------------
# Import the 'deliveries.csv' data
data <- read.csv("C:/Users/User/Desktop/Data Analyst/R project/Indian Premier League(Cricket)/deliveries.csv")

# Import the 'matches.csv' data
matches <- read.csv("C:/Users/User/Desktop/Data Analyst/R project/Indian Premier League(Cricket)/matches.csv")

# head(data)
# head(matches)

#Clean the data -------------------------------------------------------------
# Create a data frame with unique batting teams
teams <- data %>% 
  select(batting_team) %>%
  distinct()

# Rename the column to "team"
teams <- rename(teams, team = batting_team)

# Remove the header
teams <- teams[-1, ]  # This removes the first row (header)

# Define the s_team vector with the same number of values as unique teams
s_team <- c("KKR", "RCB", "CSK", "KXIP", "RR", "DD", "MI", "DC", "KTK", "PWI", "SRH", "RPS", "GL")

teams <- cbind(teams, s_team)

print(teams)

player_of_match <- matches%>% select(id,player_of_match,season) %>%
  distinct()

player_of_match <- rename(player_of_match, player=player_of_match)

Season <- data.frame(season=c(2008,2009,2010,2011,2012,2013,2014,2015,2016),T_winner=c("Rajasthan Royals","Deccan Chargers","Chennai Super Kings","Chennai Super Kings","Kolkata Knight Riders","Mumbai Indians","Kolkata Knight Riders","Mumbai Indians","Sunrisers Hyderabad"))

glimpse(matches)
matches$city[matches$city==""] <- "Dubai"

venue_city <- matches %>%
              select(city) %>% 
              distinct()

#Run rate is high at the end of 1st innings compared to 2nd innings----------------------------------------
runs_o <- data%>%
  left_join(matches, by=c("match_id"="id")) %>% 
  group_by(season,inning,over) %>% 
  summarize(no=n(),runs =sum(total_runs)) %>% 
  mutate(avg=runs/no) %>% 
  filter(inning!=3,inning!=4) %>% 
  rename(run_rate = avg)


#visualize the data
ggplot(runs_o,aes(x=season, y=run_rate,colour=over, group=over))+
  geom_line(show.legend = TRUE, size =1.25,linetype=1)+
  theme(legend.position="bottom")+
  theme(legend.direction = "horizontal") +
  facet_wrap(~inning)+
  scale_y_continuous(name="average runs per ball")+
  scale_x_discrete(name="season",limits=c(2008,2009,2010,2011,2012,2013,2014,2015,2016))+
  ggtitle("Average runs per ball by over each season and innings")

#Most batsmen are dismissed being caught
dismissal <- data %>% 
  group_by(dismissal_kind) %>% 
  summarize(total_count = n()) %>% 
  filter(dismissal_kind!= "") %>% 
  arrange(desc(total_count))

# Create a bar plot showing the total_count
ggplot(data = dismissal, aes(x = reorder(dismissal_kind, -total_count), y = total_count)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # To make it a horizontal bar plot
  labs(
    x = "Dismissal Kind",
    y = "Total Count",
    title = "Dismissal Types Frequency"
  ) +
  theme_minimal()  # Optional: Choose a theme

#Runs scored by tournament winners in each season-----------------
tournament_winner_runs <- data %>%
  left_join(matches, by = c("match_id" = "id")) %>%
  semi_join(Season, by = c("season" = "season", "batting_team" = "T_winner")) %>%
  #left_join(teams, by = c("batting_team" = "team")) %>%
  group_by(season, batting_team) %>%
  summarize(runs = sum(total_runs))

ggplot(tournament_winner_runs, aes(x = season, y = runs, fill = batting_team)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Season",
    y = "Total Runs",
    fill = "Batting Team",
    title = "Total Runs Scored by Batting Teams in Each Season"
  ) +
  theme_minimal() +
  theme(legend.position = "top")  


# wickets taken by tournament winners in each season---------------------------------------
ggplot(tournament_wickets, aes(x = season, y = total_wickets, fill = bowling_team)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    x = "Season",
    y = "Total Wickets",
    fill = "Bowling Team",
    title = "Total Wickets Taken by Tournament Winners in Each Season"
  ) +
  theme_minimal() +
  theme(legend.position = "top")


#Highest run scorers in each season: #virat Kohli had the highest runs-------------------------------
highest_run_scorer <- data%>% 
    left_join(matches, by = c("match_id" = "id")) %>%
    group_by(season, batsman) %>% 
    summarise(total_runs = sum(total_runs)) %>% 
    top_n(1, wt = total_runs)

ggplot(highest_run_scorer, aes(x = season, y = total_runs, fill = batsman)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Season",
    y = "Total Runs",
    fill = "Batsman",
    title = "Highest Run-Scorer in Each Season"
  ) +
  theme_minimal() +
  theme(legend.position = "top")  # Optional: Adjust the legend position    

#Highest Wicket takers in each season
highest_wicket_taker <- data%>% 
  left_join(matches, by = c("match_id" = "id")) %>%
  group_by(season, bowler) %>% 
  mutate(wicket_taken = ifelse(!is.na(player_dismissed), 1, 0)) %>%
  summarize(total_wickets = sum(wicket_taken)) %>% 
  top_n(1, wt = total_wickets)

#Players with most player of Match awards in each season
most_potm <- player_of_match %>% 
        group_by(season, player) %>% 
        summarize(total_count = n()) %>% 
  top_n(1, wt = total_count)


ggplot(most_potm, aes(x = season, y = total_count, fill = player)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Season",
    y = "Total Player of the Match Awards",
    fill = "Player",
    title = "Players with Most Player of the Match Awards in Each Season"
  ) +
  theme_minimal() +
  theme(legend.position = "top") +  # Optional: Adjust the legend position
  scale_x_continuous(
    breaks = seq(2008, 2017, by = 1),  # Specify the breaks (all seasons from 2008 to 2017)
    labels = seq(2008, 2017, by = 1)   # Labels for the breaks
  )

#Total runs scored by each team across all seasons
ttlruns_team <-  data%>% 
  left_join(matches, by = c("match_id" = "id")) %>% 
  group_by(batting_team) %>% 
  summarize(total_runs = sum(total_runs)) %>% 
  arrange(desc(total_runs))


ggplot(ttlruns_team, aes(x = batting_team, y = total_runs)) +
  geom_bar(stat = "identity", fill = "green") +
  labs(
    x = "Batting Team",
    y = "Total Runs",
    title = "Total Runs Scored by Each Batting Team"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

#Total wickets taken by each team across all seasons.  
ttlwickets_team <-  data%>% 
  left_join(matches, by = c("match_id" = "id")) %>% 
  group_by(bowling_team) %>% 
  mutate(wicket_taken = ifelse(!is.na(player_dismissed), 1, 0)) %>%
  summarize(total_wickets = sum(wicket_taken)) %>% 
  arrange(desc(total_wickets))

ttlwickets_team$bowling_team <- factor(
  ttlwickets_team$bowling_team,
  levels = ttlwickets_team$bowling_team[order(-ttlwickets_team$total_wickets)]
)

# Create a bar plot with reordered levels
ggplot(ttlwickets_team, aes(x = bowling_team, y = total_wickets)) +
  geom_bar(stat = "identity", fill = "salmon") +
  labs(
    x = "Bowling Team",
    y = "Total Wickets",
    title = "Total Wickets Taken by Each Bowling Team (Descending Order)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

#Highest runs scorers across all seasons.
highest_run_scorer_allseason <- data%>% 
  left_join(matches, by = c("match_id" = "id")) %>%
  group_by(batsman) %>% 
  summarise(total_runs = sum(total_runs)) %>% 
  arrange(desc(total_runs))

#Highest wicket takers across all seasons.
highest_wicket_allseason <- data%>% 
  left_join(matches, by = c("match_id" = "id")) %>%
  group_by(bowler) %>% 
  mutate(wicket_taken = ifelse(!is.na(player_dismissed), 1, 0)) %>%
  summarize(total_wickets = sum(wicket_taken)) %>% 
  arrange(desc(total_wickets))


#All Cities with Average runs, Average wickets per match and matches played.
cities_info <- data %>%
  left_join(matches, by = c("match_id" = "id")) %>%
  mutate(wicket_taken = ifelse(!is.na(player_dismissed), 1, 0)) %>%
  group_by(city) %>%
  summarize(
    avg_runs = mean(total_runs),
    total_matches = n(),
    avg_wickets_per_match = mean(wicket_taken) / total_matches
  )

ggplot(cities_info, aes(x = city, y = avg_runs)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(
    x = "City",
    y = "Average Runs",
    title = "Average Runs Scored per Match by City"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Create a bar plot for total matches
ggplot(cities_info, aes(x = city, y = total_matches)) +
  geom_bar(stat = "identity", fill = "salmon") +
  labs(
    x = "City",
    y = "Total Matches",
    title = "Total Matches Played in Each City"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Create a bar plot for average wickets per match
ggplot(cities_info, aes(x = city, y = avg_wickets_per_match)) +
  geom_bar(stat = "identity", fill = "gold") +
  labs(
    x = "City",
    y = "Average Wickets per Match",
    title = "Average Wickets Taken per Match by City"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#FUTURE STEPS------------------------------------------------------------------------------------
#Strike rate of all batsman 
#Economy rate for all bowlers
#Players with most number of catches and run outs.
#Players scoring most number of runs between 50 and 99 in a inning.
#Players scoring most number of runs above 100 in a inning.
#Runs scored by each team in 1s, 2s, 3s, 4s,and 6s.

