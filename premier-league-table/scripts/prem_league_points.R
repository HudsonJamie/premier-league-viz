library(tidyverse)
library(ggnewscale)
library(showtext)
font_add_google("Source Sans Pro")
font_add_google("Lato")
showtext_auto(enable = TRUE)

table_points <- matrix(ncol = length(date_seq), nrow = length(unique(data.edit$HomeTeam)))

for (i in 1:length(date_seq)){
  
  # Calculate the Home data
  home.data <- data.edit %>%
    # Remove rows with team names missing
    filter(!(is.na(HomeTeam) | (HomeTeam == "")
             | is.na(AwayTeam) | (AwayTeam == "")),
           Date <= date_seq[i]) %>%
    # Calculate League Points for home team
    mutate(HWin  = calculate.win(FTHG, FTAG),
           HDraw = calculate.draw(FTHG, FTAG),
           HLose = calculate.lose(FTHG, FTAG),
           HPts  = calculate.points(FTHG, FTAG)) %>%
    # Create Home Team table entry
    group_by(HomeTeam) %>% summarise(HPlyd = length(HomeTeam),
                                     HWin  = sum(HWin),
                                     HDraw = sum(HDraw),
                                     HLose = sum(HLose),
                                     HFor  = sum(FTHG),
                                     HAg   = sum(FTAG),
                                     HPts  = sum(HPts)) %>%
    rename(Team = HomeTeam)
  
  away.data <- data.edit %>%
    # Remove rows with team names missing
    filter(!(is.na(HomeTeam) | (HomeTeam == "")
             | is.na(AwayTeam) | (AwayTeam == "")),
           Date <= date_seq[i]) %>%
    # Calculate League Points for away team
    mutate(AWin  = calculate.win(FTAG, FTHG),
           ADraw = calculate.draw(FTAG, FTHG),
           ALose = calculate.lose(FTAG, FTHG),
           APts  = calculate.points(FTAG, FTHG)) %>%
    # Create Away Team table entry
    group_by(AwayTeam) %>% summarise(APlyd = length(AwayTeam),
                                     AWin  = sum(AWin),
                                     ADraw = sum(ADraw),
                                     ALose = sum(ALose),
                                     AFor  = sum(FTAG),
                                     AAg   = sum(FTHG),
                                     APts  = sum(APts)) %>%
    rename(Team = AwayTeam)
  
  table.data_points <- full_join(home.data, away.data, by = "Team") %>%
    mutate_if(is.numeric,coalesce,0) %>%
    mutate(Plyd         = HPlyd + APlyd,
           Win          = HWin  + AWin,
           Draw         = HDraw + ADraw,
           Lose         = HLose + ALose,
           GoalsFor     = HFor  + AFor,
           GoalsAgainst = HAg   + AAg,
           GoalDiff     = GoalsFor - GoalsAgainst,
           Pts          = HPts  + APts) %>% 
    select(Team, Plyd, Pts, GoalDiff, GoalsFor) %>% 
    arrange(desc(Pts), desc(GoalDiff), desc(GoalsFor), Team) %>% 
    mutate(Pos = row_number()) %>% 
    arrange(Team)
  
  total_table_points <-  rbind(table.data_points, empty_table[!empty_table$Team %in% table.data_points$Team,],
                        make.row.names = F) %>%
    arrange(desc(Pts), desc(GoalDiff), desc(GoalsFor), Team) %>% 
    mutate(Pos = row_number()) %>% 
    arrange(Team)
  
  table_points[,i] <- total_table_points$Pts
}

table_df_points <- as.data.frame(table_points)
colnames(table_df_points) <- as.character(date_seq)
table_df_points <- table_df_points %>% add_column(Team = total_table_points$Team, .before = 1)
head(table_df_points)

col_index <- seq(1:ncol(table_df_points)) 

table_df_points_long <- table_df_points %>% pivot_longer(c(- "Team"), names_to = "date", values_to = "points")
table_df_points_long$date <- as.Date(table_df_points_long$date)
dayz <- lubridate::wday(table_df_points_long$date, label = TRUE)
dayz <- lubridate::wday(as.Date(colnames(table_df_points)[-1]), label = TRUE)

as.Date(colnames(table_df_points)[-1])

weekly_df_points <- table_df_points %>%
  select("Team", col_index[dayz == "Sun"] + 1) # Plus 1 because our first column is Team



tidy_weekly_df_points <- weekly_df_points %>% pivot_longer(c(- "Team"), names_to = "date", values_to = "points")
tidy_weekly_df_points$date <-  as.Date.character(tidy_weekly_df_points$date)

ggplot(tidy_weekly_df_points, aes(x = date, y = points, group = Team)) + 
  geom_line(aes(colour = Team), size = 0.7) + 
  scale_colour_manual(values = c('#EF0107','#670E36', '#e30613', '#0057B8','#6C1D45','#034694','#1B458F','#003399','#FFCD00','#003090','#C8102E','#6CABDD','#DA291C','#241F20', "#FFF200", "#D71920", "#132257", "#FBEE23", "#7A263A", "#FDB913")) +
  new_scale_color() +
  geom_point(aes(colour = Team, fill = Team), size = 5, shape = 21,
             stroke = 1.3) +
  scale_y_continuous(breaks = seq(0,(max(tidy_weekly_df_points$points)+5),5), labels = seq(0,(max(tidy_weekly_df_points$points) + 5),5)) +
  theme_minimal() +
  scale_x_date(breaks = seq(min(tidy_weekly_df_points$date),max(tidy_weekly_df_points$date), by="1 week"), date_labels = "%d-%b-%y") +
  theme(panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_text(size = 14,
                                  family = "Source Sans Pro"),
        axis.text.y = element_text(size = 12,
                                   family = "Source Sans Pro"),
        axis.text.x = element_text(angle = 50,
                                   family = "Source Sans Pro"),
        panel.grid.major.x = element_blank(),
        legend.title =  element_blank(),
        legend.text = element_text(size=13,
                                   family = "Source Sans Pro"),
        plot.title = element_text(size=20,
                                  family = "Source Sans Pro"),
        plot.subtitle = element_text(size = 14,
                                     family = "Source Sans Pro")) +
  labs(title = paste0("Premier League Points"), x = 'Week ending', y = 'Points', subtitle = paste0("Points on Sundays")) +
  scale_fill_manual(values = c('#EF0107','#670E36', '#e30613', '#0057B8','#6C1D45','#034694','#1B458F','#003399','#FFCD00','#003090','#C8102E','#6CABDD','#DA291C','#241F20', "#FFF200", "#D71920", "#132257", "#FBEE23", "#7A263A", "#FDB913")) +
  scale_colour_manual(values = c('#F8F0E3','#95BFE5', '#F8F0E3', '#F8F0E3','#99D6EA','#F8F0E3','#C4122E','#F8F0E3','#1D428A','#003090','#00B2A9','#1C2C5B','#000000','#F8F0E3', "#00A650", "#F8F0E3", "#F8F0E3", "#ED2127", "#1BB1E7", "#231F20"))

## https://teamcolorcodes.com/soccer/premier-league-color-codes/


