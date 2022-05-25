library(tidyverse)
library(showtext)
font_add_google("Source Sans Pro")
showtext_auto(enable = TRUE)



# data.matches <- read.csv(data.file, header = TRUE)
names(data.matches)

data.edit <- data.matches %>% select(HomeTeam, AwayTeam, FTHG, FTAG, Date)
head(data.edit)
data.edit$Date <- as.Date(data.edit$Date, "%d/%m/%Y")

calculate.win <- function(ourScore, theirScore){
  return(ourScore > theirScore)
}

calculate.lose <- function(ourScore, theirScore){
  return(ourScore < theirScore)
}

calculate.draw <- function(ourScore, theirScore){
  return(ourScore == theirScore)
}

calculate.points <- function(ourScore, theirScore){
  return(ifelse(ourScore < theirScore, 0, ifelse(ourScore == theirScore, 1, 3)))
}

# Blank table 
empty_table <- data.frame(Team = unique(data.edit$HomeTeam),
                          "Plyd", "Win")

empty_table <- data.frame(matrix(ncol = 6, nrow = 20))
x <- c("Team", "Plyd", "Pts", "GoalDiff", "GoalsFor", "Pos")
colnames(empty_table) <- x
empty_table$Team <- unique(data.edit$HomeTeam)

empty_table <- empty_table %>% mutate_if(is.logical,as.numeric) %>% 
  mutate_if(is.numeric,coalesce,0) %>% 
  arrange(desc(Pts), desc(GoalDiff), desc(GoalsFor), Team)
rownames(empty_table) <- NULL

date_seq <- seq(min(data.edit$Date), max(data.edit$Date), "days")
table <- matrix(ncol = length(date_seq), nrow = length(unique(data.edit$HomeTeam)))

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

table.data <- full_join(home.data, away.data, by = "Team") %>%
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

total_table <-  rbind(table.data, empty_table[!empty_table$Team %in% table.data$Team,],
        make.row.names = F) %>%
    arrange(desc(Pts), desc(GoalDiff), desc(GoalsFor), Team) %>% 
  mutate(Pos = row_number()) %>% 
  arrange(Team)

table[,i] <- total_table$Pos
}

table_df <- as.data.frame(table)
colnames(table_df) <- as.character(date_seq)
table_df <- table_df %>% add_column(Team = total_table$Team, .before = 1)
head(table_df)

col_index <- seq(1:ncol(table_df)) 

table_df_long <- table_df %>% pivot_longer(c(- "Team"), names_to = "date", values_to = "position")
table_df_long$date <- as.Date(table_df_long$date)
dayz <- lubridate::wday(table_df_long$date, label = TRUE)
dayz <- lubridate::wday(as.Date(colnames(table_df)[-1]), label = TRUE)

as.Date(colnames(table_df)[-1])

weekly_df <- table_df %>%
  select("Team", col_index[dayz == "Sun"] + 1) # Plus 1 because our first column is Team



tidy_weekly_df <- weekly_df %>% pivot_longer(c(- "Team"), names_to = "date", values_to = "position")
tidy_weekly_df$date <-  as.Date.character(tidy_weekly_df$date)

ggplot(tidy_weekly_df, aes(x = date, y = position, group = Team)) + 
  geom_line(aes(colour = Team), size = 0.5) + 
  geom_point(aes(colour = Team), size = 5) +
  scale_y_continuous(trans = "reverse", breaks = seq(1,20,1), labels = seq(1,20,1), limits = c(20, 1)) +
  theme_minimal() +
  scale_x_date(breaks = seq(min(tidy_weekly_df$date),max(tidy_weekly_df$date), by="1 week"), date_labels = "%d-%m-%y") +
  theme(panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(angle = 50),
        legend.title = element_text(size=14),
        legend.text = element_text(size=13)) +
  labs(title = paste0("Premier League Positions"), x = 'Week', y = 'Position', subtitle = paste0("Position taken on Sundays (sorry Monday Night Football \'game in hands\')")) +
  scale_colour_manual(values = c('#EF0107','#670E36','#0057B8','#6C1D45','#034694','#1B458F','#003399','#000000','#FFCD00','#003090','#C8102E','#6CABDD','#DA291C','#241F20', "#EE2737", "#D71920", "#132257", "#122F67", "#7A263A", "#FDB913"))
## https://teamcolorcodes.com/soccer/premier-league-color-codes/

ggplot(tidy_weekly_df, aes(x = date, y = position, group = Team)) +
  geom_bump(aes(colour = Team), smooth = 10) +
  scale_colour_manual(values = c('#EF0107','#670E36', '#e30613', '#0057B8','#6C1D45','#034694','#1B458F','#003399','#FFCD00','#003090','#C8102E','#6CABDD','#DA291C','#241F20', "#FFF200", "#D71920", "#132257", "#FBEE23", "#7A263A", "#FDB913")) +
  new_scale_color() +
  geom_point(aes(colour = Team, fill = Team), size = 5, shape = 21,
             stroke = 1.3) +
  scale_y_continuous(trans = "reverse", breaks = seq(1,20,1), labels = seq(1,20,1), limits = c(20, 1)) +
  theme_minimal() +
  scale_x_date(breaks = seq(min(tidy_weekly_df$date),max(tidy_weekly_df$date), by="1 week"), date_labels = "%d-%b-%y") +
  theme(panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_text(size = 14,
                                  family = "Source Sans Pro"),
        axis.text.y = element_text(size = 12,
                                   family = "Source Sans Pro"),
        axis.text.x = element_text(angle = 50,
                                   family = "Source Sans Pro"),
        panel.grid.major = element_blank(),
        legend.title =  element_blank(),
        legend.text = element_text(size=13,
                                   family = "Source Sans Pro"),
        plot.title = element_text(size=20,
                                  family = "Source Sans Pro"),
        plot.subtitle = element_text(size = 14,
                                     family = "Source Sans Pro")) +
  labs(title = paste0("Premier League Positions"), x = 'Week ending', y = 'Position', subtitle = paste0("Position taken on Sundays")) +
  scale_fill_manual(values = c('#EF0107','#670E36', '#e30613', '#0057B8','#6C1D45','#034694','#1B458F','#003399','#FFCD00','#003090','#C8102E','#6CABDD','#DA291C','#241F20', "#FFF200", "#D71920", "#132257", "#FBEE23", "#7A263A", "#FDB913")) +
  scale_colour_manual(values = c('#F8F0E3','#95BFE5', '#F8F0E3', '#F8F0E3','#99D6EA','#F8F0E3','#C4122E','#F8F0E3','#1D428A','#003090','#00B2A9','#1C2C5B','#000000','#F8F0E3', "#00A650", "#F8F0E3", "#F8F0E3", "#ED2127", "#1BB1E7", "#231F20"))
## https://teamcolorcodes.com/soccer/premier-league-color-codes/
