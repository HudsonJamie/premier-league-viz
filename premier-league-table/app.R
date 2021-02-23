#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)

source("scripts/prem_league_tables.R")
source("scripts/prem_league_lineplot.R")


# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel(h1("Premier League Table")),
  
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = "input.tabselected == 1",
        dateInput("date", 
                  h3("Date"),
                  min = min(data.edit$Date),
                  max = Sys.Date())),
      conditionalPanel(
        condition = "input.tabselected == 2",
        dateRangeInput("daterange",
                       h3("Date range"),
                       min = min(data.edit$Date),
                       max = Sys.Date())),
      conditionalPanel(
        condition = "input.tabselected == 3",
        checkboxGroupInput(inputId = "team",
                           label = h3("Choose a Team"),
                           choices = unique(tidy_weekly_df$Team),
                           selected = "Man United"))
    ),
    
    mainPanel(
      tabsetPanel(
        id = "tabselected",
        tabPanel("Table at set date", value = 1, DT::dataTableOutput('table')),
        tabPanel("Table between dates", value = 2, DT::dataTableOutput('table_1')),
        tabPanel("League position plot", value = 3, plotOutput("line")))
      )
    )
  )


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #date.until <- input$date #Year-month-day
    
    # Blank table 
    empty_table <- data.frame(Team = unique(data.edit$HomeTeam),
                              "Plyd", "Win")
    
    empty_table <- data.frame(matrix(ncol = 9, nrow = 20))
    x <- c("Team", "Plyd", "Win", "Draw", "Lose", "GoalsFor", "GoalsAgainst",
           "GoalDiff", "Pts")
    colnames(empty_table) <- x
    empty_table$Team <- unique(data.edit$HomeTeam)
    
    empty_table <- empty_table %>% mutate_if(is.logical,as.numeric) %>% 
        mutate_if(is.numeric,coalesce,0) %>% 
        arrange(desc(Pts), desc(GoalDiff), desc(GoalsFor), Team)
    rownames(empty_table) <- NULL
    
    # Calculate the Home data
    home.data <- reactive({
        home.data <- data.edit %>%
        # Remove rows with team names missing
        filter(!(is.na(HomeTeam) | (HomeTeam == "") 
                 | is.na(AwayTeam) | (AwayTeam == "")),
               Date <= input$date) %>%
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
    })
    
    away.data <- reactive({
        away.data <- data.edit %>%
            # Remove rows with team names missing
            filter(!(is.na(HomeTeam) | (HomeTeam == "") 
                     | is.na(AwayTeam) | (AwayTeam == "")),
                   Date <= input$date) %>%
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
    })
    
    # total.team_list <- as.data.frame(unique(data.edit$HomeTeam))
    # colnames(total.team_list) <- "Team"
# 
# 
#     week.team_list <- reactive(inner_join(home.data(), away.data(), by = "Team") %>%
#                                    select(unique("Team")))
#     setdiffsteams <- c(setdiff(total.team_list, week.team_list()))
    
    table.data <- reactive({
        full_join(home.data(), away.data(), by = "Team") %>%
      mutate_if(is.numeric,coalesce,0) %>%
      mutate(Plyd         = HPlyd + APlyd,
             Win          = HWin  + AWin,
             Draw         = HDraw + ADraw,
             Lose         = HLose + ALose,
             GoalsFor     = HFor  + AFor,
             GoalsAgainst = HAg   + AAg,
             GoalDiff     = GoalsFor - GoalsAgainst,
             Pts          = HPts  + APts) %>%
      select(Team, Plyd, Win, Draw, Lose, GoalsFor, GoalsAgainst, GoalDiff, Pts) %>%
      arrange(desc(Pts), desc(GoalDiff), desc(GoalsFor), Team)
    })

# table.data_1 <- reactive({
#     table.data_1 <- table.data() %>%
#         add_row(Team = setdiffsteams$Team) %>%
#         mutate_if(is.numeric,coalesce,0) %>%
#         arrange(desc(Pts), desc(GoalDiff), desc(GoalsFor), Team)
# })
    
# table.data_df <- as.data.frame(table.data())
#     
# table.data_1 <-reactive({
#     rbind(table.data(), empty_table[!empty_table$Team %in% table.data()$Team,])
#     
# })
# 

    total_table <- reactive({
        rbind(table.data(), empty_table[!empty_table$Team %in% table.data()$Team,],
              make.row.names = F) %>%
            arrange(desc(Pts), desc(GoalDiff), desc(GoalsFor), Team)
    })
    
    # Blank table 
    empty_table_daterange <- data.frame(Team = unique(data.edit$HomeTeam),
                                        "Plyd", "Win")
    
    empty_table_daterange <- data.frame(matrix(ncol = 9, nrow = 20))
    x <- c("Team", "Plyd", "Win", "Draw", "Lose", "GoalsFor", "GoalsAgainst",
           "GoalDiff", "Pts")
    colnames(empty_table_daterange) <- x
    empty_table_daterange$Team <- unique(data.edit$HomeTeam)
    
    empty_table_daterange <- empty_table_daterange %>% mutate_if(is.logical,as.numeric) %>% 
      mutate_if(is.numeric,coalesce,0) %>% 
      arrange(desc(Pts), desc(GoalDiff), desc(GoalsFor), Team)
    rownames(empty_table_daterange) <- NULL
    
    # Calculate the Home data
    home.data_daterange <- reactive({
      home.data_daterange <- data.edit %>%
        # Remove rows with team names missing
        filter(!(is.na(HomeTeam) | (HomeTeam == "") 
                 | is.na(AwayTeam) | (AwayTeam == "")),
               Date <= input$daterange[2] & Date > input$daterange[1]) %>%
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
    })
    
    away.data_daterange <- reactive({
      away.data_daterange <- data.edit %>%
        # Remove rows with team names missing
        filter(!(is.na(HomeTeam) | (HomeTeam == "") 
                 | is.na(AwayTeam) | (AwayTeam == "")),
               Date <= input$daterange[2] & Date > input$daterange[1]) %>%
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
    })
    
    # total.team_list <- as.data.frame(unique(data.edit$HomeTeam))
    # colnames(total.team_list) <- "Team"
    # 
    # 
    #     week.team_list <- reactive(inner_join(home.data_daterange(), away.data_daterange(), by = "Team") %>%
    #                                    select(unique("Team")))
    #     setdiffsteams <- c(setdiff(total.team_list, week.team_list()))
    
    table.data_daterange <- reactive({
      full_join(home.data_daterange(), away.data_daterange(), by = "Team") %>%
        mutate_if(is.numeric,coalesce,0) %>%
        mutate(Plyd         = HPlyd + APlyd,
               Win          = HWin  + AWin,
               Draw         = HDraw + ADraw,
               Lose         = HLose + ALose,
               GoalsFor     = HFor  + AFor,
               GoalsAgainst = HAg   + AAg,
               GoalDiff     = GoalsFor - GoalsAgainst,
               Pts          = HPts  + APts) %>%
        select(Team, Plyd, Win, Draw, Lose, GoalsFor, GoalsAgainst, GoalDiff, Pts) %>%
        arrange(desc(Pts), desc(GoalDiff), desc(GoalsFor), Team)
    })
    
    total_table_daterange <- reactive({
      rbind(table.data_daterange(), empty_table_daterange[!empty_table_daterange$Team %in% table.data_daterange()$Team,],
            make.row.names = F) %>%
        arrange(desc(Pts), desc(GoalDiff), desc(GoalsFor), Team)
    })
    
    
    output$table <- DT::renderDataTable(
      DT::datatable(total_table(), list(pageLength = 20)))
    
    output$table_1 <- DT::renderDataTable(
      DT::datatable(total_table_daterange(), list(pageLength = 20))


)
    colours <- c('#EF0107','#670E36','#0057B8','#6C1D45','#034694','#1B458F','#003399','#000000','#FFCD00','#003090','#C8102E','#6CABDD','#DA291C','#241F20', "#EE2737", "#D71920", "#132257", "#122F67", "#7A263A", "#FDB913")
    teams <- c(unique(tidy_weekly_df$Team))
    
    team_col_pal <- as.data.frame(cbind(teams, colours))
    
    
    
    output$line <- renderPlot({
      
      colpal <- team_col_pal %>% 
        filter(teams %in% input$team)
      
      tidy_weekly_filtered <- tidy_weekly_df %>% 
        filter(Team %in% input$team)
      
      ggplot(tidy_weekly_filtered, 
             aes(x = date, y = position, group = Team)) + 
        geom_line(aes(colour = Team), size = 0.5) + 
        geom_point(aes(colour = Team), size = 2) +
        scale_y_continuous(trans = "reverse", breaks = seq(1,20,1), labels = seq(1,20,1), limits = c(20, 1)) +
        theme_minimal() +
        scale_x_date(date_breaks = "1 week", date_labels = "%d-%m-%y") +
        theme(panel.grid.minor = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title = element_text(size = 14),
              axis.text.y = element_text(size = 14),
              axis.text.x = element_text(angle = 50),
              legend.title = element_text(size=14),
              legend.text = element_text(size=13)) +
        labs(title = paste0("Premier League Positions"), x = 'Week', y = 'Position', subtitle = paste0('Week ',max(tidy_weekly_df$date))) +
        scale_colour_manual(values = colpal$colours)
    }, height = 700)
}

# Run the application 
shinyApp(ui = ui, server = server)
