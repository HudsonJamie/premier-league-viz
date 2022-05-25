#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(gt)
library(shinyWidgets)
library(shinythemes)
library(gtExtras)
library(ggbump)
library(ggnewscale)

source("scripts/prem_league_tables.R")
source("scripts/prem_league_lineplot.R")
source("scripts/prem_league_points.R")

css <- '.nav-tabs>li>a {
  color: black;
}'

# Define UI for application
ui <- navbarPage("Premier League Table 2021/2022",
                 tabPanel("Table at set date",
                          fluidPage( 
                                              # h1(id="big-heading", "Premier League Table 2021/2022"),
  tags$link(href = "https://fonts.googleapis.com/css?family=Lato", rel = "stylesheet"),
  tags$style(HTML('
      * {
        font-family: Lato;
      })')),
  theme = shinytheme("yeti"),
  # shinythemes::themeSelector(),
  
  sidebarLayout(
    sidebarPanel(
      # conditionalPanel(
        # condition = "input.tabselected == 1",
        dateInput("date",
                  h5("Date:"),
                  value = max(data.edit$Date),
                  min = min(data.edit$Date),
                  max = Sys.Date()),
      # conditionalPanel(
      #   condition = "input.tabselected == 2",
      #   dateRangeInput("daterange",
      #                  h5("Date range:"),
      #                  start = min(data.edit$Date),
      #                  end = Sys.Date(),
      #                  min = min(data.edit$Date),
      #                  max = Sys.Date())),
      # conditionalPanel(
      #   condition = "input.tabselected == 3",
      #   pickerInput(
      #     inputId = "team",
      #     label = h5("Choose a Team"),
      #     choices = unique(tidy_weekly_df$Team),
      #     options = list(
      #       `actions-box` = TRUE,
      #       size = 10,
      #       `selected-text-format` = "count > 3"
      #     ),
      #     multiple = TRUE,
      #     selected = "Man United"
      #   )),
      # conditionalPanel(
      #   condition = "input.tabselected == 4",
      #   pickerInput(
      #     inputId = "team_1",
      #     label = h5("Choose a Team"),
      #     choices = unique(tidy_weekly_df$Team),
      #     options = list(
      #       `actions-box` = TRUE,
      #       size = 10,
      #       `selected-text-format` = "count > 3"
      #     ),
      #     multiple = TRUE,
      #     selected = "Man United"
      #   ))
    ),
    
    mainPanel(gt_output(outputId = 'table'))
    #   tabsetPanel(
    #     id = "tabselected",
    #     tabPanel("Table at set date", value = 1, gt_output(outputId = 'table')),
    #     tabPanel("Table between dates", value = 2, gt_output(outputId = 'table_1')),
    #     tabPanel("League position plot", value = 3, plotOutput("line")),
    #     tabPanel("Total points plot", value = 4, plotOutput("line_1"))),
    #   tags$head(tags$style(HTML(css)))
    # )
  )
)),
tabPanel("Table between dates", 
         fluidPage(
  sidebarLayout(
    sidebarPanel(
      # conditionalPanel(
        # condition = "input.tabselected == 2",
        dateRangeInput("daterange",
                       h5("Date range:"),
                       start = min(data.edit$Date),
                       end = Sys.Date(),
                       min = min(data.edit$Date),
                       max = Sys.Date())),
    mainPanel(
  # tabsetPanel(
    # id = "tabselected",
    gt_output(outputId = 'table_1')
    )
  )
)),

tabPanel("League position plot", 
         fluidPage(

  sidebarLayout(
    sidebarPanel(
      # conditionalPanel(
      #   condition = "input.tabselected == 3",
        pickerInput(
          inputId = "team",
          label = h5("Choose a Team"),
          choices = unique(tidy_weekly_df$Team),
          options = list(
            `actions-box` = TRUE,
            size = 10,
            `selected-text-format` = "count > 3"
          ),
          multiple = TRUE,
          selected = "Man United")),
    mainPanel(
      plotOutput("line")
      )
        # tabsetPanel(
        #   id = "tabselected",
          # tabPanel("League position plot", value = 3, gt_output(outputId = 'line')))
      ))),

tabPanel("Total points plot", 
         fluidPage(

  sidebarLayout(
    sidebarPanel(
      # conditionalPanel(
      #   condition = "input.tabselected == 4",
        pickerInput(
          inputId = "team_1",
          label = h5("Choose a Team"),
          choices = unique(tidy_weekly_df$Team),
          options = list(
            `actions-box` = TRUE,
            size = 10,
            `selected-text-format` = "count > 3"
          ),
          multiple = TRUE,
          selected = "Man United"
        )),
    mainPanel(
      plotOutput('line_1')
      )
      # tabsetPanel(
      #   id = "tabselected",
      #   tabPanel("Total points plot", value = 4, gt_output(outputId = 'line_1')))
    ))))

# Define server logic 
server <- function(input, output) {
    

  home.data <- reactive({
    data.edit %>%
      filter(!(is.na(HomeTeam) | (HomeTeam == "") 
               | is.na(AwayTeam) | (AwayTeam == "")),
             Date <= input$date) %>%
      mutate(HWin  = calculate.win(FTHG, FTAG),
             HDraw = calculate.draw(FTHG, FTAG),
             HLose = calculate.lose(FTHG, FTAG),
             HPts  = calculate.points(FTHG, FTAG)) %>%
      group_by(HomeTeam) %>% summarise(HPlyd = length(HomeTeam),
                                       HWin  = sum(HWin),
                                       HDraw = sum(HDraw),
                                       HLose = sum(HLose),
                                       HFor  = sum(FTHG), 
                                       HAg   = sum(FTAG),
                                       HPts  = sum(HPts)) %>%
      rename(Team = HomeTeam) %>% 
      ungroup()
  })

  away.data <- reactive({
    data.edit %>%
      filter(!(is.na(HomeTeam) | (HomeTeam == "")
               | is.na(AwayTeam) | (AwayTeam == "")),
             Date <= input$date) %>%
      mutate(AWin  = calculate.win(FTAG, FTHG),
             ADraw = calculate.draw(FTAG, FTHG),
             ALose = calculate.lose(FTAG, FTHG),
             APts  = calculate.points(FTAG, FTHG)) %>%
      group_by(AwayTeam) %>% summarise(APlyd = length(AwayTeam),
                                       AWin  = sum(AWin),
                                       ADraw = sum(ADraw),
                                       ALose = sum(ALose),
                                       AFor  = sum(FTAG),
                                       AAg   = sum(FTHG),
                                       APts  = sum(APts)) %>%
      rename(Team = AwayTeam) %>%
      ungroup()
  })

  games_df <- reactive({
    data.edit %>%
      filter(Date <= input$date) %>%
      pivot_longer(contains('Team'), names_to = 'home_away', values_to = 'team', names_prefix = 'team_') %>%
      mutate(
        result = FTHG - FTAG,
        result = ifelse(home_away == 'HomeTeam', result, -result),
        win = ifelse(result == 0 , 0.5, ifelse(result > 0, 1, 0))
      ) %>%
      select(Date, team, win) %>%
      group_by(team) %>%
      summarise(
        Form = (list(win)), .groups = "drop") %>%
      ungroup()
  })

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

  final_df <- reactive({
    table.data() %>%
    full_join(games_df(), by = c("Team" = "team")) %>%
    arrange(desc(Pts), desc(GoalDiff), desc(GoalsFor), Team) %>%
    mutate(Form = map(Form, rev),
           Form = map(Form, head, 5),
           Pos = row_number(), .before = Team) %>%
    add_column(info = c(rep("Champions League", 4),
                        rep("Europa League", 1),
                        rep("", 12),
                        rep("Relagation zone", 3))) 
  })
  
  home.data_daterange <- reactive({
    data.edit %>%
      filter(!(is.na(HomeTeam) | (HomeTeam == "") 
               | is.na(AwayTeam) | (AwayTeam == "")),
             Date <= input$daterange[2] & Date >= input$daterange[1]) %>%
      mutate(HWin  = calculate.win(FTHG, FTAG),
             HDraw = calculate.draw(FTHG, FTAG),
             HLose = calculate.lose(FTHG, FTAG),
             HPts  = calculate.points(FTHG, FTAG)) %>%
      group_by(HomeTeam) %>% summarise(HPlyd = length(HomeTeam),
                                       HWin  = sum(HWin),
                                       HDraw = sum(HDraw),
                                       HLose = sum(HLose),
                                       HFor  = sum(FTHG), 
                                       HAg   = sum(FTAG),
                                       HPts  = sum(HPts)) %>%
      rename(Team = HomeTeam) %>% 
      ungroup()
  })
  
  away.data_daterange <- reactive({
    data.edit %>%
      filter(!(is.na(HomeTeam) | (HomeTeam == "")
               | is.na(AwayTeam) | (AwayTeam == "")),
             Date <= input$daterange[2] & Date >= input$daterange[1]) %>%
      mutate(AWin  = calculate.win(FTAG, FTHG),
             ADraw = calculate.draw(FTAG, FTHG),
             ALose = calculate.lose(FTAG, FTHG),
             APts  = calculate.points(FTAG, FTHG)) %>%
      group_by(AwayTeam) %>% summarise(APlyd = length(AwayTeam),
                                       AWin  = sum(AWin),
                                       ADraw = sum(ADraw),
                                       ALose = sum(ALose),
                                       AFor  = sum(FTAG),
                                       AAg   = sum(FTHG),
                                       APts  = sum(APts)) %>%
      rename(Team = AwayTeam) %>%
      ungroup()
  })
  
  games_df_daterange <- reactive({
    data.edit %>%
      filter(Date <= input$daterange[2] & Date >= input$daterange[1]) %>%
      pivot_longer(contains('Team'), names_to = 'home_away', values_to = 'team', names_prefix = 'team_') %>%
      mutate(
        result = FTHG - FTAG,
        result = ifelse(home_away == 'HomeTeam', result, -result),
        win = ifelse(result == 0 , 0.5, ifelse(result > 0, 1, 0))
      ) %>%
      select(Date, team, win) %>%
      group_by(team) %>%
      summarise(
        Form = (list(win)), .groups = "drop") %>%
      ungroup()
  })
  
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
  
  final_df_daterange <- reactive({
    table.data_daterange() %>%
      full_join(games_df_daterange(), by = c("Team" = "team")) %>%
      arrange(desc(Pts), desc(GoalDiff), desc(GoalsFor), Team) %>%
      mutate(Form = map(Form, rev),
             Form = map(Form, head, 5),
             Pos = row_number(), .before = Team) %>%
      add_column(info = c(rep("Champions League", 4),
                          rep("Europa League", 1),
                          rep("", 12),
                          rep("Relagation zone", 3))) 
  })
  
  output$table <- render_gt({
    
    final_df() %>% 
      gt() %>%
      tab_header(
        title = md("Premier League 2021/2022")
      ) %>%
      cols_label(GoalsFor = "GF",
                 GoalsAgainst = "GA",
                 GoalDiff = "GD",
                 Plyd = "P",
                 Win = "W",
                 Draw = "D",
                 Lose = "L"
      ) %>%
      gt_plt_winloss_jh(Form, max_wins = 5, type = "square") %>%
      gt_merge_stack(col1 = Team, col2 = info) %>%
      gtExtras::gt_theme_nytimes() %>%
      tab_style(
        # Select object to modify
        locations = cells_title(groups = 'title'),
        # Specify text style
        style = list(
          cell_text(
            font = "Lato",
            size='x-large',
            align = "center"
          ))) %>%
      gtExtras::gt_highlight_rows(
        rows = 1,
        fill = "grey85",
        font_weight = "normal",
      ) %>%
      gtExtras::gt_highlight_rows(
        rows = c(2:4),
        fill = "grey93",
        font_weight = "normal",
      ) %>%
      gtExtras::gt_highlight_rows(
        rows = 5,
        fill = "grey97",
        font_weight = "normal",
      ) %>%
      gtExtras::gt_highlight_rows(
        rows = c(18:20),
        fill = "grey90",
        font_weight = "normal",
      ) %>%
      tab_footnote(
        footnote = md("Form represents the five most recent results, with the most recent result the  \nleftmost box (<span style='color:#013369;'>Blue = Win</span>; <span style='color:grey;'>Grey = Draw</span>; <span style='color:#D50A0A;'>Red = Loss</span>)."),
        locations = cells_column_labels(columns = Form)
      ) %>%
      tab_source_note(
        source_note = md("Premier League data from *www.football-data.co.uk*")
      )
  })
  
    
    output$table_1 <- render_gt({
      
      final_df_daterange() %>% 
      gt() %>%
      tab_header(
        title = md("Premier League 2021/2022")
      ) %>%
      cols_label(GoalsFor = "GF",
                 GoalsAgainst = "GA",
                 GoalDiff = "GD",
                 Plyd = "P",
                 Win = "W",
                 Draw = "D",
                 Lose = "L"
      ) %>%
      gt_plt_winloss_jh(Form, max_wins = 5, type = "square") %>%
      gt_merge_stack(col1 = Team, col2 = info) %>%
      gtExtras::gt_theme_nytimes() %>%
      tab_style(
        # Select object to modify
        locations = cells_title(groups = 'title'),
        # Specify text style
        style = list(
          cell_text(
            font = "Lato",
            size='x-large',
            align = "center"
          ))) %>%
      gtExtras::gt_highlight_rows(
        rows = 1,
        fill = "grey85",
        font_weight = "normal",
      ) %>%
      gtExtras::gt_highlight_rows(
        rows = c(2:4),
        fill = "grey93",
        font_weight = "normal",
      ) %>%
      gtExtras::gt_highlight_rows(
        rows = 5,
        fill = "grey97",
        font_weight = "normal",
      ) %>%
      gtExtras::gt_highlight_rows(
        rows = c(18:20),
        fill = "grey90",
        font_weight = "normal",
      ) %>%
      tab_footnote(
        footnote = md("Form represents the five most recent results, with the most recent result the  \nleftmost box (<span style='color:#013369;'>Blue = Win</span>; <span style='color:grey;'>Grey = Draw</span>; <span style='color:#D50A0A;'>Red = Loss</span>)."),
        locations = cells_column_labels(columns = Form)
      ) %>%
      tab_source_note(
        source_note = md("Premier League data from *www.football-data.co.uk*")
      )
    })

    primary_col <- c('#EF0107','#670E36', '#e30613', '#0057B8','#6C1D45','#034694','#1B458F','#003399','#FFCD00','#003090','#C8102E','#6CABDD','#DA291C','#241F20', "#FFF200", "#D71920", "#132257", "#FBEE23", "#7A263A", "#FDB913")
    secondary_col <- c('#F8F0E3','#95BFE5', '#F8F0E3', '#F8F0E3','#99D6EA','#F8F0E3','#C4122E','#F8F0E3','#1D428A','#003090','#00B2A9','#1C2C5B','#000000','#F8F0E3', "#00A650", "#F8F0E3", "#F8F0E3", "#ED2127", "#1BB1E7", "#231F20")
    teams <- c(unique(tidy_weekly_df$Team))
    
    team_primary_pal <- as.data.frame(cbind(teams, primary_col))
    team_secondary_pal <- as.data.frame(cbind(teams, secondary_col))
    
    
    
    output$line <- renderPlot({
      
      primary_pal <- team_primary_pal %>% 
        filter(teams %in% input$team)
      
      secondary_pal <- team_secondary_pal %>% 
        filter(teams %in% input$team)
      
      tidy_weekly_filtered <- tidy_weekly_df %>% 
        filter(Team %in% input$team)
      
      ggplot(tidy_weekly_filtered, 
             aes(x = date, y = position, group = Team)) + 
        geom_bump(aes(colour = Team), smooth = 10) +
        scale_colour_manual(values = primary_pal$primary_col) +
        new_scale_color() +
        geom_point(aes(colour = Team, fill = Team), size = 3, shape = 21,
                   stroke = 2) +
        scale_y_continuous(trans = "reverse", breaks = seq(1,20,1), labels = seq(1,20,1), limits = c(20, 1)) +
        theme_minimal() +
        scale_x_date(breaks = seq(min(tidy_weekly_df$date),max(tidy_weekly_df$date), by="1 week"), date_labels = "%d-%b-%y") +
        labs(title = "Weekly Positions",
             x = toupper('Week ending'), 
             y = toupper('Position'), 
             subtitle = toupper(paste0("Position measured every Sunday"))) +
        theme(panel.grid.minor = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title = element_text(size = 14,
                                        family = "Source Sans Pro"),
              axis.text.y = element_text(size = 12,
                                         family = "Source Sans Pro",
                                         colour = "darkgrey"),
              axis.text.x = element_text(angle = 50,
                                         family = "Source Sans Pro",
                                         colour = "darkgrey"),
              panel.grid.major.x = element_blank(),
              legend.title =  element_blank(),
              legend.text = element_text(size=13,
                                         family = "Source Sans Pro"),
              plot.title = element_text(family = "Lato", face = "bold",
                                        size = 24, hjust = 0.5,
                                        colour = "#333333", 
                                        margin = margin(10,0,10,0)),
              plot.title.position = "plot",
              plot.subtitle = element_text(family = "Source Sans Pro",
                                           size = 16, 
                                           colour = "darkgrey")
        ) +
        scale_colour_manual(values = primary_pal$primary_col) +
        scale_fill_manual(values = secondary_pal$secondary_col)
    }, height = 700)
    
    output$line_1 <- renderPlot({
      
      primary_pal <- team_primary_pal %>% 
        filter(teams %in% input$team_1)
      
      secondary_pal <- team_secondary_pal %>% 
        filter(teams %in% input$team_1)
      
      tidy_weekly_filtered_points <- tidy_weekly_df_points %>% 
        filter(Team %in% input$team_1)
      
      ggplot(tidy_weekly_filtered_points, 
             aes(x = date, y = points, group = Team)) + 
        geom_line(aes(colour = Team), size = 0.7) + 
        scale_colour_manual(values = primary_pal$primary_col) +
        new_scale_color() +
        geom_point(aes(colour = Team, fill = Team), size = 3, shape = 21,
                   stroke = 2) +
        scale_y_continuous(breaks = seq(0,(max(tidy_weekly_df_points$points) + 5),5), labels = seq(0,(max(tidy_weekly_df_points$points) + 5),5), limits = c(0, max(tidy_weekly_df_points$points) + 5)) +
        theme_minimal() +
        scale_x_date(breaks = seq(min(tidy_weekly_df$date),max(tidy_weekly_df$date), by="1 week"), date_labels = "%d-%b-%y") +
        labs(title = "Total points", 
             x = toupper('Week ending'), 
             y = toupper('Points'), 
             subtitle = toupper("Points on Sundays")) +
        theme(panel.grid.minor = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title = element_text(size = 14,
                                        family = "Source Sans Pro"),
              axis.text.y = element_text(size = 12,
                                         family = "Source Sans Pro",
                                         colour = "darkgrey"),
              axis.text.x = element_text(angle = 50,
                                         family = "Source Sans Pro",
                                         colour = "darkgrey"),
              panel.grid.major.x = element_blank(),
              legend.title =  element_blank(),
              legend.text = element_text(size=13,
                                         family = "Source Sans Pro"),
              plot.title = element_text(family = "Lato", face = "bold",
                                        size = 24, hjust = 0.5,
                                        colour = "#333333", 
                                        margin = margin(10,0,10,0)),
              plot.title.position = "plot",
              plot.subtitle = element_text(family = "Source Sans Pro",
                                           size = 16, 
                                           colour = "darkgrey")
              ) +
        scale_colour_manual(values = primary_pal$primary_col) +
        scale_fill_manual(values = secondary_pal$secondary_col)
    }, height = 700)
}

# Run the application 
shinyApp(ui = ui, server = server)
