library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(randomForest)
library(tidyr)

# ========================
# LOAD DATA
# ========================
matches <- read.csv("matches.csv", stringsAsFactors = FALSE)
deliveries <- read.csv("deliveries.csv", stringsAsFactors = FALSE)

# ========================
# CLEAN DATA
# ========================
clean_names <- function(x) {
  x <- trimws(x)
  x <- gsub("Bangalore", "Bengaluru", x)
  x
}

matches$team1 <- clean_names(matches$team1)
matches$team2 <- clean_names(matches$team2)
matches$winner <- clean_names(matches$winner)
matches$toss_winner <- clean_names(matches$toss_winner)

deliveries$batter <- clean_names(deliveries$batter)
deliveries$bowler <- clean_names(deliveries$bowler)

matches <- matches %>% filter(!is.na(winner))

# ========================
# MODEL
# ========================
model_data <- matches %>%
  select(team1, team2, toss_winner, venue, season, winner) %>%
  na.omit()

model_data$winner <- as.factor(model_data$winner)

model <- randomForest(winner ~ ., data = model_data, ntree = 200)

# ========================
# UI
# ========================
ui <- dashboardPage(
  
  skin = "black",
  
  dashboardHeader(title = "🏏 IPL Analytics Pro"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dash", icon = icon("chart-line")),
      menuItem("Prediction", tabName = "pred", icon = icon("brain")),
      menuItem("Player Stats", tabName = "player", icon = icon("user")),
      menuItem("Orange Cap", tabName = "orange", icon = icon("trophy")),
      menuItem("Compare", tabName = "compare", icon = icon("balance-scale"))
    )
  ),
  
  dashboardBody(
    
    tags$head(tags$style(HTML("
      body {background-color:#111;}
      .box {border-radius:12px;}
      .content-wrapper {background-color:#111;}
      .small-box {border-radius:10px;}
    "))),
    
    tabItems(
      
      # ================= DASHBOARD =================
      tabItem(tabName="dash",
              
              fluidRow(
                valueBoxOutput("matchesBox"),
                valueBoxOutput("teamsBox"),
                valueBoxOutput("seasonsBox")
              ),
              
              fluidRow(
                box(width=6,plotOutput("winPlot")),
                box(width=6,plotOutput("tossPlot"))
              )
      ),
      
      # ================= PREDICTION =================
      tabItem(tabName="pred",
              
              fluidRow(
                box(width=4,
                    selectInput("team1_pred","Team 1:",choices=unique(matches$team1)),
                    selectInput("team2_pred","Team 2:",choices=unique(matches$team1)),
                    selectInput("toss_pred","Toss:",choices=unique(matches$toss_winner)),
                    selectInput("venue_pred","Venue:",choices=unique(matches$venue)),
                    actionButton("predict_btn","Predict", class="btn-danger")
                ),
                
                box(width=8,plotOutput("h2hPlot"))
              ),
              
              fluidRow(
                box(width=12,
                    h2(style="color:#00ffcc;",textOutput("prediction")))
              )
      ),
      
      # ================= PLAYER =================
      tabItem(tabName="player",
              
              fluidRow(
                box(width=4,
                    selectInput("player","Player:",choices=unique(deliveries$batter))
                ),
                
                box(width=8,plotOutput("playerPlot"))
              ),
              
              fluidRow(
                box(width=12,verbatimTextOutput("playerStats"))
              )
      ),
      
      # ================= ORANGE CAP =================
      tabItem(tabName="orange",
              
              fluidRow(
                box(width=4,
                    selectInput("orange_season","Season:",choices=unique(matches$season))
                ),
                
                box(width=8,plotOutput("orangePlot"))
              ),
              
              fluidRow(
                box(width=12,tableOutput("orangeTable"))
              )
      ),
      
      # ================= COMPARE =================
      tabItem(tabName="compare",
              
              fluidRow(
                box(width=4,
                    selectInput("player1","Player 1:",choices=unique(deliveries$batter)),
                    selectInput("player2","Player 2:",choices=unique(deliveries$batter))
                ),
                
                box(width=8,plotOutput("comparePlot"))
              ),
              
              fluidRow(
                box(width=12,tableOutput("compareTable"))
              )
      )
    )
  )
)

# ========================
# SERVER
# ========================
server <- function(input, output) {
  
  # KPI BOXES
  output$matchesBox <- renderValueBox({
    valueBox(nrow(matches),"Total Matches",icon=icon("gamepad"),color="purple")
  })
  
  output$teamsBox <- renderValueBox({
    valueBox(length(unique(matches$team1)),"Teams",icon=icon("users"),color="teal")
  })
  
  output$seasonsBox <- renderValueBox({
    valueBox(length(unique(matches$season)),"Seasons",icon=icon("calendar"),color="yellow")
  })
  
  # DASHBOARD PLOTS
  output$winPlot <- renderPlot({
    matches %>%
      count(winner) %>%
      ggplot(aes(winner,n,fill=winner)) +
      geom_bar(stat="identity") +
      coord_flip() +
      theme_minimal(base_size=14)
  })
  
  output$tossPlot <- renderPlot({
    matches %>%
      count(toss_winner) %>%
      ggplot(aes(toss_winner,n,fill=toss_winner)) +
      geom_bar(stat="identity") +
      coord_flip() +
      theme_minimal()
  })
  
  # PREDICTION
  pred <- reactiveVal("Click Predict")
  
  observeEvent(input$predict_btn,{
    
    newdata <- data.frame(
      team1=input$team1_pred,
      team2=input$team2_pred,
      toss_winner=input$toss_pred,
      venue=input$venue_pred,
      season=2020
    )
    
    probs <- predict(model,newdata,type="prob")
    
    t1 <- if(input$team1_pred %in% colnames(probs)) probs[,input$team1_pred] else 0
    t2 <- if(input$team2_pred %in% colnames(probs)) probs[,input$team2_pred] else 0
    
    total <- t1+t2
    if(total==0){t1=0.5;t2=0.5}else{t1=t1/total;t2=t2/total}
    
    winner <- ifelse(t1>t2,input$team1_pred,input$team2_pred)
    
    pred(paste0("🏆 ",winner,"\n",
                input$team1_pred,": ",round(t1*100,2),"%\n",
                input$team2_pred,": ",round(t2*100,2),"%"))
  })
  
  output$prediction <- renderText({pred()})
  
  # H2H
  output$h2hPlot <- renderPlot({
    matches %>%
      filter((team1==input$team1_pred & team2==input$team2_pred) |
               (team1==input$team2_pred & team2==input$team1_pred)) %>%
      count(winner) %>%
      ggplot(aes(winner,n,fill=winner)) +
      geom_bar(stat="identity")
  })
  
  # PLAYER
  output$playerPlot <- renderPlot({
    deliveries %>%
      filter(batter==input$player) %>%
      group_by(bowler) %>%
      summarise(runs=sum(batsman_runs)) %>%
      head(10) %>%
      ggplot(aes(bowler,runs)) +
      geom_bar(stat="identity") +
      coord_flip()
  })
  
  output$playerStats <- renderText({
    d <- deliveries %>% filter(batter==input$player)
    paste("Runs:",sum(d$batsman_runs),
          "\nBalls:",nrow(d))
  })
  
  # ORANGE CAP
  output$orangePlot <- renderPlot({
    ids <- matches %>% filter(season==input$orange_season) %>% pull(id)
    deliveries %>% filter(match_id %in% ids) %>%
      group_by(batter) %>% summarise(runs=sum(batsman_runs)) %>%
      arrange(desc(runs)) %>% head(10) %>%
      ggplot(aes(batter,runs)) +
      geom_bar(stat="identity") +
      coord_flip()
  })
  
  output$orangeTable <- renderTable({
    ids <- matches %>% filter(season==input$orange_season) %>% pull(id)
    deliveries %>% filter(match_id %in% ids) %>%
      group_by(batter) %>% summarise(Runs=sum(batsman_runs)) %>%
      arrange(desc(Runs)) %>% head(10)
  })
  
  # COMPARE
  get_stats <- function(p){
    d <- deliveries %>% filter(batter==p)
    data.frame(Player=p,Runs=sum(d$batsman_runs),Balls=nrow(d))
  }
  
  output$compareTable <- renderTable({
    rbind(get_stats(input$player1),get_stats(input$player2))
  })
  
  output$comparePlot <- renderPlot({
    stats <- rbind(get_stats(input$player1),get_stats(input$player2))
    stats_long <- pivot_longer(stats,-Player)
    
    ggplot(stats_long,aes(name,value,fill=Player))+
      geom_bar(stat="identity",position="dodge")
  })
}

shinyApp(ui, server)