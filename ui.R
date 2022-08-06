#install.packages(c('dplyr','tidyr','DT','ggplot2','shiny','shinydashboard'),dependencies = T)

library(dplyr)
library(tidyr)
library(DT)
library(ggplot2)
library(shiny)
library(shinydashboard)

deliveries <- read.csv("deliveries.csv")
matches <- read.csv("matches.csv")

#Joining both the tables
combined_table <- inner_join(deliveries,matches,by=c("match_id"="id"))

# Bowler Analysis ---------------------------------------------------------

#understanding what all data to exclude ()
bowler.balls.count <- combined_table %>% group_by(bowler,season) %>% summarise(balls=n())
bowler.balls.count <- bowler.balls.count %>% mutate(combine = paste0(bowler,season),overs=balls/6)
bowler.exclude <- bowler.balls.count %>% filter(overs<=25)

#Exclude the bowlers with less than or equal to 25 overs bowled in any of the season
combined_table <- combined_table %>% mutate(combine=paste0(bowler,season))
combined_table.bowler <- combined_table %>% anti_join(bowler.exclude,by='combine')
batsman <- combined_table %>% group_by(batsman,season) %>% summarise(ballsfaced=n())

# Bowling statistics ------------------------------------------------------

bowler.function <- function(table) {
  bowler.table <- table %>% group_by(bowler,inning,season) %>% 
    summarise(balls=n()) %>% mutate(overs=balls/6)
  
  #calculate number of dot balls
  bowler.dotball <- table %>% filter(total_runs==0) %>%
    group_by(bowler,inning,season) %>% summarise(dotballs=n())
  
  #calculate Maidens
  bowler.maiden <- table %>% group_by(bowler,inning,season,match_id,over) %>%
    summarise(runsgiven=sum(total_runs),balls=n()) %>% filter(runsgiven==0) %>%
    filter(balls>5) %>% group_by(bowler,inning,season) %>% summarise(maiden=n())
  
  #calculate batsman_runs and extra_runs
  bowler.runs <- table %>% group_by(bowler,inning,season) %>% 
    summarise(bat_runs = sum(batsman_runs),extra_runs=sum(extra_runs))
  
  #calculate wickets
  bowler.dismissal <- unique(deliveries$dismissal_kind)[c(2,3,5,6,7,9)]
  
  bowler.wickets <- table %>% filter(dismissal_kind %in% bowler.dismissal) %>%
    group_by(bowler,inning,season) %>% summarise(wickets=n())
  
  #calculate number of matches
  bowler.matches <- table %>% group_by(bowler,inning,season,match_id) %>%
    summarise(matches=n()) %>% group_by(bowler,inning,season) %>%
    summarise(matches=n())
  
  #5 wickets in an innings
  bowler.5wicket <- table %>% filter(dismissal_kind %in% bowler.dismissal) %>%
    group_by(bowler,inning,season,match_id) %>% summarise(wickets=n()) %>%
    filter(wickets > 4) %>% group_by(bowler,inning,season) %>% summarise(fivewicket=n())
  
  #4 wickets in an innings
  bowler.4wicket <- table %>% filter(dismissal_kind %in% bowler.dismissal) %>%
    group_by(bowler,inning,season,match_id) %>% summarise(wickets=n()) %>%
    filter(wickets > 3) %>% group_by(bowler,inning,season) %>% summarise(fourwicket=n())
  
  bowler.table <- Reduce(function(x,y)merge(x,y,all=T),list(bowler.table,bowler.dotball,bowler.maiden,bowler.runs,bowler.wickets,bowler.matches,bowler.5wicket,bowler.4wicket))
  
  #Adding few extra calculated fields
  bowler.table <- bowler.table %>% mutate(bow_avg = bat_runs/wickets, economy = bat_runs/overs, per_dotball = (dotballs/balls)*100)
  
  #Replacing all NA values with 0
  bowler.table <- bowler.table %>% replace(is.na(.),0)
  
  #Limit to one decimal
  num_col <- bowler.table %>% sapply(is.numeric)
  bowler.table[num_col] <- bowler.table[num_col] %>% lapply(round,1)
  bowler.table <- bowler.table %>% as.data.frame()
  
}

bowler.powerplay <- bowler.function(combined_table.bowler %>% filter(over<7)) %>% mutate(stage='Powerplay')
bowler.middle <- bowler.function(combined_table.bowler %>% filter(over>6 & over<16)) %>% mutate(stage='Middle')
bowler.death <- bowler.function(combined_table.bowler %>% filter(over>15)) %>% mutate(stage='Death')

bowler.complete <- bowler.powerplay %>% union(bowler.middle) %>% union(bowler.death)

bowling.parameters <- names(bowler.complete[c(5,7,8,9,10,12,13,14,15,16)])

# Batsman Analysis --------------------------------------------------------

# under what all data to exclude (for batsman)
batsman.balls.count <- combined_table %>% group_by(batsman,season) %>% summarise(ballsfaced=n(),runs=sum(batsman_runs))
batsman.exclude <- batsman.balls.count %>% filter(ballsfaced<=90) %>% mutate(combine.batsman=paste0(batsman,season))

#Exclude the batsman with less than or equal to 90 balls faced in any of the season
combined_table <- combined_table %>% mutate(combine.batsman=paste0(batsman,season),combine.non_strike=paste0(non_striker,season))
combined_table.batsman <- combined_table %>% anti_join(batsman.exclude,by='combine.batsman')

# Batsman statistics ------------------------------------------------------

batsman.function <- function(table) {
  batsman.matches_strike <- table %>% group_by(batsman,inning,season,match_id) %>%
    summarise(innings=n()) %>% group_by(batsman,inning,season,match_id) %>%
    summarise(innings=n()) %>% as.data.frame()
  
  batsman.matches_non.strike <- table %>% group_by(non_striker,inning,season,match_id) %>%
    summarise(innings=n()) %>% group_by(non_striker,inning,season,match_id) %>%
    summarise(innings=n()) %>% rename(batsman=non_striker) %>% as.data.frame()
  
  batsman.matches <- bind_rows(batsman.matches_strike,batsman.matches_non.strike) %>%
    group_by(batsman,inning,season,match_id) %>% summarise(count=n()) %>%
    group_by(batsman,inning,season) %>% summarise(innings_played = n()) %>%
    mutate(combine.batsman=paste0(batsman,season)) %>% anti_join(batsman.exclude,by='combine.batsman') %>%
    select('batsman','inning','season','innings_played')
  
  # NUmber of Not outs
  batsman.notout <- table %>% group_by(player_dismissed,inning,season) %>%
    summarise(dismissed=n()) %>% filter(player_dismissed!='') %>% rename(batsman=player_dismissed) %>%
    mutate(combine.batsman=paste0(batsman,season)) %>% anti_join(batsman.exclude,by='combine.batsman') %>%
    select('batsman','season','inning','dismissed') %>%
    right_join(batsman.matches,by=c('batsman','inning','season')) %>% mutate(Not.out=innings_played - dismissed)
  
  #Runs made by batsman
  batsman.runs <- table %>% group_by(batsman,inning,season) %>%
    summarise(runs_made = sum(batsman_runs))
  
  #number of boundries (4's)
  boundries <- table %>% group_by(batsman,inning,season) %>%
    filter(batsman_runs == 4) %>% summarise(boundries=n())
  
  #number of sixes (6's)
  sixes <- table %>% group_by(batsman,inning,season) %>%
    filter(batsman_runs == 6) %>% summarise(sixes = n())
  
  #Highest score in an innings
  highest.score <- table %>% group_by(batsman,inning,season,match_id) %>%
    summarise(batsman_runs = sum(batsman_runs)) %>% group_by(batsman,inning,season) %>% summarise(maximum_score = max(batsman_runs))
  
  #Centuries
  centuries <- table %>% group_by(batsman,inning,season,match_id) %>%
    summarise(batsman_runs=sum(batsman_runs)) %>% filter(batsman_runs>99) %>%
    group_by(batsman,inning,season) %>% summarise(centuries = n())
  
  #Half centuries
  half.centuries <- table %>% group_by(batsman,inning,season,match_id) %>%
    summarise(batsman_runs=sum(batsman_runs)) %>% filter(batsman_runs > 49 & batsman_runs < 100) %>%
    group_by(batsman,inning,season) %>% summarise(half_centuries = n())
  
  #Balls faced
  balls.faced <- table %>% group_by(batsman,inning,season) %>%
    summarise(balls_faced = n())
  
  batsman.table <- Reduce(function(x,y)merge(x,y,all=T),list(batsman.notout,batsman.runs,balls.faced,highest.score,centuries,half.centuries,boundries,sixes)) %>%
    replace(is.na(.),0)
  
  #Adding few extra calculated columns
  batsman.table <- batsman.table %>% mutate(batting_avg = runs_made/dismissed, strike_rate = (runs_made/balls_faced)*100)
  
  #Replacing all inf & NA values with 0
  batsman.table[batsman.table ==Inf] <- 0
  batsman.table <- batsman.table %>% replace(is.na(.),0)
  
  #Limit to one decimal
  num_col <- batsman.table %>% sapply(is.numeric)
  batsman.table[num_col] <- batsman.table[num_col] %>% lapply(round,1)
  batsman.table <- batsman.table %>% as.data.frame()
  
}

batsman.powerplay <- batsman.function(combined_table.batsman %>% filter(over<7)) %>% mutate(stage='Powerplay')
batsman.middle <- batsman.function(combined_table.batsman %>% filter(over>6 & over<16)) %>% mutate(stage='Middle')
batsman.death <- batsman.function(combined_table.batsman %>% filter(over>15)) %>% mutate(stage='Death')

batsman.complete <- batsman.powerplay %>% union(batsman.middle) %>% union(batsman.death)

batting.parameters <- names(batsman.complete[c(7,8,9,10,11,12,13,14,15)])


# Dashboard UI ------------------------------------------------------------

ui <- dashboardPage(
  dashboardHeader(title = "IPL Stats & Analysis"),
  dashboardSidebar(id="",
                   sidebarMenu(
                     menuItem('Bowlers stats', tabName='Bowlers'),
                     #br(),
                     menuItem('Bowlers Analysis',tabName = 'Bowlersanalysis'),
                     #br(),
                     menuItem('Batsman stats', tabName = 'Batsman'),
                     menuItem('Batsman Analysis',tabName = 'batsmananalysis'),
                     menuItem('Match & Toss Analysis',tabName = 'matchtoss')
                   )),
  
  dashboardBody(
    tags$head( 
      tags$style(HTML(".main-sidebar { font-size: 21px; }")) #change the font size to 21
    ),
    tabItems(
      tabItem(tabName = 'Bowlers',
              HTML('Note: Click on the 3 lines above to get the complete menu'),
              box(plotOutput(outputId = 'Topgraph')),
              
              box(selectInput(inputId = 'year',
                              label = 'select a specific year',
                              choices = combined_table %>% distinct(season),
                              selected = 2008)),
              
              box(selectInput(inputId = 'stage_input',
                              label = 'select the stage of the match',
                              choices = c("Complete (1-20)","Powerplay (1-6)","Middle (7-15)","Death (16-20)"),
                              selected = "Complete (1-20)")),
              
              box(selectInput(inputId = 'inning_input',
                              label = 'select innings',
                              choices = c('1','2','1 & 2'),
                              selected = '1 & 2')),
              box(selectInput(inputId = 'bowlermetric_input',
                              label = 'select the metric to be viewed (sorted in descending order)',
                              choices = bowling.parameters,
                              selected = 'Wickets taken')),
              DT::dataTableOutput(outputId = "completetable")
      ),
      
      tabItem(tabName = 'Bowlersanalysis',
              box(selectInput(inputId = 'bowler1',
                              label = 'select the first bowler to be compared',
                              choices = as.character(bowler.complete$bowler),
                              selected = "Harbhajan Singh")),
              box(selectInput(inputId = 'bowler2',
                              label = 'select the second bowler to be compared',
                              choices = as.character(bowler.complete$bowler),
                              selected = "A Mishra")),
              box(selectInput(inputId = 'parameter1',
                              label = 'select the first parameter to be analyzed',
                              choices = bowling.parameters,
                              selected = 'overs')),
              box(selectInput(inputId = 'parameter2',
                              label = 'select the second parameter to be analyzed',
                              choices = bowling.parameters,
                              selected = 'wickets')),
              
              box(plotOutput(outputId = 'graph1')),
              box(plotOutput(outputId = 'graph2'))
      ),
      tabItem(tabName = 'Batsman',
              box(plotOutput(outputId = 'Topgraph_bat')),
              
              box(selectInput(inputId = 'year_bat',
                              label = 'select a specific year',
                              choices = combined_table %>% distinct(season),
                              selected = 2008)),
              
              box(selectInput(inputId = 'stage_input_bat',
                              label = 'select the stage of the match',
                              choices = c("Complete (1-20)","Powerplay (1-6)","Middle (7-15)","Death (16-20)"),
                              selected = "Complete (1-20)")),
              
              box(selectInput(inputId = 'inning_input_bat',
                              label = 'select innings',
                              choices = c('1','2','1 & 2'),
                              selected = '1 & 2')),
              
              box(selectInput(inputId = 'batsmanmetric_input',
                              label = 'select the metric to be viewed (sorted in descending order)',
                              choices = batting.parameters,
                              selected = 'batting_avg')),
              
              DT::dataTableOutput(outputId = "batsmantable")
      ),
      tabItem(tabName = 'batsmananalysis',
              HTML('Work in progress')),
      tabItem(tabName = 'matchtoss',
              HTML('Work in progress'))
    )
  )
)
