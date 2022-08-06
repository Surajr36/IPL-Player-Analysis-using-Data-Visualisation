server <- function(input,output) {
  
  inning.function <- function(inning.input,year_selected){
    if (inning.input == '1') {
      year_selected %>% filter(inning==1)
    } else if (inning.input == '2'){
      year_selected %>% filter(inning==2)
    } else {
      year_selected.inning <- year_selected %>% group_by(bowler,stage) %>% 
        summarise(balls=sum(balls),overs=sum(overs),dotballs=sum(dotballs),maiden=sum(maiden),bat_runs=sum(bat_runs),extra_runs=sum(extra_runs),
                  wickets=sum(wickets),matches=sum(matches),fivewicket=sum(fivewicket),fourwicket=sum(fourwicket)) %>%
        mutate(bow_avg = bat_runs/wickets, economy = bat_runs/overs, per_dotball = (dotballs/balls)*100)
      
      num_col <- year_selected.inning %>% sapply(is.numeric)
      year_selected.inning[num_col] <- year_selected.inning[num_col] %>% lapply(round,1)
      year_selected.inning %>% as.data.frame()
    }
    
  }
  
  stage.function <- function(stage.input,year_selected.inning){
    if(stage.input=='Powerplay (1-6)'){
      year_selected.inning %>% filter(stage=='Powerplay')
    } else if (stage.input=='Middle (7-15)') {
      year_selected.inning %>% filter(stage=='Middle')      
    } else if (stage.input=='Death (16-20)') {
      year_selected.inning %>% filter(stage=='Death')      
    } else {
      year_inning_selected.stage <- year_selected.inning %>% group_by(bowler) %>% summarise(balls=sum(balls),overs=sum(overs),
                                                                                            dotballs=sum(dotballs),maiden=sum(maiden),bat_runs=sum(bat_runs),extra_runs=sum(extra_runs),
                                                                                            wickets=sum(wickets),matches=sum(matches),fivewicket=sum(fivewicket),fourwicket=sum(fourwicket)) %>%
        mutate(bow_avg = bat_runs/wickets, economy = bat_runs/overs, per_dotball = (dotballs/balls)*100)
      
      num_col <- year_inning_selected.stage %>% sapply(is.numeric)
      year_inning_selected.stage[num_col] <- year_inning_selected.stage[num_col] %>% lapply(round,1)
      year_inning_selected.stage %>% as.data.frame()
    } 
  }
  
  metric.function <- function(bowlermetric_input,year_inning_selected.stage) {
    if (bowlermetric_input == 'wickets') {
      year_inning_selected.stage %>% select(c('bowler','overs','maiden','per_dotball','wickets','fivewicket','fourwicket','matches','bow_avg')) %>%
        arrange(desc(wickets))
    } else if (bowlermetric_input == 'bat_runs') {
      year_inning_selected.stage %>% select(c('bowler','overs','maiden','per_dotball','bat_runs','extra_runs','matches','bow_avg','economy')) %>%
        arrange(desc(bat_runs))
    } else if (bowlermetric_input == 'maiden') {
      year_inning_selected.stage %>% select(c('bowler','overs','maiden','per_dotball','bat_runs','extra_runs','matches','bow_avg','economy')) %>%
        arrange(desc(maiden))
    } else if (bowlermetric_input == 'per_dotball') {
      year_inning_selected.stage %>% select(c('bowler','overs','maiden','per_dotball','wickets','fivewicket','matches','bow_avg','economy')) %>%
        arrange(desc(per_dotball))
    } else if (bowlermetric_input == 'overs') {
      year_inning_selected.stage %>% select(c('bowler','overs','maiden','bat_runs','extra_runs','wickets','matches','bow_avg','economy')) %>%
        arrange(desc(overs))
    } else if (bowlermetric_input == 'economy') {
      year_inning_selected.stage %>% select(c('bowler','overs','maiden','bat_runs','extra_runs','wickets','matches','bow_avg','economy')) %>%
        arrange(desc(economy))
    } else if (bowlermetric_input == 'extra_runs') {
      year_inning_selected.stage %>% select(c('bowler','overs','maiden','bat_runs','extra_runs','wickets','matches','bow_avg','economy')) %>%
        arrange(desc(extra_runs))
    } else if (bowlermetric_input == 'bow_avg'){
      year_inning_selected.stage %>% select(c('bowler','overs','maiden','bat_runs','extra_runs','wickets','matches','bow_avg','economy')) %>%
        arrange(desc(bow_avg))
    } else if (bowlermetric_input == 'fourwicket') {
      year_inning_selected.stage %>% select(c('bowler','overs','maiden','wickets','fivewicket','fourwicket','matches','bow_avg','economy')) %>%
        arrange(desc(fourwicket))
    } else {
      year_inning_selected.stage %>% select(c('bowler','overs','maiden','per_dotball','wickets','fivewicket','fourwicket','matches','bow_avg')) %>%
        arrange(desc(fivewicket))
    }
    
  }
  
  plot.function <- function(year_inning_stage_selected.metric,bowlermetric_input) {
    if (bowlermetric_input == 'wickets') {
      year_inning_stage_selected.metric %>% select(c('bowler','wickets')) %>%
        arrange(desc(wickets)) %>% head(6) %>% ggplot(aes(x=reorder(bowler,-wickets),y=wickets)) + geom_bar(stat = 'identity') + xlab('Bowlers')
    } else if (bowlermetric_input == 'bat_runs') {
      year_inning_stage_selected.metric %>% select(c('bowler','bat_runs')) %>%
        arrange(desc(bat_runs)) %>% head(6) %>% ggplot(aes(x=reorder(bowler,-bat_runs),y=bat_runs)) + geom_bar(stat = 'identity') + xlab('Bowlers')
    } else if (bowlermetric_input == 'maiden') {
      year_inning_stage_selected.metric %>% select(c('bowler','maiden')) %>%
        arrange(desc(maiden)) %>% head(6) %>% ggplot(aes(x=reorder(bowler,-maiden),y=maiden)) + geom_bar(stat = 'identity') + xlab('Bowlers')
    } else if (bowlermetric_input == 'per_dotball') {
      year_inning_stage_selected.metric %>% select(c('bowler','per_dotball')) %>%
        arrange(desc(per_dotball)) %>% head(6) %>% ggplot(aes(x=reorder(bowler,-per_dotball),y=per_dotball)) + geom_bar(stat = 'identity') + xlab('Bowlers')
    } else if (bowlermetric_input == 'overs') {
      year_inning_stage_selected.metric %>% select(c('bowler','overs')) %>%
        arrange(desc(overs)) %>% head(6) %>% ggplot(aes(x=reorder(bowler,-overs),y=overs)) + geom_bar(stat = 'identity') + xlab('Bowlers')
    } else if (bowlermetric_input == 'economy') {
      year_inning_stage_selected.metric %>% select(c('bowler','economy')) %>%
        arrange(desc(economy)) %>% head(6) %>% ggplot(aes(x=reorder(bowler,-economy),y=economy)) + geom_bar(stat = 'identity') + xlab('Bowlers')
    } else if (bowlermetric_input == 'extra_runs') {
      year_inning_stage_selected.metric %>% select(c('bowler','extra_runs')) %>%
        arrange(desc(extra_runs)) %>% head(6) %>% ggplot(aes(x=reorder(bowler,-extra_runs),y=extra_runs)) + geom_bar(stat = 'identity') + xlab('Bowlers')
    } else if (bowlermetric_input == 'bow_avg'){
      year_inning_stage_selected.metric %>% select(c('bowler','bow_avg')) %>%
        arrange(desc(bow_avg)) %>% head(6) %>% ggplot(aes(x=reorder(bowler,-bow_avg),y=bow_avg)) + geom_bar(stat = 'identity') + xlab('Bowlers')
    } else if (bowlermetric_input == 'fourwicket') {
      year_inning_stage_selected.metric %>% select(c('bowler','fourwicket')) %>%
        arrange(desc(fourwicket)) %>% head(6) %>% ggplot(aes(x=reorder(bowler,-fourwicket),y=fourwicket)) + geom_bar(stat = 'identity') + xlab('Bowlers')
    } else {
      year_inning_stage_selected.metric %>% select(c('bowler','fivewicket')) %>%
        arrange(desc(fivewicket)) %>% head(6) %>% ggplot(aes(x=reorder(bowler,-fivewicket),y=fivewicket)) + geom_bar(stat = 'identity') + xlab('Bowlers')
    }
  }
  
  inning.function.batsman <- function(inning.input,year_selected){
    if (inning.input == '1') {
      year_selected %>% filter(inning==1)
    } else if (inning.input == '2'){
      year_selected %>% filter(inning==2)
    } else {
      year_selected.inning <- year_selected %>% group_by(batsman,stage) %>% 
        summarise(dismissed=sum(dismissed),innings_played=sum(innings_played),Not.out=sum(Not.out),runs_made=sum(runs_made),balls_faced=sum(balls_faced),maximum_score=max(maximum_score),
                  centuries=sum(centuries),half_centuries=sum(half_centuries),boundries=sum(boundries),sixes=sum(sixes)) %>%
        mutate(batting_avg = runs_made/dismissed, strike_rate = (runs_made/balls_faced)*100)
      
      num_col <- year_selected.inning %>% sapply(is.numeric)
      year_selected.inning[num_col] <- year_selected.inning[num_col] %>% lapply(round,1)
      year_selected.inning %>% as.data.frame()
    }
    
  }
  
  stage.function.batsman <- function(stage.input,year_selected.inning){
    if(stage.input=='Powerplay (1-6)'){
      year_selected.inning %>% filter(stage=='Powerplay')
    } else if (stage.input=='Middle (7-15)') {
      year_selected.inning %>% filter(stage=='Middle')      
    } else if (stage.input=='Death (16-20)') {
      year_selected.inning %>% filter(stage=='Death')      
    } else {
      year_inning_selected.stage <- year_selected.inning %>% group_by(batsman) %>% summarise(dismissed=sum(dismissed),Not.out=sum(Not.out),runs_made=sum(runs_made),
                                                                                             balls_faced=sum(balls_faced),maximum_score=max(maximum_score),
                                                                                             centuries=sum(centuries),half_centuries=sum(half_centuries),
                                                                                             boundries=sum(boundries),sixes=sum(sixes)) %>%
        mutate(batting_avg = runs_made/dismissed, strike_rate = (runs_made/balls_faced)*100)
      
      num_col <- year_inning_selected.stage %>% sapply(is.numeric)
      year_inning_selected.stage[num_col] <- year_inning_selected.stage[num_col] %>% lapply(round,1)
      year_inning_selected.stage %>% as.data.frame()
    }
  }
  
  metric.function.batsman <- function(batsmanmetric_input,year_inning_selected.stage) {
    if (batsmanmetric_input == 'runs_made') {
      year_inning_selected.stage %>% select(c('batsman','runs_made','balls_faced','maximum_score','centuries','half_centuries','batting_avg','strike_rate','boundries','sixes')) %>%
        arrange(desc(runs_made))
    } else if (batsmanmetric_input == 'balls_faced') {
      year_inning_selected.stage %>% select(c('batsman','runs_made','balls_faced','maximum_score','centuries','half_centuries','batting_avg','strike_rate','boundries','sixes')) %>%
        arrange(desc(balls_faced))
    } else if (batsmanmetric_input == 'maximum_score') {
      year_inning_selected.stage %>% select(c('batsman','runs_made','balls_faced','maximum_score','centuries','half_centuries','batting_avg','strike_rate','boundries','sixes')) %>%
        arrange(desc(maximum_score))
    } else if (batsmanmetric_input == 'centuries') {
      year_inning_selected.stage %>% select(c('batsman','runs_made','balls_faced','maximum_score','centuries','half_centuries','batting_avg','strike_rate','boundries','sixes')) %>%
        arrange(desc(centuries))
    } else if (batsmanmetric_input == 'half_centuries') {
      year_inning_selected.stage %>% select(c('batsman','runs_made','balls_faced','maximum_score','centuries','half_centuries','batting_avg','strike_rate','boundries','sixes')) %>%
        arrange(desc(half_centuries))
    } else if (batsmanmetric_input == 'boundries') {
      year_inning_selected.stage %>% select(c('batsman','runs_made','balls_faced','maximum_score','centuries','half_centuries','batting_avg','strike_rate','boundries','sixes')) %>%
        arrange(desc(boundries))
    } else if (batsmanmetric_input == 'sixes') {
      year_inning_selected.stage %>% select(c('batsman','runs_made','balls_faced','maximum_score','centuries','half_centuries','batting_avg','strike_rate','boundries','sixes')) %>%
        arrange(desc(sixes))
    } else if (batsmanmetric_input == 'batting_avg'){
      year_inning_selected.stage %>% select(c('batsman','runs_made','balls_faced','maximum_score','centuries','half_centuries','batting_avg','strike_rate','boundries','sixes')) %>%
        arrange(desc(batting_avg))
    } else {
      year_inning_selected.stage %>% select(c('batsman','runs_made','balls_faced','maximum_score','centuries','half_centuries','batting_avg','strike_rate','boundries','sixes')) %>%
        arrange(desc(strike_rate))
    }
    
  }
  
  plot.function.batsman <- function(year_inning_stage_selected.metric,batsmanmetric_input) {
    if (batsmanmetric_input == 'runs_made') {
      year_inning_stage_selected.metric %>% select(c('batsman','runs_made')) %>%
        arrange(desc(runs_made)) %>% head(6) %>% ggplot(aes(x=reorder(batsman,-runs_made),y=runs_made)) + geom_bar(stat = 'identity') + xlab('Batsman')
    } else if (batsmanmetric_input == 'balls_faced') {
      year_inning_stage_selected.metric %>% select(c('batsman','balls_faced')) %>%
        arrange(desc(balls_faced)) %>% head(6) %>% ggplot(aes(x=reorder(batsman,-balls_faced),y=balls_faced)) + geom_bar(stat = 'identity') + xlab('Batsman')
    } else if (batsmanmetric_input == 'maximum_score') {
      year_inning_stage_selected.metric %>% select(c('batsman','maximum_score')) %>%
        arrange(desc(maximum_Score)) %>% head(6) %>% ggplot(aes(x=reorder(batsman,-maximum_score),y=maximum_score)) + geom_bar(stat = 'identity') + xlab('Batsman')
    } else if (batsmanmetric_input == 'centuries') {
      year_inning_stage_selected.metric %>% select(c('batsman','centuries')) %>%
        arrange(desc(centuries)) %>% head(6) %>% ggplot(aes(x=reorder(batsman,-centuries),y=centuries)) + geom_bar(stat = 'identity') + xlab('Batsman')
    } else if (batsmanmetric_input == 'half_centuries') {
      year_inning_stage_selected.metric %>% select(c('batsman','half_centuries')) %>%
        arrange(desc(half_centuries)) %>% head(6) %>% ggplot(aes(x=reorder(batsman,-centuries),y=centuries)) + geom_bar(stat = 'identity') + xlab('Batsman')
    } else if (batsmanmetric_input == 'boundries') {
      year_inning_stage_selected.metric %>% select(c('batsman','boundries')) %>%
        arrange(desc(boundries)) %>% head(6) %>% ggplot(aes(x=reorder(batsman,-boundries),y=boundries)) + geom_bar(stat = 'identity') + xlab('Batsman')
    } else if (batsmanmetric_input == 'sixes') {
      year_inning_stage_selected.metric %>% select(c('batsman','sixes')) %>%
        arrange(desc(sixes)) %>% head(6) %>% ggplot(aes(x=reorder(batsman,-sixes),y=sixes)) + geom_bar(stat = 'identity') + xlab('Batsman')
    } else if (batsmanmetric_input == 'batting_avg'){
      year_inning_stage_selected.metric %>% select(c('batsman','batting_avg')) %>%
        arrange(desc(batting_avg)) %>% head(6) %>% ggplot(aes(x=reorder(batsman,-batting_avg),y=batting_avg)) + geom_bar(stat = 'identity') + xlab('Batsman')
    } else {
      year_inning_stage_selected.metric %>% select(c('batsman','strike_rate')) %>%
        arrange(desc(strike_rate)) %>% head(6) %>% ggplot(aes(x=reorder(bowler,-strike_rate),y=strike_rate)) + geom_bar(stat = 'identity') + xlab('Batsman')
    }
  }
  
  # Function ends -----------------------------------------------------------
  
  
  output$completetable <- DT::renderDataTable({
    year_selected <- bowler.complete %>% filter(season == input$year)
    year_selected.inning <- inning.function(input$inning_input,year_selected) %>% as.data.frame()
    year_inning_selected.stage <- stage.function(input$stage_input,year_selected.inning) %>% as.data.frame()
    year_inning_stage_selected.metric <- metric.function(input$bowlermetric_input,year_inning_selected.stage) %>% as.data.frame()
    
    datatable(data = year_inning_stage_selected.metric,
              options = list(pageLength=10),
              rownames = F)
  })
  
  output$Topgraph <- renderPlot({
    year_selected <- bowler.complete %>% filter(season == input$year)
    year_selected.inning <- inning.function(input$inning_input,year_selected) %>% as.data.frame()
    year_inning_selected.stage <- stage.function(input$stage_input,year_selected.inning) %>% as.data.frame()
    year_inning_stage_selected.metric <- metric.function(input$bowlermetric_input,year_inning_selected.stage) %>% as.data.frame()
    
    plot.function(year_inning_stage_selected.metric,input$bowlermetric_input)
  })
  
  output$graph1 <- renderPlot({
    bowler.analysis_table <- bowler.complete %>% group_by(bowler,season) %>% 
      summarise(balls=sum(balls),overs=sum(overs),dotballs=sum(dotballs),maiden=sum(maiden),
                bat_runs=sum(bat_runs),extra_runs=sum(extra_runs),wickets=sum(wickets),
                matches=sum(matches),fivewicket=sum(fivewicket),fourwicket=sum(fourwicket)) %>%
      mutate(bow_avg = bat_runs/wickets, economy = bat_runs/overs, per_dotball = (dotballs/balls)*100)
    
    bowler.analysis_table <- bowler.analysis_table %>% mutate(season=as.factor(season))
    
    bowler_selected.param1 <- bowler.analysis_table %>% filter(bowler==input$bowler1 | bowler==input$bowler2) %>% select(bowler,season,input$parameter1,input$parameter2)
    
    bowler_selected.param1 %>% ggplot(aes_string(x='season',y=input$parameter1,fill='bowler',group='bowler')) + 
      geom_bar(stat = 'identity',position = 'dodge') +
      ggtitle('For the parameter selected above')
    
  })
  
  output$graph2 <- renderPlot({
    bowler.analysis_table <- bowler.complete %>% group_by(bowler,season) %>% 
      summarise(balls=sum(balls),overs=sum(overs),dotballs=sum(dotballs),maiden=sum(maiden),
                bat_runs=sum(bat_runs),extra_runs=sum(extra_runs),wickets=sum(wickets),
                matches=sum(matches),fivewicket=sum(fivewicket),fourwicket=sum(fourwicket)) %>%
      mutate(bow_avg = bat_runs/wickets, economy = bat_runs/overs, per_dotball = (dotballs/balls)*100)
    
    bowler.analysis_table <- bowler.analysis_table %>% mutate(season=as.factor(season))
    
    bowler_selected.param2 <- bowler.analysis_table %>% filter(bowler==input$bowler1 | bowler==input$bowler2)
    
    bowler_selected.param2 %>% ggplot(aes_string(x='season',y=input$parameter2,colour='bowler',group='bowler')) + 
      geom_line() +
      ggtitle('For the parameter selected above')
  })
  
  output$batsmantable <- DT::renderDataTable({
    year_selected <- batsman.complete %>% filter(season == input$year_bat)
    year_selected.inning <- inning.function.batsman(input$inning_input_bat,year_selected) %>% as.data.frame()
    year_inning_selected.stage <- stage.function.batsman(input$stage_input_bat,year_selected.inning) %>% as.data.frame()
    year_inning_stage_selected.metric <- metric.function.batsman(input$batsmanmetric_input,year_inning_selected.stage) %>% as.data.frame()
    
    datatable(data = year_inning_stage_selected.metric,
              options = list(pageLength=10),
              rownames = F)
  })
  
  output$Topgraph_bat <- renderPlot({
    year_selected <- batsman.complete %>% filter(season == input$year_bat)
    year_selected.inning <- inning.function.batsman(input$inning_input_bat,year_selected) %>% as.data.frame()
    year_inning_selected.stage <- stage.function.batsman(input$stage_input_bat,year_selected.inning) %>% as.data.frame()
    year_inning_stage_selected.metric <- metric.function.batsman(input$batsmanmetric_input,year_inning_selected.stage) %>% as.data.frame()
    
    plot.function.batsman(year_inning_stage_selected.metric,input$batsmanmetric_input)
  })
  
}
