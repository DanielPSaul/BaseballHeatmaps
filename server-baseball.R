#### SERVER CODE SNIPPET (NOT OPERATIONAL) ####

# Pitching Heat Map Tab
#################################
{
  #update
  plotH <- reactive({
    filter(data, Season %in% input$SeasonHM)
  })
  observeEvent(plotH(), {
    choices <- sort(unique(plotH()$Game))
    updatePickerInput(session = session, inputId="GameHM", choices = choices)
  })
  
  plotHM <- reactive({
    req(input$GameHM)
    filter(plotH(), Game %in% input$GameHM)
  })
  observeEvent(plotHM(), {
    choices <- sort(unique(plotHM()$Pitcher))
    updatePickerInput(session = session, inputId = "PitcherHM", choices = choices)
  })
  
  plotHM2 <- reactive({
    req(input$PitcherHM)
    filter(plotHM(), Pitcher %in% input$PitcherHM)
  })
  plotHM5 <- reactive({
    plotHM2() %>%
      filter(Game %in% input$GameHM,
             Pitcher %in% input$PitcherHM,
             BatterSide %in% input$BatterSideHM,
             TaggedPitchType %in% input$PitchHM,
             HitType %in% input$HitTypeHM,
             PitchCall %in% input$PitchCallHM)
  })
  
  
  
  inat <- reactive({
    get_inat_obs(Photo_Url = input$PitcherHM)
  })
  
  output$imageH <- renderUI({
    hm_data <- plotHM5()
    tags$img(src=hm_data$Photo_Urls[1], height = "150px", align = 'left')
  })
  
  output$hm <- renderPlotly({
    
    hm_data <- plotHM5()
    plotTitle <- paste0("<b>Heat Map Analysis</b><br>")
    
    hm <- ggplot(hm_data, aes(x = PlateLocSide, y = PlateLocHeight)) +
      facet_wrap(~ TaggedPitchType)+
      stat_density2d(aes(fill=..ndensity..), geom = "tile", contour = FALSE) +
      scale_fill_distiller(palette = "Spectral", direction = -1) +
      geom_segment(aes(x = -1, y = 1.5, xend = 1, yend = 1.5), color = "#000000") +
      geom_segment(aes(x = -1, y = 3.5, xend = 1, yend = 3.5), color = "#000000") +
      geom_segment(aes(x = -1, y = 1.5, xend = -1, yend = 3.5), color = "#000000") +
      geom_segment(aes(x = 1, y = 1.5, xend = 1, yend = 3.5), color = "#000000") +
      xlab('Ball Side') +
      ylab('Ball Height')+
      xlim(-4, 4) +
      ylim(-1, 5.5) +
      coord_fixed()+
      ggtitle(plotTitle)
    
    hm <- ggplotly(hm)
    
  })
  
  output$infotable <- renderTable({
    kdat <- plotHM5()
    #kdat2 <- kdat %>% count(PlayResult) %>% group_by(Pitcher)
    kdat$AB <- case_when(
      kdat$PlayResult == "Single" ~ 1,
      kdat$PlayResult == "Double" ~ 1,
      kdat$PlayResult == "Triple" ~ 1,
      kdat$PlayResult == "HomeRun" ~ 1,
      kdat$PlayResult == "Out" ~ 1,
      kdat$PlayResult == "FieldersChoice" ~ 1,
      kdat$PlayResult == "Error" ~ 1,
      kdat$PlayResult == "Undefined" ~ 0,
      kdat$PlayResult == "Sacrifice" ~ 0
    )
    kdat$H <- case_when(
      kdat$PlayResult == "Single" ~ 1,
      kdat$PlayResult == "Double" ~ 1,
      kdat$PlayResult == "Triple" ~ 1,
      kdat$PlayResult == "HomeRun" ~ 1,
      kdat$PlayResult == "Out" ~ 0,
      kdat$PlayResult == "FieldersChoice" ~ 0,
      kdat$PlayResult == "Error" ~ 0,
      kdat$PlayResult == "Undefined" ~ 0,
      kdat$PlayResult == "Sacrifice" ~ 0
    )
    kdat$TotalBases <- case_when(
      kdat$PlayResult == "Single" ~ 1,
      kdat$PlayResult == "Double" ~ 2,
      kdat$PlayResult == "Triple" ~ 3,
      kdat$PlayResult == "HomeRun" ~ 4,
      kdat$PlayResult == "Out" ~ 0,
      kdat$PlayResult == "FieldersChoice" ~ 0,
      kdat$PlayResult == "Error" ~ 0,
      kdat$PlayResult == "Undefined" ~ 0,
      kdat$PlayResult == "Sacrifice" ~ 0
    )
    kdat$TotalPitches <- case_when(
      kdat$PitchCall == "StrikeSwinging" ~ 1,
      kdat$PitchCall == "InPlay" ~ 1,
      kdat$PitchCall == "StrikeCalled" ~ 1,
      kdat$PitchCall == "BallCalled" ~ 1,
      kdat$PitchCall == "FoulBall" ~ 1,
      kdat$PitchCall == "BallIntentional" ~ 1,
      kdat$PitchCall == "HitByPitch" ~ 1,
      kdat$PitchCall == "BallinDirt" ~ 1,
      kdat$PitchCall == "Undefined" ~ 1
    )
    kdat$SWaMiss <- case_when(
      kdat$PitchCall == "StrikeSwinging" ~ 1,
      kdat$PitchCall == "InPlay" ~ 0,
      kdat$PitchCall == "StrikeCalled" ~ 0,
      kdat$PitchCall == "BallCalled" ~ 0,
      kdat$PitchCall == "FoulBall" ~ 0,
      kdat$PitchCall == "BallIntentional" ~ 0,
      kdat$PitchCall == "HitByPitch" ~ 0,
      kdat$PitchCall == "BallinDirt" ~ 0,
      kdat$PitchCall == "Undefined" ~ 0
    )
    # kdat2 <- kdat2 %>% rename("Count" = "n")
    #count(kdat$Pitcher)
    #kdat
    
    df = subset(kdat, select = -c(GameID))
    df2 =df %>%
      group_by(Pitcher)  %>%
      
      summarize(battingAverage = sum(H)/sum(AB),
                sluggingPct = sum(TotalBases)/sum(AB),
                swingMissPct = sum(SWaMiss)/sum(TotalPitches),
                Count = sum(TotalPitches))
    
    df3 = subset(df2, select = c(Pitcher, Count, battingAverage, sluggingPct, swingMissPct))
    df3
  })
  
  
}

#####################
# Batting Heat Map Tab
{
  #update
  plotB <- reactive({
    filter(data, Season %in% input$SeasonHM2)
  })
  observeEvent(plotB(), {
    choices <- sort(unique(plotB()$Game))
    updatePickerInput(session = session, inputId="GameHM2", choices = choices)
  })
  
  plotBM <- reactive({
    req(input$GameHM2)
    filter(plotB(), Game %in% input$GameHM2)
  })
  observeEvent(plotBM(), {
    choices <- sort(unique(plotBM()$Batter))
    updatePickerInput(session = session, inputId = "PitcherHM2", choices = choices)
  })
  
  plotBM2 <- reactive({
    req(input$PitcherHM2)
    filter(plotBM(), Batter %in% input$PitcherHM2)
  })
  plotBM5 <- reactive({
    plotBM2() %>%
      filter(Game %in% input$GameHM2,
             Batter %in% input$PitcherHM2,
             #BatterSide %in% input$BatterSideHM2,
             TaggedPitchType %in% input$PitchHM2,
             HitType %in% input$HitTypeHM2,
             PitchCall %in% input$PitchCallHM2)
  })
  
  
  
  inat2 <- reactive({
    get_inat_obs(Photo_Url = input$PitcherHM)
  })
  output$imageB <- renderUI({
    kdat <- plotBM5()
    tags$img(src=kdat$Photo_Urls[1], height = "150px", align = 'left')
  })
  
  output$bm <- renderPlotly({
    
    kdat <- plotBM5()
    plotTitle <- paste0("<b>Heat Map Analysis</b><br>")
    
    bm <- ggplot(kdat, aes(x = PlateLocSide, y = PlateLocHeight)) +
      facet_wrap(~ HitType)+
      stat_density2d(aes(fill=..ndensity..), geom = "tile", contour = FALSE) +
      scale_fill_distiller(palette = "Spectral", direction = -1) +
      geom_segment(aes(x = -1, y = 1.5, xend = 1, yend = 1.5), color = "#000000") +
      geom_segment(aes(x = -1, y = 3.5, xend = 1, yend = 3.5), color = "#000000") +
      geom_segment(aes(x = -1, y = 1.5, xend = -1, yend = 3.5), color = "#000000") +
      geom_segment(aes(x = 1, y = 1.5, xend = 1, yend = 3.5), color = "#000000") +
      xlab('Ball Side') +
      ylab('Ball Height')+
      xlim(-4, 4) +
      ylim(-1, 5.5) +
      coord_fixed()+
      ggtitle(plotTitle)
    
    bm <- ggplotly(bm)
    
  })
  
  output$infotable2 <- renderTable({
    kdat <- plotBM5()
    #kdat2 <- kdat %>% count(PlayResult) %>% group_by(Pitcher)
    kdat$AB <- case_when(
      kdat$PlayResult == "Single" ~ 1,
      kdat$PlayResult == "Double" ~ 1,
      kdat$PlayResult == "Triple" ~ 1,
      kdat$PlayResult == "HomeRun" ~ 1,
      kdat$PlayResult == "Out" ~ 1,
      kdat$PlayResult == "FieldersChoice" ~ 1,
      kdat$PlayResult == "Error" ~ 1,
      kdat$PlayResult == "Undefined" ~ 0,
      kdat$PlayResult == "Sacrifice" ~ 0,
      kdat$PlayResult == "Homerun" ~ 1
    )
    kdat$H <- case_when(
      kdat$PlayResult == "Single" ~ 1,
      kdat$PlayResult == "Double" ~ 1,
      kdat$PlayResult == "Triple" ~ 1,
      kdat$PlayResult == "HomeRun" ~ 1,
      kdat$PlayResult == "Out" ~ 0,
      kdat$PlayResult == "FieldersChoice" ~ 0,
      kdat$PlayResult == "Error" ~ 0,
      kdat$PlayResult == "Undefined" ~ 0,
      kdat$PlayResult == "Sacrifice" ~ 0,
      kdat$PlayResult == "Homerun" ~ 1
    )
    kdat$TotalBases <- case_when(
      kdat$PlayResult == "Single" ~ 1,
      kdat$PlayResult == "Double" ~ 2,
      kdat$PlayResult == "Triple" ~ 3,
      kdat$PlayResult == "HomeRun" ~ 4,
      kdat$PlayResult == "Out" ~ 0,
      kdat$PlayResult == "FieldersChoice" ~ 0,
      kdat$PlayResult == "Error" ~ 0,
      kdat$PlayResult == "Undefined" ~ 0,
      kdat$PlayResult == "Sacrifice" ~ 0,
      kdat$PlayResult == "Homerun" ~ 4
    )
    kdat$TotalPitches <- case_when(
      kdat$PitchCall == "StrikeSwinging" ~ 1,
      kdat$PitchCall == "InPlay" ~ 1,
      kdat$PitchCall == "StrikeCalled" ~ 1,
      kdat$PitchCall == "BallCalled" ~ 1,
      kdat$PitchCall == "FoulBall" ~ 1,
      kdat$PitchCall == "BallIntentional" ~ 1,
      kdat$PitchCall == "HitByPitch" ~ 1,
      kdat$PitchCall == "BallinDirt" ~ 1,
      kdat$PitchCall == "Undefined" ~ 1
    )
    kdat$SWaMiss <- case_when(
      kdat$PitchCall == "StrikeSwinging" ~ 1,
      kdat$PitchCall == "InPlay" ~ 0,
      kdat$PitchCall == "StrikeCalled" ~ 0,
      kdat$PitchCall == "BallCalled" ~ 0,
      kdat$PitchCall == "FoulBall" ~ 0,
      kdat$PitchCall == "BallIntentional" ~ 0,
      kdat$PitchCall == "HitByPitch" ~ 0,
      kdat$PitchCall == "BallinDirt" ~ 0,
      kdat$PitchCall == "Undefined" ~ 0
    )
    # kdat2 <- kdat2 %>% rename("Count" = "n")
    #count(kdat$Pitcher)
    #kdat
    
    df = subset(kdat, select = -c(GameID))
    df2 =df %>%
      group_by(Batter)  %>%
      
      summarize(battingAverage = sum(H)/sum(AB),
                sluggingPct = sum(TotalBases)/sum(AB),
                swingMissPct = sum(SWaMiss)/sum(TotalPitches),
                Count = sum(TotalPitches))
    
    df3 = subset(df2, select = c(Batter, Count, battingAverage, sluggingPct, swingMissPct))
    df3
  })
  
}



#######################