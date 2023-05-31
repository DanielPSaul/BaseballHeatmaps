server <- function(input, output, session) {
  ggplot2::theme()
  # Game Stats Data Table tab
  
  #Centers plot title
  #theme_update(plot.title = element_text(hjust = 0.5))
  

  data20 <- read_csv("program/database/UGA_2020_Regular_Season2.csv")
  #data$Date <-as.Date(data$Date,format="%Y%m%d")
  
  data22 <- read_csv("program/database/UGA_2022_Regular_Season.csv")
  
  data22 <- data22[!(is.na(data22$Date)), ]

  
  
  
  data22 <- data22 %>%
    rename(HitType = TaggedHitType)
  # Transform Date to make it consistent with old data 
  data22$Date <- format(data22$Date, '%m/%d/%y')
  
  #2021 data --------- not working yet 
  data21 <- read_csv("program/database/UGA_2021_Regular_Season.csv")
 
  data21 <- data21[!(is.na(data21$Date)), ]
  
  #data21$Time = substr(data21$Time,1,nchar(data21$Time)-3)
  
  # Make a season column for filtering purposes
        #2022 Data
  data22$Season <- '2022'
        # 2021 Data
  data21$Season <- '2021'
        # 2022 Data
  data20$Season <- '2020'
  # Remove Starting 0 to make it consistent with old data 
  #data$Date <- gsub("^0", "", data$Date)
  
  # Combine seasons 
  data <- rbind.fill(data20,data21,data22) #add data21 when working
  # Remove Starting 0 in data column to make it consistent with older seasons  
  data$Date <- gsub("^0", "", data$Date)
  
  data <- data %>%
    filter(data$PitcherTeam == "GEO_BUL")
  
  data$Game <- paste(
    data$BatterTeam, '-', data$Date, '-', str_sub(data$GameID, -1)
  )
  # create PA column for Post Game Pitching Tab
  
  # data$InningString = toString(data$Inning)
  # data$PAString = toString(data$PAofInning)
  # data$DistinctPA <- as.integer(paste(
  #   data$Inning, data$PAofInning))
  
  data <- data %>%
    mutate(DistinctPA = as.numeric(paste0(data$Inning,data$PAofInning)))
  
  
  
  roster_photos_df <- read_csv("program/database/UGA Baseball Roster Photos.csv")
  data <- left_join(data, roster_photos_df, by = "Pitcher")
  
  # New Pitch Count column
  data <- data %>%
    group_by(GameID, Pitcher) %>%
    arrange(Time) %>%
    mutate(PitchCount = row_number())
  data
  
  #create hit locations for spray chart
  data['hitlocationx'] = sin(data$Bearing*(pi/180))*data$Distance
  
  data['hitlocationy'] = cos(data$Bearing*(pi/180))*data$Distance

  
  #xBA logistic regression
  
  xbadata2020 <- data20[, colnames(data20)[c(2,10,26,48,49)]]
  xbadata2021 <- data21[, colnames(data21)[c(6,9,25,47,48)]] 
  xbadata2022 <- data22[, colnames(data22)[c(6,9,25,47,48)]] 
  
  xbadata2020$Season <- "2020"
  xbadata2021$Season <- "2021"
  xbadata2022$Season <- "2022"
  
  df <- rbind(xbadata2020,xbadata2021,xbadata2022)
  
  
  
  #filter by uga pitchers
  df1 <- filter(df, PitcherTeam %in% c("GEO_BUL"))
  
  
  
  df1$avg <- ifelse(df1$PlayResult == "Single" | df1$PlayResult == "Double" | df1$PlayResult == "Triple" | df1$PlayResult == "Homerun" | df1$PlayResult == "HomeRun", 1, 0)
  
  
  set.seed(99) # Set Seed so that same sample can be reproduced in future 
  
  ######## Modelling -------------------------
  
  # Create Binary variable 
  
  
  # Split Data 
  dt = sort(sample(nrow(df1), nrow(df1)*.7))
  train<-df1[dt,]
  test<-df1[-dt,]
  
  #model
  model1 = glm( avg ~ ExitSpeed+Angle, data = train, family = "binomial")
  
  # run predicted test
  model.test = test
  predicted <- predict(model1, model.test, type="response")
  test$Probabilites = predicted
  
  prob <- test %>%
    group_by(Pitcher, Season) %>%
    dplyr::summarize(Mean = mean(Probabilites, na.rm=TRUE))
  
  xba <- drop_na(prob)
  
  xba <- rename(xba, xba = Mean)
  
  #xba column join
  data <- left_join(data, xba, by = c("Pitcher"="Pitcher", "Season"="Season"))
  
  #homepage schedule table
  get.df_10 <- function(x){
    myurl <- read_html("https://www.sicemdawgs.com/uga-baseball-schedule/") # read our webpage as html
    myurl <- html_table(myurl)  # convert to an html table for ease of use
    to.parse <- myurl[[1]]  # pull the first item in the list
    to.parse <- to.parse %>%
      select(Date, Opponent, Location, `Time (ET)/TV`)
    to.parse
  }   
  liveish_data <- reactive({
    invalidateLater(60000)   # refresh the report every 60k milliseconds (60 seconds)
    get.df_10()                # call our function from above
  })
  output$table5 <- DT::renderDataTable(DT::datatable({
    df_10 <- liveish_data()}))
  
  #use python notes from today to use larger segments of geom tile instead of just tiny points of each pitch
  # if(data$PlayResult != 'Out' | data$PlayResult != 'Undefined'){
  #   data['Hit'] = 1
  # }
  # else(data['Hit'] = 0)
  
  
  # Filter data based on selections 
  gdata <- data[, colnames(data)[c(1,2,171,4,6,7,9,11,13:21,23:28,30,33:34,38,48:50,77,78)]] 
  
  ## unfilter this soon 
  # gdata <- data %>%
  #   select(Season,
  #          Game,
  #          Pitcher,
  #          Inning,
  #          TaggedPitchType,
  #          Batter,
  #          PitchCall,
  #          Balls,
  #          Strikes)
  
  ######## ------------------ Reactive Test --------------------
  # gdata1 <- reactive({
  #   filter(gdata, Season %in% input$Season1)
  # })
  # observeEvent(gdata1(), {
  #   choices <- unique(gdata1()$Game)
  #   updatePickerInput(session = session, inputId="Game1", choices = choices)
  # })
  # 
  # gdata2 <- reactive({
  #   req(input$Game1)
  #   filter(gdata1(), Game %in% input$Game1)
  # })
  # observeEvent(gdata2(), {
  #   choices <- unique(gdata2()$Pitcher)
  #   updatePickerInput(session = session, inputId = "Pitcher1", choices = choices)
  # })
  # 
  # gdata3 <- reactive({
  #   req(input$Pitcher1)
  #   filter(gdata2(), Pitcher %in% input$Pitcher1)
  # })
  # gdata4 <- reactive({
  #   gdata3() %>%
  #     filter(Game %in% input$Game1,
  #            Pitcher %in% input$Pitcher1,
  #            Inning %in% input$Inning1,
  #            TaggedPitchType %in% TaggedPitchType1)
  # })
  # 
  ######## ------------------ Reactive Test --------------------


  output$table1 <- DT::renderDataTable(DT::datatable({ 
    # gdata <- data[, colnames(data)[c(2,171,4,6,7,9,11,13:21,23:28,30,33:34,38,48:50,77,78)]] 
    # gdata <- gdata[, colnames(gdata)[c(2,171,4,6,7,9,11,13:21,23:28,30,33:34,38,48:50,77,78)]] 

    #gdata5 <- gdata4()

    if (input$Season1 != "All") {
      data <- data[data$Season == input$Season1,]
    }
    if (input$Game1 != "All") {
      data <- data[data$Game == input$Game1,]
    }
    if (input$Pitcher1 != "All") {
      data <- data[data$Pitcher == input$Pitcher1,]
    }
    if (input$Inning1 != "All") {
      data <- data[data$Inning == input$Inning1,]
    }
    if (input$TaggedPitchType1 != "All") {
      data <- data[data$TaggedPitchType == input$TaggedPitchType1,]
    }
    # must include "data" here or else DataTable is blank
    # df = data.frame(Pitcher = data$Pitcher,RelSpeed = data$RelSpeed)
    # df <- df %>%
    #   group_by(Pitcher) %>%
    #   summarise_at(vars(RelSpeed), list(ReleaseSpeed = mean))
    # 

 ######## ------------------

    gdata
    
  }))
  
  # Seasons DataTable
  # Maybe find a way to aggregate these together 
  
  {
    ######## ------------------ Reactive Test --------------------##########

    
      ######## ------------------ Reactive Test --------------------##########
      

    output$table2 <- DT::renderDataTable(DT::datatable({


if (input$Season1b != "All") {
  data <- data[data$Season == input$Season1b,]
}
if (input$Pitcher1b != "All") {
  data <- data[data$Pitcher == input$Pitcher1b,]
}
 
if (input$BatterSide1b != "All") {
  data <- data[data$BatterSide == input$BatterSide1b,]
}

if (input$TaggedPitchType1b != "All") {
  data <- data[data$TaggedPitchType == input$TaggedPitchType1b,]
}
      
    # data <- merge(x = data, y = xba, by = c("Pitcher"="Pitcher", "Season"="Season"), all = TRUE)
     #data <- left_join(data, xba, by = c("Pitcher"="Pitcher", "Season"="Season"))

      seasonsdata <- data %>%
        group_by(Pitcher, Season
        ) %>%
        mutate('OutInt' = case_when(PlayResult == 'Out' | KorBB == 'Strikeout' ~ 1 ,TRUE ~ 0)) %>%
        mutate('TotalIP' = sum(OutInt)/3,
               'TotalKs' = case_when(KorBB == 'Strikeout' ~ 1 ,TRUE ~ 0),
               'TotalWalks' = case_when(KorBB == 'Walk' ~ 1 ,TRUE ~ 0),
               'Kper9' = ((sum(TotalKs))/TotalIP)*9) %>%
        mutate('TotalHR'= case_when(PlayResult == 'HomeRun' | PlayResult == 'Homerun' ~ 1 ,TRUE ~ 0)) %>%
        mutate('HRper9' = ((sum(TotalHR))/TotalIP)*9) %>%
        mutate('H' =  case_when(PlayResult == 'Single' | PlayResult == 'Double' | 
                                  PlayResult == 'Triple' | PlayResult == 'HomeRun' |
                                  PlayResult == 'Homerun' ~ 1, TRUE ~ 0)) %>% 
        mutate('AB' = case_when(PlayResult == 'Single' | PlayResult == 'Double' | 
                                  PlayResult == 'Triple' | PlayResult == 'HomeRun' | PlayResult == 'Homerun' |
                                  PlayResult == 'FieldersChoice' | PlayResult =='Error' |
                                  PlayResult == 'Out' | KorBB == 'Strikeout' ~ 1, TRUE ~ 0)) %>% 
        mutate('K' = (sum(TotalKs))) %>% 
        mutate('HR' = (sum(TotalHR))) %>% 
        mutate('SF' = case_when(PlayResult == 'Sacrifice' && HitType == 'FlyBall' ~ 1, TRUE ~ 0)) %>%
        mutate('BABIPNumerator' = (sum(H) - sum(TotalHR)),
               'BABIPDenominator' = (sum(AB) - sum(TotalKs) - sum(TotalHR) + sum(SF)),
               'BABIP' = BABIPNumerator/BABIPDenominator,
               'Opp.BA' = sum(H)/sum(AB)) %>%
        mutate('Average Velocity' = mean(RelSpeed),
               'WalksPer9' = ((sum(TotalWalks))/TotalIP)*9) %>%
        mutate('Average EV' = mean(ExitSpeed, na.rm = TRUE),
               'Average LA' = mean(Angle, na.rm = TRUE)) %>%
        mutate('InPlayInt' = case_when(HitType != 'Undefined' ~ 1, TRUE ~ 0),
               'InPlay' =  sum(InPlayInt),
               'LD' = case_when(HitType == 'LineDrive' ~ 1, TRUE ~ 0),
               'FB' = case_when(HitType == 'FlyBall' ~ 1, TRUE ~ 0),
               'GB' = case_when(HitType == 'GroundBall' ~ 1, TRUE ~ 0),
               'LDRate' = sum(LD)/InPlay,
               'FBRate' = sum(FB)/InPlay,
               'GBRate' = sum(GB)/InPlay,
               'HardHitIn' = case_when(ExitSpeed >= 95 ~ 1, TRUE ~ 0),
               'HardHit' = sum(HardHitIn)/InPlay) %>%
        mutate('HBP' = case_when(PitchCall == 'HitByPitch' ~ 1, TRUE ~ 0)) %>%
        mutate('WHIP' = (sum(H)+sum(TotalWalks))/TotalIP
                 ) %>%
        select(Photo_Url, Pitcher, TotalIP, 'Kper9' ,TotalKs, 'HRper9',
               TotalHR, 'TotalWalks', 'WalksPer9',
               RelSpeed, 'Average Velocity',ExitSpeed, 'Average EV',
               Angle, 'Average LA', 'GBRate', 'LDRate', 'FBRate', 'HardHit', 'BABIP',
               'Opp.BA',WHIP, Season, xba, HBP) %>%
        summarise(
          'IP' = round(mean(TotalIP),1),
          'Opp .BA' = round(mean(Opp.BA),3),
          'xBA' = mean(xba),
          'BABIP' = round(mean(BABIP), 3),
          'WHIP' = round(mean(WHIP),3),
          'Avg Velocity' = round(mean(RelSpeed), 2),
          'Avg Exit Velocity' = round(mean(ExitSpeed, na.rm = TRUE), 2),
          'Avg Launch Angle' = round(mean(Angle, na.rm = TRUE),2),
          "HardHit%" = round(mean(HardHit, na.rm = TRUE),2),
          'GB%' = round(mean(GBRate, na.rm =  TRUE),2),
          'LD%' = round(mean(LDRate, na.rm = TRUE),2),
          'FB%' = round(mean(FBRate, na.rm = TRUE),2),
          'K/9' = round(mean(Kper9),3),
          'HR/9' = round(mean(HRper9),3),
          'BB/9' = round(mean(WalksPer9),3),
          'SO' = sum(TotalKs),
          'HR' = sum(TotalHR),
          'Walks' = sum(TotalWalks),
          'HBP' = sum(HBP)
        ) %>%
        mutate_if(is.numeric, ~round(., 3)) #%>% 
      # formatStyle(
      #   columns = "Avg Velocity", 
      #   backgroundColor = styleInterval(c(1,11,20), c("white", "red", "green", "yellow"))
      # )

      unique(seasonsdata)
      
      
    }))
  }
  
  # creating new game column tab 
  {
    output$table3 <- DT::renderDataTable(DT::datatable({
      
      if (input$Season2p != "All") {
        data <- data[data$Season == input$Season2p,]
      }
      if (input$Pitcher2p != "All") {
        data <- data[data$Pitcher == input$Pitcher2p,]
      }
      
      if (input$BatterSide2p != "All") {
        data <- data[data$BatterSide == input$BatterSide2p,]
      }
      
      if (input$TaggedPitchType2p != "All") {
        data <- data[data$TaggedPitchType == input$TaggedPitchType2p,]
      }

      postgame <- data %>%
        group_by(Pitcher, Date) %>%
        mutate('TotalHR' = case_when(PlayResult == 'HomeRun' | PlayResult == 'Homerun' ~ 1 ,TRUE ~ 0)) %>%
        mutate('Ball' = case_when(PitchCall == 'BallCalled' ~ 1, TRUE ~ 0 )) %>%
        mutate('Strikes' = case_when(PitchCall == 'StrikeSwinging' | 
                                      PitchCall == 'StrikeCalled' |
                                       PitchCall == 'FoulBall'
                                       ~ 1, TRUE ~ 0)) %>%
       mutate('OutInt' = case_when(PlayResult == 'Out' |PlayResult == 'FieldersChoice' | KorBB == 'Strikeout' ~ 1 ,TRUE ~ 0)) %>%
       mutate('TotalHits' = case_when(PlayResult == 'Single' | PlayResult == 'Double' |
                                      PlayResult == 'Triple' | PlayResult == 'HomeRun' |
                                     PlayResult == 'Homerun' ~ 1, TRUE ~ 0)) %>%
        mutate('TotalIP' = sum(OutInt)/3,
               'TotalKs' = case_when(KorBB == 'Strikeout' ~ 1 ,TRUE ~ 0),
               'TotalWalks' = case_when(KorBB == 'Walk' ~ 1 ,TRUE ~ 0),
               'HBP' = case_when(PitchCall == 'HitByPitch' ~ 1, TRUE ~ 0)) %>%
        mutate('WHIP' = (sum(TotalHits)+sum(TotalWalks))/TotalIP) %>%
        mutate('Leadoff' = case_when((PAofInning * PitchofPA) == 1 ~ 1, TRUE ~ 0)) %>%
        mutate('LeadoffOut' = case_when((PAofInning == 1) & (PlayResult == 'Out') ~ 1, TRUE ~ 0)) %>%
        mutate('Single' = case_when(PlayResult == 'Single' ~ 1, TRUE ~ 0)) %>%
        mutate('Double' = case_when(PlayResult == 'Double' ~ 1, TRUE ~ 0)) %>%
        mutate('Triple' = case_when(PlayResult == 'Triple' ~ 1, TRUE ~ 0)) %>%
        # mutate('SacFly' = case_when(PlayResult == 'Sacrific' && TaggedHitType == 'FlyBall' ~ 1, TRUE ~ 0)) %>%
        # mutate('IBB' = case_when(PitchCall == 'BallIntentional' && PlayResult == 'Walk' ~ 1, TRUE ~ 0)) %>%
         select(Photo_Url, Game, BatterTeam, Pitcher, Season, Date, BatterSide, 
                TotalIP, Ball, Strikes, RunsScored, TotalKs, TotalWalks, HBP, WHIP, TotalHR, PlayResult,
                TotalHits, Leadoff, LeadoffOut, Single, Double, Triple, DistinctPA, xba) %>%
        summarise(
          'Opponent' = BatterTeam,
          'IP' = round(TotalIP,1),
          'PA' = n_distinct(DistinctPA),
          'H' = sum(TotalHits),
          'Balls' = sum(Ball),
          'Strike' = round(sum(Strikes),2),
          'K%' = round((Strike/(Strike+Balls)),2),
          'Runs Allowed' = sum(RunsScored), 
          #ER = 
          'SO' = sum(TotalKs),
          'Walks' = sum(TotalWalks),
          # Free base
          'HBPs' = sum(HBP),
          'LeadoffOut' = sum(LeadoffOut),
          'Leadoff' = sum(Leadoff),
          # SB
          # SB ATT?
          # LOB
          'WHIP' = round(mean(WHIP),3),
          'K/PA' = round((SO/PA),2),
          'BB/PA' = round((Walks/PA),2),
          'Singles' = sum(Single),
          'Doubles' = sum(Double),
          'Triples' = sum(Triple),
          'HR' = sum(TotalHR),
          # 'SacFly' = sum(SacFly),
          # 'IBB' = sum(IBB),
          #First Pitch K,
          'FIP' = (
            ((13*HR) + (3*(Walks+HBPs)) - (2*SO))/(IP)+2.54
                       ),
          'wOBA' = (
            ((.69*Walks) + (.72*HBPs) + (.89 *Singles) + (1.27*Doubles) + (1.62*Triples) + (2.1*HR))/(PA)
            )) %>%
        mutate_if(is.numeric, ~round(., 3)) %>%
        distinct()
    
      
       
       postgame

    }))
  }
  # end of game column tab 
  
  
  # Pitch Location Analysis
  {
    x <- c(-.71,.71,.71,-.71,-.71)
    z <- c(1.5,1.5,3.5,3.5,1.5)
    # store in dataframe
    sz <- data_frame(x,z)
    
    #update
    plotK <- reactive({
      filter(data, Season %in% input$Season5)
    })
    observeEvent(plotK(), {
      choices <- sort(unique(plotK()$Game))
      updatePickerInput(session = session, inputId="Game5", choices = choices)
    })
    
    plotKZ <- reactive({
      req(input$Game5)
      filter(plotK(), Game %in% input$Game5)
    })
    observeEvent(plotKZ(), {
      choices <- sort(unique(plotKZ()$Pitcher))
      updatePickerInput(session = session, inputId = "Pitcher5", choices = choices)
    })
    
    plotKZ2 <- reactive({
      req(input$Pitcher5)
      filter(plotKZ(), Pitcher %in% input$Pitcher5)
    })
    plotKZ5 <- reactive({
      plotKZ2() %>%
        filter(Game %in% input$Game5,
               Pitcher %in% input$Pitcher5,
               BatterSide %in% input$BatterSide5,
               TaggedPitchType %in% input$Pitch5,
               HitType %in% input$HitType5,
               PitchCall %in% input$PitchCall5)
    })
    
    
    
    inat <- reactive({
      get_inat_obs(Photo_Url = input$Pitcher5)
    })
    output$image1 <- renderUI({
      kdat <- plotKZ5()
      tags$img(src=kdat$Photo_Urls[1], height = "150px", align = 'left')
    })
    
    output$kz <- renderPlotly({
      
      kdat <- plotKZ5()
      plotTitle <- paste0("<b>Pitch Location Analysis</b><br>")
      
      # #right batter
      # photo <- readPNG('www/strike_zone.png', native = TRUE)
      # pic2 <- image_read(photo)
      # image <- image_fill(pic2, 'none')
      # raster <- as.raster(image)
      
      # #left batter
      # photo2 <- readPNG('www/strike_zone_left.png', native = TRUE)
      # pic3 <- image_read(photo2)
      # image2 <- image_fill(pic3, 'none')
      # raster3 <- as.raster(image2)
      
      a= kdat$HitType
      b= kdat$PitchCall
      c= kdat$Pitcher
      d= input$BatterSide5
      e= kdat$Inning
      f= round(kdat$RelSpeed, 1)
      
      kz <- ggplot(kdat, 
                   aes(x = PlateLocSide, y = PlateLocHeight, color= TaggedPitchType)) +
        geom_point(aes(text = paste0('<b>',c,'</b><br>','<b>Inning: </b>',e ,'<br>', '<b>','Velocity: ','</b>', f, '<br><b>Hit Type: </b>',a,'<br><b>Pitch Call: </b>', b)), alpha = (4.5/5), size = 1 ) +
        facet_wrap(~ TaggedPitchType)+
        geom_segment(aes(x = -1, y = 1.5, xend = 1, yend = 1.5), color = "#000000") +
        geom_segment(aes(x = -1, y = 3.5, xend = 1, yend = 3.5), color = "#000000") +
        geom_segment(aes(x = -1, y = 1.5, xend = -1, yend = 3.5), color = "#000000") +
        geom_segment(aes(x = 1, y = 1.5, xend = 1, yend = 3.5), color = "#000000") +
        geom_segment(aes(x = -0.33, y = 1.5, xend = -0.33, yend = 3.5), color = "#000000") +
        geom_segment(aes(x = 0.33, y = 1.5, xend = 0.33, yend = 3.5), color = "#000000") +
        geom_segment(aes(x = -1, y = 2.16, xend = 1, yend = 2.16), color = "#000000") +
        geom_segment(aes(x = -1, y = 2.83, xend = 1, yend = 2.83), color = "#000000") +
        scale_color_gdocs() +
        labs(color='<b>Pitch Type</b><br>')+
        xlab('Ball Side') +
        ylab('Ball Height')+
        xlim(-5, 5) +
        ylim(-1, 5.5) + 
        coord_fixed(ratio = 1) +
        ggtitle(plotTitle)
      
      
      # if(length(d)>1) {
      #   kz <- kz + annotation_raster(raster3, -3.7, .8, -.3, 6) + annotation_raster(raster, -.8, 3.7, -.3, 6)
      # }
      # else if (d=="Right") {
      #   kz <- kz + annotation_raster(raster, -.8, 3.7, -.3, 6)
      # } 
      # else if(d=='Left') {
      #   kz <- kz + annotation_raster(raster3, -3.7, .8, -.3, 6)
      # }
      kz <- ggplotly(kz, tooltip = 'text')
      
    })
    
    
  }
  
  # Heat Map Tab
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
  
  
  
  # batting heat map
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
  
  
  
  
  
  
  # Release Point
  #update
  plotDat <- reactive({
    filter(data, Season %in% input$Season3)
  })
  observeEvent(plotDat(), {
    choices <- sort(unique(plotDat()$Game))
    updatePickerInput(session = session, inputId="Game3", choices = choices)
  })
  
  plotDat2 <- reactive({
    req(input$Game3)
    filter(plotDat(), Game %in% input$Game3)
  })
  observeEvent(plotDat2(), {
    choices <- sort(unique(plotDat2()$Pitcher))
    updatePickerInput(session = session, inputId = "Pitcher3", choices = choices)
  })
  
  plotDat3 <- reactive({
    req(input$Pitcher3)
    filter(plotDat2(), Pitcher %in% input$Pitcher3)
  })
  plotDat4 <- reactive({
    plotDat3() %>%
      filter(Game %in% input$Game3,
             Pitcher %in% input$Pitcher3,
             TaggedPitchType %in% input$Pitch3)
  })
  
  output$p1 <- renderPlotly({
    pdat <- plotDat4()
    
    # photo <- readPNG('www/mound.png', native = TRUE)
    # pic2 <- image_read(photo)
    # image <- image_fill(pic2, 'none')
    # raster <- as.raster(image)
    
    
    plotTitle <- paste0("<b>Release Point Analysis</b><br>")
    a= pdat$Game
    b= pdat$TaggedPitchType
    c= pdat$Pitcher
    e= pdat$Inning
    f= round(pdat$RelSpeed, 1)
    
    
    p1 <- ggplot(pdat, 
                 aes(x = RelSide, y = RelHeight, color= TaggedPitchType)) +
      geom_point(aes(text = paste0('<b>',c,'</b>', '<br>','<b>Game: </b>',a,'<br>', '<b>Inning: </b>',e ,'<br>', '<b>','Velocity: ','</b>', f,'<br><b>Pitch Type: </b>', b)), alpha = (3/5), size = 1.2 ) +
      facet_wrap(~ Game)+
      #annotation_raster(raster, -6, 6, -1.5, 3)+
      scale_color_gdocs() +
      labs(color='<b>Pitch Type</b><br>')+
      xlab('Release Side') +
      ylab('Realse Height')+
      xlim(-3, 3) +
      ylim(3, 8) + 
      coord_fixed(ratio = 1) +
      ggtitle(plotTitle)
    p1 <- ggplotly(p1, tooltip = 'text')
  })
  
  
  
  
  #pitch velo
  # plotV <- reactive({
  #   subV <- data %>%
  #     group_by(GameID, Pitcher) %>%
  #     arrange(Time) %>%
  #     mutate(PitchCount = row_number()) %>% 
  #     filter(
  #       Pitcher %in% input$Pitcher4,
  #       TaggedPitchType %in% input$Pitch4,
  #       Game %in% input$Game4)
  #   subV
  # })
  
  plotVel <- reactive({
    filter(data, Season %in% input$Season4)
  })
  observeEvent(plotVel(), {
    choices <- sort(unique(plotVel()$Game))
    updatePickerInput(session = session, inputId="Game4", choices = choices)
  })
  
  plotVel1 <- reactive({
    req(input$Game4)
    filter(plotVel(), Game %in% input$Game4)
  })
  observeEvent(plotVel1(), {
    choices <- sort(unique(plotVel1()$Pitcher))
    updatePickerInput(session = session, inputId = "Pitcher4", choices = choices)
  })
  
  plotVel2 <- reactive({
    req(input$Pitcher4)
    filter(plotVel1(), Pitcher %in% input$Pitcher4)
  })
  
  
  plotVel3 <- reactive({
    plotVel2() %>%
      filter(Season %in% input$Season4,
             Pitcher %in% input$Pitcher4,
             Game %in% input$Game4,
             TaggedPitchType %in% input$Pitch4)
  })
  
  output$p2 <- renderPlotly({
    
    Vdat <- plotVel3()
    
    plotTitle <- paste0("<b>Pitch Velocity</b><br>")
    Velocity = round(Vdat$RelSpeed, 1)
    PitchNumber = Vdat$PitchCount
    
    p2 <- ggplot(Vdat, 
                 aes(x = PitchNumber, y = Velocity, color= TaggedPitchType)) +
      geom_line(alpha=.45) +
      geom_point()+
      facet_wrap(~ Pitcher)+
      #scale_color_brewer(palette = "Dark2") +
      labs(color='<b>Pitch Type</b><br>')+
      xlab('Pitch Count') +
      #scale_color_gdocs() +
      ylab('Velocity') +
      ggtitle(plotTitle)
    p2 <- ggplotly(p2)
  })
  
  
  
  
  
  # Pitch Velocity Distribution
  #update
  plotDist <- reactive({
    filter(data, Season %in% input$Season7)
  })
  observeEvent(plotDist(), {
    choices <- unique(plotDist()$Game)
    updatePickerInput(session = session, inputId="Game7", choices = choices)
  })
  
  plotDist2 <- reactive({
    req(input$Game7)
    filter(plotDist(), Game %in% input$Game7)
  })
  observeEvent(plotDist2(), {
    choices <- sort(unique(plotDist2()$Pitcher))
    updatePickerInput(session = session, inputId = "Pitcher7", choices = choices)
  })
  
  plotDist3 <- reactive({
    req(input$Pitcher7)
    filter(plotDist2(), Pitcher %in% input$Pitcher7)
  })
  plotDist4 <- reactive({
    plotDist3() %>%
      filter(Game %in% input$Game7,
             Pitcher %in% input$Pitcher7,
             TaggedPitchType %in% input$Pitch7)
  })
  
  inat <- reactive({
    get_inat_obs(Photo_Url = input$Pitcher5)
  })
  output$image1 <- renderUI({
    kdat <- plotKZ5()
    tags$img(src=kdat$Photo_Urls[1], height = "150px", align = 'left')
  })
  
  output$dist <- renderPlotly({
    pdist <- plotDist4()
    
    
    
    
    plotTitle <- paste0("<b>Velocity Distributions</b><br>")
    a= pdist$Pitcher
    f= round(pdist$RelSpeed, 1)
    
    dist <- ggplot(pdist, aes(x=RelSpeed, fill=Pitcher)) + 
      geom_density(aes(text = paste0('<b>',a,'</b>')), alpha=.3)+
      facet_wrap(~ TaggedPitchType)+
      labs(color='<b>Pitcher</b><br>')+
      xlab('Velocity (mph)') +
      ylab('Frequency of Speed')+
      ggtitle(plotTitle)
    
    dist <- ggplotly(dist, tooltip = 'text')
  })
  
  
  # Pitch Movement Profiles
  #update
  plotM <- reactive({
    filter(data, Season %in% input$Season6)
  })
  observeEvent(plotM(), {
    choices <- sort(unique(plotM()$Game))
    updatePickerInput(session = session, inputId="Game6", choices = choices)
  })
  
  plotMove <- reactive({
    req(input$Game6)
    filter(plotM(), Game %in% input$Game6)
  })
  observeEvent(plotMove(), {
    choices <- sort(unique(plotMove()$Pitcher))
    updatePickerInput(session = session, inputId = "Pitcher6", choices = choices)
  })
  
  plotMove2 <- reactive({
    req(input$Pitcher6)
    filter(plotMove(), Pitcher %in% input$Pitcher6)
  })
  plotMove3 <- reactive({
    plotMove2() %>%
      filter(Pitcher %in% input$Pitcher6,
             TaggedPitchType %in% input$Pitch6,
             Game %in% input$Game6)
  })
  
  output$hv <- renderPlotly({
    pmove <- plotMove3()
    
    plotTitle <- paste0("<b>Pitch Movement</b><br>")
    Velocity = round(pmove$RelSpeed, 1)
    r = pmove$PlayResult
    PitchNumber = pmove$PitchCount
    s = round(pmove$SpinRate, 2)
    a = pmove$Game
    e = pmove$Inning
    f= round(pmove$RelSpeed, 1)
    tilt = pmove$Tilt
    
    hv <- ggplot(pmove, 
                 aes(x = HorzBreak, y = InducedVertBreak, color= TaggedPitchType)) +
      geom_point(aes(text = paste0('<b>Game: </b>',a,'<br>', '<b>Inning: </b>',e ,'<br>','<b>Pitch Result: </b>', r,'<br><b>Velocity: </b>', f, '<br><b>Spin Rate: </b>', s, '<br><b>Tilt: </b>', tilt), alpha = (3/5)))+
      facet_wrap(~ Game)+
      #scale_color_brewer(palette = "Dark2") +
      labs(color='<b>Pitch Type</b><br>')+
      xlab('Horizontal Break') +
      scale_color_gdocs() +
      ylab('Induced Vertical Break') +
      ggtitle(plotTitle) +
      coord_fixed(ratio = 1)+
      xlim(-30, 30) +
      ylim(-30, 30)
    hv <- ggplotly(hv, tooltip = 'text')
  })
  
  plotSpin <- reactive({
    filter(data, Season %in% input$Season9)
  })
  observeEvent(plotSpin(), {
    choices <- sort(unique(plotSpin()$Game))
    updatePickerInput(session = session, inputId="Game9", choices = choices)
  })
  
  plotSpin2 <- reactive({
    req(input$Game9)
    filter(plotSpin(), Game %in% input$Game9)
  })
  observeEvent(plotSpin2(), {
    choices <- sort(unique(plotSpin2()$Pitcher))
    updatePickerInput(session = session, inputId = "Pitcher9", choices = choices)
  })
  
  plotSpin3 <- reactive({
    req(input$Pitcher9)
    filter(plotSpin2(), Pitcher %in% input$Pitcher9)
  })
  plotSpin4 <- reactive({
    plotSpin3() %>%
      filter(Game %in% input$Game9,
             Pitcher %in% input$Pitcher9,
             TaggedPitchType %in% input$Pitch9)
  })
  
  
  output$spin <- renderPlotly({
    pspin <- plotSpin4()
    
    plotTitle <- paste0("<b>Spin Rate Distributions</b><br>")
    a= pspin$Pitcher
    f= round(pspin$SpinRate, 1)
    
    spindist <- ggplot(pspin, aes(x=SpinRate, fill=Pitcher)) + 
      geom_density(aes(text = paste0('<b>',a,'</b>')), alpha=.3)+
      facet_wrap(~ TaggedPitchType)+
      labs(color='<b>Pitcher</b><br>')+
      xlab('Spin Rate') +
      ylab('Frequency of Spin Rate')+
      ggtitle(plotTitle)
    
    spindist <- ggplotly(spindist, tooltip = 'text')
  })
  
  
  # Spray chart
  plotSpray <- reactive({
    filter(data, Season %in% input$Season8)
  })
  observeEvent(plotSpray(), {
    choices <- sort(unique(plotSpray()$Game))
    updatePickerInput(session = session, inputId="Game8", choices = choices)
  })
  
  plotSpray2 <- reactive({
    req(input$Game8)
    filter(plotSpray(), Game %in% input$Game8)
  })
  observeEvent(plotSpray2(), {
    choices <- sort(unique(plotSpray2()$Pitcher))
    updatePickerInput(session = session, inputId = "Pitcher8", choices = choices)
  })
  
  plotSpray3 <- reactive({
    req(input$Pitcher8)
    filter(plotSpray2(), Pitcher %in% input$Pitcher8)
  })
  plotSpray4 <- reactive({
    plotSpray3() %>%
      filter(Pitcher %in% input$Pitcher8,
             TaggedPitchType %in% input$Pitch8,
             Game %in% input$Game8,
             HitType %in% input$HitType8,
             PlayResult %in% input$PlayResult8)
  })
  
  # inat <- reactive({
  #   get_inat_obs(Photo_Url = input$Pitcher8)
  # })
  # output$image2 <- renderUI({
  #   tags$img(src=plotSpray4$Photo_Urls[1], height = "150px", align = 'left')
  # })
  
  output$spray <- renderPlotly({
    spraydata <- plotSpray4()
    
    
    # playerhead1 <- image_read(plotSpray4()$Photo_Urls[1])
    # playerhead2 <- image_fill(playerhead1, 'none') %>% image_colorize(opacity = 40, color = 'white')
    # raster10 <- as.raster(playerhead2)
    
    a = spraydata$Game
    b = spraydata$HitType
    c = spraydata$PlayResult
    d = spraydata$Inning
    e = spraydata$TaggedPitchType
    
    spray <- ggplot(spraydata, 
                  aes(x = hitlocationx, y = hitlocationy, color=  PlayResult)) +
      geom_mlb_stadium(stadium_ids = "indians" ,stadium_segments = "all", stadium_transform_coords = TRUE ) + 
      geom_point(aes(text = paste0('<b>Game: </b>',a, 
                                   '<br><b>Inning: </b>', d,
                                   '<br><b>Hit Type: </b>', b,
                                   '<br><b>Play Result: </b>', c,
                                   '<br><b>Pitch Type: </b>', e)
                     ) ,
                 alpha = (4/5), size = 1.5 )+
      facet_wrap(~ Pitcher)+
      coord_fixed(ratio = 1)+
      xlab('') +
      ylab('') +
      scale_color_gdocs() +
      #annotation_raster(raster10, -400, -300, 250, 400)+
      xlim(-400, 400) +
      ylim(-100, 430)
    spray <- ggplotly(spray, tooltip = 'text')
  })
  
  
  ## Ump Plot
  plotUmp <- reactive({
    umpReact <- data %>%
      filter(Date %in% input$game,
             PitchCall %in% c("BallCalled", "StrikeCalled")) %>%
      mutate(
        realStrike = ifelse((PlateLocHeight>=1.6 & PlateLocHeight <= 3.5 & PlateLocSide >= -0.95 
                             & PlateLocSide <= 0.95), 1, 0),
        realBall = ifelse((PlateLocHeight>=1.6 & PlateLocHeight <= 3.5 & PlateLocSide >= -0.95 
                           & PlateLocSide <= 0.95), 0, 1),
        ballCalledBall = ifelse((realStrike == 0 & PitchCall == 'BallCalled'), 1, 0),
        strikeCalledStrike = ifelse((realStrike == 1 & PitchCall == 'StrikeCalled'), 1, 0),
        goodCall = ifelse((realStrike == 0 & PitchCall == "BallCalled") 
                          | (realStrike == 1 & PitchCall == "StrikeCalled"), 1, 0)
      ) %>%
      mutate(RealDate = as.Date(Date, format = "%m/%d/%y")) %>%
      arrange(RealDate)
    umpReact
  })
  
  output$umpireTable <- renderTable({
    umpData <- plotUmp()
    
    ballsCalledBallsProp <- sum(umpData$ballCalledBall)/sum(umpData$realBall) # wrong
    strikesCalledStrikesProp <- sum(umpData$strikeCalledStrike)/sum(umpData$realStrike)
    correctCallsProp <- sum(umpData$goodCall)/nrow(umpData)
    Pitcher = umpData$Pitcher
    Inning = umpData$Inning
    Date1 = umpData$GameDate
    
    df <- data.frame(ballsCalledBallsProp,strikesCalledStrikesProp, correctCallsProp)
    names(df) <- c("% balls called correctly", "% strikes called correctly", "% correct calls")
    
    df
  })
  
  output$umpirePlot <- renderPlotly({
    umpData <- plotUmp()
    plotTitle <- paste0(umpData$Date," Home Plate Umpire Report")
    ggplotly(
      ggplot(umpData, 
             aes(x = PlateLocSide, y = PlateLocHeight, color = PitchCall)) +
        geom_point(aes(text = paste0(Pitcher, '<br>Inning: ',Inning)),alpha = 5/6, size = 1.5 ) + 
        geom_segment(aes(x = -1, y = 1.5, xend = 1, yend = 1.5), color = "#000000") +
        geom_segment(aes(x = -1, y = 3.5, xend = 1, yend = 3.5), color = "#000000") +
        geom_segment(aes(x = -1, y = 1.5, xend = -1, yend = 3.5), color = "#000000") +
        geom_segment(aes(x = 1, y = 1.5, xend = 1, yend = 3.5), color = "#000000") +
        geom_segment(aes(x = -0.33, y = 1.5, xend = -0.33, yend = 3.5), color = "#000000") +
        geom_segment(aes(x = 0.33, y = 1.5, xend = 0.33, yend = 3.5), color = "#000000") +
        geom_segment(aes(x = -1, y = 2.16, xend = 1, yend = 2.16), color = "#000000") +
        geom_segment(aes(x = -1, y = 2.83, xend = 1, yend = 2.83), color = "#000000") +
        scale_color_manual(values=c("#73B761", "#E13102")) +
        xlim(-5, 5) +
        ylim(-1, 6) + 
        coord_fixed(ratio = 1) +
        ggtitle(plotTitle), tooltip = 'text')
  })
  
  
  
  
}

