#### UI CODE SNIPPET (NOT OPERATIONAL) ####

# Pitching Heat Map Tab
tabPanel("Pitching Heat Maps",
         fluidRow(
           column(2,
                  htmlOutput("imageH"),
                  
           ),
           column(2,
                  pickerInput(
                    inputId = "SeasonHM",
                    label = "Select a Season",
                    choices = sort(unique(data$Season)),
                    #selected = c('2022'),
                    options = list(`actions-box` = TRUE),
                    multiple = TRUE
                  )),
           column(2,
                  pickerInput(
                    inputId = "GameHM",
                    label = "Select a Game",
                    choices = sort(unique(data$Game)),
                    selected = data$Game[1:100000],
                    options = list(`actions-box` = TRUE),
                    multiple = TRUE
                    
                  )),
           column(2,
                  pickerInput(
                    inputId = "PitcherHM", 
                    label = "Select a Pitcher", 
                    choices = unique(data$Pitcher),
                    selected = c('Hancock, Emerson'),
                    options = list(`actions-box` = TRUE),
                    multiple = TRUE
                  )),
           column(2,
                  pickerInput(
                    inputId = "BatterSideHM", 
                    label = "Batter Side", 
                    choices = c('Right', 'Left'),
                    selected = c('Right'),
                    options = list(`actions-box` = TRUE),
                    multiple = TRUE
                  )),
           column(2,
                  pickerInput(
                    inputId = "PitchHM",
                    label = "Select a Pitch",
                    choices = unique(data$TaggedPitchType),
                    selected = c('Fastball', 'ChangeUp', 'Curveball', 'Slider', 'Other', 'Cutter', 'Sinker', 'Splitter', 'Undefined'),
                    options = list(`actions-box` = TRUE), 
                    multiple = TRUE
                    
                  )),
           column(2,
                  pickerInput(
                    inputId = "HitTypeHM",
                    label = "Batted Ball Outcome:",
                    choices = unique(data$HitType),
                    selected = c('Undefined', 'Popup', 'LineDrive', 'GroundBall', 'FlyBall', 'Bunt'),
                    options = list(`actions-box` = TRUE),
                    multiple = TRUE
                    
                  )),
           column(2,
                  pickerInput(
                    inputId = "PitchCallHM",
                    label = "Pitch Call:",
                    choices = unique(data$PitchCall),
                    selected = c('StrikeCalled', 'BallCalled', 'FoulBall', 'InPlay', 'StrikeSwinging', 'HitByPitch', 'BallIntentional'),
                    options = list(`actions-box` = TRUE),
                    multiple = TRUE
                  ))),
         
         HTML("<br>"),
         p("Please give the application approximately 20-30 seconds to render the heat map outputs. We recommend selecting many games/data points to better represent the pitch types color-wise."),
         HTML("<br>"),
         
         plotlyOutput(outputId = "hm", height = 'auto', width = 'auto'),
         
         HTML("<br>"),
         
         tableOutput('infotable')
         
),

# Batting Heat Map
tabPanel("Batting Heat Maps",
         fluidRow(
           column(2,
                  htmlOutput("imageB"),
                  
           ),
           column(2,
                  pickerInput(
                    inputId = "SeasonHM2",
                    label = "Select a Season",
                    choices = sort(unique(data$Season)),
                    #selected = c('2022'),
                    options = list(`actions-box` = TRUE),
                    multiple = TRUE
                  )),
           column(2,
                  pickerInput(
                    inputId = "GameHM2",
                    label = "Select a Game",
                    choices = sort(unique(data$Game)),
                    selected = data$Game[1:100000],
                    options = list(`actions-box` = TRUE),
                    multiple = TRUE
                    
                  )),
           column(2,
                  pickerInput(
                    inputId = "PitcherHM2",
                    label = "Select a Batter",
                    choices = unique(data$Batter),
                    selected = c('Tate, Connor'),
                    options = list(`actions-box` = TRUE),
                    multiple = TRUE
                  )),
           column(2,
                  pickerInput(
                    inputId = "PitchHM2",
                    label = "Select a Pitch",
                    choices = unique(data$TaggedPitchType),
                    selected = c('Fastball', 'ChangeUp', 'Curveball', 'Slider', 'Other', 'Cutter', 'Sinker', 'Splitter', 'Undefined'),
                    options = list(`actions-box` = TRUE),
                    multiple = TRUE
                    
                  )),
           column(2,
                  pickerInput(
                    inputId = "HitTypeHM2",
                    label = "Batted Ball Outcome:",
                    choices = unique(data$HitType),
                    selected = c('Undefined', 'Popup', 'LineDrive', 'GroundBall', 'FlyBall', 'Bunt'),
                    options = list(`actions-box` = TRUE),
                    multiple = TRUE
                    
                  )),
           column(2,
                  pickerInput(
                    inputId = "PitchCallHM2",
                    label = "Pitch Call:",
                    choices = unique(data$PitchCall),
                    selected = c('StrikeCalled', 'BallCalled', 'FoulBall', 'InPlay', 'StrikeSwinging', 'HitByPitch', 'BallIntentional'),
                    options = list(`actions-box` = TRUE),
                    multiple = TRUE
                  ))),
         
         HTML("<br>"),
         p("Please give the application approximately 20-30 seconds to render the heat map outputs. We recommend selecting many games/data points to better represent the pitch types color-wise."),
         HTML("<br>"),
         
         plotlyOutput(outputId = "bm", height = 'auto', width = 'auto'),
         
         HTML("<br>"),
         
         tableOutput('infotable2')
),
####################