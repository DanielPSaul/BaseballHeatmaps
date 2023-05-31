# current
#install.packages("devtools", type = "win.binary")
#library(devtools)
library(shiny)
library(plyr)
library(readr)
library(shinydashboard)
library(dplyr)
library(markdown)
library(knitr)
library(ggplot2)
library(ggrepel)
library(plotly)
library(shinyWidgets)
library(png)
library(patchwork) 
library(grid)
library(magick)
library(scales)
library(bslib)
library(thematic)
library(hrbrthemes)
library(ggthemes)
library(stringr)
library(gt)
library(stats)
library(rvest)
library(tidyr)
library(wesanderson)

#mlb stadium outlines
#devtools::install_github("bdilday/GeomMLBStadiums")
#library(GeomMLBStadiums)

#thematic
#thematic_shiny(font = "auto")

# Old sample data
data20 <- read_csv("program/database/UGA_2020_Regular_Season2.csv")
#data$Date <-as.Date(data$Date,format="%Y%m%d")

# New csvs
feb18 <- read_csv("program/database/data/20220218-UGeorgia-1.csv")
feb19 <- read_csv("program/database/data/20220219-UGeorgia-1.csv")
feb20 <- read_csv("program/database/data/20220220-UGeorgia-1.csv")
feb22 <- read_csv("program/database/data/20220222-UGeorgia-1.csv")
feb25 <- read_csv("program/database/data/20220225-UGeorgia-1_unverified.csv")
feb26 <- read_csv("program/database/data/20220226-UGeorgia-1_unverified.csv")
feb26_2 <- read_csv("program/database/data/20220226-UGeorgia-2_unverified.csv")
mar4 <- read_csv("program/database/data/20220304-GeorgiaTech-1_unverified.csv")
mar5 <- read_csv("program/database/data/20220305-UGeorgia-1_unverified.csv")
mar9 <- read_csv("program/database/data/20220309-GeorgiaSouthern-1.csv")
mar11 <- read_csv("program/database/data/20220311-UGeorgia-1_unverified.csv")
mar13 <- read_csv("program/database/data/20220313-UGeorgia-1_unverified.csv")
mar13_2 <- read_csv("program/database/data/20220313-UGeorgia-2_unverified.csv")
mar15 <- read_csv("program/database/data/20220315-UGeorgia-1_unverified.csv")
mar18 <- read_csv("program/database/data/20220318-UGeorgia-1_unverified.csv")
mar19 <- read_csv("program/database/data/20220319-UGeorgia-1_unverified.csv")
mar20 <- read_csv("program/database/data/20220320-UGeorgia-1_unverified.csv")
#mar22 <- read_csv("program/database/data/20220322-UGeorgia-1_unverified.csv")
mar25 <- read_csv("program/database/data/20220325-UKentucky-1_unverified.csv")
mar26 <- read_csv("program/database/data/20220326-UKentucky-1_unverified.csv")
#mar27 <- read_csv("program/database/data/20220327-UGeorgia-1_unverified.csv")
mar29 <- read_csv("program/database/data/20220329-UGeorgia-1_unverified.csv")
mar31 <- read_csv("program/database/data/20220331-UGeorgia-1_unverified.csv")
apr1 <- read_csv("program/database/data/20220401-UGeorgia-1_unverified.csv")
apr2 <- read_csv("program/database/data/20220402-UGeorgia-1_unverified.csv")
apr15 <- read_csv("program/database/data/20220414-UGeorgia-1_unverified.csv")
apr16 <- read_csv("program/database/data/20220416-UGeorgia-1_unverified.csv")
apr19 <- read_csv("program/database/data/20220419-UGeorgia-1_unverified.csv")
apr26 <- read_csv("program/database/data/20220426-UGeorgia-1_unverified.csv")


newdata <- rbind(feb18, feb19, feb20, feb22, feb25, feb26, feb26_2, mar4, mar5,
                 mar9, mar11, mar13, mar13_2, mar15, mar18, mar19, mar20, mar25, 
                 mar26, mar29, mar31, apr1, apr2, apr15, apr16, apr19, apr26)
data22 <- newdata
data22 <- data22[!(is.na(data22$Date)), ]

data22 <- data22 %>%
  rename(HitType = TaggedHitType) %>% 
  rename(Pitcher = Pitcher)
# Transform Date to make it consistent with old data 
data22$Date <- format(data22$Date, '%m/%d/%y')

#2021 data
data21 <- read_csv("program/database/UGA_2021_Regular_Season.csv")

data21 <- data21[!(is.na(data21$Date)), ]

#data21$Time = substr(data21$Time,1,nchar(data21$Time)-3)


# Make a season column for filtering purposes
data22$Season <- '2022'
data21$Season <- '2021'
data20$Season <- '2020'

# Combine seasons 
data <- rbind.fill(data20,data21,data22)


# Remove Starting 0 in data column to make it consistent with older seasons  
data$Date <- gsub("^0", "", data$Date)

#data <- read_csv("database/UGA_2021_Regular_Season.csv")
#data2 <- read_csv("database/UGA_2021_Regular_Season.csv")

# data <- rbind(data1, data2)

# Filter for UGA pitchers only
data <- data %>%
  filter(data$PitcherTeam == "GEO_BUL")

#create game column
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

data <- data %>% drop_na(Season)
#write.csv(data,'UGA_2020_Regular_Season3.csv')

#data <- read_csv('UGA_2020_Regular_Season3.csv')

# Pitch Count Fix
data <- data %>%
  group_by(GameID, Pitcher) %>%
  arrange(Time) %>%
  mutate(PitchCount = row_number())

#xba column join
#data <- left_join(data, xba, by = c("Pitcher"="Pitcher", "Season"="Season"))

# Format Date
#data$Date = as.Date(data$Date, format = "%D")

theme2 <- bslib::bs_theme(version = 4)

ui <- tagList(
  # shinythemes::themeSelector(),
  
  navbarPage(
    
    
    title=div(tags$script(HTML("var header = $('.navbar > .container-fluid');
 header.append('<div style=\"float:left\"><a href=\"https://georgiadogs.com/sports/baseball\"><img src=\"georgialogo.png\" alt=\"alt\" style=\"float:right;width:60px;height:40px;padding-top:10px;padding-right:10px;\"> </a>`</div>');
 console.log(header)")
    )),
    
    theme = shinythemes::shinytheme("cosmo"),
    
    
    # Markdown document for Introduction/Glossary/Instructions for using app, whatever we want it to be
    tabPanel("UGA Baseball Homepage",
             
             fluidRow(
               column(4,
                      img(
                        src = "ugabaseball.jpeg", height = 425, width = 425
                      )
               ),
               
               column(3,
                      includeMarkdown("program/intro.Rmd")
               ),
               fluidRow(
                 column(5,
                        # Datatable
                        box(
                          status = "info",
                          headerPanel(HTML("<b>Schedule</b>")),
                          solidHeader = T,
                          br(),
                          DT::dataTableOutput("table5", height = "250px"),
                          width = 7,
                          height = "300px"
                        ))),
               
               
               
               
             )
             
    ),
    
    
    
    
    
    
    # TrackMan Game Data
    tabPanel("Game Data",
             
             fluidRow(
               column(2,
                      # instead of uga baseball image, put player roster photo
                      img(src = "ugabaseball.jpeg", height = 155, width = 180)
               ),
               column(2,
                      pickerInput(
                        inputId = "Season1",
                        label = "Select a Season",
                        choices = sort(unique(data$Season)),
                        selected = c('2022'),
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE
                      )),
               column(2,
                      pickerInput(
                        inputId = "Game1",
                        label = "Select a Game",
                        choices = sort(unique(data$Game)),
                        selected = data$Game[1:100000],
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE
                        
                      )),
               column(2,
                      pickerInput(
                        inputId = "Pitcher1", 
                        label = "Select a Pitcher", 
                        choices = sort(unique(data$Pitcher)),
                        selected = c("All",
                                     unique(as.character(data$Pitcher))),
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE
                      )),
               column(2,
                      pickerInput(
                        inputId = "Inning1", 
                        label = "Inning", 
                        choices = sort(unique(data$Inning)),
                        selected = data$Inning[1:100000],
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE
                      )),
               column(2,
                      pickerInput(
                        inputId = "TaggedPitchType1",
                        label = "Select a Pitch",
                        choices = unique(data$TaggedPitchType),
                        selected = c('Fastball', 'ChangeUp', 'Curveball', 'Slider', 'Other', 'Cutter', 'Sinker', 'Splitter', 'Undefined'),
                        options = list(`actions-box` = TRUE), 
                        multiple = TRUE
                        
                      )),
               #   column(2,
               #          pickerInput("Season1",
               #                      "Season:",
               #                      c("All",
               #                        sort(unique(data$Season))))
               #   ),
               #   column(2,
               #          pickerInput("Game1",
               #                      "Game:",
               #                      c("All",
               #                        sort(unique(data$Game))))
               #   ),
               #   column(2,
               #          pickerInput("Pitcher1",
               #                      "Pitcher:",
               #                      c(sort(unique(data$Pitcher))))
               #   ),
               #   # column(2,
               #   #        pickerInput("Opponent1",
               #   #                    "Opponent",
               #   #                    c("All",
               #   #                      unique(as.character(data$BatterTeam))))
               #   # ),
               #   column(2,
               #          pickerInput("Inning1",
               #                      "Inning:",
               #                      c("All",
               #                        unique(as.character(data$Inning))))
               #   ),
               #   # column(2,
               #   #        pickerInput("Batter1",
               #   #                    "Batter:",
               #   #                    c("All",
               #   #                      unique(as.character(data$Batter))))
               #   # ),
               #   column(2,
               #          pickerInput("TaggedPitchType1",
               #                      "Pitch Type:",
               #                      c("All",
               #                        sort(unique(data$TaggedPitchType))))
               #   )
               # ),
               # Create a new row for the table.
               DT::dataTableOutput("table1")
             )
    ),
    
    # TrackMan Season Data
    tabPanel("Seasons Data",
             fluidRow(
               column(2,
                      img(src = "ugabaseball.jpeg", height = 155, width = 180)
               ),
               column(2,
                      pickerInput("Season1b",
                                  "Season:",
                                  selected = 2022,
                                  c("All",
                                    sort(unique(as.character(data$Season)))))
               ),
               column(2,
                      pickerInput("Pitcher1b",
                                  "Pitcher:",
                                  c("All",
                                    sort(unique(as.character(data$Pitcher)))))
               ),
               column(2,
                      pickerInput("BatterSide1b",
                                  "Lefty/Righty:",
                                  c("All",
                                    unique(as.character(data$BatterSide))))
               ),
               column(2,
                      pickerInput("TaggedPitchType1b",
                                  "Pitch Type:",
                                  c("All",
                                    unique(as.character(data$TaggedPitchType))))
               )
             ),
             # Create a new row for the table.
             DT::dataTableOutput("table2"),
             
             ### figure out how to auto show 50 and then drop the pitchers filter 
    ),
    
    ### post game pitching report replication ----------------------
    
    tabPanel("Post Game Pitching Report",
             fluidRow(
               column(2,
                      img(src = "ugabaseball.jpeg", height = 155, width = 180)
               ),
               column(2,
                      pickerInput("Season2p",
                                  "Season:",
                                  selected = 2022,
                                  c("All",
                                    sort(unique(as.character(data$Season)))))
               ),
               column(2,
                      pickerInput("Pitcher2p",
                                  "Pitcher:",
                                  selected = c('Cannon, Jonathan'),
                                  c("All",
                                    sort(unique(as.character(data$Pitcher)))))
               ),
               column(2,
                      pickerInput("BatterSide2p",
                                  "Lefty/Righty:",
                                  c("All",
                                    unique(as.character(data$BatterSide))))
               ),
               column(2,
                      pickerInput("TaggedPitchType2p",
                                  "Pitch Type:",
                                  c("All",
                                    unique(as.character(data$TaggedPitchType))))
               )
             ),
             # Create a new row for the table.
             DT::dataTableOutput("table3"),
             
             
    ),
    
    
    
    
    
    ### post game pitching report replication ----------------------
    
    
    
    #Pitch Location Tab 
    
    tabPanel("Pitch Location Analysis",
             fluidRow(
               column(2,
                      htmlOutput("image1"),
                      
               ),
               column(2,
                      pickerInput(
                        inputId = "Season5",
                        label = "Select a Season",
                        choices = sort(unique(data$Season)),
                        #selected = c('2022'),
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE
                      )),
               column(2,
                      pickerInput(
                        inputId = "Game5",
                        label = "Select a Game",
                        choices = sort(unique(data$Game)),
                        selected = data$Game[1:100000],
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE
                        
                      )),
               column(2,
                      pickerInput(
                        inputId = "Pitcher5", 
                        label = "Select a Pitcher", 
                        choices = unique(data$Pitcher),
                        selected = c('Hancock, Emerson'),
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE
                      )),
               column(2,
                      pickerInput(
                        inputId = "BatterSide5", 
                        label = "Batter Side", 
                        choices = c('Right', 'Left'),
                        selected = c('Right'),
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE
                      )),
               column(2,
                      pickerInput(
                        inputId = "Pitch5",
                        label = "Select a Pitch",
                        choices = unique(data$TaggedPitchType),
                        selected = c('Fastball', 'ChangeUp', 'Curveball', 'Slider', 'Other', 'Cutter', 'Sinker', 'Splitter', 'Undefined'),
                        options = list(`actions-box` = TRUE), 
                        multiple = TRUE
                        
                      )),
               column(2,
                      pickerInput(
                        inputId = "HitType5",
                        label = "Batted Ball Outcome:",
                        choices = unique(data$HitType),
                        selected = c('Undefined', 'Popup', 'LineDrive', 'GroundBall', 'FlyBall', 'Bunt'),
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE
                        
                      )),
               column(2,
                      pickerInput(
                        inputId = "PitchCall5",
                        label = "Pitch Call:",
                        choices = unique(data$PitchCall),
                        selected = c('StrikeCalled', 'BallCalled', 'FoulBall', 'InPlay', 'StrikeSwinging', 'HitByPitch', 'BallIntentional'),
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE
                      ))),
             
             HTML("<br>"),
             
             plotlyOutput(outputId = "kz", height = 'auto', width = '100%')
             
    ),
    
    ###################
    # Heat Map tab
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
    
    
    
    # Release Point Graphic
    tabPanel("Release Point Analysis",
             fluidRow(
               column(2,
                      pickerInput(
                        inputId = "Season3",
                        label = "Select a Season",
                        choices = sort(unique(data$Season)),
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE
                      )),
               column(2, 
                      pickerInput(inputId = "Game3",
                                  label = "Select a Game",
                                  choices = unique(data$Game),
                                  selected = data$Game[1:100000],
                                  options = list(`actions-box` = TRUE),
                                  multiple = TRUE
                      )),
               column(2,
                      pickerInput(inputId = "Pitcher3", 
                                  label = "Select a Pitcher",
                                  choices = unique(data$Pitcher),
                                  #selected = c('Hancock, Emerson', 'Cannon, Jonathan', 'Wilcox, Cole'),
                                  options = list(`actions-box` = TRUE),
                                  multiple = TRUE
                      )),
               column(2,
                      pickerInput(inputId = "Pitch3",
                                  label = "Select a Pitch",
                                  choices = unique(data$TaggedPitchType),
                                  selected = data$TaggedPitchType[1:10],                     
                                  options = list(`actions-box` = TRUE),
                                  multiple = TRUE
                      )),
             ),
             plotlyOutput(outputId = "p1",height = 'auto', width = 'auto')
    ),
    
    
    # Pitch Velocity Graphic
    tabPanel("Pitch Velocity",
             fluidRow(style = "border: 4px black;",
                      column(2, pickerInput(
                        inputId = "Season4",
                        label = "Select a Season",
                        choices = sort(unique(data$Season)),
                        #selected = c('2022'),
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE
                      )),      
                      column(2, pickerInput(
                        inputId = "Game4",
                        label = "Select a Game",
                        choices = unique(data$Game),
                        options = list(`actions-box` = TRUE),
                        multiple = FALSE
                        
                      )), 
                      column(2, pickerInput(
                        inputId = "Pitcher4", 
                        label = "Select a Pitcher", 
                        choices = unique(data$Pitcher),
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE
                      )),
                      column(2, pickerInput(
                        inputId = "Pitch4",
                        label = "Select a Pitch",
                        choices = unique(data$TaggedPitchType),
                        selected = c('Fastball', 'ChangeUp', 'Curveball', 'Slider', 'Other', 'Cutter', 'Sinker', 'Splitter', 'Undefined'),               options = list(`actions-box` = TRUE), 
                        multiple = TRUE
                        
                      )),
                      
             ),
             splitLayout(
               style = "border: 3px solid black;",
               plotlyOutput(outputId = "p2")),
             
             
             html('<br>'),
             
             fluidRow(
               column(2,
                      pickerInput(
                        inputId = "Season7",
                        label = "Select a Season",
                        choices = sort(unique(data$Season)),
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE
                      )),
               column(2,
                      pickerInput(
                        inputId = "Game7",
                        label = "Select a Game Date",
                        #selected = data$Game[1:100000],
                        choices = sort(unique(data$Game)),
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE
                      )),
               column(2, 
                      pickerInput(
                        inputId = "Pitcher7", 
                        label = "Select a Pitcher(s)",
                        choices = unique(data$Pitcher),
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE
                      )),
               column(2,
                      pickerInput(
                        inputId = "Pitch7",
                        label = "Select a Pitch",
                        selected = c('Fastball', 'ChangeUp', 'Curveball', 'Slider', 'Other', 'Cutter', 'Sinker', 'Splitter', 'Undefined'),               choices = unique(data$TaggedPitchType),
                        options = list(`actions-box` = TRUE), 
                        multiple = TRUE
                      )),
             ),
             
             splitLayout(
               style = "border: 3px solid black;",
               plotlyOutput(outputId = "dist")),
             
             
    ),
    
    # Pitch Movement Profiles
    tabPanel("Pitch Movement Profile",
             fluidRow(
               column(2,
                      pickerInput(
                        inputId = "Season6",
                        label = "Select a Season",
                        choices = sort(unique(data$Season)),
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE
                      )),
               column(2,
                      pickerInput(
                        inputId = "Game6",
                        label = "Select a Game Date",
                        #selected = data$Game[1:100000],
                        choices = sort(unique(data$Game)),
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE
                      )),
               column(2, 
                      pickerInput(
                        inputId = "Pitcher6", 
                        label = "Select a Pitcher", 
                        #selected = c('Cannon, Jonathan', 'Hancock, Emerson', 'Wilcox, Cole', 'Ross, Dylan'),
                        choices = unique(data$Pitcher),
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE
                      )),
               column(2,
                      pickerInput(
                        inputId = "Pitch6",
                        label = "Select a Pitch",
                        selected = c('Fastball', 'ChangeUp', 'Curveball', 'Slider', 'Other', 'Cutter', 'Sinker', 'Splitter', 'Undefined'),               choices = unique(data$TaggedPitchType),
                        options = list(`actions-box` = TRUE), 
                        multiple = TRUE
                      )),
             ),
             
             
             splitLayout(
               style = "border: 3px solid black;",
               plotlyOutput(outputId = "hv")),
             
             html('<br>'),
             
             fluidRow(
               column(2,
                      pickerInput(
                        inputId = "Season9",
                        label = "Select a Season",
                        choices = sort(unique(data$Season)),
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE
                      )),
               column(2,
                      pickerInput(
                        inputId = "Game9",
                        label = "Select a Game Date",
                        #selected = data$Game[1:100000],
                        choices = sort(unique(data$Game)),
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE
                      )),
               column(2, 
                      pickerInput(
                        inputId = "Pitcher9", 
                        label = "Select a Pitcher(s)",
                        choices = unique(data$Pitcher),
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE
                      )),
               column(2,
                      pickerInput(
                        inputId = "Pitch9",
                        label = "Select a Pitch",
                        selected = c('Fastball'),               
                        choices = unique(data$TaggedPitchType),
                        options = list(`actions-box` = TRUE), 
                        multiple = TRUE
                      )),
             ),
             
             splitLayout(
               style = "border: 3px solid black;",
               plotlyOutput(outputId = "spin")),
             
             
    ),
    
    # Spray chart
    tabPanel("Spray Chart",
             fluidRow(
               column(2,
                      pickerInput(
                        inputId = "Season8",
                        label = "Select a Season",
                        choices = sort(unique(data$Season)),
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE
                      )),
               column(2,
                      pickerInput(
                        inputId = "Game8",
                        label = "Select a Game Date",
                        choices = sort(unique(data$Game)),
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE
                      )),
               column(2, 
                      pickerInput(
                        inputId = "Pitcher8", 
                        label = "Select a Pitcher", 
                        choices = unique(data$Pitcher),
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE
                      )),
               column(2,
                      pickerInput(
                        inputId = "Pitch8",
                        label = "Select a Pitch",
                        selected = c('Fastball', 'ChangeUp', 'Curveball', 'Slider', 'Other', 'Cutter', 'Sinker', 'Splitter', 'Undefined'),               choices = unique(data$TaggedPitchType),
                        options = list(`actions-box` = TRUE), 
                        multiple = TRUE
                      )),
               column(2,
                      pickerInput(
                        inputId = "HitType8",
                        label = "Select a  HitType",
                        selected = c('Bunt', 'FlyBall', 'Curveball', 'GroundBall', 'LineDrive', 'Popup', 'NA'),               
                        choices = unique(data$HitType),
                        options = list(`actions-box` = TRUE), 
                        multiple = TRUE
                      )),
               column(2,
                      pickerInput(
                        inputId = "PlayResult8",
                        label = "Select a  Play Result",
                        selected = data$PlayResult[1:100000],
                        choices = unique(data$PlayResult),
                        options = list(`actions-box` = TRUE), 
                        multiple = TRUE
                      )),
             ),
             
             splitLayout(
               style = "border: 3px solid black;",
               plotlyOutput(outputId = "spray", height = 600, width = 1400)),
             
             
    ),
    
    # Umpire Post-Game Analysis
    tabPanel("Umpire Analysis",
             sidebarLayout(
               sidebarPanel(
                 selectInput("game", "Game Date:", choices = sort(data$Date), selected=as.factor(levels(data$date)[1])),
                 "All pitches that are not called strikes or called balls have been filtered out. These
                 metrics are only evaluating pitches where the umpire must make a call (excluding check-swing
                 decisions)."
               ),
               
               mainPanel(plotlyOutput("umpirePlot",height = 'auto', width = 'auto'),
                         tableOutput("umpireTable"))
             )
    )
  ), setBackgroundColor("#FFFFFF")
)