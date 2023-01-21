ui = tagList(
  div(style = "background-color:#59A96A;padding: 1px 40px;height: 40px",
      titlePanel(title="",windowTitle="FUTMaster")),
  tags$head(
    tags$style(HTML('.navbar-nav > li > a, .navbar-brand {
                            padding-top:10px !important; 
                            padding-bottom:0px !important;
                            height: 10px;
                            }
                           .navbar {min-height:80px !important;}')),
    tags$style(".fa-trophy {font-size: 30px; padding-top: 20px; color:#ffd700", inline = TRUE)),
  navbarPage(theme = shinythemes::shinytheme("simplex"),
             title = div(img(src = "football-icon-vector-13.jpg", id = "logo",
                             width = "60",
                             height = "60")),
             navbarMenu(icon("trophy", class = NULL, lib = "font-awesome", verify_fa = FALSE, inline = TRUE),
                        tabPanel("Home", 
                                  h2(div(style = "color: black; display:inline;", "Welcome to FUT Master")),
                                  slickROutput("slidepics", 
                                               width = "50%",
                                               height = "auto"),
                                  br(),
                                  h3(" FIFA Ultimate Team 2022"),
                                  p("FIFA Ultimate Team or FUT is the online game modus of the world-famous
                                  football simulation game FIFA. The games are released yearly with FIFA 22 
                                  representing the latest version of the game, being released in October 2021.
                                  FUT is based around players creating their own personal team based on cards that
                                  can be collected in the game. By playing against other players online with the
                                  aforementioned team, a player earns in-game currency which allows the player to
                                  purchase new players. The aim is to build a very strong and well-balanced - the ultimate - team.
                                  The coin gains are heavily based on in-game performance and thus, rewards the better
                                  performing players. It needs to be added, that players can also invest their personal
                                  money and purchase more in-game currency to improve their team."),
                                  
                                  p("Regardless, of how the currency is earned, the process of improving the team 
                                  is not as straightforward, as just purchasing players with higher overall scores. 
                                  The team performance is significantly improved when the chemistry is high. 
                                  The chemistry score is calculated based on the players in the team linking according 
                                  to their clubs they play for, Leagues they play in, and nationalities, as well as 
                                  their position. Putting a player in a position which is not the position mentioned
                                  on the card, a penalty will be put on the chemistry score. Players sharing the same
                                  league they play in, the same club they play for and / or the same nationality get a
                                  chemistry boost. This chemistry score is very powerful and players need to maximise it
                                  in order to enhance their performance on the pitch."), 
                                  
                                  p("As the producer of the game (Electronic Arts) releases a lot of player cards and adds
                                  more to the market every week, the search for the perfect match for a players team is
                                  close to the search of a needle in the haystack. The game nor any external website is
                                  providing an adequate solution for this issue. This app is aiming at helping professional
                                  players, streamers and all hobby-players to find the most suitable next addition to their
                                  personal squad."),
                                    
                                  p("The app is looking beyond the three important categories regarding the chemistry 
                                  (nationality, club & league) and is taking into consideration the playing style of the
                                  player. For that the six most important player attributes (Pace, Shooting, Dribbling,
                                  Passing, Defending and Physicality) are analysed per player. This allows the gamer to 
                                  specifically look for player which are according to his/her playing style. The playing 
                                  style could be highly focused at very technical players (high dribbling), at very good 
                                  passing players (high players) amongst others.  The network data analysis will be able 
                                  to provide tailored solutions for the playing style in combination with the three important 
                                  chemistry categories (nationality, club & league)."),
                                  
                                  br(),
                                  h3("General Description"),
                                  p("The data used for this analysis stems from two datasets which were merged to create
                                  the dataframe necessary for this analysis. The dataframe contains 2250 rows with the
                                  best 2250 cards in the game FIFA 21. These logically include many players several times
                                  as many - especially good - players have received several special cards next to their 
                                  normal card, thanks to special performances during the season. The dataset provides the 
                                  most important cards for all FIFA players and resembles the sample of cards which all
                                  players use for their squad creation. This is due to the fact that all other cards have 
                                  such low ratings, that a competitive match could not be won."),
                                  
                                  br(),
                                  h3("The Team"),
                                  p("This app was created by a team of Network 
                                    Data Analysis students as part of the MSc in 
                                    Business Information Management program. The
                                    four students who created this app are 
                                    Sheila Shukla (497418), Alexander Rasche 
                                    (493715), Yazeed  Abul - Ata (494734), 
                                    and Niels Vehring (472367).")),
                        tabPanel("Descriptive Statistics",
                                 mainPanel(h1("Descriptive Statistics"),
                                           p("This tab provides the descriptive 
                                       statistics for each player in the top 2500 cards in FIFA 21. 
                                       The data table below provides the viewer with an 
                                       overview of information such as the nationality,
                                       league, club, position and all important skills."),
                                           br(),
                                           br(),
                                  tabsetPanel(
                                    tabPanel("Player Statistics", h3("Player Overview"),
                                             p("This tab provides the descriptive statistics for each player card in the game included in the 
                                               data set. The data table below shows important characteristics of each card regarding its overall rating, its version,
                                               the ratings of the six most important skills (pace, passing, shooting, dribbling, defending and physicality), the 
                                               nationality and club of th eplyaer, as well as the cards price."),
                                             selectInput(inputId = "statistics_position", label = "Choose Variable to be plotted over years", 
                                                         choices = 
                                                           c("ST" = "ST", "RWB" = "RWB",
                                                                "RW"="RW","RM"="RM","RF"="RF",
                                                                "RB"="RB","LWB"="LWB",
                                                                "LW"="LW","LM"="LM",
                                                                "LF"="LF","LB"="LB","GK"="GK",
                                                                "CM"="CM","CF"="CF","CDM"="CDM",
                                                                "CB"="CB","CAM"="CAM"),
                                                         selected = c("CF"), multiple = T),
                                           tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected {background-color: #58508d !important;}')),
                                           DT::dataTableOutput("pstatistics"),
                                             selectInput("statistics_selection","Choose category",
                                           choices = c("Overall Rating", 
                                                       "Positions", 
                                                       "Price")
                                           ),
                                           br(),
                                 plotlyOutput("player_hist"),
                                 textOutput("player_mean"),
                                 textOutput("player_sd"),
                                 textOutput("phist_sd")
                                 ),
                                   tabPanel("Nationality Statistics", h3("Nationality Overview"),
                                            p("This table gives an aggregated overview per nationality of each card. As nationality is an important factor for the team chemistry, this is an important breakdown
                                            of all cards. The n_cards column gives the amount of cards with the corresponding nationality, and n_players the amount of players from that particular nationality. The rest 
                                            of the columns give the mean and stadard deviation for the overall rating and price of each card per nationality."),
                                            tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected {background-color: #58508d !important;}')),
                                            DT::dataTableOutput("nstatistics"),
                                            br(),
                                              plotlyOutput("nhist")
                                            ),
                               tabPanel("League Statistics", h3("League Overview"),
                                        p("This table gives an aggregated overview per league of each card. As the league of a player is an important factor for the team chemistry, this is an important breakdown
                                            of all cards. The n_cards column gives the amount of cards with the corresponding league, and n_players the amount of players from that particular league The rest 
                                            of the columns give the mean and stadard deviation for the overall rating and price of each card per league. Note:
                                            Icon is also considered a league within FIFA."),
                                        tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected {background-color: #58508d !important;}')),
                                        DT::dataTableOutput("lstatistics"),
                                        br(),
                                                         plotlyOutput("lhist")
                                        ),
                               tabPanel("Attribute Statistics", h3("Attribute Overview"),
                                        p("For this last table the six major skills (pace, passing, shooting, dribbling, defending and physicality) are aggregated. The table gives
                                          the standard deviation and mean of the rating per skill and the further columns give counts of how often a card in the data set has been rated very good, good or average 
                                          for this particular skill. A very good rating is given to all cards with 90 or higher in this skills. The good rating was awarded to cards with a rating between
                                          80 and 90 for this particular skill, and the average rating was appplied to cards with a rating between 70 and 80."),
                                        tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected {background-color: #58508d !important;}')),
                                        DT::dataTableOutput("astatistics"),
                                        br(),
                                        selectInput("statistics_attributes","Choose attribute",
                                                    choices = c("Pace","Shooting", 
                                                                "Passing","Dribbling",
                                                                "Defense","Physicality")
                                                    ),
                                        plotlyOutput("ahist")
                                        )
                               )
                               )
                               ),
                                    tabPanel("Network Exploration",
                                                 mainPanel(h1("Network Exploration"),
                                             p("This tab provides information about the network itself. The analysis regarding the composition of the network,
                                               according to the nodes is displyaed below."),
                                    tabsetPanel(
                                         tabPanel("Player Attribute Network", 
                                                  sidebarPanel(
                                                    selectInput("network_selection", "Choose the Network specifics",
                                                                choices = c("Bipartite Network: Players and Attributes",
                                                                            "Projected Network: Players based on Attributes",
                                                                            "Projected Network: Attributes based on Players")),
                                                        plotlyOutput("network_histograms"),
                                                    br(),
                                                        textOutput("network_path"),
                                                        textOutput("diameter"),
                                                        textOutput("edges"),
                                                        textOutput("node_count"),
                                                        textOutput("clustering_coefficient")
                                                    ),
                                                    mainPanel(h3("FIFA Player Attribute Network Statistics"),
                                                            p("In this section, the FIFA Player Attribute Network and its detailed information are displayed below. The FIFA Player Attribute Network 
                                                            consists of the ratings given to all six different skills according to their numerical value on the specific card. This section allows you to 
                                                            view the different analysis levels of the FIFA Player Attribute Network."),
                                                            tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected {background-color: #58508d !important;}')),
                                                            DT::dataTableOutput("statistics")          
                                                  )
                                                  ),
                                         tabPanel("Bipartite Network Visualization", 
                                                  mainPanel(h3("Bipartite Network between Developer and Games
                                                                of the Top 500 critically acclaimed games"),
                                                            p("In this section, the bipartite networks among nationalities and leagues are presented. The network shows the connections within the groups 
                                                              of a common league or a common nationality. As these are the 2 key ingredients for a balanced, high-chemistry FIFA team, these networks represent a 
                                                              viable opportunity to scout a new matching player for the Ultimate Team"),
                                                            ),
                                                    tabsetPanel(
                                                    tabPanel("Bipartite Nationality",
                                                             selectInput("nation_selection", "Choose Nation:",
                                                                         choices = c(dt.fut.22.nation$nationality)
                                                                         ),
                                                    visNetworkOutput("vis.network.1")
                                                    ),
                                                    tabPanel("Bipartite League",
                                                        selectInput("league_selection", "Choose League:",
                                                                    choices = unique(dt.bipartite.data$league_name)
                                                                    ),
                                                        visNetworkOutput("vis.network.2")
                                                      ),
                                                    )
                                                  ),
                                    tabPanel("Projection into Playerspace", 
                                             sidebarPanel(
                                               multiInput(
                                                 inputId = "pn_player", label = "Select a Player :",
                                                 choices = unique(dt.bipartite.data$NameId),
                                                 selected = c("A. Bakasetas - TOTS - (86)","A. Armstrong - What If - (85)", "A. Areola - Rare - (82)", width =
                                                                "350px")
                                                 ),
                                              ),
                                             mainPanel(
                                               h3("Projection into Player Space"),
                                               p("In the following section you can plot the connection between different players by the attributes they have in common. 
                                                 You can select different players and visualize the similarities of their attributes based on the
                                                 width of the relevant edge. Therefore, users who are deciding between potential player acquisitions can input all their
                                                 options into this network and see which are more similar and which are different. This will allow users to evaluate replacement
                                                 options for their teams."),  
                                               visNetworkOutput("vis.network.3")
                                             )
                                             )
                                    )
                                    )
                                    ),
                          tabPanel("Similarity Analysis",
                                   sidebarPanel(width = 4,
                                                selectInput("similarity_player", "Choose a player:", paste(dt.fut.22.Id$NameId), selected = "L. Messi - TOTY - (98)"
                                                          ),
                                                selectInput("similarity_card", "Choose a card type:", choices = c(
                                                                                                          "Flashback SBC",
                                                                                                          "FOF PTG",
                                                                                                          "FUT Birthday",
                                                                                                          "FUT Freeze",
                                                                                                          "FUT Future Stars",
                                                                                                          "Futties",
                                                                                                          "Headliners",
                                                                                                          "Hero",
                                                                                                          "Icon",
                                                                                                          "Icon Moment",
                                                                                                          "IF",
                                                                                                          "League Player",
                                                                                                          "MOTM",
                                                                                                          "Non-Rare",
                                                                                                          "OTW",
                                                                                                          "Player Moments",
                                                                                                          "POTM",
                                                                                                          "Premium SBC",
                                                                                                          "Rare",
                                                                                                          "Record Breaker",
                                                                                                          "Rule Breaker",
                                                                                                          "Season Reward",
                                                                                                          "Showdown SBC",
                                                                                                          "Squad Foundations",
                                                                                                          "Storyline",
                                                                                                          "Summer Stars",
                                                                                                          "TOTGS",
                                                                                                          "TOTS",
                                                                                                          "TOTY",
                                                                                                          "UCL Live",
                                                                                                          "UEL Live",
                                                                                                          "What If"),
                                                        selected = "TOTS", multiple = T
                                                        ),
                                                sliderInput("similarity_price", "Price in coins:",
                                                    min = 0, max = 6500000,
                                                    value = c(0,6500000)
                                                    ),
                                                sliderInput("similarity_rating", "Overall rating",
                                                    min = 0, max = 99,
                                                    value = c(0,99)
                                                    ), 
                                                checkboxGroupInput(inputId = "similarity_position",
                                                           label = "Position:", choices = 
                                                             c("ST" = "ST", "RWB" = "RWB",
                                                               "RW"="RW","RM"="RM","RF"="RF",
                                                               "RB"="RB","LWB"="LWB",
                                                               "LW"="LW","LM"="LM",
                                                               "LF"="LF","LB"="LB","GK"="GK",
                                                               "CM"="CM","CF"="CF","CDM"="CDM",
                                                               "CB"="CB","CAM"="CAM"),
                                                           selected = c("ST", "RW", "LW"), inline = TRUE),
                                   ),
                        mainPanel(h3("Similarity Analysis"),
                                  p("The radar chart below provides one player's nearest neighbors according to the six most important skill ratings. The drop-down menu, 
                                    allows to select any card and it will be presented on the radar chart along with the default nearest neighbors. This chart can be used
                                    to analyse the preferred card and looking at its closest resemblances. This is particualry helpful to get a decent sample of opportunities 
                                    as a user who is searching for a player with a certain mixture of skills (playing style). The outside axes correspond to the 
                                    skill ratings. For example the default Messi TOTS card is closely resembles by the TOTS cards Nani, S. Berghuis, H. Barnes, A. dos Santos, and
                                    C. Ronaldo. The neighbors can be filtered according to the card type, the in-game price in coins, its overall rating, its position."),
                                   plotlyOutput("similarity.vis", width = 800, height=700)
                                  )
                          )
                        )
             )
  )






