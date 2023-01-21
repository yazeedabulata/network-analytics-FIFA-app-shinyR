source("./global.R")
server = function(input, output) {
  
####Reactive Values######################  
  reactive.Set.1 <- reactive({
    dt.fut.22.Id %>%
      filter(dt.fut.22.Id$Name != gsub("[[:space:]]*$","",gsub("- .*",'', input$similarity_player))) 
  })
  
  
  reactive.Set.2 <- reactive({
    reactive.Set.1() %>%
      select(39,2,3,4,5,6,7,9,10,11,12,13,14,15,16,17,21,29,30,31) %>%
      filter(reactive.Set.1()$POS %in% input$similarity_position,
             reactive.Set.1()$VER %in% input$similarity_card) %>%
      filter(Price >= input$similarity_price[1]) %>%
      filter(Price <= input$similarity_price[2]) %>%
      filter(RAT >= input$similarity_rating)  
  })
  
  reactive.Set.3 <- reactive({
    dt.fut.22.Id %>%
      select(39,2,3,4,5,6,7,9,10,11,12,13,14,15,16,17,21,29,30,31) %>%
      filter(dt.fut.22.Id$NameId == input$similarity_player)
  })
  
  reactive.Set.4 <- reactive({
    rbind(reactive.Set.3(),reactive.Set.2())
    
  })
   
  reactive.Set.5 <- reactive({
    reactive.Set.4() %>%
      select(8:13)
  })
  
  reactive.Set.6 <- reactive({
    as.numeric(knnx.index(reactive.Set.5(), reactive.Set.5()[1, , drop=FALSE], k=6))
  })
  
  reactive.Set.7 <- reactive({
    reactive.Set.4()[reactive.Set.6(),]
  })
  
  reactive.Set.8 <- reactive({
    reactive.Set.7() %>%
      select(8:13)
  })  

  selectedDataPn <- reactive({
    select = input$pn_player
    dt.bipartite.data %>% filter(dt.bipartite.data$NameId %in% select)
  })  
  
  selectedNationBp <- reactive({
    select = input$nation_selection
    dt.bipartite.data %>% filter(dt.bipartite.data$nationality %in% select)
  })
  
  selectedLeagueBp <- reactive({
    select = input$league_selection
    dt.bipartite.data %>% filter(dt.bipartite.data$league_name %in% select)
  })
  
#################### Descriptive Statistics ###################################################  
####Player Statistics Table########
output$pstatistics <- DT::renderDataTable(DT::datatable(dt.fut.22.player %>%
                                                          filter(POS %in% input$statistics_position),
                                                        options = list(
                                                          columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                                          initComplete = JS(
                                                            "function(settings, json) {",
                                                            "$(this.api().table().header()).css({'color': 'white', 'backgroundColor': '#59A96A'});","}"))) %>%
                                            formatStyle(colnames(dt.fut.22.player), 
                                                        color = 'black', backgroundColor = 'white'))

output$player_hist <- renderPlotly({
  if(input$statistics_selection == "Overall Rating"){
    ggplotly(ggplot(data = dt.fut.22, aes(x = RAT)) + geom_histogram(stat = "count", color='#59A96A', fill= '#59A96A', binwidth = 1) +
               labs(title="Distribution of Overall Ratings",x="Overall Rating", y = "Frequency") +
               theme(plot.background = element_rect(fill = '#FFFFFF', colour = '#FFFFFF'), 
                     axis.text.x = element_text(size = 9, color = "black"), axis.title.x = element_text(size = 12, color = '#7F7F7F', face = "bold.italic"),
                     axis.text.y = element_text(size = 9, color = "black"), axis.title.y = element_text(size = 12, color = '#7F7F7F', face = "bold.italic"),
                     plot.title = element_text(size = 12, face = "bold", color = '#7F7F7F', hjust = 0.5, vjust = -1), 
                     panel.background = element_rect(fill = '#FFFFFF', colour = "#FFFFFF", size = 2, linetype = "solid"), 
                     panel.grid.major = element_line(size = 0.5, colour = "#ECECEC"), 
                     panel.grid.minor = element_line(size = 0.5, colour = "#EEEEEE", linetype = "dashed")))
  }else
    if(input$statistics_selection == "Positions"){
      ggplotly(ggplot(data = dt.fut.22, aes(x = fct_infreq(POS))) + geom_histogram(stat = "count", color='#59A96A', fill= '#59A96A') +
                 labs(title="Distribution of Player Positions",x="Positions", y = "Frequency") +
                 theme(plot.background = element_rect(fill = '#FFFFFF', colour = '#FFFFFF'), 
                       axis.text.x = element_text(size = 9, color = "black"), axis.title.x = element_text(size = 12, color = '#7F7F7F', face = "bold.italic"),
                       axis.text.y = element_text(size = 9, color = "black"), axis.title.y = element_text(size = 12, color = '#7F7F7F', face = "bold.italic"),
                       plot.title = element_text(size = 12, face = "bold", color = '#7F7F7F', hjust = 0.5, vjust = -1), 
                       panel.background = element_rect(fill = '#FFFFFF', colour = "#FFFFFF", size = 2, linetype = "solid"), 
                       panel.grid.major = element_line(size = 0.5, colour = "#ECECEC"), 
                       panel.grid.minor = element_line(size = 0.5, colour = "#EEEEEE", linetype = "dashed")))
      }else
        if(input$statistics_selection == "Price"){
          ggplotly(ggplot(data = dt.fut.22, aes(x = Price)) + geom_histogram(color='#59A96A', fill= '#59A96A', binwidth = 1000) + ylim(0, 75) +
                     labs(title="Distribution of Player Card Prices",x="Price", y = "Frequency") + scale_x_continuous(labels = scales::comma, limits = c(0, 300000)) +
                     theme(plot.background = element_rect(fill = '#FFFFFF', colour = '#FFFFFF'), 
                           axis.text.x = element_text(size = 9, color = "black"), axis.title.x = element_text(size = 12, color = '#7F7F7F', face = "bold.italic"),
                           axis.text.y = element_text(size = 9, color = "black"), axis.title.y = element_text(size = 12, color = '#7F7F7F', face = "bold.italic"),
                           plot.title = element_text(size = 12, face = "bold", color = '#7F7F7F', hjust = 0.5, vjust = -1), 
                           panel.background = element_rect(fill = '#FFFFFF', colour = "#FFFFFF", size = 2, linetype = "solid"), 
                           panel.grid.major = element_line(size = 0.5, colour = "#ECECEC"), 
                           panel.grid.minor = element_line(size = 0.5, colour = "#EEEEEE", linetype = "dashed")))
          }
  })

output$player_mean<- renderText({
  if(input$statistics_selection == "Overall Rating"){
    paste("Mean = ", mean(dt.fut.22$RAT))
  }else
    if(input$statistics_selection == "Positions"){
      paste("")
    }else
      if(input$statistics_selection == "Price"){
        paste("Mean = ", mean(dt.fut.22$Price))
      }
  })

output$player_sd <- renderText({
  if(input$statistics_selection == "Overall Rating"){
    paste("sd = ", mean(dt.fut.22$RAT))
  }else
    if(input$statistics_selection == "Positions"){
      ""
    }else
      if(input$statistics_selection == "Price"){
        paste("sd = ", mean(dt.fut.22$Price))
      }
  })

####Nationality Statistics Table#############################################
output$nstatistics <- DT::renderDataTable(DT::datatable(dt.fut.22.nation,
                                                        options = list(
                                                          columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                                          initComplete = JS(
                                                            "function(settings, json) {",
                                                            "$(this.api().table().header()).css({'color': 'white', 'backgroundColor': '#59A96A'});","}"))) %>%
                                            formatStyle(colnames(dt.fut.22.nation), 
                                                        color = 'black', backgroundColor = 'white') %>% formatRound( colnames(dt.fut.22.nation.top.10), digits = 5))

output$nhist <- renderPlotly(ggplotly(ggplot(data = dt.fut.22.nation.top.10, aes(x = nationality, y = n_cards)) + geom_histogram(stat = "identity", color='#59A96A', fill= '#59A96A')  +
                                        labs(title="Distribution of Top 10 Player Nationalities",x="Nationality", y = "Frequency") +
                                        theme(plot.background = element_rect(fill = '#FFFFFF', colour = '#FFFFFF'), 
                                              axis.text.x = element_text(size = 9, color = "black"), axis.title.x = element_text(size = 12, color = '#7F7F7F', face = "bold.italic"),
                                              axis.text.y = element_text(size = 9, color = "black"), axis.title.y = element_text(size = 12, color = '#7F7F7F', face = "bold.italic"),
                                              plot.title = element_text(size = 12, face = "bold", color = '#7F7F7F', hjust = 0.5, vjust = -1), 
                                              panel.background = element_rect(fill = '#FFFFFF', colour = "#FFFFFF", size = 2, linetype = "solid"), 
                                              panel.grid.major = element_line(size = 0.5, colour = "#ECECEC"), 
                                              panel.grid.minor = element_line(size = 0.5, colour = "#EEEEEE", linetype = "dashed"))))
  
####League Statistics Table###############################################
output$lstatistics <- DT::renderDataTable(DT::datatable(dt.fut.22.league,
                                                        options = list(
                                                          columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                                          initComplete = JS(
                                                            "function(settings, json) {",
                                                            "$(this.api().table().header()).css({'color': 'white', 'backgroundColor': '#59A96A'});",
                                                            "}"))) %>%
                                            formatStyle(colnames(dt.fut.22.league), 
                                                        color = 'black', backgroundColor = 'white')%>% formatRound( colnames(dt.fut.22.league.top.10), digits = 5))

output$lhist <- renderPlotly(ggplotly(ggplot(data = dt.fut.22.league.top.10, aes(x = league_name, y = n_cards)) + geom_histogram(stat = "identity", color='#59A96A', fill= '#59A96A')  +
                                        labs(title="Distribution of Top 10 Player Leagues",x="League", y = "Frequency") +
                                        theme(plot.background = element_rect(fill = '#FFFFFF', colour = '#FFFFFF'), 
                                              axis.text.x = element_text(size = 7, color = "black"), axis.title.x = element_text(size = 12, color = '#7F7F7F', face = "bold.italic"),
                                              axis.text.y = element_text(size = 9, color = "black"), axis.title.y = element_text(size = 12, color = '#7F7F7F', face = "bold.italic"),
                                              plot.title = element_text(size = 12, face = "bold", color = '#7F7F7F', hjust = 0.5, vjust = -1), 
                                              panel.background = element_rect(fill = '#FFFFFF', colour = "#FFFFFF", size = 2, linetype = "solid"), 
                                              panel.grid.major = element_line(size = 0.5, colour = "#ECECEC"), 
                                              panel.grid.minor = element_line(size = 0.5, colour = "#EEEEEE", linetype = "dashed"))))
  
#####Attribute Statistics Table##########################################################
output$astatistics <- DT::renderDataTable(DT::datatable(dt.fut.22.att,
                                                        options = list(
                                                          columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                                          initComplete = JS(
                                                            "function(settings, json) {",
                                                            "$(this.api().table().header()).css({'color': 'white', 'backgroundColor': '#59A96A'});",
                                                            "}"))) %>%
                                            formatStyle(colnames(dt.fut.22.att), 
                                                        color = 'black', backgroundColor = 'white'))

output$ahist <- renderPlotly({
  if(input$statistics_attributes == "Pace"){
    ggplotly(ggplot(data = dt.fut.22.att.freq, aes(x = PAC_R)) + geom_histogram(stat = "count", color='#59A96A', fill= '#59A96A')  +
               labs(title="Distribution of Pace Attributes",x="Pace Attribute", y = "Frequency") +
               theme(plot.background = element_rect(fill = '#FFFFFF', colour = '#FFFFFF'), 
                     axis.text.x = element_text(size = 9, color = "black"), axis.title.x = element_text(size = 12, color = '#7F7F7F', face = "bold.italic"),
                     axis.text.y = element_text(size = 9, color = "black"), axis.title.y = element_text(size = 12, color = '#7F7F7F', face = "bold.italic"),
                     plot.title = element_text(size = 12, face = "bold", color = '#7F7F7F', hjust = 0.5, vjust = -1), 
                     panel.background = element_rect(fill = '#FFFFFF', colour = "#FFFFFF", size = 2, linetype = "solid"), 
                     panel.grid.major = element_line(size = 0.5, colour = "#ECECEC"), 
                     panel.grid.minor = element_line(size = 0.5, colour = "#EEEEEE", linetype = "dashed")))
  }else
    if(input$statistics_attributes == "Shooting"){
      ggplotly(ggplot(data = dt.fut.22.att.freq, aes(x = SHO_R)) + geom_histogram(stat = "count", color='#59A96A', fill= '#59A96A')  +
                 labs(title="Distribution of Pace Attributes",x="Pace Attribute", y = "Frequency") +
                 theme(plot.background = element_rect(fill = '#FFFFFF', colour = '#FFFFFF'), 
                       axis.text.x = element_text(size = 9, color = "black"), axis.title.x = element_text(size = 12, color = '#7F7F7F', face = "bold.italic"),
                       axis.text.y = element_text(size = 9, color = "black"), axis.title.y = element_text(size = 12, color = '#7F7F7F', face = "bold.italic"),
                       plot.title = element_text(size = 12, face = "bold", color = '#7F7F7F', hjust = 0.5, vjust = -1), 
                       panel.background = element_rect(fill = '#FFFFFF', colour = "#FFFFFF", size = 2, linetype = "solid"), 
                       panel.grid.major = element_line(size = 0.5, colour = "#ECECEC"), 
                       panel.grid.minor = element_line(size = 0.5, colour = "#EEEEEE", linetype = "dashed")))
    }else
      if(input$statistics_attributes == "Passing"){
        ggplotly(ggplot(data = dt.fut.22.att.freq, aes(x = PAS_R)) + geom_histogram(stat = "count", color='#59A96A', fill= '#59A96A')  +
                   labs(title="Distribution of Pace Attributes",x="Pace Attribute", y = "Frequency") +
                   theme(plot.background = element_rect(fill = '#FFFFFF', colour = '#FFFFFF'), 
                         axis.text.x = element_text(size = 9, color = "black"), axis.title.x = element_text(size = 12, color = '#7F7F7F', face = "bold.italic"),
                         axis.text.y = element_text(size = 9, color = "black"), axis.title.y = element_text(size = 12, color = '#7F7F7F', face = "bold.italic"),
                         plot.title = element_text(size = 12, face = "bold", color = '#7F7F7F', hjust = 0.5, vjust = -1), 
                         panel.background = element_rect(fill = '#FFFFFF', colour = "#FFFFFF", size = 2, linetype = "solid"), 
                         panel.grid.major = element_line(size = 0.5, colour = "#ECECEC"), 
                         panel.grid.minor = element_line(size = 0.5, colour = "#EEEEEE", linetype = "dashed")))
      }else
        if(input$statistics_attributes == "Dribbling"){
          ggplotly(ggplot(data = dt.fut.22.att.freq, aes(x = DRI_R)) + geom_histogram(stat = "count", color='#59A96A', fill= '#59A96A')  +
                     labs(title="Distribution of Pace Attributes",x="Pace Attribute", y = "Frequency") +
                     theme(plot.background = element_rect(fill = '#FFFFFF', colour = '#FFFFFF'), 
                           axis.text.x = element_text(size = 9, color = "black"), axis.title.x = element_text(size = 12, color = '#7F7F7F', face = "bold.italic"),
                           axis.text.y = element_text(size = 9, color = "black"), axis.title.y = element_text(size = 12, color = '#7F7F7F', face = "bold.italic"),
                           plot.title = element_text(size = 12, face = "bold", color = '#7F7F7F', hjust = 0.5, vjust = -1), 
                           panel.background = element_rect(fill = '#FFFFFF', colour = "#FFFFFF", size = 2, linetype = "solid"), 
                           panel.grid.major = element_line(size = 0.5, colour = "#ECECEC"), 
                           panel.grid.minor = element_line(size = 0.5, colour = "#EEEEEE", linetype = "dashed")))
        }else
          if(input$statistics_attributes == "Defense"){
            ggplotly(ggplot(data = dt.fut.22.att.freq, aes(x = DEF_R)) + geom_histogram(stat = "count", color='#59A96A', fill= '#59A96A')  +
                       labs(title="Distribution of Pace Attributes",x="Pace Attribute", y = "Frequency") +
                       theme(plot.background = element_rect(fill = '#FFFFFF', colour = '#FFFFFF'), 
                             axis.text.x = element_text(size = 9, color = "black"), axis.title.x = element_text(size = 12, color = '#7F7F7F', face = "bold.italic"),
                             axis.text.y = element_text(size = 9, color = "black"), axis.title.y = element_text(size = 12, color = '#7F7F7F', face = "bold.italic"),
                             plot.title = element_text(size = 12, face = "bold", color = '#7F7F7F', hjust = 0.5, vjust = -1), 
                             panel.background = element_rect(fill = '#FFFFFF', colour = "#FFFFFF", size = 2, linetype = "solid"), 
                             panel.grid.major = element_line(size = 0.5, colour = "#ECECEC"), 
                             panel.grid.minor = element_line(size = 0.5, colour = "#EEEEEE", linetype = "dashed")))
          }else
            if(input$statistics_attributes == "Physicality"){
              ggplotly(ggplot(data = dt.fut.22.att.freq, aes(x = PHY_R)) + geom_histogram(stat = "count", color='#59A96A', fill= '#59A96A')  +
                         labs(title="Distribution of Pace Attributes",x="Pace Attribute", y = "Frequency") +
                         theme(plot.background = element_rect(fill = '#FFFFFF', colour = '#FFFFFF'), 
                               axis.text.x = element_text(size = 9, color = "black"), axis.title.x = element_text(size = 12, color = '#7F7F7F', face = "bold.italic"),
                               axis.text.y = element_text(size = 9, color = "black"), axis.title.y = element_text(size = 12, color = '#7F7F7F', face = "bold.italic"),
                               plot.title = element_text(size = 12, face = "bold", color = '#7F7F7F', hjust = 0.5, vjust = -1), 
                               panel.background = element_rect(fill = '#FFFFFF', colour = "#FFFFFF", size = 2, linetype = "solid"), 
                               panel.grid.major = element_line(size = 0.5, colour = "#ECECEC"), 
                               panel.grid.minor = element_line(size = 0.5, colour = "#EEEEEE", linetype = "dashed")))
            }
})


#########################################Network Table Codes#######################################################
output$statistics <- DT::renderDataTable({
  if(input$network_selection == "Bipartite Network: Players and Attributes"){
    DT::datatable(dt.g.bipartite.attributes, style = "bootstrap", rownames = FALSE,
                  options = list(
                    columnDefs = list(list(className = 'dt-center', targets = "_all")),
                    initComplete = JS("function(settings, json) {",
                      "$(this.api().table().header()).css({'color': 'white', 'backgroundColor': '#59A96A'});","}"))) %>%
      formatStyle(c('name', 'type', 'degree', 
                    'closeness', 'betweenness', 'eigenvector'), 
                  color = 'black', backgroundColor = '#white') %>%
      formatRound(columns = c('closeness', 'eigenvector'), digits = 7)
  }
  
  else if(input$network_selection == "Projected Network: Players based on Attributes"){
    DT::datatable(dt.g.attributes.players, style = "bootstrap", rownames = FALSE,
                  options = list(
                    columnDefs = list(list(className = 'dt-center', targets = "_all")),
                    initComplete = JS("function(settings, json) {",
                      "$(this.api().table().header()).css({'color': 'white', 'backgroundColor': '#59A96A'});","}"))) %>%
      formatStyle(c('name', 'degree', 
                    'closeness', 'betweenness', 'eigenvector'), 
                  color = 'black', backgroundColor = '#white') %>%
      formatRound(columns = c('closeness', 'eigenvector'), digits = 7)
    
  }
  
  else if(input$network_selection == "Projected Network: Attributes based on Players"){
    DT::datatable(dt.g.attributes, style = "bootstrap", rownames = FALSE,
                  options = list(
                    columnDefs = list(list(className = 'dt-center', targets = "_all")),
                    initComplete = JS("function(settings, json) {",
                      "$(this.api().table().header()).css({'color': 'white', 'backgroundColor': '#59A96A'});","}"))) %>%
      formatStyle(c('name', 'degree', 
                    'closeness', 'betweenness', 'eigenvector'), 
                  color = 'black', backgroundColor = '#white') %>%
      formatRound(columns = c('closeness', 'eigenvector'), digits = 7)
    
  }
  
})

#Network Histogram Codes
output$network_histograms <- renderPlotly({
  plot.bipartite.attributes <- ggplot(data = dt.bipartite.attributes, aes(x = Degree)) + geom_histogram(color='#59A96A', fill= '#59A96A', binwidth = 100) +
    labs(title="Bipartite Network: Attributes and Players",x="Degree", y = "Frequency") +
    theme(plot.background = element_rect(fill = '#F5F5F5', colour = '#F5F5F5'),
          axis.text.x = element_text(size = 9, color = "black"), axis.title.x = element_text(size = 10, color = "black", face = "bold"),
          axis.text.y = element_text(size = 9, color = "black"), axis.title.y = element_text(size = 10, color = "black", face = "bold"),
          plot.title = element_text(size = 10, face = "bold", color = "black"),
          panel.background = element_rect(fill = '#F5F5F5', colour = "#F5F5F5", size = 2),
          panel.grid.major = element_line(size = 0.5, colour = "#ECECEC"), 
          panel.grid.minor = element_line(size = 0.5, linetype = 'dashed', colour = "#ECECEC")
      
    ) 
  plot.bipartite.attributes.players <- 
    ggplot(data = dt.bipartite.attributes.players, aes(x = Degree)) + geom_histogram(color='#59A96A', fill= '#59A96A', binwidth = 1) +
    labs(title="Attributes based on Players",x="Degree", y = "Frequency") +
    theme(plot.background = element_rect(fill = '#F5F5F5', colour = '#F5F5F5'),
          axis.text.x = element_text(size = 9, color = "black"), axis.title.x = element_text(size = 10, color = "black", face = "bold"),
          axis.text.y = element_text(size = 9, color = "black"), axis.title.y = element_text(size = 10, color = "black", face = "bold"),
          plot.title = element_text(size = 10, face = "bold", color = "black"),
          panel.background = element_rect(fill = '#F5F5F5', colour = "#F5F5F5", size = 2),
          panel.grid.major = element_line(size = 0.5, colour = "#ECECEC"), 
          panel.grid.minor = element_line(size = 0.5, linetype = 'dashed', colour = "#ECECEC")
    ) 
  plot.bipartite.attributes.players.inv <- 
    ggplot(data = dt.bipartite.attributes.players.inv, aes(x = Degree)) + geom_histogram(color='#59A96A', fill= '#59A96A', binwidth = 1) +
    labs(title="Attributes based on Players",x="Degree", y = "Frequency") +
    theme(plot.background = element_rect(fill = '#F5F5F5', colour = '#F5F5F5'),
          axis.text.x = element_text(size = 9, color = "black"), axis.title.x = element_text(size = 10, color = "black", face = "bold"),
          axis.text.y = element_text(size = 9, color = "black"), axis.title.y = element_text(size = 10, color = "black", face = "bold"),
          plot.title = element_text(size = 10, face = "bold", color = "black"),
          panel.background = element_rect(fill = '#F5F5F5', colour = "#F5F5F5", size = 2),
          panel.grid.major = element_line(size = 0.5, colour = "#ECECEC"), 
          panel.grid.minor = element_line(size = 0.5, linetype = 'dashed', colour = "#ECECEC")
    ) 
  print(plot.bipartite.attributes.players.inv + ggtitle("Projected Network: Attributes based on Players") + labs(x = "Degree", y="Count"))

    if(input$network_selection == "Bipartite Network: Players and Attributes"){
    ggplotly(plot.bipartite.attributes)}
  else if(input$network_selection == "Projected Network: Players based on Attributes"){
    ggplotly(plot.bipartite.attributes.players)}
  else if(input$network_selection == "Projected Network: Attributes based on Players"){
    ggplotly(plot.bipartite.attributes.players.inv)}

})

#################################################Network Descriptive Codes#########################################
output$network_path <- renderText({
  if(input$network_selection == "Bipartite Network: Players and Attributes"){
    paste("Average Path:" , g.bp.attributes.apl, sep= " ")}
  else if(input$network_selection == "Projected Network: Players based on Attributes"){
    paste("Average Path:" , g.att.pl.apl, sep= " ")}
  else if(input$network_selection == "Projected Network: Attributes based on Players"){
    paste("Average Path:" , g.players.att.apl, sep= " ")}
  })
output$diameter <- renderText({
  if(input$network_selection == "Bipartite Network: Players and Attributes"){
    paste("Diameter:" , g.bp.attributes.diameter, sep= " ") }
  else if(input$network_selection == "Projected Network: Attributes based on Players"){
    paste("Diameter" , g.att.players.diameter, sep= " ")}
  else if(input$network_selection == "Projected Network: Attributes based on Players"){
    paste("Diameter" , g.players.att.diameter, sep= " ")}
})
output$edges <- renderText({
  if(input$network_selection == "Bipartite Network: Players and Attributes"){
    paste("Number of Edges:" , gsize(g.bipartite.attributes), sep= " ")}
  else if(input$network_selection == "Projected Network: Players based on Attributes"){
    paste("Number of Edges:" , gsize(g.attributes.players), sep= " ")}
  else if(input$network_selection == "Projected Network: Attributes based on Players"){
    paste("Number of Edges:" , gsize(g.attributes), sep= " ")}
})
output$node_count <- renderText({
  if(input$network_selection == "Bipartite Network: Players and Attributes"){
    paste("Number of Nodes:" , gorder(g.bipartite.attributes), sep= " ")}
  else if(input$network_selection == "Projected Network: Players based on Attributes"){
    paste("Number of Nodes:" , gorder(g.attributes.players), sep= " ")}
  else if(input$network_selection == "Projected Network: Attributes based on Players"){
    paste("Number of Nodes:" , gorder(g.attributes), sep= " ")}
})
output$clustering_coefficient <- renderText({
  if(input$network_selection == "Bipartite Network: Players and Attributes"){
    paste("Clustering Coefficient:" , transitivity(g.bipartite.attributes), sep= " ")}
  else if(input$network_selection == "Projected Network: Attributes based on Players"){
    paste("Clustering Coefficient:" , transitivity(g.attributes.players), sep= " ")}
  else if(input$network_selection == "Projected Network: Attributes based on Players"){
    paste("Clustering Coefficient:" , transitivity(g.attributes), sep= " ")}
}) 


#################################################bipartite graph###################################################
output$vis.network.1 <- renderVisNetwork({
  nationality.temp <- selectedNationBp()[, nationality]
  dt.temp <- dt.bipartite.data[nationality %in% nationality.temp, ]
  all.temp.players <- dt.temp[, list(name=unique(NameId), type=TRUE)]
  all.temp.attributes <- dt.temp[, list(name=unique(attribute), type=FALSE)]
  vertices.players.temp<- rbind(all.temp.players, all.temp.attributes)
  g.temp <- graph.data.frame(dt.temp[, list(NameId, attribute)], directed=FALSE, vertices = vertices.players.temp)
  vis.temp <- toVisNetworkData(g.temp)
  vis.nodes.temp <- vis.temp$nodes
  vis.edges.temp <- vis.temp$edges
  vis.nodes.temp$type[vis.nodes.temp$type == "TRUE"] <- "Players" 
  vis.nodes.temp$type[vis.nodes.temp$type == "FALSE"] <- "Attributes"
  vis.nodes.temp$group <- vis.nodes.temp$type
  vis.2 <- visNetwork(vis.nodes.temp, vis.edges.temp) %>%
    visNodes(font = list(color = "black")) %>%
    visGroups(groupname = "Players", color = list(background = "#59A96A", border = "#59A96A", highlight = "#FFC000"), shape = "dot") %>%
    visGroups(groupname = "Attributes", color = list(background = "#4D7A95", border = "#4D7A95", highlight = "#FFC000"), shape = "square") %>%
    visEdges(color = "#B8DABF") %>%  
    visIgraphLayout()
})
output$vis.network.2 <- renderVisNetwork({
  league.temp <- selectedLeagueBp()[, league_name]
  dt.temp <- dt.bipartite.data[league_name %in% league.temp, ]
  all.temp.players <- dt.temp[, list(name=unique(NameId), type=TRUE)]
  all.temp.attributes <- dt.temp[, list(name=unique(attribute), type=FALSE)]
  vertices.players.temp<- rbind(all.temp.players, all.temp.attributes)
  g.temp <- graph.data.frame(dt.temp[, list(NameId, attribute)], directed=FALSE, vertices = vertices.players.temp)
  vis.temp <- toVisNetworkData(g.temp)
  vis.nodes.temp <- vis.temp$nodes
  vis.edges.temp <- vis.temp$edges
  vis.nodes.temp$type[vis.nodes.temp$type == "TRUE"] <- "Players" 
  vis.nodes.temp$type[vis.nodes.temp$type == "FALSE"] <- "Attributes"
  vis.nodes.temp$group <- vis.nodes.temp$type
  vis.2 <- visNetwork(vis.nodes.temp, vis.edges.temp) %>%
    visNodes(font = list(color = "black")) %>%
    visGroups(groupname = "Players", color = list(background = "#59A96A", border = "#59A96A", highlight = "#FFC000"), shape = "dot") %>%
    visGroups(groupname = "Attributes", color = list(background = "#4D7A95", border = "#4D7A95", highlight = "#FFC000"), shape = "square") %>%
    visEdges(color = "#B8DABF") %>%  
    visIgraphLayout()
})
##################################################projected network################################################
output$vis.network.3 <- renderVisNetwork({
  validate(need(length(c(input$pn_player)) >= 2, "Not enough players were selected:
                lease select at least two players to view their connections "))
  validate(need(dim(selectedDataPn())[1]>=3, "Sorry, no connections match your search,
           Please change the input filters." )) 
  name.temp <- selectedDataPn()[, NameId]
  dt.temp <- dt.bipartite.data[NameId %in% name.temp, ]
  all.temp.players <- dt.temp[,
                              list(name=unique(NameId), type=TRUE)]
  all.temp.attributes <- dt.temp[, 
                                 list(name=unique(attribute), type=FALSE)]
  vertices.games.temp<- rbind(all.temp.players, all.temp.attributes)
  g.temp <- graph.data.frame(dt.temp[, list(NameId, attribute)],
                             directed=FALSE, vertices = vertices.games.temp)
  g.temp <- bipartite.projection(g.temp)$proj2
  vis.temp <- toVisNetworkData(g.temp)
  vis.nodes.temp <- vis.temp$nodes
  vis.edges.temp <- vis.temp$edges
  vis.edges.temp$value <- vis.edges.temp$weight
  vis.2 <- visNetwork(vis.nodes.temp, vis.edges.temp) %>%
    visNodes(font = list(color = "black"), color = list(background = "#59A96A", border = "#59A96A", highlight = "#FFC000"), shape = "dot") %>%
    visEdges(color = "#B8DABF") %>%  
    visIgraphLayout(layout = "layout_in_circle")
})
#####Similarity Analysis##################################################
output$similarity.vis <- renderPlotly({
  validate(
    need(dim(reactive.Set.2())[1]>=5, "Sorry, no five similar players were found. 
           Please change the input filters."
    )
  )
  
  plot_ly(
    type = 'scatterpolar',
    mode = "closest",
    fill = 'toself'
  ) %>%
    add_trace(
      r = c(as.matrix(reactive.Set.8()[1,])),
      theta = c("Pace", "Shooting", "Passing", "Dribbling", "Defending", "Physicality"),
      showlegend = TRUE,
      mode = "markers",
      name = reactive.Set.7()[1,1],
      fillcolor = "#59A96AAA",
      marker = list(color = "#59A96A")
    ) %>%
    add_trace(
      r = c(as.matrix(reactive.Set.8()[2,])),
      theta = c("Pace", "Shooting", "Passing", "Dribbling", "Defending", "Physicality"),
      showlegend = TRUE,
      mode = "markers",
      visible="legendonly",
      name = reactive.Set.7()[2,1],
      fillcolor = "#8FAADCAA",
      marker = list(color = "#8FAADC")
    ) %>%
    add_trace(
      r = c(as.matrix(reactive.Set.8()[3,])),
      theta = c("Pace", "Shooting", "Passing", "Dribbling", "Defending", "Physicality"),
      showlegend = TRUE,
      mode = "markers",
      visible="legendonly",
      name = reactive.Set.7()[3,1],
      fillcolor = "#F4B183AA",
      marker = list(color = "#F4B183")
    ) %>%
    add_trace(
      r = c(as.matrix(reactive.Set.8()[4,])),
      theta = c("Pace", "Shooting", "Passing", "Dribbling", "Defending", "Physicality"),
      showlegend = TRUE,
      mode = "markers",
      visible="legendonly",
      name = reactive.Set.7()[4,1],
      fillcolor = "#FFD966AA",
      marker = list(color = "#FFD966")
    ) %>%
    add_trace(
      r = c(as.matrix(reactive.Set.8()[5,])),
      theta = c("Pace", "Shooting", "Passing", "Dribbling", "Defending", "Physicality"),
      showlegend = TRUE,
      mode = "markers",
      visible="legendonly",
      name = reactive.Set.7()[5,1],
      fillcolor = "#FF8F8FAA",
      marker = list(color = "#FF8F8F")
    ) %>%
    add_trace(
      r = c(as.matrix(reactive.Set.8()[6,])),
      theta = c("Pace", "Shooting", "Passing", "Dribbling", "Defending", "Physicality"),
      showlegend = TRUE,
      mode = "markers",
      visible="legendonly",
      name = reactive.Set.7()[6,1],
      fillcolor = "#B686DAAA",
      marker = list(color = "#B686DA")
    ) %>%
    layout(
      polar = list(
        radialaxis = list(
          visible = T,
          range = c(0,100)
        )
      ),
      showlegend=TRUE
    )
})

#End
}


