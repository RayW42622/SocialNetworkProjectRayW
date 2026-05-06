# Shiny App 
# Section 1. First install and activate all your required packages. 

library(shiny)
library(bslib)
library(dplyr)
library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)

library(visNetwork)

# Section 2. Design the site in the UI section (US = User Interface). This is where we define how everything looks and 
# how people can use the app. 

ui <-fluidPage(
  
  titlePanel("Relationship Network for Journey To the West Afterstory"),
  
  page_sidebar(
    title = "The fan-favorite afterstory of a classic", 
    sidebar = sidebar ("Menu options"), 
    card(
      card_header("Introduction"), "This is a page focusing on showing the character relationships in the show Journey to the West Afterstory. For each plot, Toggle between degree centrality and degree centrality under each section to change node sizes according to them, or change the bar graphs. This may tell you that the involvement in the main story matters more than the amount of connections in terms of being a broker.

…
"),
    card(
      card_header("Concepts"), "Some of the concepts involved in the graphs, you may want to know",
      selectInput("select", 
                  "select an option", 
                  choices = list("Sides" = "Good or Evil, only as described in the show, but not in other materials.", 
                                 "Home" = "The places the characters call home, aren't necessarily their places of birth."),
                  selected =1), 
      textOutput("ourVariable")
      ), 
    
    
    #Card1
    card(card_header("All characters, choose how to sort"),
         selectInput("NodesA",
                     "Nodes by color", 
                     choices = list( "Sides" = "Side"), 
                     selected = 1),
         selectInput("AxisA",
                     "Axis", 
                     choices = list("Home" = "Home", 
                                    "Origin" = "Origin"), 
                     selected = 1),
         selectInput("sizeA",
                     "choose a centrality measure", 
                     choices = list("Degree Centrality" = "degree", 
                                    "Betweenness Centrality" = "betweenness")),
         plotOutput("JTWA_NetworkID"), height = "1000px"),
    
    #Card1.5
    card(card_header("Same graph, omitted insignificant characters"),
         selectInput("NodesB",
                     "Nodes by color", 
                     choices = list( "Sides" = "Side"), 
                     selected = 1),
         selectInput("ShapeB",
                     "Nodes by shape", 
                     choices = list("Home" = "Home", 
                                    "Origin" = "Origin"), 
                     selected = 1),
         selectInput("sizeB",
                     "choose a centrality measure", 
                     choices = list("Degree Centrality" = "degree", 
                                    "Betweenness Centrality" = "betweenness")),
         plotOutput("JTWA_NetworkIDSimple"), height = "1500px"),
    
    #Card1.55
    card(card_header("Let's focus on the transforming characters"),
         selectInput("NodesC",
                     "Nodes by color", 
                     choices = list( "Sides" = "Side",
                                     "True Identity" = "True_Identity"),
                     selected = 1),
         selectInput("AxisC",
                     "Axis", 
                     choices = list("Home" = "Home", 
                                    "Origin" = "Origin"), 
                     selected = 1),
         selectInput("sizeC",
                     "choose a centrality measure", 
                     choices = list("Degree Centrality" = "degree", 
                                    "Betweenness Centrality" = "betweenness")),
         plotOutput("JTWA_NetworkIDTF"), height = "1000px"),
    
    #Card1.555
    card(card_header("Thresholding: omit edges below certain density"),
         
         numericInput( 
           "thres", 
           "Threshold", 
           value = 50, 
           min = 1, 
           max = 100 
         ),
         selectInput("NodesD",
                     "Nodes by color", 
                     choices = list( "Sides" = "Side",
                                     "True Identity" = "True_Identity"),
                     selected = 1),
         selectInput("sizeD",
                     "choose a centrality measure", 
                     choices = list("Degree Centrality" = "degree", 
                                    "Betweenness Centrality" = "betweenness"),
                     selected = 1),
         selectInput("ShapeD",
                     "Nodes by shape", 
                     choices = list("Home" = "Home", 
                                    "Origin" = "Origin"), 
                     selected = 1),
         plotOutput("JTWA_NetworkIDTH"), height = "1000px"),
    
    #Card2
    card(card_header("An interactive network"), 
         "click on each node to see who this character is connected to.", 
         radioButtons("size_by", "Centrality Measure", 
                      choices = c("Degree" = "degree", 
                      "Betweenness Centrality" = "betweenness"), 
         selected = "degree"),
         visNetworkOutput("int_network"), height = "2000px"),
    
    #Card3
    card(card_header("bar"),
         selectInput("sizeE",
                     "choose a centrality measure", 
                     choices = list("Degree Centrality" = "degree", 
                                    "Betweenness Centrality" = "betweenness"), 
                     selected = 1), 
         plotOutput("JTWA_Bar"), height = "1700px")
    
    ))

# Section 2. The server section defines how our app works. Here's where we will put all the network analysis. 

server <- function(input, output) {
  
  # CARD 1 
  
  output$ourVariable <- renderText({
    paste(input$select)
  })
  
# let's create a simple example network with 10 nodes and calulate the degree centrality

# now let's get it visualized and reactive to our choice from above! 

#Card1
network <- reactive({ 
  JTWA_nodes <-  read.csv("Data/NodesJTWA.csv")
  JTWA_edges <- read.csv("Data/EdgesJTWA.csv")
  
  JTWA_net <- tbl_graph(nodes = JTWA_nodes, 
                        edges= JTWA_edges,
                        directed = TRUE) |>
    activate(nodes) |> 
    mutate(
      degree = centrality_degree() *40, 
      betweenness = centrality_betweenness() *40)
  
  JTWA_net
})
output$JTWA_NetworkID <- renderPlot({
  JTWA_net <- network()
  p<- ggraph(JTWA_net, layout="hive", axis = .data[[input$AxisA]], sort.by = .data[[input$NodesA]]) +
    geom_edge_arc(aes(width=Density*1.5), alpha = 0.3,
                  end_cap = circle(4, 'mm'),
                  alpha = 0.2) + 
    geom_axis_hive(colour = "grey", size = 1, length = 5, label = TRUE) +
    scale_edge_width(range = c(.1,3))+ 
    geom_node_point(aes(color=.data[[input$NodesA]], size = .data[[input$sizeA]]), show.legend = TRUE) +
    scale_color_manual(values=c("red", "black", "lightblue")) +
    scale_size(range = c(1, 10))+
    geom_node_text(aes(label=Revised_Names), color = "black", size = 2) + 
    theme_void() + 
    coord_fixed()

  p
})

# CARD 1.5
network1.5 <- reactive({ 
  JTWA_nodes <-  read.csv("Data/NodesJTWA.csv")
  JTWA_edges <- read.csv("Data/EdgesJTWA.csv")
  
  JTWA_net <- tbl_graph(nodes = JTWA_nodes, 
                        edges= JTWA_edges,
                        directed = TRUE) |>
    activate(nodes) |> 
    mutate(
      degree = centrality_degree() *40, 
      betweenness = centrality_betweenness() *40)
  
  JTWA_net <- JTWA_net |> filter(Insignificant_Characters != 1)
})
output$JTWA_NetworkIDSimple <- renderPlot({
  JTWA_net <- network1.5()
  p<- ggraph(JTWA_net, layout = 'sugiyama') +
    geom_edge_link(aes(width=Density*2), alpha = 0.4, color = "grey", arrow = arrow(length = unit(4, 'mm')))+ 
    geom_node_point(aes(color=.data[[input$NodesB]], size = .data[[input$sizeB]], shape = .data[[input$ShapeB]])) +
    scale_color_manual(values=c("red", "black", "lightblue")) +
    geom_node_text(aes(label=Revised_Names), color = "black", size = 2, repel = TRUE) + 
    theme_void()
  
  p
})

# CARD 1.55
network1.55 <- reactive({ 
  JTWA_nodes <-  read.csv("Data/NodesJTWA.csv")
  JTWA_edges <- read.csv("Data/EdgesJTWA.csv")
  
  JTWA_net <- tbl_graph(nodes = JTWA_nodes, 
                        edges= JTWA_edges,
                        directed = TRUE) |>
    activate(nodes) |> 
    mutate(
      degree = centrality_degree() *40, 
      betweenness = centrality_betweenness() *40)
  
  JTWA_net <- JTWA_net |> filter(Have_Alternative == 1)
})
output$JTWA_NetworkIDTF <- renderPlot({
  JTWA_net <- network1.55()
  p<- ggraph(JTWA_net, layout="hive", axis = .data[[input$AxisC]], sort.by = .data[[input$NodesC]]) +
    geom_edge_arc(aes(width=Density*1.5), alpha = 0.3,
                  end_cap = circle(4, 'mm'),
                  alpha = 0.2) + 
    geom_axis_hive(colour = "grey", size = 1, length = 5, label = TRUE) +
    scale_edge_width(range = c(.1,3))+ 
    geom_node_point(aes(color=.data[[input$NodesC]], size = .data[[input$sizeC]]), show.legend = TRUE) +
    scale_size(range = c(1, 10))+
    geom_node_text(aes(label=Revised_Names), color = "black", size = 2) + 
    theme_void() + 
    coord_fixed() 
  
  p
})

#Card1.555
network1.555 <- reactive({ 
  JTWA_nodes <-  read.csv("Data/NodesJTWA.csv")
  JTWA_edges <- read.csv("Data/EdgesJTWA.csv")
  
  JTWA_net <- tbl_graph(nodes = JTWA_nodes, 
                        edges= JTWA_edges,
                        directed = TRUE) |>
    activate(nodes) |> 
    mutate(
      degree = centrality_degree() *40, 
      betweenness = centrality_betweenness() *40)
  JTWA_net
})
output$JTWA_NetworkIDTH <- renderPlot({
  JTWA_net <- network1.555()
  JTWA_net <- JTWA_net |> activate(edges) |> filter(Density >= input$thres)
  JTWA_net <- JTWA_net |> activate(nodes) |> mutate(degree2 = centrality_degree()) |> filter(degree2>0)
  p<- ggraph(JTWA_net, layout = 'fr') +
    geom_edge_link(aes(width=Density*2), alpha = 0.4, color = "grey", arrow = arrow(length = unit(4, 'mm')))+ 
    geom_node_point(aes(color=.data[[input$NodesD]], size = .data[[input$sizeD]], shape = .data[[input$ShapeD]])) +
    geom_node_text(aes(label=Revised_Names), color = "black", size = 2, repel = TRUE) + 
    theme_void()
  
  p
})
#Card3
# we're going to use another example network like from above but visNetwork requires separate edge and nodes lists 

network2 <- reactive({
  set.seed(123)
  JTWA_nodes <-  read.csv("Data/NodesJTWA.csv")
  JTWA_edges <- read.csv("Data/EdgesJTWA.csv")
  
  JTWA_net <- tbl_graph(nodes = JTWA_nodes, 
                        edges= JTWA_edges,
                        directed = TRUE) |>
    activate(nodes) |> 
    mutate(
      degree = centrality_degree() *100, 
      betweenness = centrality_betweenness() *100)
  JTWA_net2 <- JTWA_net
  
  
  nodes_df <- JTWA_net2 |> 
    activate(nodes) |> 
    as_tibble() |> 
    rowid_to_column("id") |> 
    mutate(value = if (input$size_by == "degree") degree else betweenness) # have to give size based on "value" for visNetwork
  
  edges_df <- JTWA_net2 |> 
    activate(edges) |> 
    as_tibble() |> 
    rename(from = 1, to =2 )
  
  nodes_df <- nodes_df |> select(id | Revised_Names | degree | betweenness | value | Origin | Side | Home | True_Identity) |>
  rename(label = Revised_Names)
  list(nodes = nodes_df, edges = edges_df)
})

output$int_network <- renderVisNetwork({
   net2 <- network2()
   nodes <- net2$nodes
   edges <- net2$edges 
  
   
  visNetwork(nodes, net2$edges) |> 
    
    visNodes(borderWidth = 1,
             size = 200,
             color = list(
               background= "pink", 
               border = "red", 
               highlight =  "purple"))|>
    
    visEdges(
      color = list(color = "purple", highlight = "black")) |> 
    
    visOptions(
      highlightNearest = list(enabled = TRUE, hover = TRUE), 
      nodesIdSelection = FALSE) |>
    
    visInteraction(
      dragNodes = TRUE, 
      dragView = TRUE, 
      zoomView = TRUE) |> 
    
    visPhysics(solver = "forceAtlas2Based", 
               forceAtlas2Based = list(gravitationalConstant = -200), stabilization = TRUE)#layout
    
})

#Card4
network4 <- reactive({
  JTWA_nodes <-  read.csv("Data/NodesJTWA.csv")
  JTWA_edges <- read.csv("Data/EdgesJTWA.csv")
  
  JTWA_net <- tbl_graph(nodes = JTWA_nodes, 
                        edges= JTWA_edges,
                        directed = TRUE) |>
    activate(nodes) |> 
    mutate(
      degree = centrality_degree(), 
      betweenness = centrality_betweenness())
  
  JTWA_net
})
output$JTWA_Bar <- renderPlot({
  JTWA_net <- network() 
  JTWA_df <- JTWA_net |> activate(nodes) |> as_tibble()
  p<- ggplot(JTWA_df, aes(x= reorder(Revised_Names, .data[[input$sizeE]]), y=.data[[input$sizeE]])) + 
    geom_col(fill = "lightblue") + 
    coord_flip() 
  
  p
})



}

# Run the application 
shinyApp(ui = ui, server = server)



