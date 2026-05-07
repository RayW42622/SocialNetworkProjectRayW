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
      card_header("Introduction"), HTML("This is a page focusing on showing the character relationships in the show Journey to the West Afterstory. The network is directed, the nodes are the characters and the edges are the number of lines they talk to each other.
<br>
<br> For each plot, Toggle between degree centrality and degree centrality under each section to change node sizes according to them, or change the bar graphs. Use a number to change the size of the texts in case you miss any important names. This may tell you that the involvement in the main story matters more than the amount of connections in terms of being a broker.
<br>
<br> For the first and third graph, the axis can be toggled to show the homes or origins of a character (check the box below for the definitions). The shapes in the second and fourth graph are of the same attributes. The nodes panel on the first two graphs only contain the sides the characters are on, while it can be changed between sides and true identity at the last 2 graphs since we may get less nodes to work with. In the last graph, you can type a number and see what connections have more lines than this number, this can show you the most dense connections. The interactive network can be enlarged, and by clicking on each node, you can see what characters this character connects to.
<br>
<br> Observing with degree centrality, the absolute main Sun Wukong reasonably ranked the first, while other highly important characters followed. What is surprising is that the rest of the main cast in the novel (Tang Monk, Sha Seng, Zhu Bajie, Xiao Bailong) aren’t in the top 5 since they have less involvement in the main story. The most noteworthing point is that Tang Monk, a character who hardly interacts with anyone apart from his disciples, has more degree and betweenness centrality than all of them except Wukong. The original characters also play major roles than most characters from religions and the novel. This signifies the creators’ attempt to add originality to the show instead of being a plain sequel.
<br>
<br> From the show, I particularly studied the transforming characters. These characters change identities to outsmart their enemies. By sorting them with true identities, we can see that all characters on the list are highly intelligent: Sun Wukong, the absolute main character that gathers information from everywhere, has the most transformation, which is 8 of them, while Six-Eared Macaque, a smart villain who tricked Sun Wukong, has 6 fake identities. And since they are constantly tricking each other, their characters are quite densely connected. Transformation plots are featured in the original novel as the dominant method for battle of wits against Tang Monk and other disciples of his, but was underdeveloped because Sun Wukong has the ability to see through them. As the best way to pay homage to it, the producers removed Wukong’s ability to see through transformations and made him infer whether one is true or fake with his intelligence. This increases the complexity of the show and keeps the style of the novel.
")),
    card(
      card_header("Concepts"), "Some of the concepts involved in the graphs, you may want to know",
      selectInput("select", 
                  "select an option", 
                  choices = list("Select" = HTML("Select"),
                                 "Yaoguai" = HTML("Something that engages in Tao’s ways and gains supernatural powers, often takes the form of a human. Like monsters in Chinese, but not necessarily evil."),
                                 "Sides" = HTML("Good or Evil, only as described in the show, but not in other materials. Ex. Wude Xingjun is described as justice in Tao belief, but is an evil character in the show, so he belongs to the evil category."), 
                                 "Home" = HTML("The places the characters call home, aren't necessarily their places of birth: Lingshan: Buddhas’ sanctuary, Celestial Court: home for Tao deities, Dark Realm: home for most Yaoguai and antagonists, Earth: home for human and some Yaoguai, Hell: home for the dead, Outworlds: home for deities away from the court, Sea: home for the dragons and water-related Yaoguai, Asura Realm: home for Ayinafa and his traps"),
                                 "Origin" = "The source a character is from: Tao religion, Buddha religion, Chinese Folklore religion, Chinese History, original characters or from the novel Journey to the West.",
                                 "True Identity" = "If a node is a fake character, its true identity will be labeled. Ex. Zhu Bajie (Fake)’s true identity is Six-Eared Macaque, that means that Six-Eared Macaque took the shape of Zhu Bajie at a point in the story.",
                                  "Insignificant Characters" = "A character that doesn't have an actual name, is one of the mob characters or have no influence to the plot. ex: shrimp soldier."),
                  selected =1), 
      textOutput("ourVariable"),
      height = "500px"
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
         numericInput( 
                     "TsizeA", 
                     "Text size", 
                      value = 3, 
                       min = 1, 
                      max = 50 
         ),
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
         numericInput( 
           "TsizeB", 
           "Text size", 
           value = 3, 
           min = 1, 
           max = 50 
         ),
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
         numericInput( 
           "TsizeC", 
           "Text size", 
           value = 3, 
           min = 1, 
           max = 50 
         ),
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
         numericInput( 
           "TsizeD", 
           "Text size", 
           value = 3, 
           min = 1, 
           max = 50 
         ),
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
    scale_shape_manual(values=c(1, 2, 3, 4, 5, 6, 7, 8, 16, 17)) +
    scale_size(range = c(1, 10))+
    geom_node_text(aes(label=Revised_Names), color = "black", size = input$TsizeA) + 
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
  p<- ggraph(JTWA_net, layout = 'fr') +
    geom_edge_link(aes(width=Density*2), alpha = 0.4, color = "grey", arrow = arrow(length = unit(4, 'mm')))+ 
    geom_node_point(aes(color=.data[[input$NodesB]], size = .data[[input$sizeB]], shape = .data[[input$ShapeB]])) +
    scale_color_manual(values=c("red", "black", "lightblue")) +
    scale_shape_manual(values=c(1, 2, 3, 4, 5, 6, 7, 8, 16, 17)) +
    geom_node_text(aes(label=Revised_Names), color = "black", size = input$TsizeB, repel = TRUE) + 
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
    scale_shape_manual(values=c(1, 2, 3, 4, 5, 6, 7, 8, 16, 17)) +
    geom_node_text(aes(label=Revised_Names), color = "black", size = input$TsizeC) + 
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
    geom_node_text(aes(label=Revised_Names), color = "black", size = input$TsizeD, repel = TRUE) + 
    scale_shape_manual(values=c(1, 2, 3, 4, 5, 6, 7, 8, 16, 17)) +
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
    labs(x = "characters") +
    geom_col(aes(fill = Side, color = Origin)) + 
    coord_flip() 
  
  p
})



}

# Run the application 
shinyApp(ui = ui, server = server)



