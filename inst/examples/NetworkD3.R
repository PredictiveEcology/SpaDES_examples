# install.packages("reproducible") # if you don't have reproducible
reproducible::Require("networkD3")



# Create list of nodes, and the group that they are associated with. 
#  The group indicates the colour of the node ball, and the legend entry
#  size: is the ball size. These can be made independent for each ball
NodeNames = c("MPB", "Fire", "Climate Change", "Caribou", "Birds", "Vegetation", "Forest Harvest",
              "Oil & Gas", "Predators", "Caribou RSF", "Wolf Management", "Seismic Restoration",
              "Fire management", "Timber supply", "MPB management", "data")

Nodes <- data.frame(name = NodeNames,
                    group = c("Natural", "Natural", "Anthropogenic", "Wildlife", "Wildlife", "Trees", "Anthropogenic",
                              "Anthropogenic", "Wildlife", "Wildlife", "Management", "Management",
                              "Management", "Management", "Management", "data"),
                    size = 25, stringsAsFactors = FALSE)
Nodes[Nodes$name == "data", "size"] <- 4

FromTo <- data.frame(matrix(byrow = TRUE, ncol = 3,
                            c(
                              "Climate Change", "MPB", 1.5,
                              "MPB", "Vegetation", 1.5,
                              "MPB management", "MPB", 1.5,
                              "MPB management", "Vegetation", 1.5,
                              "Fire", "Caribou", 1.5,
                              "Forest Harvest", "Caribou", 1.5,
                              "Climate Change", "Caribou", 1.5,
                              "Oil & Gas", "Caribou", 1.5,
                              "Forest Harvest", "Fire", 1.5,
                              "Forest Harvest", "MPB", 1.5,
                              "Climate Change", "Forest Harvest", 1.5,
                              "Caribou RSF", "Caribou", 1,
                              "Caribou RSF", "Vegetation", 1,
                              "Predators", "Caribou", 1,
                              "Climate Change", "Vegetation", 1.5,
                              "Vegetation", "Fire", 1.5,
                              "Climate Change", "Fire", 1.5,
                              "Fire", "Birds", 1.5,
                              "Vegetation", "Birds", 1.5,
                              "Wolf Management", "Predators", 1,
                              "Seismic Restoration", "Caribou", 1.5,
                              "Seismic Restoration", "Vegetation", 1.5,
                              "Fire management", "Fire", 1.5,
                              "Fire management", "Vegetation", 1.5,
                              "Timber supply", "Forest Harvest", 1.5
                            )), stringsAsFactors = FALSE)
FromToDataNode <- data.frame(X1 = "data",
                             X2 = Nodes[!Nodes$group %in% c("data", "Management"),"name"],
                             X3 = 2)
FromTo <- rbind(FromTo, FromToDataNode)

# The function "forceNetwork" needs the nodes to be numeric, so this next line
#  converts the words to numerics
FromToNumeric <- data.frame(source = match(FromTo$X1, Nodes$name) - 1,
                            target = match(FromTo$X2, Nodes$name) - 1,
                            value = as.numeric(FromTo$X3))#,
#linkSize = sample(2:10, size = NROW(FromTo), replace = TRUE))

# Link colours -- make default blue, and modify from there, brown = data, green = management
linkCol <- rep("blue", NROW(FromToNumeric))
linkCol[FromToNumeric$source == match("data", Nodes$name) - 1 |
          FromToNumeric$target == match("data", Nodes$name) - 1] <- "brown"
managementNodes <- grep("anagement", Nodes$group) - 1
linkCol[FromToNumeric$source %in% managementNodes |
          FromToNumeric$target %in% managementNodes] <- "green"

#MyClickScript <- 'alert("You clicked " + d.name + " which is in row " +
#       (d.index + 1) +  " of your original R data frame");'

forceNetwork(Links = FromToNumeric, Nodes = Nodes, Source = "source",
             Target = "target", Value = "value", NodeID = "name", Nodesize = "size",
             Group = "group", opacity = 0.9, zoom = TRUE, bounded = TRUE, legend = TRUE,
             fontFamily = "cursive", opacityNoHover = 0.4, fontSize = 30,
             #Nodesize = 10,
             #linkWidth = 5,
             charge = -80,
             #linkWidth = JS("function(d) { return Math.sqrt(d.value2); }"),
             colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);"),
             linkColour = linkCol, arrows = TRUE,
             #clickAction = MyClickScript,
             linkDistance = JS("function(d){return d.value * 100}")
)
