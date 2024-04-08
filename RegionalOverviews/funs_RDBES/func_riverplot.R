#'Function which prepare riverplot ( Sankey network)
#'
#' @param df data frame
#' @param left variable on the left side of the plot, e.g. FlagCountry
#' @param right variable on the left side of the plot, e.g. LandingCountry
#' @param value determines the width of the edges
#'
#' @return riverplot (Sankey network)
#'
#' @author Kasia Krakówka NMFRI <kkrakowka@@mir.gdynia.pl>
#' 

func_riverplot <-
  function(df = df,
           left = 'CLvesselFlagCountry',
           right = 'CLlandingCountry',
           value = 'CLscientificWeight_ton'){
    # transform name of variable to field in df 
    df <- as.data.frame(df)
    df$left <- df[ ,left]
    df$right <- tolower(df[ ,right])
    df$value <- df[ ,value]
    #colors
    colours <- read.csv2("../../data/colours2.csv")
    country <- read.table("../../data/aux_countries.txt", sep = ",", header = T)
    names(country)[1] <- "CountryName"

    col <- merge(colours, country, by = "CountryName", all.x = T)
    col1 <- col[, c("ISO2Code", "colour5")]
    col2<-col1%>%
      mutate(ISO2Code=paste0(ISO2Code," "))
    col_river<-rbind(col1,col2)
    
    df$right <- paste0(toupper(df$right)," ")
    
    # From these flows we need to create a node data frame: it lists every entities involved in the flow
    nodes <- data.frame(name=c(as.character(df$left), as.character(df$right)) %>% unique())
    nodes <- merge(nodes,col_river,by.x="name",by.y="ISO2Code",all.x=TRUE)
    nodes$group<-nodes$name
    domain <- as.factor(nodes$group)
    range <- as.factor(nodes$colour5)
    
    flow_color<-paste0(
      'd3.scaleOrdinal()
     .domain(["', 
      paste0(domain, collapse = '", "'),
      '"]) .range(["',
      paste0(range, collapse = '", "'),
      '"])')
    
    # Connection must be provided using id, not using real name of field
    df$IDleft=match(df$left, nodes$name)-1 
    df$IDright=match(df$right, nodes$name)-1
    
    sankeyNetwork(Links = df, Nodes = nodes,
                  Source = "IDleft", Target = "IDright",
                  Value = "value", NodeID = "name", 
                  sinksRight=FALSE, 
                  colourScale=flow_color,
                  NodeGroup = "group",
                  nodeWidth=40, fontSize=13, nodePadding=20,
                  LinkGroup = "left")
  }