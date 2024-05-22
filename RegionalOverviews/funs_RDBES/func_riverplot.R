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
           center = 'HarbourCountry2',
           threesteps = FALSE,
           value = 'CLscientificWeight_ton'){
    # transform name of variable to field in df 
    df <- as.data.frame(df)
    df$left <- df[ ,left]
    df$right <- df[ ,right]
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
    
 
    if (threesteps){
      df$center <- df[ ,center]  
      df$right <- toupper(df$right)
      df$left <- toupper(df$left)
      df$center <- toupper(df$center)
      col3<-col2%>%
       mutate(ISO2Code=paste0(ISO2Code," "))
      col_river<-rbind(col_river,col3)
      df$check<-ifelse(df$center==df$right,'','add')
      final_data1<-df[df$check==''& df$left==df$right,c('left','right','value')]      
      final_data1$right <- paste0(toupper(final_data1$right)," ")
      final_data2<-df[df$check==''&df$left==df$right,c('left','right','value')]      
      final_data2$right <- paste0(toupper(final_data2$right),"  ")
      final_data2$left <- paste0(toupper(final_data2$left)," ")
      final_data11<-rbind(final_data1,final_data2)
      final_data<-final_data11
      if (nrow(df[df$check==''&df$left!=df$right,])!=0){
      final_data3<-df[df$check==''&df$left!=df$right,c('left','right','value')] 
      final_data3$right <- paste0(toupper(final_data3$right)," ")
      final_data4<-df[df$check==''&df$left!=df$right,c('right','value')] 
      final_data4$left <- paste0(toupper(final_data4$right)," ")      
      final_data4$right <- paste0(toupper(final_data4$right),"  ")
      final_data22<-rbind(final_data3,final_data4)
      final_data<-rbind(final_data11,final_data22)
      }
      if(nrow(df[df$check=='add',])!=0){
      subset1<-df[df$check=='add',c('left','center','value')]
      subset1$center <- paste0(toupper(subset1$center)," ")
      subset1%>%rename('right'='center')->subset1
      subset2<-df[df$check=='add',c('center','right','value')]
      subset2$center <- paste0(toupper(subset2$center)," ")
      subset2$right <- paste0(toupper(subset2$right),"  ")
      subset2%>%rename('left'='center')->subset2
      subset<-rbind(subset1,subset2)
      final_data<-rbind(final_data,subset)
      }

      nodes <- data.frame(name=c(as.character(final_data$left),
                                 as.character(final_data$right)) %>% unique())
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
      final_data$IDleft=match(final_data$left, nodes$name)-1 
      final_data$IDright=match(final_data$right, nodes$name)-1
      sankeyNetwork(Links = final_data, Nodes = nodes,
                    Source = "IDleft", Target = "IDright",
                    Value = "value", NodeID = "name", 
                    sinksRight=FALSE, 
                    colourScale=flow_color,
                    NodeGroup = "group",
                    nodeWidth=40, fontSize=13, nodePadding=20#,                    LinkGroup = "left"
                    )
      
    }else{
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
  }