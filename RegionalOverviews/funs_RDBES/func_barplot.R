# Authors: 
# - Jonathan Stounberg
# - Kasia Krakówka
# - Ana Cláudia Fernandes
# 
# Dev. notes: 
#
# - 20240404: Added subplot

barplot <- function(data = data,
                     x = "",
                     y = "", 
                     group = "",
                     asPct = F,
                     title = "",
                     ylab = "",
                     xlab = "",
                     col_cou= F,
                     save_plot_to_list=TRUE,
                    groupNiceName = "",
                     facet =  ""){ # is available only with asPac=T
  
 #need to be exported in rdbesvisualise package
 # RDBESvisualise::colourCountryTab

  # temporal solution
    if(regionSelected == "LDF"){
      colours <- data.table(koloryRDBES)
    }else{
      colours <- data.table(colours)
    }

  colours$ISO2Code <- colours$country
  colours$colour5 <- colours$color
  col <- colours[ ,c("ISO2Code","colour5")]
  col <- setNames(object = col$colour5, nm = col$ISO2Code)

  # set parameters
  data <- data.frame(data)
  data$x <- data[, x]
  data$y <- data[, y]
  if(facet!="") 
    data$facet <- data[, facet]
  
  if(is.factor(data$x)){
    data$x<-as.character(data$x)
  }
  
  if (group != "") {
    data$grp <- as.factor(data[, group])
  } else {
    data$grp <- as.factor(data$x)
  }
  
  setDT(data)
  
  # Ustal globalną kolejność x na podstawie sum(y), zanim cokolwiek przekształcimy
  x_order <- data[, .(y_sum = sum(.SD[[1]], na.rm = TRUE)), by = x, .SDcols = y]
  x_order <- x_order[order(-y_sum), x]
  data[, x := factor(x, levels = x_order)]
  
  setorder(data, x)
  
  if(facet!= "") {
    data <- data[ , .(y = sum(y, na.rm = T)), by = .(x, grp, facet)]
  } else {
    data <- data[ , .(y = sum(y, na.rm = T)), by = .(x, grp)]
  }
  
  #
  if (asPct == T) {
    if(facet != ""){
      data <- data[ ,. (y = (y/sum(y, na.rm = T))*100,
                        grp = unique(grp)),
                    by = .(x,facet)] 
      all_combinations <- CJ(facet = unique(data$facet),
                             grp = unique(data$grp),
                             x = unique(data$x),
                             unique = TRUE)
      data<- merge(all_combinations, data, by = c("facet", "grp", "x"), all.x = TRUE)
      data[is.na(y), y := 0]
      
    }else{
 
    data <- data[ ,. (y = (y/sum(y, na.rm = T))*100,
                      grp = unique(grp)),
                  by = .(x)] 
    
    }
    
      
    #plot struff
    p <- ggplot(data=data, aes(x = x, y = y, fill = grp)) + #_reordered, y = y, fill = grp)) + 
      geom_bar(stat="identity") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      ggtitle(paste(title)) +
      xlab(paste(xlab)) +
      ylab(paste(ylab)) + 
      labs(fill = group)
    
    if(groupNiceName != ''){
      p <- p + guides(fill=guide_legend(groupNiceName))
    }
    
    if(facet != ""){

      p<- p +facet_wrap(~facet)
    }
      
    
  } else {

    t1<-tapply(data[,y], list(data[,x]), sum, na.rm=T)
    t1<-sort(t1, decreasing=T)
    t1[is.na(t1)]<-0
    sub_t1 <- determine_what_to_inset(freq = t1, target_ratio = 10)
    if(!is.null(sub_t1) & length(sub_t1) > 4){
      t2 <- t1[names(t1) %in% sub_t1] 
      unique_x<-unique(data$x)
      col2<-subset(colours, select = colour5)
      col2<-distinct(col2)
      col2 <- as.data.table(cbind(col2[1:length(unique_x),c("colour5")], unique_x))
      col2 <- setNames(object = col2$colour5, nm = col2$unique_x)
      
      #plot struff
      p0 <- ggplot(data=data, aes(x = reorder(x, -y), y = y, fill = grp)) +
        geom_bar(stat="identity") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
        ggtitle(paste(title)) +
        xlab(paste(xlab)) +
        ylab(paste(ylab)) 
      
      if(group %like% "Country" || col_cou == T){
        p1 <- p0 + scale_fill_manual(values = col)
      }else{
        p1 <- p0 + scale_fill_manual(values = col2)
      }
      
      
      df <- data.frame(x=names(t2),y=t2)
      if (group != "") {
        df$grp <- as.factor(df[, group])
      } else {
        df$grp <- as.factor(df$x)
        if (group == "")
          p1 <- p1 + theme(legend.position="none")
      }
      p2_0 <- ggplot(data=df, aes(x = reorder(x, -y), y = y, fill = grp)) +
        geom_bar(stat="identity") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
        xlab(paste(xlab)) +
        ylab(paste("")) +
        scale_x_discrete(label = function(x) stringr::str_trunc(x, 12))
      
      
      if(group %like% "Country" || col_cou == T){
        p2 <- p2_0 +
          scale_fill_manual(values = col)#sub_col[length(t1[!t1 %in% t2])+1:length(sub_col)])
        
      }else{
        p2 <- p2_0 +
          scale_fill_manual(values = col2)#sub_col[length(t1[!t1 %in% t2])+1:length(sub_col)])
        
      }
      
      
      p2 <- p2 + theme(legend.position="none")
      
      
      p<- p1 + inset_element(p2, left = 0.4, bottom = 0.4, right = 0.99, top = 0.99)
      
    }else{
    #plot struff
    p <- ggplot(data=data, aes(x = reorder(x, -y), y = y, fill = grp)) +
      geom_bar(stat="identity") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      ggtitle(paste(title)) +
      xlab(paste(xlab)) +
      ylab(paste(ylab))
    }
    }

  
  if (group %like% "Country" || col_cou == T)
    p <- p + scale_fill_manual(values = col)
  
  if (group == "")
    p <- p + theme(legend.position="none")
  
  print(p)
}

## Function to use when ploting countries in the x-axis

	barplot1 <- function(data,
                     x = "",
                     y = "",
                     group = "",
                     asPct = FALSE,
                     title = "",
                     xlab = "Country",
                     ylab = "",
                     col_cou = FALSE,
                     all_countries = NULL) {

  	  # Read country colours if requested
  if (col_cou || grepl("Country", group)) {
    colours <- data.table(colours)
    colours$ISO2Code <- colours$country
    colours$colour5 <- colours$color
    col <- colours[ ,c("ISO2Code","colour5")]
    col <- setNames(object = col$colour5, nm = col$ISO2Code)
  }

  data <- as.data.frame(data)

  # Get all countries (even those missing y data)
  all_countries <- sort(unique(as.character(data[[x]])))

  if (group != "") {
    data$grp <- as.factor(data[[group]])

    full_grid <- expand.grid(
      xval = all_countries,
      grp = unique(data$grp)
    )
    names(full_grid) <- c(x, "grp")

    df <- data %>%
      mutate(grp = as.factor(.data[[group]])) %>%
      group_by(.data[[x]], grp) %>%
      summarise(yval = sum(.data[[y]], na.rm = TRUE), .groups = "drop") %>%
      right_join(full_grid, by = c(x, "grp")) %>%
      mutate(yval = replace_na(yval, 0))
  } else {
    df <- data %>%
      group_by(.data[[x]]) %>%
      summarise(yval = sum(.data[[y]], na.rm = TRUE), .groups = "drop")

    # Ensure all countries are represented
    df <- df %>%
      right_join(data.frame(temp_x = all_countries), by = setNames("temp_x", x)) %>%
      mutate(yval = replace_na(yval, 0)) %>%
      mutate(grp = .data[[x]])
  }

  # Set factor levels to ensure full country inclusion and order
  df[[x]] <- factor(df[[x]], levels = all_countries)

  # Convert to percent if needed
  if (asPct) {
    df <- df %>%
      group_by(.data[[x]]) %>%
      mutate(yval = (yval / sum(yval)) * 100) %>%
      ungroup()
  }

  # Build plot
  p <- ggplot(df, aes(x = .data[[x]], y = yval, fill = grp)) +
    geom_col() +
    theme_minimal() +
    xlab(xlab) +
    ylab(ifelse(asPct, "Percentage (%)", ylab)) +
    ggtitle(title) +
    theme(axis.text.x = element_text(angle = 0, hjust = 1))

  # Apply colours
  if (col_cou || grepl("Country", group)) {
    p <- p + scale_fill_manual(values = col)
  }

  # Hide legend if no grouping
  if (group == "") {
    p <- p + theme(legend.position = "none")
  } else {
    p <- p + labs(fill = group)
  }

  print(p)
}
