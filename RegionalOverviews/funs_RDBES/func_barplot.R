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
                     save_plot_to_list=TRUE){
  
  
  #colour
  colours <- read.csv2("../../data/colours2.csv")  
  country <- read.table("../../data/aux_countries.txt", sep = ",", header = T)
  names(country)[1] <- "CountryName"
  
  col <- merge(colours, country, by = "CountryName", all.x = T)
  col <- col[, c("ISO2Code", "colour5")]
  col <- setNames(object = col$colour5, nm = col$ISO2Code)

  # set parameters
  data <- data.frame(data)
  data$x <- data[, x]
  data$y <- data[, y]
  
  if(is.factor(data$x)){
    data$x<-as.character(data$x)
  }
  
  if (group != "") {
    data$grp <- as.factor(data[, group])
  } else {
    data$grp <- as.factor(data$x)
  }
  
  setDT(data)
  data <- data[ ,. (y = sum(y, na.rm = T)),
                        by = .(x, grp)] 
  
  #
  if (asPct == T) {
    data <- data[ ,. (y = (y/sum(y, na.rm = T))*100,
                      grp = unique(grp)),
                  by = .(x)] 
    
    #plot struff
    p <- ggplot(data=data, aes(x = as.factor(x), y = y, fill = grp)) +
      geom_bar(stat="identity") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      ggtitle(paste(title)) +
      xlab(paste(xlab)) +
      ylab(paste(ylab)) + labs(fill = group)
    
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
      col2 <- setNames(object = col2$V1, nm = col2$unique_x)
      
      #plot struff
      p1 <- ggplot(data=data, aes(x = reorder(x, -y), y = y, fill = grp)) +
        geom_bar(stat="identity") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
        ggtitle(paste(title)) +
        xlab(paste(xlab)) +
        ylab(paste(ylab)) +
        scale_fill_manual(values = col2)
      
      df <- data.frame(x=names(t2),y=t2)
      if (group != "") {
        df$grp <- as.factor(df[, group])
      } else {
        df$grp <- as.factor(df$x)
        if (group == "")
          p1 <- p1 + theme(legend.position="none")
      }
      p2 <- ggplot(data=df, aes(x = reorder(x, -y), y = y, fill = grp)) +
        geom_bar(stat="identity") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
        xlab(paste(xlab)) +
        ylab(paste("")) +
        scale_fill_manual(values = col2)#sub_col[length(t1[!t1 %in% t2])+1:length(sub_col)])
      
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
						 xlab = "",
						 ylab = "",
						 title = "",
						 country_col_file = "../../data/colours2.csv",
						 country_code_file = "../../data/aux_countries.txt") {
	  
	# Load color mapping
	  colours <- read.csv2(country_col_file)
	  country <- read.table(country_code_file, sep = ",", header = TRUE)
	  names(country)[1] <- "CountryName"
	  
	  col <- merge(colours, country, by = "CountryName", all.x = TRUE)
	  col <- col[, c("ISO2Code", "colour5")]
	  col <- setNames(col$colour5, col$ISO2Code)

	  # Prepare data
	  data <- data.frame(data)
	  
	  # Ensure x is a factor with alphabetical levels
	  data[[x]] <- factor(data[[x]], levels = sort(as.character(unique(data[[x]]))))
	  
	  data$x <- data[[x]]
	  data$y <- data[[y]]
	  
	  # Now use x as the axis and fill
	  p <- ggplot(data, aes(x = x, y = y, fill = x)) +
		geom_col() +
		xlab(xlab) +
		ylab(ylab) +
		ggtitle(title) +
		theme_minimal() +
		theme(axis.text.x = element_text(angle = 0, hjust = 1),
			  legend.position = "none") +
		scale_fill_manual(values = col)

	  print(p)
	}
