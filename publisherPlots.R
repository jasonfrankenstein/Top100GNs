# publisherPlots.R
# FIT5147 Data Visualization Assessment 4
# Jason Franks
# Monash Student No:28849892

# Draw a line graph of the publishers' performance
DrawPublishersLine <- function(workingDF)
{
  publisher_index_sum <- group_by(workingDF, VENDOR, Month) %>%
    summarise(sumIndex = sum(INDEX, na.rm = TRUE))
  
  # line graph
  ggplot(na.omit(publisher_index_sum), aes(x =Month, y = sumIndex, group=VENDOR, colour = VENDOR)) +
    geom_point(stat="identity") + 
    geom_line() +
    theme(legend.position = "right", legend.direction = "vertical", plot.title = element_text(hjust = 0.5)) +
    scale_x_date("Month", labels=date_format( "%B"), date_breaks="1 month")+
    scale_y_continuous("Sum of Index") +
    labs( color="Publisher", title = "2017 Indexed Monthly Sales by Publisher")
}

# draw a bar chart pf pubisher performance. Wrap to polar coordinates for a wind rose chart
DrawPublishersBar <- function(workingDF, polar)
{
  publisher_index_sum <- group_by(workingDF, VENDOR, Month) %>%
    summarise(sumIndex = sum(INDEX, na.rm = TRUE))
  
  plot <- ggplot(publisher_index_sum, aes(Month, sumIndex, fill = VENDOR)) +
    geom_bar( stat = "identity", color = "white") +
    scale_x_date("Month", labels=date_format( "%B"), date_breaks="1 month") +
    scale_y_continuous("Sum of Index") +
    theme(axis.text.x = element_text(angle=0, vjust = 1, hjust=1))+
    labs( color = "Publisher", title = "2017 Indexed Monthly Sales by Publisher") +
    guides(fill=guide_legend(title="Publisher"))
  
  if( polar == TRUE) # polarr coordinates make an ace wind rose chart
  {
    plot <- plot + coord_polar()
  }
  
  plot
}

DrawPublishersOverall <- function(workingDF)
{
  publisher_index_sum <- group_by(workingDF, VENDOR ) %>%
    summarise(sumIndex = sum(INDEX, na.rm = TRUE))
  
  ggplot(publisher_index_sum, aes(VENDOR, sumIndex, fill = VENDOR)) +
    geom_bar( stat = "identity", color = "white") +
    scale_x_discrete("Publisher") +
    scale_y_continuous("Sum of Index") +
    theme(axis.text.x = element_text(angle=90, vjust = 1, hjust=1))+
    labs( color = "Publisher", title = "2017 Indexed Sales by Publisher") +
    guides(fill=guide_legend(title="Publisher"))
}