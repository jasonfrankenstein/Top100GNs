# publisherPlots.R
# FIT5147 Data Visualization Assessment 4
# Jason Franks
# Monash Student No:28849892

# draw a bar chart showing the indexed sales of the top 20 titles, coloured by publisher
DrawTitleSalesPlot <- function(workingDF)
{
  # group by description and sum the index
  title_index_sum <- group_by(workingDF, DESCRIPTION, VENDOR) %>%
    summarise(sumIndex = sum(INDEX, na.rm = TRUE))
  
  # set the order
  title_index_sum <- title_index_sum[order(title_index_sum$sumIndex, decreasing=TRUE),]
  
  # set a rank to find the top 20
  title_index_sum$rank <- rank(-title_index_sum$sumIndex,ties.method="min")
  topNTitles <- title_index_sum[title_index_sum$rank <= 20,]
  
  topRank <- max(topNTitles$rank)
  # draw it
  ggplot(topNTitles, aes(DESCRIPTION, sumIndex, fill = VENDOR)) +
    geom_bar(stat = "identity", color = "white") +
    theme(axis.text.x = element_text(angle=90, vjust = 1, hjust=1))+
    scale_y_continuous("Sum of Index") +
    scale_x_discrete("Title") +
    labs(colour="Publisher", title = paste0( "2017 Top ", topRank, " Titles by Index" ) )+
    guides(fill=guide_legend(title="Publisher"))
}

# Draw a bar chart showing the top 20 most frequently ordered appearing books
DrawTitleFrequencyByPlot <- function( workingDF, titleFreqBy)
{
  if( nrow( workingDF) > 0 ) # do nothing if there are no rows, otherwise we get error messages immediately
  {
    # For this one, group by item, ie we are looking for specific volumes of specific books
    if( titleFreqBy == "item")
    {
      mostFreqByItem <- as.data.frame(table(workingDF$ITEM.CODE))
      mostFreqByItem <- mostFreqByItem[order(mostFreqByItem$Freq, decreasing=TRUE),]
      
      mostFreqByItem$rank <- rank(-mostFreqByItem$Freq,ties.method="min")
      mostFreqByItem <- mostFreqByItem[mostFreqByItem$rank <= 20,]
      
      mostFreqByItem <- merge( unique(workingDF[,c("DESCRIPTION", "VENDOR", "ITEM.CODE")]), mostFreqByItem, by.x ="ITEM.CODE", by.y ="Var1")
      
      topRank <- max(mostFreqByItem$rank)
      
      ggplot(mostFreqByItem, aes(ITEM.CODE, as.numeric(Freq), fill = VENDOR)) +
        # draw the title of the book inside the bar, because the item codes on the x Axis are not very illuminating by themselves
        geom_bar(width = 1, stat = "identity", color = "white", position="dodge") +
        geom_text(aes(label=DESCRIPTION), position=position_stack( vjust=.5)) +
        scale_x_discrete("Item Code") +
        scale_y_continuous("Freqency") +
        theme(axis.text.x = element_text(angle=0, vjust = 1, hjust=1))+
        labs(colour="Publisher", title = paste0( "2017 Top ", topRank, " Most Frequently Ordered Items") ) + 
        guides(fill=guide_legend(title="Publisher")) +
        coord_flip() # draw it sideways
    }
    # Group by TITLE, so we are collapsing all items with a matching title together for this plot
    else if( titleFreqBy == "title")
    {
      mostFreqByDesc <- as.data.frame(table(workingDF$DESCRIPTION))
      mostFreqByDesc <- mostFreqByDesc[order(mostFreqByDesc$Freq, decreasing=TRUE),]
      
      mostFreqByDesc$rank <- rank(-mostFreqByDesc$Freq,ties.method="min")
      mostFreqByDesc <- mostFreqByDesc[mostFreqByDesc$rank <= 20,]
      
      mostFreqByDesc <- merge( unique(workingDF[,c("DESCRIPTION", "VENDOR")]), mostFreqByDesc, by.x ="DESCRIPTION", by.y ="Var1")
      
      topRank <- max(mostFreqByDesc$rank)
      
      ggplot(mostFreqByDesc, aes(DESCRIPTION, as.numeric(Freq), fill = VENDOR)) +
        geom_bar(width = 1, stat = "identity", color = "white") +
        theme(axis.text.x = element_text(angle=0, vjust = 1, hjust=1))+
        labs(colour="Publisher", title = paste0("Top ", topRank, " Most Frequently Ordered Titles")) + 
        guides(fill=guide_legend(title="Publisher")) + 
        scale_y_continuous("Freqency") +
        scale_x_discrete("Title") +
        coord_flip() # draw sideways
    }
  }
}

# draw a plot of the number of volumes per title, it how many item codes appear in the charts for a given DESCRIPTION
DrawTitleVolumePlot <- function( workingDF )
{
    itemsPerTitle <- group_by(workingDF, DESCRIPTION)  %>% 
      summarise(count=length(unique(ITEM.CODE)))
    
    itemsPerTitle <- itemsPerTitle[order(-itemsPerTitle$count),]
    
    # rank them and find the top 20
    itemsPerTitle$rank <- rank(-itemsPerTitle$count,ties.method="min")
    itemsPerTitle <- itemsPerTitle[itemsPerTitle$rank<=20,]
    
    itemsPerTitle <- merge( unique(workingDF[,c("DESCRIPTION", "VENDOR")]), itemsPerTitle, by ="DESCRIPTION")
    
    topRank <- max(itemsPerTitle$rank)
  
    ggplot(itemsPerTitle, aes(DESCRIPTION, as.numeric(count), fill = VENDOR)) +
      geom_bar(width = 1, stat = "identity", color = "white") +
      scale_x_discrete("Title") +
      scale_y_continuous("Number of Volumes Ordered") +
      theme(axis.text.x = element_text(angle=0, vjust = 1, hjust=1))+
      labs(colour="Publisher", title = paste0("2017 Top ", topRank, " Volumes per Title" )) + 
      coord_flip() # draw sideways
}