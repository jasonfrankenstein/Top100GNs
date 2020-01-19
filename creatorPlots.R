# creatorPlots.R
# FIT5147 Data Visualization Assessment 4
# Jason Franks
# Monash Student No:28849892

# Draw plots for the top 20 creators or breaks down by discipline
DrawCreatorPlots <- function( creators, discipline)
{
  if( nrow(creators ) > 0 )# do nothing if there are no rows, otherwise we get errors
  {
  # Drop the rows for Various--they are not interesting for this plot
    creators <- subset(creators, Name !="Various") 
  
  
    xlabel = discipline
    if( discipline != "Creator")
    {
      creators <- subset(creators, role == discipline)
    }
    
    # rank the creators
    creators$rank <- rank(-creators$sumIndex,ties.method="min")
    
    # find the top 20 or fewer
    topNCreators <- creators[creators$rank <= 20,]
    topRank <- max( topNCreators$rank)
    
    ggplot(topNCreators, aes(Name, sumIndex, fill = str_wrap(Name,30))) +
      geom_bar(stat = "identity", color = "white") +
      theme(axis.title.x=element_text( xlabel),
            axis.text.x = element_text(angle=90, vjust = 1, hjust=1),
            axis.ticks.x=element_blank()) +
      labs(colour=xlabel, title = paste0( "2017 Top ", topRank, " ", xlabel, "s" ) )+
      scale_y_continuous("Sum of Index") +
      scale_x_discrete(xlabel) +
      guides(fill=FALSE) # hide the legend--it's big and ugly and unnecessary this time
  }
}