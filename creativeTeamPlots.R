# creativeTeamPlots.R
# FIT5147 Data Visualization Assessment 4
# Jason Franks
# Monash Student No:28849892

# Draw plots for the top 10 creative teams or break down by discipline
DrawCreativeTeamPlots <- function( workingDF, discipline)
{
  team_index_sum <- data.frame(matrix(ncol=3, nrow=0))
  
  # there has to be a more elegant way of handling the switch values on the input  variable--
  # and there is. To use the columns byt heir numeric index and to use aes_string in ggplot
  # Alas, alack, str_wrap doesn't work nicely inside aes_string, so we'll go with ugly
  # but functional in order to get some legible plots
  
  # slice the data according to the requested creative discipline
  if( discipline == "all") # no discipline filter
  {
    team_index_sum <- group_by(workingDF, CreativeTeam, VENDOR) %>%
      summarise(sumIndex = sum(INDEX, na.rm = TRUE))
  }
  else if( discipline == "Writer")
  {
    team_index_sum <- group_by(workingDF, Writer, VENDOR) %>%
      summarise(sumIndex = sum(INDEX, na.rm = TRUE))
  }
  else if( discipline == "InteriorArtist")
  {
    team_index_sum <- group_by(workingDF, InteriorArtist, VENDOR) %>%
      summarise(sumIndex = sum(INDEX, na.rm = TRUE))
  }
  else if( discipline == "CoverArtist")
  {
    team_index_sum <- group_by(workingDF, CoverArtist, VENDOR) %>%
      summarise(sumIndex = sum(INDEX, na.rm = TRUE))
  }
  
  team_index_sum <- team_index_sum[order(team_index_sum$sumIndex, decreasing=TRUE),]
  
  # get the top 20... ish
  team_index_sum$rank <- rank(-team_index_sum$sumIndex,ties.method="min")
  topNTeams <- team_index_sum[team_index_sum$rank <= 10,]
  
  topRank <- max( topNTeams$rank)
  
  plot <- ggplot(topNTeams )
  xlabel <- discipline
  if( discipline == "all")
  {
    xlabel <- "Creative Team"
    plot <- ggplot(topNTeams,  aes(CreativeTeam, sumIndex, fill = str_wrap(CreativeTeam,30))) 
  }
  else if( discipline == "Writer")
  {
    xlabel <- "Writer"
    plot <- ggplot(topNTeams,  aes(Writer, sumIndex, fill = str_wrap(Writer,30))) 
  }
  else if( discipline == "InteriorArtist")
  {
    xlabel <- "Interior Artist"
    plot <- ggplot(topNTeams,  aes(InteriorArtist, sumIndex, fill = str_wrap(InteriorArtist,30))) 
  }
  else if( discipline == "CoverArtist")
  {
    xlabel <- "Cover Artist"
    plot <- ggplot(topNTeams, aes(CoverArtist, sumIndex, fill = str_wrap(CoverArtist,30))) 
  }
  
  plot <- plot + geom_bar(stat = "identity", color = "white") +
    scale_y_continuous("Sum of Index") +
    scale_x_continuous(xlabel) +
    theme(axis.title.x=element_text(xlabel),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    scale_y_continuous("Sum of Index") +
    scale_x_discrete(xlabel) +
    labs(colour=discipline, title = paste0( "2017 Top ", topRank, " ", xlabel, "s") ) +
    guides(fill=guide_legend(title=xlabel))
  
  plot
}