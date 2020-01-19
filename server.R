# server.R
# FIT5147 Data Visualization Assessment 4
# Jason Franks
# Monash Student No:28849892

# Uncomment to install required packages:
# install.packages("shiny")
# install.packages("ggplot2") 
# install.packages("data.table")
# install.packages("dplyr")
# install.packages("stringr")
# install.packages("scales")

library(shiny)
library(ggplot2) 
library(data.table)
library(dplyr)
library(stringr)
library(scales)

# include other code modules
source( file="publisherPlots.R")
source( file="titlePlots.R")
source( file="creativeTeamPlots.R")
source( file="creatorPlots.R")

# Start by loading data globally. We will share this for all server projects
top100GNs2017 <- read.csv("diamonddata_gn_clean.csv")
top100GNs2017$ITEM.CODE <- as.character(top100GNs2017$ITEM.CODE )
top100GNs2017$DESCRIPTION <- as.character(top100GNs2017$DESCRIPTION )
top100GNs2017$CreativeTeam <- as.character(top100GNs2017$CreativeTeam )
top100GNs2017$Writer <- as.character(top100GNs2017$Writer )
top100GNs2017$InteriorArtist <- as.character(top100GNs2017$InteriorArtist )
top100GNs2017$CoverArtist <- as.character(top100GNs2017$CoverArtist )
top100GNs2017$Month <- as.Date(top100GNs2017$Month )
top100GNs2017$WriterIsArtist <- as.logical(top100GNs2017$WriterIsArtist )
top100GNs2017$WriterIsCoverArtist <- as.logical(top100GNs2017$WriterIsCoverArtist )
top100GNs2017$ArtistIsCoverArtist <- as.logical(top100GNs2017$ArtistIsCoverArtist )
top100GNs2017$CreatorOwned <- as.logical(top100GNs2017$CreatorOwned )
top100GNs2017$Creators <- paste(top100GNs2017$Writer, top100GNs2017$InteriorArtist, top100GNs2017$CoverArtist, sep=",")


# Now define the server
shinyServer(function(input, output) {
  # Reactive expression to generate the requested filtered data set.
  # Otherwise we have to do this for every plot.
  currentDF <- reactive({
    retval <- top100GNs2017
    if( input$titleType == "wfh" )
    {
      retval <- top100GNs2017[top100GNs2017$CreatorOwned == FALSE,]
    }               
    else if( input$titleType == "creatorOwned" )
    {
      retval <- top100GNs2017[top100GNs2017$CreatorOwned == TRUE,]
    }
    
    retval <- retval[retval$VENDOR %in% input$publishers,]
    return(retval)
  
  })
  
  # Another reactive expression, to get an unpacked list of all 
  # creators and their roles.
  currentCreators <- reactive({
    
    # Get the filtered data
    workingDF <- currentDF()
    
    if( nrow( workingDF) == 0)
    {
      df <- data.frame(matrix(ncol = 5, nrow = 0))
      colnames(df) <- c("Name", "Writer", "InteriorArtist", "CoverArtist", "role" )
      return( df)   
    }
    
    # wrap in data table
    DT <- as.data.table(workingDF) 
    
    # Unlist all of the comma-separated writers in the Writers field and pivot into rows, keeping INDEX and ITEM.CODE fields
    DTWriters = {
      DT[, .(Name = unlist(strsplit(as.character(Writer), ",", fixed = TRUE)),Writer=1, InteriorArtist=0,CoverArtist=0), 
         by = .(INDEX, ITEM.CODE)]}
    
    # And for artists
    DTInteriorArtists = {
      DT[, .(Name = unlist(strsplit(as.character(InteriorArtist), ",", fixed = TRUE)),Writer=0, InteriorArtist=1,CoverArtist=0), 
         by = .(INDEX, ITEM.CODE)]}
    
    # And cover artists
    DTCoverArtists = {
      DT[, .(Name = unlist(strsplit(as.character(CoverArtist), ",", fixed = TRUE)), Writer=0, InteriorArtist=0,CoverArtist=1), 
         by = .(INDEX, ITEM.CODE)]}
    
    # Combine into a single object
    creatorDF <- rbind(DTWriters, DTInteriorArtists, DTCoverArtists )
    
    # trim whitespace
    creatorDF$Name <- trimws(creatorDF$Name, which = "both")
    
    # Now we want to flatten  everything down so there's a single row for every creator
    # There must be a more efficient way to do this but grouping and summarizing twice does the job ok
    # We have to do this because, when a creator has multiple roles inside the same title, 
    # that title will be counted twice or three times when we calculate their score
    creators <- group_by(creatorDF, Name, ITEM.CODE, INDEX ) %>%
      summarise(isWriter = sum(Writer), isInteriorArtist=sum(InteriorArtist), isCoverArtist=sum(CoverArtist))
    
    # Then group again. 
    creators <- group_by(creators, Name ) %>%
      summarise(Writer = sum(isWriter), InteriorArtist=sum(isInteriorArtist), CoverArtist=sum(isCoverArtist), sumIndex = sum(INDEX))
    
    # Now set the creators role
    creators$role <- ""
    creators$role[creators$Writer > 0] <- "Writer"
    creators$role[creators$CoverArtist > 0] <- "Cover Artist"
    creators$role[creators$InteriorArtist > 0] <- "Interior Artist"
    creators$role <- ifelse( (creators$Writer > 0 ) & (( creators$InteriorArtist >0)| (creators$CoverArtist > 0)),  "Writer Artist", creators$role)
    
    # restore DF standard behaviour
    creators <- as.data.frame(creators)
    
    return(creators)
  })
  
  output$publishersLine <- renderPlot({
    workingDF <- currentDF()
    
    DrawPublishersLine(workingDF)
  })
  
  # Now draw the plots. These will call into the imported code modules to draw a specific chart
  output$publishersChart <- renderPlot({
    workingDF <- currentDF()
    if( input$barType == "line")
    {
      DrawPublishersLine(workingDF)
    }
    else
    {
      DrawPublishersBar(workingDF, input$barType == "polar")
    }
      
  })
  
  output$publishersOverall <- renderPlot({
    workingDF <- currentDF()
    
    DrawPublishersOverall(workingDF)
    
  })
  
  
  output$titleSales <- renderPlot({
    DrawTitleSalesPlot( currentDF() )
  })
  
  
  output$titleVolumes <- renderPlot({
    DrawTitleVolumePlot( currentDF())
  })
  
  output$titleFrequencyBreakdown <- renderPlot({
    DrawTitleFrequencyByPlot( currentDF(), input$titleFreqBy)
  })
  
  output$teamSales <- renderPlot({
   DrawCreativeTeamPlots(currentDF(), input$teamDiscipline)
  })
  
  output$creatorSales <- renderPlot({
    DrawCreatorPlots(currentCreators(), input$creatorDiscipline)
  })
  
})

