# creatorNetwork.R
# requires The following libraries included in parent script: 
#  library(igraph)

setwd("E:\\R\\DataViz\\Assignment4")
publisherList <- scan("publishers.txt", what="", sep="\n")
# start by loading data globally. (
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
top100GNs2017$Creators <- paste(top100GNs2017$Writer, top100GNs2017$InteriorArtist,top100GNs2017$CoverArtist, sep=",")
#top100GNs2017$Creators <- gsub(" ", "", top100GNs2017$Creators, fixed = TRUE)


DT <- as.data.table(top100GNs2017) 
# Unlist all of the comma-separated writers in the Writers field and pivot into rows, keeping INDEX and ITEM.CODE fields

DTWriters = {
  DT[, .(Name = unlist(strsplit(as.character(Writer), ",", fixed = TRUE)),Writer=1, InteriorArtist=0,CoverArtist=0), 
     by = .(INDEX, ITEM.CODE)]}

#And for artists
DTInteriorArtists = {
  DT[, .(Name = unlist(strsplit(as.character(InteriorArtist), ",", fixed = TRUE)),Writer=0, InteriorArtist=1,CoverArtist=0), 
     by = .(INDEX, ITEM.CODE)]}
# And cover artists
DTCoverArtists = {
  DT[, .(Name = unlist(strsplit(as.character(CoverArtist), ",", fixed = TRUE)), Writer=0, InteriorArtist=0,CoverArtist=1), 
     by = .(INDEX, ITEM.CODE)]}

# combine into a single DF
creatorDF <- rbind(DTWriters, DTInteriorArtists, DTCoverArtists )

# trim whitespace
creatorDF$Name <- trimws(creatorDF$Name, which = "both")

# Now we want to flatten  everything down so there's a single row for every creator
# There must be a more efficient way to do this but grouping and summarizing twice does the job ok
# We have to do this because if a creator has multiple roles inside the same title 
# that title will be counted twice or three times when we sum the index


creators <- group_by(creatorDF, Name, ITEM.CODE, INDEX ) %>%
  summarise(isWriter = sum(Writer), isInteriorArtist=sum(InteriorArtist), isCoverArtist=sum(CoverArtist))

# The group again. This 
creators <- group_by(creators, Name ) %>%
  summarise(Writer = sum(isWriter), InteriorArtist=sum(isInteriorArtist), CoverArtist=sum(isCoverArtist), sumIndex = sum(INDEX))

creators$role <- ""
creators$role[creators$Writer > 0] <- "Writer"
creators$role[creators$InteriorArtist > 0] <- "Interior Artist"
creators$role[creators$CoverArtist > 0] <- "Cover Artist"
creators$role <- ifelse( (creators$Writer > 0 ) & (( creators$InteriorArtist >0)| (creators$CoverArtist > 0)),  "Writer Artist", creators$role)

creators <- subset(creators, !is.na(creators$Name))

creators <- as.data.frame((creators))

# Helper function to populate the matrix
addToMatrix <- function( matrix, col1, col2)
{
  adjMatrix[col1,col2] <<- adjMatrix[col1,col2] + 1 
  adjMatrix[col2,col1] <<- adjMatrix[col2,col1] + 1 
  
  matrix
}


populateMatrix <- function(row, matrix) 
{
  toks <- unlist(strsplit(as.character(row$Creators), ",", fixed = TRUE))
  toks <- cat( unlist(strsplit(as.character(row$InteriorArtist), ",", fixed = TRUE)))
  toks <- cat( unlist(strsplit(as.character(row$CoverArtist), ",", fixed = TRUE)))
  toks <- trimws(toks, which = "both")
  
  
  toks <- unlist(strsplit("A,B, (A) C, (CA) D, (W/A) E, ,F", ",", fixed = TRUE))
  toks <- trimws(toks, which = "both")
  
  nextToks <- toks
  for( tok in toks )
  {   
    lnt <- length(nextToks)                
    
    if( lnt>1)
    {
      nextToks <- nextToks[2:lnt]
      for( tok2 in nextToks)
      {                     
        addToMatrix( matrix, prevTok, tok[2])
      }
    }
  }
}


plotCreatorNetworkOld1 <- function( creators, currentDF)
{
  creators <- subset(creators, !is.na(creators$Name))
  
  creators <- head(creators,20)
  
  # create adjacency matrix
  adjMatrix <- data.frame(matrix(0L, ncol = nrow(creators), nrow = nrow(creators)))
  colnames(adjMatrix) <- creators$Name
  rownames(adjMatrix) <- creators$Name
  
  adjMatrix <- addToMatrix( adjMatrix, "Jill Thompson", "Bryan Hitch")
  adjMatrix <- addToMatrix( adjMatrix, "Jill Thompson", "Amanda Conner")
  adjMatrix <- addToMatrix( adjMatrix, "Jill Thompson", "Amanda Conner")
  
  #apply(creators, 1, populateMatrix)
  
  
  g <- graph_from_adjacency_matrix( as.matrix(adjMatrix), mode = "undirected", weighted=TRUE )
  
  V(g)$sumIndex <- creators[creators$Name == V(g)$name, 5] 
  #E(g)$weights <- ??
  #plot(g, edge.width=E(g)$weights*10, vertex.size=V(g)$sumIndex*4)
  #graphics.off()
  #par("mar")
  #par(mar=c(1,1,1,1))
  plot(g, vertex.size=as.numeric(V(g)$sumIndex)*4)
}

plotCreatorNetwork <- function( creators, workingDF)
{
  creators <- as.data.frame(creators)
  creators <- subset(creators, !is.na(creators$Name))
  
  workingDF <- as.data.frame(top100GNs2017)
  workingDF <- head(workingDF,500)
  
  cocreators <- sub('[ ,.]+$', '', workingDF$Creators )
  
#  cocreators <- rbind( writers,interiorArtists,coverArtists)
  ## Make edgelist by repeating 1st elements each length(vector)-1L
  adjlist <- strsplit(cocreators, '\\s*,\\s*')
  lens <- lengths(adjlist)
  adjlist[lens==1L] <- lapply(adjlist[lens==1L], rep, times=2)
  
  edgelist <- cbind(
    unlist(lapply(adjlist, tail, -1L)),                        # col1
    rep(sapply(adjlist, `[`, 1L), times=lengths(adjlist)-1L)   # col2
  )
  
  ## make graph
  #g <- graph_from_adj_list(adjlist, mode="all")
  g <- graph_from_edgelist(edgelist, directed=FALSE)
  
  
  A <- get.adjacency(g, sparse = FALSE)
  
  g2 <- network::as.network.matrix(A)
  
  
  V(g)$sumIndex <- as.numeric(creators$sumIndex[match(V(g)$name,creators$Name)])
  
  plot(g)
  
  plot(g, vertex.size=as.numeric(V(g)$sumIndex))
}

populateMatrix <- function(row) {           
  toks <- unlist(strsplit(as.character(row["Creators"]), ",", fixed = TRUE))    
  toks <- trimws(toks, which = "both")
  
  toks <- unique(toks)
  nextToks <- toks
  numToks <- 1
  for( tok in toks )
  {   
    if( tok != "")
    {
      #print( paste0( tok, "-", tok))
      addToMatrix( adjMatrix, tok, tok) 
      if(numToks < length(toks) )
      {
        nextToks <- toks[(1+numToks):length(toks)]
        for( tok2 in nextToks)
        {   
          if( tok2 != "")
          {
          #print( paste0( tok, "--", tok2))
          addToMatrix( adjMatrix, tok, tok2)
          # addToMatrix( adjMatrix, tok2, tok)
          }
        }
      }
    }
    numToks = numToks +1
  }
}

apply(top100GNs2017, 1, populateMatrix)

g <- graph_from_adjacency_matrix(data.matrix(adjMatrix), weighted=TRUE, mode="undirected")
g <- simplify(g, remove.multiple = F, remove.loops = T) 

A <- get.adjacency(g, sparse = FALSE)

g2 <- network::as.network.matrix( adjMatrix, matrix.type="adjacency", mode=undirected) #A

gplot.target(g2, degree(g),  circ.lab = FALSE, # change to TRUE to see legend on concentric blue circles
             circ.col="skyblue", usearrows = FALSE, 
             vertex.col=c("blue", rep("red", 32), "yellow"),
             edge.col="darkgray")
#plotCreatorNetwork( creators, top100GNs2017)