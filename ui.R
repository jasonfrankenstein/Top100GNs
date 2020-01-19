# ui.R
# FIT5147 Data Visualization Assessment 4
# Jason Franks
# Monash Student No:28849892

# Uncomment to install required packages:
# install.packages("shinythemes")
# install.packages("shiny")

library(shiny)
library(shinythemes)

publisherList <- scan("publishers.txt", what="", sep="\n")
selectedPublishers <- c( "Marvel", "DC", "Image", "Dark Horse") # Big 4 publishers

# Define UI for random distribution app ----
shinyUI(fluidPage( theme=shinytheme("superhero"), # of course
  
  # App title ----
  titlePanel("Top 100 Monthly Graphic Novels of 2017"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel( width=3, 
      
      # Input: Select the random distribution type ----
      HTML("<p><h2>Filters</h2></p>"),
      radioButtons("titleType", "Title Ownership:",
                   c("All" = "all",
                     "Work For Hire" = "wfh",
                     "Creator Owned" = "creatorOwned")),
      
      # br() element to introduce extra vertical spacing ----
      br(),
      
      # Input: checkboxes for every publisher
      checkboxGroupInput("publishers", "Publishers:",
                         publisherList, selected = selectedPublishers  )
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  
                  tabPanel("Home", 
                           fluidRow( 
                             HTML("<p><h3>Introduction</h3></p>
                                  <p>Graphic novels are long comicbooks bound with a spine. 
                                  They are distributed to comic stores in what is known as the Direct Market
                                  almost exclusively by a single distributor, 
                                  <a href='https://www.diamondcomics.com/' target ='top'>Diamond</a>.
                                  </p>
                                  <p>Diamond's business was built around serialized monthly superhero comics, and their
                                  newer graphic novels trade follows the same model. In this application we will look at
                                  Diamond's top 100 graphic novel charts for 2017.
                                  </p>
                                  <p>These charts rank every item in terms of quantity sold. While they do not provide
                                  exact numbers, they do index the sales quantities on their own peculiar scale:
                                  </p>"
                              )
                           ),
                           fluidRow(
                                     HTML("<p><h4>The Batman Index</h4></p>")
                           ),
                           fluidRow(
                             column(width=5, HTML("<p><center><img src='./batman_logo.png' alt='Batman Logo' style='width: auto;max-height: 100%;'></center></p>")),
                             column(width=7, HTML("<p>According to Diamond, their sales index &quot;presents 
                                                  Diamond's monthly sales for individual titles in relation to 
                                                  total sales for the issue of DC Comics' monthly Batman 
                                                  comic book. (Batman is used as the control title - with a value 
                                                  of 100 - because sales of Batman usually remain relatively stable.)&quot;</p>
                                                  <p>In these visualizations I have used this Batman index to compare performance
                                                  of titles within and between months.</p>"))
                             ),
                           fluidRow(
                             HTML("<p><h3>How to Use This App</h3></p>
                                  <p>Use this app to explore the top 100 monthly graphic novels by <b>Publisher</b>, <b>Title</b>, 
                                  <b>Creative team</b> or individual <b>Creator</b> using the tabs across the top of the page.</p>
                                  <p>You can control the scope of the search by restricting the publishers considered 
                                  or the ownership status (creator-owned, or work-for-hire) using the <b>filter panel</b> on the left.</p>
                                  <p><b>Please be patient. Some of the charts can take a long time to load.</b><p>"
                              )
                           )
                  
                        ),

                        tabPanel("Publishers", 
                                 
                            fluidRow(
                              HTML("<p>Publishers are the vendors who sell a selection of titles to Diamond. They may be large media operations 
                                (eg Marvel is owned by Disney and DC by Time Warner), or they might be small self-publishing operations offering 
                                only a single title.</p>
                                <p>I have measured publisher performance by summing the index across all titles associated with each publisher.</p> 
                                <p><b>Use the radio button to select whether to display this data as a line chart, a bar chart or a Wind Rose diagram.</b></p>"
                              )
                            ),
                                 
                           fluidRow( 
                              radioButtons("barType", "Show:",
                                          c("Wind Rose" = "polar",
                                            "Line Chart" = "line",
                                            "Bar Chart" = "bar"
                                            ),
                                          inline=TRUE
                              ),
                              plotOutput("publishersChart")
                           ),
                           fluidRow(
                             HTML("<p>This chart shows publisher performance across the entire year.</p>" ),
                             plotOutput("publishersOverall")
                           )
                   ),
                  tabPanel("Books", 
                           fluidRow(
                             HTML("<p>Books are the actual items shipped to stores. Here Iwe see the performance of the top 20 books,
                                  once again measured by summing the index across all titles over the entire year.</p>"
                            )
                           ),
                           fluidRow( 
                              plotOutput("titleSales")
                           ), 
                           fluidRow(
                             HTML("<p>Many titles have multiple volumes, each of which has its own item code. (Eg Saga comes in 8 volumes, each with the same title but a different code). 
                                  These are the titles with the highest number of items ordered through the year:</p>"
                              ),
                             
                                plotOutput("titleVolumes")
                             ),
                           fluidRow(
                            HTML( "<p>Because of this, titles can appear multiple times in the charts, within a month or between months.
                                  Titles can also appear mutliple times if they are reordered across multiple months.</p>
                                  <p><b>Use the radio buttons to view the most frequently ordered books aggregated by title, by index (ie
                                  by the title or the item code.</b></p>
                                  <p></b>Notice when you view by item code you may see the same title appear multiple times.</b></p>"
                            ),
                            radioButtons("titleFreqBy", "Show Frequency by:", c("Item" = "item", "Title" = "title"), inline=TRUE),
                            plotOutput("titleFrequencyBreakdown")
                          )
                  ),
                  tabPanel("Creative Teams",
                          
                           fluidRow( 
                             HTML( "<p>A typical creative team for a graphic novel may have half a dozen or more specialized
                                   creators working on it: letterers, colorists, inkers, book designers, editors, etc. However, 
                                   the Diamond listings only list the writer, the interior artist, and the cover artist, so I have restricted 
                                   these charts to just those three disciplines.</p>
                                   <p>NOTE: Each discipline may be pracitised by a team, or a single creator may work across multiple disciplines on
                                   given title. ie there may be a team of writers or artists, or just one person n each role--or one person in all roles.</p>
                                   <p>Performance is measured by summing the index across all titles associated with each creative team for all
                                   books across every month.</p> 
                                   <p><b>Choose a discipline from the dropdown in order to see its top 10 creators, or choose 'all' to see the top 10 
                                   complete creative teams.</b></p>"
                             )
                           ),
                           fluidRow( 
                             selectInput("teamDiscipline", "Discipline:", c("All" = "all", "Writers" = "Writer", "Interior Artists" = "InteriorArtist", 
                                                                                            "Cover Artists" = "CoverArtist")),
                             plotOutput("teamSales") 
                           )
                  ),
                  tabPanel("Creators",
                           fluidRow( 
                             HTML( "<p>I have measured the performance of individual creators by summing the index across all titles they 
                                    worked on in 2017. Creators are only counted once for each book, regardless of how many roles (disciplines) 
                                    they filled on a particular project, or how many other creators were on the team with them.</p>
                                    <p>NOTE: Writer-artists are creators who filled the role of writer and the role of interior artist and/or 
                                    cover artist on at least one title published during the year. Interior artists did cover art remain 
                                    classified only as interior artists.</p>
                                    <p><b>Choose a discipline from the dropdown, or select all, to view the top creators.</b></p>"
                             )
                           ),
                           fluidRow( 
                              selectInput("creatorDiscipline", "Discipline:", c("All" = "Creator", "Writers" = "Writer", "Interior Artists" = "Interior Artist", "Cover Artists" = "Cover Artist", "Writer Artists" = "Writer Artist")
                             ),
                             plotOutput("creatorSales")
                          )
                  )
        )
    )
  )
)
)


