##### Header #####
# 
# Author:       Matthew H. Grinnell
# Affiliation:  Pacific Biological Station, Fisheries and Oceans Canada (DFO) 
# Group:        Offshore Assessment, Aquatic Resources, Research, and Assessment
# Address:      3190 Hammond Bay Road, Nanaimo, BC, Canada, V9T 6N7
# Contact:      e-mail: matt.grinnell@dfo-mpo.gc.ca | tel: 250.756.7055
# Project:      Herring
# Code name:    Locations.R
# Version:      1.0
# Date started: Jan 08, 2019
# Date edited:  Feb 05, 2019
# 
# Overview: 
# Show herring spawn events within a given distance from a point.
# 
# Requirements: 
# 
# 
# Notes: 
# This is a Shiny web application; run it by clicking the 'Run App' button.
#
# References:
# 

# TODO:
# 1. Include 'incomplete' spawns on these maps -- might require loading and 
#    processing the raw spawn data, not the data from the data summaries

##### Housekeeping #####

# General options
graphics.off( )       # Turn graphics off

# Install missing packages and load required packages (if required)
UsePackages <- function( pkgs, locn="https://cran.rstudio.com/" ) {
  # Reverse the list 
  rPkgs <- rev( pkgs )
  # Identify missing (i.e., not yet installed) packages
  newPkgs <- rPkgs[!(rPkgs %in% installed.packages( )[, "Package"])]
  # Install missing packages if required
  if( length(newPkgs) )  install.packages( newPkgs, repos=locn )
  # Loop over all packages
  for( i in 1:length(rPkgs) ) {
    # Load required packages using 'library'
    eval( parse(text=paste("suppressPackageStartupMessages(library(", rPkgs[i], 
      "))", sep="")) )
  }  # End i loop over package names
}  # End UsePackages function

# Make packages available
UsePackages( pkgs=c("tidyverse", "sp", "rgdal", "raster", "rgeos", 
  "scales", "ggforce", "plyr", "viridis", "shiny", "shinycssloaders", "DT") )

##### Controls ##### 

# Saved csv datafile (from Spawn.R)
spawnLoc <- file.path( "Data", "SpawnRaw.csv" )

# Input coordinate reference system (spill)
crsSpill <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# Input coordinate reference system (herring sections)
crsSect <- "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000
+y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

# Input coordinate reference system (land)
crsLand <- "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000
+y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

# Input coordinate reference system (spawn)
crsSpawn <- "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000
+y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

# Output coordinate reference system (BC Albers)
crsOut <- "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000
+y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

# Geographic projection
geoProj <- "Projection: BC Albers (NAD 1983)"

# Location of the BC stocks shapefiles
locStocks <- list( loc=file.path("Data", "Polygons"), lyr="SectionsIntegrated" )

# Location of the BC land file
locLand <- list( loc=file.path("Data", "Polygons"), lyr="GSHHS_h_L1_Alb" )

# Change default ggplot theme to 'black and white'
theme_set( theme_bw() )

# Modify default theme
myTheme <- theme( 
  legend.box.background=element_rect(fill=alpha("white", 0.7)),
  legend.box.margin=margin(1, 1, 1, 1, "mm"),
  legend.key=element_blank(), legend.margin=margin(), legend.text.align=1,
  panel.grid.major=element_line(colour="darkgrey", size=0.2),
  panel.grid.minor=element_line(colour="darkgrey", size=0.1),
  legend.background=element_rect(fill="transparent"),
  plot.margin=unit(c(0.1, 0.6, 0.1, 0.1), "lines") )

##### Functions #####

# Load helper functions
source( file=file.path( "..", "HerringFunctions", "Functions.R") )

##### Data #####

# Cross-walk table for SAR to region and region name
regions <- read_csv( file=
    "Region, RegionName, Major
  HG, Haida Gwaii, TRUE
  PRD, Prince Rupert District, TRUE
  CC, Central Coast, TRUE
  SoG, Strait of Georgia, TRUE
  WCVI, West Coast of Vancouver Island, TRUE
  A27, Area 27, FALSE
  A2W, Area 2 West, FALSE",
  col_types=cols() )

# Load spawn data, and aggregate by location code
spawn <- read_csv( file=spawnLoc, col_types=cols(), guess_max=10000 ) %>%
  group_by( Year, Region, StatArea, Section, LocationCode ) %>%
  summarise( Eastings=unique(Eastings), Northings=unique(Northings),
    Longitude=unique(Longitude), Latitude=unique(Latitude),
    SpawnIndex=SumNA(c(SurfSI, MacroSI, UnderSI)) ) %>%
  ungroup( ) %>%
  # , !is.na(SpawnIndex)
  filter( !is.na(Eastings), !is.na(Northings) ) %>%
  select( Year, Region, StatArea, Section, LocationCode, Eastings,
    Northings, Longitude, Latitude, SpawnIndex )

# Convert location to Albers
ConvLocation <- function( xy ) {
  # Make a matrix
  xyMat <- matrix( xy, ncol=2 )
  # Convert to spatial points
  xySP <- SpatialPoints( coords=xyMat, proj4string=CRS(crsSpill) )
  # Transform to correct projection
  xySP <- spTransform( x=xySP, CRSobj=CRS(crsOut) )
  # Make a data frame
  xyDF <- data.frame( xySP ) %>%
    rename( Eastings=coords.x1, Northings=coords.x2 )
  # Return the points
  return( list(xySP=xySP, xyDF=xyDF) )
}  # End ConvLocation function

# Load the Section shapefile (has Statistical Areas and Regions)
secPoly <- readOGR( dsn=file.path(locStocks$loc), layer=locStocks$lyr, 
  verbose=FALSE )

# Load land polygon
# TODO: Use ggmap to load a better terrain/satellite map
landPoly <- readOGR( dsn=file.path(locLand$loc), layer=locLand$lyr, 
  verbose=FALSE )

# Function to wrangle shapefiles
ClipPolys <- function( stocks, land, pt, buf ) {
  # Function to perform some light wrangling
  UpdateSections <- function( dat ) {
    # Some light wrangling
    dat@data <- dat@data %>%
      mutate( StatArea=as.character(StatArea), 
        Section=as.character(Section) ) %>%
      select( SAR, StatArea, Section )
    # Get results
    res <- dat
    # Return updated sections
    return( res )
  }  # End UpdateSections function
  # Update sections
  secBC <- UpdateSections( dat=stocks )
  # Project to BC
  secBC <- spTransform( x=secBC, CRSobj=CRS(crsOut) )
  # Get a buffer around the region(s) in question
  buff <- gBuffer( spgeom=pt, width=buf, byid=FALSE )
  # Calculate the extent
  extBuff <- bbox( buff )
  # Convert the extent to a table
  extDF <- tibble( Eastings=extBuff[1, ], Northings=extBuff[2, ] )
  # Determine x:y aspect ration (for plotting)
  xyRatio <- diff(extDF$Eastings) / diff(extDF$Northings)
  # Crop the sections
  secBC <- crop( x=secBC, y=extBuff )
  # Determine section centroids
  secCent <- gCentroid( spgeom=secBC, byid=TRUE )
  # Convert to data frame
  secCentDF <- secCent %>%
    as_tibble( ) %>%
    rename( Eastings=x, Northings=y ) %>%
    mutate( Section=formatC(secBC$Section, width=3, flag="0") ) %>%
    arrange( Section )
  # Convert to data frame and select stat areas in question
  secDF <- secBC %>%
    fortify( region="Section" ) %>%
    rename( Eastings=long, Northings=lat, Section=group ) %>%
    as_tibble( )
  # Transform
  landSPDF <- spTransform( x=land, CRSobj=CRS(crsOut) )
  # Clip the land to the buffer: big
  landSPDF <- crop( x=landSPDF, y=extBuff )
  # Convert to data frame
  landDF <- landSPDF %>%
    fortify( region="id" ) %>%
    rename( Eastings=long, Northings=lat ) %>%
    as_tibble( )
  # Build a list to return
  res <- list( secDF=secDF, secCentDF=secCentDF, landDF=landDF, extDF=extDF, 
    extBuff=extBuff, xyRatio=xyRatio )
  # Return info
  return( res )
}  # End ClipPolys function

# Function to crop (spatially) spawn
CropSpawn <- function( dat, yrs, si, ext, grp ) {
  # Filter spawn index
  dat <- dat %>%
    filter( Year>=yrs[1], Year<=yrs[2], SpawnIndex>=si[1], SpawnIndex<=si[2])
  # Convert to a spatial object
  coordinates( dat ) <- ~ Eastings+Northings
  # Give the projection
  crs( dat ) <- CRS( crsSpawn )
  # Transform
  datSP <- spTransform( x=dat, CRSobj=CRS(crsOut) )
  # Clip to extent
  # TODO: 'crop' generates a warning re "seq.default(along = cand): partial 
  # argument match of 'along' to 'along.with'"
  datSP <- crop( x=datSP, y=ext )
  # Make a data frame
  dat <- data.frame( datSP ) %>%
    as_tibble( )
  # Light wrangling
  dat <- dat %>%
    mutate( Year=as.integer(Year),
      StatArea=formatC(StatArea, width=2, format="d", flag="0"),
      Section=formatC(Section, width=3, format="d", flag="0") ) %>%
    select( Year, Region, StatArea, Section, LocationCode, Eastings, Northings,
      Longitude, Latitude, SpawnIndex ) %>%
    arrange( Year, Region, StatArea, Section, LocationCode )
  # Summarise spawns by location
  if( "loc" %in% grp ) {
    dat <- dat %>%
      group_by( Region, StatArea, Section, LocationCode, Eastings, 
        Northings, Longitude, Latitude ) %>%
      summarise( SpawnIndex=mean(SpawnIndex, na.rm=TRUE), Number=n() ) %>%
      ungroup( ) %>%
      mutate( Number=as.integer(Number) )
  }  # End if summarising by location
  # Return the data
  return( dat )
}  # End CropSpawn function

# Draw a circle
MakeCircle <- function( center=c(0,0), radius=1, nPts=100 ){
  # Vector of points
  tt <- seq( from=0, to=2*pi, length.out=nPts )
  # X values (math!)
  xx <- center[1] + radius * cos(tt)
  # Y values (and geometry!)
  yy <- center[2] + radius * sin(tt)
  # Return the data (x and y for a circle)
  return( tibble(Eastings=xx, Northings=yy) )
}  # End MakeCircle function

# Format the regions table
regTab <- regions %>% 
  rename( 'Region name'=RegionName ) %>%
  mutate( Type=ifelse(Major, "Major", "Minor") ) %>% 
  rename( 'Region code'=Region ) %>%
  select( 'Region name', 'Region code', Type )

##### User interface #####

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel( "Pacific Herring spawn index by year and location -- 
    DRAFT DO NOT USE FOR PLANNING" ),
  
  # Sidebar with input parameters 
  sidebarLayout(
    # Sidebar (input etc)
    sidebarPanel( width=3,
      
      h3( "Event location (decimal degrees)" ),
      # TODO: Allow input in Eastings and Northings?
      bootstrapPage(
        div( style="display:inline-block; width: 40%",
          numericInput(inputId="longitude", label="Longitude", value=-123.96) ),
        div( style="display:inline-block; width: 40%",
          numericInput(inputId="latitude", label="Latitude", value=49.21) )
      ),
      
      h3( "Buffers (kilometres, km)" ),
      bootstrapPage(
        div( style="display:inline-block; width: 40%", 
          numericInput(inputId="bufSpill", label="Circle (radius)", value=10) ),
        div( style="display:inline-block; width: 40%",
          numericInput(inputId="bufMap", label="Distance to map edge", 
            value=12) )
      ),
      
      h3( "Subset spawn index data" ),
      sliderInput( inputId="yrRange", label="Years", min=min(spawn$Year), 
        max=max(spawn$Year), value=range(spawn$Year), sep="" ),
      sliderInput( inputId="siRange", label="Spawn index (tonnes, t)", 
        min=floor(min(spawn$SpawnIndex, na.rm=TRUE)), 
        max=ceiling(max(spawn$SpawnIndex, na.rm=TRUE)), 
        value=range(spawn$SpawnIndex, na.rm=TRUE) ),
      
      h3( "Map features" ),
      bootstrapPage(
        div( style="display:inline-block; vertical-align: text-top",
          checkboxGroupInput(inputId="location", label="Event", 
            choiceNames=c("Point"), choiceValues=c("pt"), selected=c("pt")) ),
        # Add horizontal padding on this 'div' to make white space
        div( style="display:inline-block; vertical-align: text-top;
          padding: 0px 12px",
          checkboxGroupInput(inputId="polys", label="Polygons", 
            choiceNames=c("Sections"), choiceValues=c("sec"), 
            selected=c("sec")) ),
        div( style="display:inline-block; vertical-align: text-top",
          checkboxGroupInput(inputId="summary", label="Summarise spawns", 
            choiceNames=c("By Location"), choiceValues=c("loc")) )
      ),
      
      # h3( "View results" ),
      div( style="text-align: center",
        submitButton("Update", icon("refresh")) )
      
    ),  # End sidebar panel
    
    # Show a plot of the generated distribution
    mainPanel( width=9,
      # Start tabs
      tabsetPanel( type="tabs", selected="Map",
        
        tabPanel( title="Map", br(),
          withSpinner(ui_element=plotOutput(outputId="map", width="100%", 
            height="700px")) ),
        
        tabPanel( title="Data", br(),
          withSpinner(ui_element=DT::dataTableOutput(outputId="dat")) ),
        
        tabPanel( title="Download", br(), style="width: 350pt",
          bootstrapPage(
            div( style="display:inline-block",
              downloadButton(outputId="downloadMap", label="Download map")),
            div( style="display:inline-block",
              downloadButton(outputId="downloadData", label="Download data")))
        ),
        
        tabPanel( title="Regions", br(), style="width: 350pt",
          withSpinner(ui_element=DT::dataTableOutput(outputId="regTab")) ),

        tabPanel( title="About", br(), style="width: 350pt",
          p( HTML("For more information on Pafic Herring spawn data, contact",
            "<a href=mailto:Jaclyn.Cleary@dfo-mpo.gc.ca>Jaclyn Cleary</a>,", 
            "<a href=mailto:Matthew.Grinnell@dfo-mpo.gc.ca>Matthew", 
            "Grinnell</a>, or",
            "<a href=mailto:Matthew.Thompson@dfo-mpo.gc.ca@dfo-mpo.gc.ca>Matt",
            "Thompson</a>, DFO Science, Pacific Biological Station.") ),
          p( HTML("To view the source code and report issues, visit our",
            "<a href=https://github.com/grinnellm/SpawnLocations>GitHub",
            "repository</a>, or contact Matthew Grinnell.")),
          p( HTML("For details on how to calculate the Pacific Herring spawn",
            "index, read the", 
            "<a href=https://github.com/grinnellm/HerringSpawnDocumentation/blob/master/SpawnIndexTechnicalReport.pdf>",
            "draft spawn index technical report</a>.") ),
          br(),
          img( src='HerringDFO.jpg', style="width: 100%" ),
          p( HTML("<font color='grey'>Pacific Herring (<em>Clupea",
            "pallasii</em>). Image credit:",
            "<a href=http://www.pac.dfo-mpo.gc.ca/>Fisheries and Oceans",
            "Canada</a>.</font>") ),
          h3( "Note" ),
          p( HTML("The 'spawn index' represents the raw survey data",
            "only, and is not scaled by the spawn survey scaling parameter",
            "<em>q</em>; therefore it is a relative index of spawning biomass",
            "(<a href=http://www.dfo-mpo.gc.ca/csas-sccs/Publications/SAR-AS
            /2018/2018_002-eng.html>CSAS 2018</a>).") ),
          p( "[Something to reflect that 'incomplete' spawns are not included",
            "here. Incomplete spawns include spawns that were observed but not",
            "quantified/surveyed, and spawns that lack sufficient data to",
            "calculate the spawn index.]")
        )
      )  # End tabs
    )  # End main panel
  )  # End sidebar layout
)  # End ui

##### Server #####

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Get the spill location
  spill <- reactive( 
    ConvLocation( xy=c(input$longitude, input$latitude) )
  )
  
  # Clip stock and land shapefiles
  shapesSub <- reactive(
    ClipPolys( stocks=secPoly, land=landPoly, pt=spill()$xySP, 
      buf=input$bufMap*1000 )
  )
  
  # Make a circle
  circDF <- reactive( 
    MakeCircle( center=coordinates(spill()$xySP), 
      radius=input$bufSpill*1000 )
  )
  
  # Get spawn data
  spawnSub <- reactive( 
    CropSpawn( dat=spawn, yrs=input$yrRange, si=input$siRange,
      ext=shapesSub()$extBuff, grp=input$summary )
  )
  
  # Get the data
  output$dat <- DT::renderDataTable( {
    
    # Ensure there are spawn locations to show
    validate( need(nrow(spawnSub()) >= 1, 
      "Error: no spawns match these criteria.") )
    
    # Light wrangling
    res <- spawnSub() %>%
      mutate( Eastings=Eastings/1000, Northings=Northings/1000,
        LocationCode=as.character(LocationCode) ) %>%
      rename( 'Statistical Area'=StatArea, 'Location code'=LocationCode,
        'Spawn index (t)'=SpawnIndex, 'Eastings (km)'=Eastings,
        'Northings (km)'=Northings )
    
    # If grouping by location
    if( "loc" %in% input$summary ) {
      # Rename and format
      res <- res %>%
        rename( 'Number of spawns'=Number, 
          'Mean spawn index (t)'='Spawn index (t)' ) %>%
        datatable( options=list(lengthMenu=list(c(15, -1), list('15', 'All')), 
          pageLength=15, searching=FALSE, ordering=FALSE) ) %>%
        formatRound( columns=c('Mean spawn index (t)', 'Eastings (km)',
          'Northings (km)', 'Longitude', 'Latitude'), digits=3 )
    } else {  # End if grouping by location, otherwise
      # Format
      res <- res %>%
        datatable( options=list(lengthMenu=list(c(15, -1), list('15', 'All')), 
          pageLength=15, searching=FALSE, ordering=FALSE) ) %>%
        formatRound( columns=c('Spawn index (t)', 'Eastings (km)',
          'Northings (km)', 'Longitude', 'Latitude'), digits=3 )  
    }  # End if not grouping by location
    
    # Return the table
    return( res )
  } )  # End data
  
  # Regions table
  output$regTab <- DT::renderDataTable(
    # Format options for table
    regTab %>%
      datatable( options=list(lengthChange=FALSE, searching=FALSE,
        paging=FALSE, ordering=FALSE, lengthChange=FALSE, autoWidth=FALSE),
        rownames=FALSE,
        caption="[Include some information regarding the difference
        between Major and Minor areas with respect to the spawn index.
        For example, we search more in the Major areas.]" )
  )  # End regions
  
  # Make the map
  output$map <- renderPlot(res=150, {
    
    # Ensure map buffer is larger than spill buffer
    validate( need(input$bufSpill <= input$bufMap, 
      "Error: spill buffer can not exceed map bufer.") )
    
    # If grouping by location
    if( "loc" %in% input$summary ) {
      # Calculate the number of spawns: sum of Number
      nSpawns <- format( sum(spawnSub()$Number), big.mark=",")
    } else {  # End if grouping by location, otherwise
      # Calculate the number of spawns: number of rows
      nSpawns <- format( nrow(spawnSub()), big.mark="," )  
    }  # End if not grouping by location
    
    # Ensure there are spawn locations to show
    validate( need(nrow(spawnSub()) >= 1, 
      "Error: no spawns match these criteria.") )
    
    # Plot the area (default map)
    hMap <- ggplot( data=shapesSub()$landDF, aes(x=Eastings, y=Northings) ) +
      geom_polygon( data=shapesSub()$landDF, aes(group=group), fill="lightgrey" )
    
    # If showing section lines and labels
    if( "sec" %in% input$polys ) {
      hMap <- hMap + 
        geom_path( data=shapesSub()$secDF, aes(group=Section), size=0.25,
          colour="black" ) +
        geom_label( data=shapesSub()$secCentDF, alpha=0.5, aes(label=Section) )
    }  # End if showing sections
    
    # If showing the point location
    if( "pt" %in% input$location ) {
      hMap <- hMap + 
        geom_point( data=spill()$xyDF, colour="red", shape=42, size=8 )
    }  # End if showing the point location
    
    # If aggregating by location
    if( "loc" %in% input$summary ) {
      hMap <- hMap +
        geom_point( data=spawnSub(), aes(colour=SpawnIndex, size=Number),
          alpha=0.5 ) +
        labs( colour="Mean\nspawn\nindex (t)", size="Number\nof spawns" ) +
        scale_size_area( )
    } else {  # End if aggregatign by location, otherwise
      hMap <- hMap +
        geom_point( data=spawnSub(), aes(colour=SpawnIndex), size=4, 
          alpha=0.5 ) +
        labs( colour="Spawn\nindex (t)" )
    }  # End if not aggregating by location
    
    # Add map layers
    hMap <- hMap +
      geom_path( data=circDF(), colour="red", size=0.5 ) +
      scale_colour_viridis( na.value="black", labels=comma ) +
      coord_equal( ) +
      labs( title=paste("Number of Pacific Herring spawns:", nSpawns ), 
        x="Eastings (km)", y="Northings (km)", caption=geoProj ) +
      scale_x_continuous( labels=function(x) comma(x/1000), expand=c(0, 0) ) + 
      scale_y_continuous( labels=function(x) comma(x/1000), expand=c(0, 0) ) +
      myTheme
    
    # Save the map (if download requested)
    output$downloadMap <- downloadHandler( filename="SpawnMap.png",
      content=function(file) ggsave( filename=file, plot=hMap, dpi=600, 
        height=6.5, width=7 ),
      contentType="image/png" )
    
    # Print the map
    return( hMap )
  } )  # End map
  
  # Save data (spawn index; if download requested)
  output$downloadData <- downloadHandler( filename="SpawnData.csv",
    content=function(file) write_csv( x=spawnSub(), path=file ),
    contentType="text/csv" )
  
}  # End server

##### App #####

# Run the application 
shinyApp( ui=ui, server=server )
