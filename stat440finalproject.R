library(tidyverse)
library(ggplot2)
library(readr)

library(shinydashboard)
library(shiny)
library(shinyjs)
library(httr)
library(base64enc)
library(jsonlite)

options(shiny.host = '127.0.0.1')
options(shiny.port = 5183)


# Assuming you have loaded your data into the 'data' variable
jsCode <- "
      shinyjs.redirect = function(url) {
        window.location.href = url;
      }
    "
ui <- dashboardPage(
  dashboardHeader(title = "Spotify Insights"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Application Description", tabName = "description"),
      menuItem("Data Visualization: Top Artists", tabName = "visual01"),
      menuItem("Data Visualization: Top Tracks", tabName = "visual02"))
  ),
  dashboardBody(
    useShinyjs(),
    extendShinyjs(text = jsCode, functions = c("redirect")),
    tabItems(
      tabItem(tabName = "description", 
              h1("STAT440 Final Project: Spotify Insights"),
              actionButton("login", "Login with Spotify", style = "background-color: #4CAF50"),
              h3("Description:"),
              h4("This application makes use of Spotify’s Web API. By signing in to your Spotify account and granting permissions, this application is able to retrieve information about the artists and tracks that you listen to the most. It then uses this retrieved information to draw insights about your favorite genres and artists."),
              h3("Procedure"),
              h4("1. Click on the green Login with Spotify Button that is above the Description section"),
              h4("2. Login to your Spotify account and grant the requested permissions"),
              h4("3. You should be redirected back to this page after signing in. You can then start to look at insights about your listening habits."),
              h3("Raw Data for Your Top Artists (Medium Term)"),
              fluidPage(box(dataTableOutput("rawartists"), width = "100%")),
              h3("Raw Data for Your Top Tracks (Medium Term)"),
              fluidPage(box(dataTableOutput("rawtracks"), width = "100%"))
      ),
      # Second tab content
      tabItem(tabName = "visual01",
              h2("Data Visualization: Top Artists"),
              fluidRow(
                box(title = "Visualization", plotOutput("plot01")),
                box(title = "Controls",
                    # Time Frame
                    radioButtons(inputId = "selecttimeframe01",
                                 label = "Choose Your Listening Time Frame",
                                 choices = list("Long Term" = "long_term",
                                                "Medium Term" = "medium_term",
                                                "Short Term" = "short_term"),
                                 selected = "medium_term"),
                    selectInput("plotType01",
                                label = "Choose Plot Type",
                                choices = list("Frequency" = "Count",
                                               "Average Popularity" = "Average_Popularity"),
                                selected = "Count"),
                    actionButton("runBottom01", "Generate Visualization")
                ),
                box(title = "Summary Table", tableOutput("table01"))
              )),
      # Third tab content
      tabItem(tabName = "visual02",
              h2("Data Visualization: Top Tracks"),
              fluidRow(
                box(title = "Visualization", plotOutput("plot02")),
                box(title = "Controls",
                    # Time Frame
                    radioButtons(inputId = "selecttimeframe02",
                                 label = "Choose Your Listening Time Frame",
                                 choices = list("Long Term" = "long_term",
                                                "Medium Term" = "medium_term",
                                                "Short Term" = "short_term"),
                                 selected = "medium_term"),
                    # Artist
                    selectInput("plotType02",
                                label = "Choose Plot Type",
                                choices = list("Frequency" = "Count",
                                               "Average Popularity of Songs" = "Average_Popularity_of_Songs"),
                                selected = "Count"),
                    actionButton("runBottom02", "Generate Visualization")
                ),
                box(title = "Summary Table", tableOutput("table02"))
              )
      )
    )
  )
)

populateArtistTable <- function(artists) {
  num_of_artists <- length(artists)
  db_topArtists <- data.frame(artist_name = character(), 
                              popularity = numeric(), 
                              genre = character())
  
  for (artist in artists) {
    artist_name <- artist$name
    popularity <- artist$popularity
    genre <- artist$genre
    if (length(genre) == 0) {
      new_row = data.frame(artist_name = artist_name, popularity = popularity, genre = 'No Genre')
      db_topArtists = rbind(db_topArtists, new_row)
    } else {
      for (currGenre in genre) {
        new_row = data.frame(artist_name = artist_name, popularity = popularity, genre = currGenre)
        db_topArtists = rbind(db_topArtists, new_row)
      }
    }
  }
  return(db_topArtists)
}

populateTracksTable <- function(tracks) {
  num_of_tracks <- length(tracks)
  db_topTracks <- data.frame(track_name = character(),
                             artist_name = character(),
                             track_popularity = numeric())
  
  for (track in tracks) {
    track_name <- track$name
    track_popularity <- track$popularity
    track_artists <- track$artists
    for (track_artist in track_artists) {
      new_row = data.frame(track_name = track_name, artist_name = track_artist$name, track_popularity = track_popularity)
      db_topTracks = rbind(db_topTracks, new_row)
    }
  }
  return(db_topTracks)
}

getAccessToken <- function(code) {
  clientID <- "3f5aca96acf844409e6f20e95222163f"
  clientSecret <- "b53ae0ba369c459c8a7218de7351f156"
  
  clientSecret64 <- base64encode(charToRaw(paste0(clientID, ":", clientSecret)))
  data <- list(grant_type = "authorization_code", code = code, redirect_uri = 'http://127.0.0.1:5183')
  
  response1 <- POST(
    url = 'https://accounts.spotify.com/api/token',
    add_headers(Authorization = paste0("Basic ", clientSecret64)),
    body = data,
    encode = "form"
  )
  response_content <- content(response1, as = "text")
  response_json <- fromJSON(response_content)
  
  access_token <- response_json$access_token
  return(access_token )
}

server <- function(input, output, session) {
  clientID <- "3f5aca96acf844409e6f20e95222163f"
  clientSecret <- "b53ae0ba369c459c8a7218de7351f156"
  shared_data <- reactiveValues(access_token = "null")
  
  # Get the Access Token to Spotify API by Exchanging the Authorization Code
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['code']])) {
      code <- query[['code']]
      shared_data$access_token = getAccessToken(code)
      
      # Populate the Artists Table
      response2 <- GET(
        url = 'https://api.spotify.com/v1/me/top/artists?time_range=short_term&limit=20&offset=0',
        add_headers(Authorization = paste("Bearer", shared_data$access_token))
      )
      artists <- content(response2, as = "parsed")$items
      db_topArtists <- populateArtistTable(artists)
      
      output$rawartists <- renderDataTable({
        db_topArtists
      })
      
      db <- db_topArtists %>% 
        group_by(genre) %>% 
        summarise(`Count_of_Genre` = n())
      db <- db %>% slice_max(`Count_of_Genre`, n = 5, with_ties = FALSE)
      
      # Data Visualization for Genre: Output default plot and Table(medium_term, Count)
      output$plot01 <- renderPlot({
        # Data for count
        par(mar = c(11, 4, 2, 1))
        barplot(db$Count_of_Genre, col = "#4CAF50",
                main = "Your Favorite Genres Based on your Favorite Artists", xlab = "", ylab = "Frequency",
                names.arg = db$genre, las = 2)
        mtext("Genre", side = 1, line = 9, font = 2)
      })
      
      output$table01 <- renderTable({
        # Data for count
        db
      })
      
      # Populate the Tracks Table
      response3 <- GET(
        url = 'https://api.spotify.com/v1/me/top/tracks?time_range=short_term&limit=20&offset=0',
        add_headers(Authorization = paste("Bearer", shared_data$access_token))
      )
      
      tracks <- content(response3, as = "parsed")$items
      db_topTracks <- populateTracksTable(tracks)
      
      output$rawtracks <- renderDataTable({
        db_topTracks
      })
      
      db1 <- db_topTracks %>% 
        group_by(artist_name) %>% 
        summarise(`Artist Count` = n())
      db1 <- db1 %>% slice_max(`Artist Count`, n = 5, with_ties = FALSE)
      
      # Data Visualization for Genre: Output default plot and Table(medium_term, Count)
      output$plot02 <- renderPlot({
        # Data for count
        par(mar = c(11, 4, 2, 1))
        barplot(db1$`Artist Count`, col = "#4CAF50",
                main = "Your Favorite Artists Based on Your Top Tracks", xlab = "", ylab = "Frequency",
                names.arg = db1$artist_name, las = 2)
        mtext("Artist", side = 1, line = 9, font = 2)
      })
      
      output$table02 <- renderTable({
        # Data for count
        db1
      })
    }
  })
  
  # Observe the Event the User Logs in to Spotify and Gets the Authorization Code
  observeEvent(input$login, {
    buttonClicked(TRUE)
    
    # Redirect when the button is clicked
    scope <- "playlist-read-private playlist-read-collaborative user-read-private user-read-email user-top-read"
    redirect_uri <- "http://127.0.0.1:5183"
    authorization_url <- paste0(
      "https://accounts.spotify.com/authorize?",
      "response_type=code",
      "&client_id=", URLencode(clientID),
      "&scope=", URLencode(scope),
      "&redirect_uri=", URLencode(redirect_uri)
    )
    
    js$redirect(authorization_url)
  })
  
  # Observe the User Chooses Options for Plot01
  observeEvent(input$runBottom01, {
    plot01_type <- input$plotType01
    plot01_time_frame <- input$selecttimeframe01
    
    url = paste0('https://api.spotify.com/v1/me/top/artists?time_range=', plot01_time_frame, '&limit=20&offset=0')
    
    response <- GET(
      url = url,
      add_headers(Authorization = paste("Bearer", shared_data$access_token))
    )
    artists <- content(response, as = "parsed")$items
    db_topArtists <- populateArtistTable(artists)
    
    if (plot01_type == 'Count') {
      db <- db_topArtists %>% 
        group_by(genre) %>% 
        summarise(`Count_of_Genre` = n())
      db <- db %>% slice_max(`Count_of_Genre`, n = 5, with_ties = FALSE)
      
      output$plot01 <- renderPlot({
        # Data for count
        par(mar = c(11, 4, 2, 1))
        barplot(db$Count_of_Genre, col = "#4CAF50",
                main = "Your Favorite Genres Based on your Favorite Artists", xlab = "", ylab = "Frequency",
                names.arg = db$genre, las = 2)
        mtext("Genre", side = 1, line = 9, font = 2)
      })
      
      output$table01 <- renderTable({
        # Data for count
        db
      })
    } else if (plot01_type == 'Average_Popularity') {
      db <- db_topArtists %>% 
        group_by(genre) %>% 
        summarise(`Avg_Popularity` = mean(popularity)) %>% 
        arrange(desc(Avg_Popularity)) %>% 
        head(3)
      
      output$plot01 <- renderPlot({
        plot(x = 1:10, y = 1:10, xlab = "*Number is the Popularity Score for Each Genre", ylab = "", type = "n", axes = FALSE, main = "Top 3 Most Popular Genres that you Listen to")
        text(x = c(2.3,5.5,8.5), y = c(6,6,6),
             labels = paste0(round(db$Avg_Popularity)),cex = 4)
        text(x = c(2.3,5.5,8.5), y = c(8,8,8), labels = c('Top1', 'Top2', 'Top3'), cex = 2)
        text(x = c(2.5,5.5,8.5), y = c(4,4,4), labels = paste0(db$genre), cex = 1)
      })
      
      output$table01 <- renderTable({
        # Data for count
        db
      })
    }
  })
  
  observeEvent(input$runBottom02, {
    plot02_type <- input$plotType02
    plot02_time_frame <- input$selecttimeframe02
    url = paste0('https://api.spotify.com/v1/me/top/tracks?time_range=', plot02_time_frame, '&limit=20&offset=0')
    response <- GET(
      url = url,
      add_headers(Authorization = paste("Bearer", shared_data$access_token))
    )
    
    tracks <- content(response, as = "parsed")$items
    db_topTracks <- populateTracksTable(tracks)
    
    if (plot02_type == "Count") {
      db <- db_topTracks %>% 
        group_by(artist_name) %>% 
        summarise(`Artist Count` = n())
      db <- db %>% slice_max(`Artist Count`, n = 5, with_ties = FALSE)
      output$plot02 <- renderPlot({
        # Data for count
        par(mar = c(11, 4, 2, 1))
        barplot(db$`Artist Count`, col = "#4CAF50",
                main = "Your Favorite Artists Based on Your Top Tracks", xlab = "", ylab = "Frequency",
                names.arg = db$artist_name, las = 2)
        mtext("Artist", side = 1, line = 9, font = 2)
        output$table02 <- renderTable({
          # Data for count
          db
        })
      })
    } else if (plot02_type == "Average_Popularity_of_Songs") {
      db <- db_topTracks %>% 
        group_by(artist_name) %>% 
        summarise(`Avg_Popularity_of_Songs` = mean(track_popularity)) %>% 
        arrange(desc(Avg_Popularity_of_Songs)) %>% 
        head(3)
      
      output$plot02 <- renderPlot({
        plot(x = 1:10, y = 1:10, xlab = "*Number is the Popularity Score for Each Artist", ylab = "", type = "n", axes = FALSE, main = "Top 3 Artists with the Most Popular Songs")
        text(x = c(2.3,5.5,8.5), y = c(6,6,6),
             labels = paste0(round(db$Avg_Popularity_of_Songs)),cex = 4)
        text(x = c(2.3,5.5,8.5), y = c(8,8,8), labels = c('Top1', 'Top2', 'Top3'), cex = 2)
        text(x = c(2.5,5.5,8.5), y = c(4,4,4), labels = paste0(db$artist_name), cex = 1)
      })
      
      output$table02 <- renderTable({
        # Data for count
        db
      })
    }
  })
  

  buttonClicked <- reactiveVal(FALSE)
}

shinyApp(ui = ui, server = server)
