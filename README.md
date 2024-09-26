# STAT440 Final Project: Spotify Insights

## Overview

This project is a web-based Shiny application developed as part of the STAT440 course. The application provides insights into a user's Spotify data, including visualizations of their top artists and tracks. Users can log in with their Spotify credentials to generate personalized visualizations based on their listening habits.

## Features

- **Login with Spotify**: Secure login via the Spotify API to access personalized data.
- **Top Artists Visualization**: Displays a visual representation of the user's most-listened-to artists.
- **Top Tracks Visualization**: Provides insights into the user's most frequently played songs.
- **Interactive Dashboard**: An intuitive and interactive interface built using Shiny and shinydashboard.

## Installation

To run this Shiny app locally, you'll need to have R and the following R packages installed:

```r
install.packages(c("tidyverse", "ggplot2", "readr", "shinydashboard", "shiny", "shinyjs", "httr", "base64enc", "jsonlite"))
```

## Usage

1. Clone the repository or download the project files.

2. Open the `stat440finalproject.R` file in RStudio or any R environment.

3. Run the application by executing:

```r
shiny::runApp('path_to_stat440finalproject.R')
```
4. The app will open in your browser. Use the "Login with Spotify" button to authenticate and generate your personalized Spotify insights.

## Project Structure

- `stat440finalproject.R`: Main Shiny app script containing UI and server logic.
