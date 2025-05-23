library(shiny)
library(DT)
library(plotly)
library(leaflet)
library(dplyr)
library(tidyr)
library(shinydashboard)
library(scales)
library(viridis)
library(ggridges)
library(cowplot)
library(AzureStor)

if (!require("randomcoloR")) install.packages("randomcoloR")
library(randomcoloR)

use_azure <- TRUE  # Set to FALSE to use local files

if (use_azure) {
  endpoint <- storage_endpoint(
    "https://defaultresourcegrou89d2.blob.core.windows.net/",
    key = Sys.getenv("AZURE_STORAGE_KEY")
  )
  container <- storage_container(endpoint, "app-package-florida-consumer-func-2025-116ee8e")
}

fbs_colors <- c(
  "#E41A1C", # red
  "#377EB8", # blue
  "#4DAF4A", # green
  "#984EA3", # purple
  "#FF7F00", # orange
  "#FFFF33", # yellow
  "#A65628", # brown
  "#F781BF", # pink
  "#999999", # gray
  "#1B9E77", # teal
  "#D95F02", # dark orange
  "#7570B3", # indigo
  "#E7298A", # magenta
  "#66A61E", # olive
  "#E6AB02", # gold
  "#A6761D"  # dark brown
)

shinyServer(function(input, output, session) {

  # Define load_csv function FIRST
  load_csv <- function(filename, use_azure = FALSE) {
    if (use_azure) {
      tmp <- tempfile(fileext = ".csv")
      storage_download(container, filename, dest = tmp, overwrite = TRUE)
      read.csv(tmp)
    } else {
      read.csv(file.path("data", filename))
    }
  }

  # THEN use it to load your data
  teams <- load_csv("teams_geocoded.csv", use_azure = use_azure)
  games <- load_csv("games.csv", use_azure = use_azure)
  recruiting <- load_csv("recruiting.csv", use_azure = use_azure)
  rankings <- load_csv("rankings.csv", use_azure = use_azure)
  sp <- load_csv("sp_ratings.csv", use_azure = use_azure)
  talent <- load_csv("talent_data.csv", use_azure = use_azure)
  
  # Ensure talent has conference info
  if (!"conference" %in% colnames(talent)) {
    # teams should have columns: school, conference
    teams_conf <- teams %>% select(school, conference)
    talent <- talent %>%
      left_join(teams_conf, by = c("team" = "school"))
  }

  # Calculate winner for each game
  games <- games %>%
    mutate(
      winner = case_when(
        homePoints > awayPoints ~ homeTeam,
        awayPoints > homePoints ~ awayTeam,
        TRUE ~ NA_character_
      )
    )

  # Team wins per year
  team_wins <- games %>%
    filter(!is.na(winner)) %>%
    group_by(year, winner) %>%
    summarise(wins = n(), .groups = "drop")

  # Points scored and allowed per team per year
  team_points <- games %>%
    select(year, homeTeam, awayTeam, homePoints, awayPoints, homeConference, awayConference) %>%
    mutate(
      homePointsAllowed = awayPoints,
      awayPointsAllowed = homePoints
    ) %>%
    pivot_longer(
      cols = c(homeTeam, awayTeam),
      names_to = "location",
      values_to = "team"
    ) %>%
    mutate(
      points_scored = ifelse(location == "homeTeam", homePoints, awayPoints),
      points_allowed = ifelse(location == "homeTeam", awayPoints, homePoints),
      conference = ifelse(location == "homeTeam", homeConference, awayConference)
    ) %>%
    group_by(year, team, conference) %>%
    summarise(
      total_points_scored = sum(points_scored, na.rm = TRUE),
      total_points_allowed = sum(points_allowed, na.rm = TRUE),
      .groups = "drop"
    )

  # Conference-level aggregation
  conference_points <- team_points %>%
    group_by(year, conference) %>%
    summarise(
      total_points_scored = sum(total_points_scored, na.rm = TRUE),
      total_points_allowed = sum(total_points_allowed, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Overview tab
  output$overviewBar <- renderPlotly({
    req(input$year)
    data <- team_points %>% filter(year == input$year)
    if (input$conference != "All") {
      data <- data %>% filter(conference == input$conference)
    }
    data <- data %>%
      arrange(desc(total_points_scored)) %>%
      head(5)
    print(data)
    plot_ly(
      data = data,
      y = ~reorder(team, total_points_scored),
      x = ~total_points_scored,
      type = 'bar',
      orientation = 'h',
      name = 'Points Scored',
      marker = list(color = 'steelblue')
    ) %>%
      add_trace(
        x = ~total_points_allowed,
        name = 'Points Allowed',
        marker = list(color = 'firebrick')
      ) %>%
      layout(
        barmode = 'stack',
        xaxis = list(title = 'Points'),
        yaxis = list(title = ''),
        title = 'Top 5 Teams by Points Scored'
      )
  })
  output$summaryTable <- renderDT({
    req(input$year)
    filtered_games <- games %>% filter(year == input$year)
    if (input$conference != "All") {
      filtered_games <- filtered_games %>%
        filter(homeConference == input$conference | awayConference == input$conference)
    }
    datatable(
      team_wins %>%
        filter(year == input$year) %>%
        arrange(desc(wins)),
      options = list(pageLength = 10),
      caption = paste("Teams by Wins in", input$year)
    )
  })
  
  # Teams tab
  output$teamsTable <- renderDT({
    datatable(teams)
  })
  
  # Games tab
  output$gamesTable <- renderDT({
    datatable(games)
  })
  output$gamesPlot <- renderPlotly({
    plot_ly(data = games, x = ~year, y = ~homePoints, type = "box", color = ~homeConference)
  })
  
  # Recruiting tab
  output$recruitingTable <- renderDT({
    datatable(recruiting)
  })
  output$recruitingPlot <- renderPlotly({
    plot_ly(data = recruiting, x = ~year, y = ~stars, type = "box", color = ~school)
  })
  
  # Rankings tab
  output$rankingsTable <- renderDT({
    datatable(rankings)
  })
  output$rankingsPlot <- renderPlotly({
    plot_ly(data = rankings, x = ~year, y = ~rank, type = "box", color = ~poll)
  })
  
  # SP+ tab
  output$spTable <- renderDT({
    datatable(sp)
  })
  output$spPlot <- renderPlotly({
    plot_ly(data = sp, x = ~year, y = ~rating, type = "box", color = ~conference)
  })
  
  # Talent tab
  output$talentTable <- renderDT({
    datatable(talent)
  })
  output$talentPlot <- renderPlotly({
    plot_ly(data = talent, x = ~year, y = ~talent, type = "box", color = ~team)
  })
  
  # Map tab (example: team locations)
  
  # Filter teams for valid lat/lon and conference
  teams_map <- teams %>%
    filter(!is.na(latitude), !is.na(longitude), !is.na(conference))
  
  # Get all unique FBS conferences in your data
  fbs_conferences <- sort(unique(teams$conference))

  # Generate as many distinct colors as needed
  fbs_colors <- randomcoloR::distinctColorPalette(length(fbs_conferences))

  # Create the color factor palette
  pal <- colorFactor(viridis::viridis(length(unique(teams$conference))), teams$conference)
  
  # Add jitter to overlapping points
  teams_map <- teams_map %>%
    group_by(latitude, longitude) %>%
    mutate(
      n = n(),
      latitude_jit = ifelse(n > 1, jitter(latitude, amount = 0.05), latitude),
      longitude_jit = ifelse(n > 1, jitter(longitude, amount = 0.05), longitude)
    ) %>%
    ungroup()
  
  output$mapPlot <- renderLeaflet({
    # Start with all teams with valid lat/lon and conference
    teams_map <- teams %>%
      filter(!is.na(latitude), !is.na(longitude), !is.na(conference))
    
    # Filter by conference (if not "All")
    if (!is.null(input$conference) && input$conference != "All") {
      teams_map <- teams_map %>% filter(conference == input$conference)
    }
    
    # Filter by elevation slider
    if (!is.null(input$map_elevation)) {
      teams_map <- teams_map %>%
        filter(
          !is.na(elevation),
          elevation >= input$map_elevation[1],
          elevation <= input$map_elevation[2]
        )
    }
    
    # Filter by capacity slider
    if (!is.null(input$map_capacity)) {
      teams_map <- teams_map %>%
        filter(
          !is.na(capacity),
          capacity >= input$map_capacity[1],
          capacity <= input$map_capacity[2]
        )
    }
    
    # Filter by win percentage slider (requires calculation)
    if (!is.null(input$map_winrate)) {
      # Calculate win percentage per team for the selected year
      win_pct_df <- games %>%
        filter(year == input$year) %>%
        select(homeTeam, awayTeam, homePoints, awayPoints) %>%
        mutate(
          home_win = homePoints > awayPoints,
          away_win = awayPoints > homePoints
        ) %>%
        pivot_longer(cols = c(homeTeam, awayTeam), names_to = "location", values_to = "team") %>%
        mutate(win = ifelse(location == "homeTeam", home_win, away_win)) %>%
        group_by(team) %>%
        summarise(
          games_played = n(),
          wins = sum(win, na.rm = TRUE),
          win_pct = ifelse(games_played > 0, wins / games_played, NA_real_),
          .groups = "drop"
        )
      teams_map <- teams_map %>%
        left_join(win_pct_df, by = c("school" = "team")) %>%
        filter(!is.na(win_pct), win_pct * 100 >= input$map_winrate)
    }
    
    # Use jittered columns if available
    lat_col <- if ("latitude_jit" %in% names(teams_map)) "latitude_jit" else "latitude"
    lon_col <- if ("longitude_jit" %in% names(teams_map)) "longitude_jit" else "longitude"
    teams_map <- teams_map %>%
      rename(lat = !!lat_col, lon = !!lon_col)
    
    # If no teams after filtering, show empty map
    if (nrow(teams_map) == 0) {
      return(leaflet() %>% addTiles())
    }
    
    leaflet(teams_map) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~lon,
        lat = ~lat,
        popup = ~school,
        color = ~pal(conference),
        radius = 6,
        fillOpacity = 0.8
      )
  })

  # Pan/zoom to fit all teams in selected conference
  observe({
    req(input$conference)
    teams_map <- teams %>%
      filter(!is.na(latitude), !is.na(longitude), !is.na(conference))
    if (input$conference != "All") {
      teams_map <- teams_map %>% filter(conference == input$conference)
    }
    if (nrow(teams_map) > 0) {
      leafletProxy("mapPlot", session) %>%
        fitBounds(
          lng1 = min(teams_map$longitude, na.rm = TRUE),
          lat1 = min(teams_map$latitude, na.rm = TRUE),
          lng2 = max(teams_map$longitude, na.rm = TRUE),
          lat2 = max(teams_map$latitude, na.rm = TRUE)
        )
    }
  })

  # Trend plot
  output$trendPlot <- renderPlotly({
    # Example: Average points scored per year by conference
    avg_points <- conference_points %>%
      group_by(year, conference) %>%
      summarise(avg_scored = mean(total_points_scored), .groups = "drop")
    plot_ly(
      data = avg_points,
      x = ~year,
      y = ~avg_scored,
      color = ~conference,
      type = "scatter",
      mode = "lines+markers"
    ) %>%
      layout(
        title = "Average Points Scored by Conference Over Time",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Avg Points Scored")
      )
  })

  output$mostWinsBox <- renderValueBox({
    filtered_games <- games %>% filter(year == input$year)
    if (input$conference != "All") {
      filtered_games <- filtered_games %>%
        filter(homeConference == input$conference | awayConference == input$conference)
    }
    winners <- filtered_games %>%
      mutate(
        winner = case_when(
          homePoints > awayPoints ~ homeTeam,
          awayPoints > homePoints ~ awayTeam,
          TRUE ~ NA_character_
        )
      ) %>%
      filter(!is.na(winner))
    top_team <- winners %>%
      group_by(winner) %>%
      summarise(wins = n(), .groups = "drop") %>%
      arrange(desc(wins)) %>%
      slice(1)
    valueBox(
      value = top_team$wins,
      subtitle = paste("Most Wins:", top_team$winner),
      icon = icon("trophy"),
      color = "yellow"
    )
  })

  output$mostPointsBox <- renderValueBox({
    req(input$year)
    filtered_games <- games %>% filter(year == input$year)
    if (input$conference != "All") {
      filtered_games <- filtered_games %>%
        filter(homeConference == input$conference | awayConference == input$conference)
    }
    top_points <- team_points %>%
      filter(year == input$year) %>%
      arrange(desc(total_points_scored)) %>%
      slice(1)
    valueBox(
      value = top_points$total_points_scored,
      subtitle = paste("Most Points Scored:", top_points$team),
      icon = icon("football-ball"),
      color = "green"
    )
  })

  # Dynamically update conference dropdown based on games data
  observe({
    conf_choices <- c("All", sort(unique(c(games$homeConference, games$awayConference))))
    updateSelectInput(session, "conference", choices = conf_choices, selected = "All")
    updateSelectInput(session, "conference2", choices = conf_choices, selected = "All")
  })

  output$topWinPctBox <- renderValueBox({
    req(input$year)
    filtered_games <- games %>% filter(year == input$year)
    if (input$conference != "All") {
      filtered_games <- filtered_games %>%
        filter(homeConference == input$conference | awayConference == input$conference)
    }
    winners <- filtered_games %>%
      mutate(
        winner = case_when(
          homePoints > awayPoints ~ homeTeam,
          awayPoints > homePoints ~ awayTeam,
          TRUE ~ NA_character_
        )
      ) %>%
      filter(!is.na(winner))
    win_pct <- winners %>%
      group_by(winner) %>%
      summarise(wins = n()) %>%
      left_join(
        filtered_games %>%
          pivot_longer(cols = c(homeTeam, awayTeam), values_to = "team") %>%
          group_by(team) %>%
          summarise(games_played = n()),
        by = c("winner" = "team")
      ) %>%
      mutate(win_pct = wins / games_played) %>%
      arrange(desc(win_pct)) %>%
      slice(1)
    if (nrow(win_pct) == 0 || is.na(win_pct$win_pct)) {
      valueBox(
        value = "N/A",
        subtitle = "Top Win %: N/A",
        icon = icon("percent"),
        color = "purple"
      )
    } else {
      valueBox(
        value = scales::percent(win_pct$win_pct, accuracy = 0.1),
        subtitle = paste("Top Win %:", win_pct$winner),
        icon = icon("percent"),
        color = "purple"
      )
    }
  })

  output$topMarginBox <- renderValueBox({
    req(input$year)
    filtered_games <- games %>% filter(year == input$year)
    if (input$conference != "All") {
      filtered_games <- filtered_games %>%
        filter(homeConference == input$conference | awayConference == input$conference)
    }
    all_margins <- bind_rows(
      filtered_games %>%
        transmute(team = homeTeam, margin = homePoints - awayPoints),
      filtered_games %>%
        transmute(team = awayTeam, margin = awayPoints - homePoints)
    )
    margin <- all_margins %>%
      group_by(team) %>%
      summarise(avg_margin = mean(margin, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(avg_margin)) %>%
      slice(1)
    valueBox(
      value = round(margin$avg_margin, 1),
      subtitle = paste("Top Avg Point Differential:", margin$team),
      icon = icon("plus-minus"),
      color = "orange"
    )
  })

  output$noTeamsMsg <- renderUI({
    req(input$year)
    teams_in_year <- games %>%
      filter(year == input$year) %>%
      select(homeTeam, awayTeam) %>%
      unlist() %>%
      unique() %>%
      as.character()
    teams_map <- teams %>%
      filter(school %in% teams_in_year, !is.na(latitude), !is.na(longitude), !is.na(conference))
    if (!is.null(input$conference) && input$conference != "All") {
      teams_map <- teams_map %>% filter(conference == input$conference)
    }
    if (nrow(teams_map) == 0) {
      tags$div(
        style = "color: red; font-weight: bold; font-size: 1.5em; margin-top: 20px; margin-bottom: 20px; text-align: center;",
        "No location data available in selected year."
      )
    }
  })

  teams$conference <- as.character(teams$conference)

  observe({
    school_choices <- sort(unique(teams$school))
    updateSelectInput(session, "school1", choices = school_choices)
    updateSelectInput(session, "school2", choices = school_choices)
  })

  observeEvent(input$compareSchools, {
    req(input$school1, input$school2)
    school1_data <- teams %>% filter(school == input$school1)
    school2_data <- teams %>% filter(school == input$school2)
    showModal(modalDialog(
      title = paste("Compare:", input$school1, "vs", input$school2),
      fluidRow(
        column(6, h4(input$school1), verbatimTextOutput("school1Info")),
        column(6, h4(input$school2), verbatimTextOutput("school2Info"))
      ),
      easyClose = TRUE
    ))
    output$school1Info <- renderPrint({ school1_data })
    output$school2Info <- renderPrint({ school2_data })
  })

  # New code for the new UI elements
  observe({
    conf_choices <- c(sort(unique(c(games$homeConference, games$awayConference))))
    updateSelectInput(session, "conference_compare1", choices = conf_choices)
    updateSelectInput(session, "conference_compare2", choices = c("None", conf_choices))
  })

  observeEvent(input$compareConferences, {
    req(input$conference_compare1, input$conference_compare2)
    conf1 <- input$conference_compare1
    conf2 <- input$conference_compare2
    year <- input$year

    # Aggregate metrics for each conference and year
    sp_summary <- sp %>%
      filter(year == year) %>%
      group_by(conference) %>%
      summarise(
        offense = mean(offense.rating, na.rm = TRUE),
        defense = mean(defense.rating, na.rm = TRUE),
        specialTeams = mean(specialTeams.rating, na.rm = TRUE)
      )
    talent_summary <- talent %>%
      filter(year == year) %>%
      group_by(conference) %>%
      summarise(talent = mean(talent, na.rm = TRUE))

    # Join all summaries
    radar_data <- sp_summary %>%
      left_join(talent_summary, by = "conference")

    # Get values for each conference
    conf1_row <- radar_data %>% filter(conference == conf1)
    conf2_row <- if (conf2 != "None") radar_data %>% filter(conference == conf2) else NULL

    # Check for missing data
    if (nrow(conf1_row) == 0 || (conf2 != "None" && nrow(conf2_row) == 0)) {
      showModal(modalDialog(
        title = "Data Not Available",
        "One or both conferences do not have data for the selected year.",
        easyClose = TRUE
      ))
      return()
    }

    categories <- c("offense", "defense", "specialTeams", "talent")
    conf1_values <- as.numeric(conf1_row[1, categories])
    conf2_values <- if (!is.null(conf2_row) && nrow(conf2_row) > 0) as.numeric(conf2_row[1, categories]) else NULL

    if (any(is.na(conf1_values)) || (!is.null(conf2_values) && any(is.na(conf2_values)))) {
      showModal(modalDialog(
        title = "Missing Data",
        "One or more metrics are missing for the selected conference(s).",
        easyClose = TRUE
      ))
      return()
    }

    showModal(modalDialog(
      title = if (conf2 != "None") paste("Compare:", conf1, "vs", conf2) else paste("Conference:", conf1),
      plotlyOutput("radarPlot"),
      easyClose = TRUE
    ))

    output$radarPlot <- renderPlotly({
      p <- plot_ly(
        type = 'scatterpolar',
        r = conf1_values,
        theta = categories,
        fill = 'toself',
        name = conf1
      )
      if (!is.null(conf2_values)) {
        p <- p %>% add_trace(
          r = conf2_values,
          theta = categories,
          fill = 'toself',
          name = conf2
        )
      }
      p %>% layout(
        polar = list(radialaxis = list(visible = TRUE)),
        showlegend = TRUE
      )
    })
  })

  categories <- c("offense", "defense", "specialTeams", "talent")
  conf1_values <- c(80, 70, 60, 90)
  conf2_values <- c(75, 65, 70, 80)

  fig <- plot_ly(
    type = 'scatterpolar',
    r = conf1_values,
    theta = categories,
    fill = 'toself',
    name = 'Conference 1'
  ) %>%
    add_trace(
      r = conf2_values,
      theta = categories,
      fill = 'toself',
      name = 'Conference 2'
    ) %>%
    layout(
      polar = list(
        radialaxis = list(
          visible = TRUE,
          range = c(0, 100)
        )
      ),
      showlegend = TRUE
    )
  fig

  # Add a reactive for the selected stat
  stat_choices <- c(
    "Talent" = "talent",
    "Offense Rating" = "offense.rating",
    "Defense Rating" = "defense.rating",
    "Capacity" = "capacity",
    "Elevation" = "elevation",
    "Timezone" = "timezone"
  )
  
  output$teamsStatPlot <- renderPlotly({
    req(input$year)
    req(input$stat_choice)
    teams_year <- teams %>% filter(!is.na(conference))
    talent_year <- talent %>% filter(year == input$year)
    sp_year <- sp %>% filter(year == input$year)
    data <- teams_year %>%
      left_join(talent_year %>% select(team, talent), by = c("school" = "team")) %>%
      left_join(sp_year %>% select(team, conference, offense.rating, defense.rating), by = c("school" = "team", "conference" = "conference"))
    if (input$conference != "All") {
      data <- data %>% filter(conference == input$conference)
    }
    stat_col <- input$stat_choice
    # Numeric stats: ridgeline plot
    if (stat_col %in% c("talent", "offense.rating", "defense.rating", "capacity", "elevation")) {
      data <- data %>% filter(!is.na(.data[[stat_col]]))
      p <- ggplot(data, aes(x = .data[[stat_col]], y = conference, fill = conference)) +
        ggridges::geom_density_ridges(alpha = 0.7, scale = 1.2, rel_min_height = 0.01, na.rm = TRUE) +
        labs(
          x = names(stat_choices)[stat_choices == stat_col],
          y = "Conference",
          title = paste("Distribution of", names(stat_choices)[stat_choices == stat_col], "by Conference")
        ) +
        theme_minimal() +
        theme(legend.position = "none")
      # Add altitude threshold for elevation
      if (stat_col == "elevation") {
        p <- p + geom_vline(xintercept = 1500, linetype = "dashed", color = "red") +
          annotate("text", x = 1500, y = Inf, label = "High Altitude (1500m)", vjust = -0.5, hjust = 0, color = "red", size = 3)
      }
      ggplotly(p)
    } else if (stat_col == "timezone") {
      # Categorical: bar plot
      data <- data %>% filter(!is.na(timezone))
      p <- ggplot(data, aes(x = timezone, fill = conference)) +
        geom_bar(position = "dodge") +
        labs(
          x = "Timezone",
          y = "Number of Teams",
          title = "Teams by Timezone and Conference"
        ) +
        theme_minimal()
      ggplotly(p)
    } else {
      plotly_empty()
    }
  })
  
  # Update stat choices in UI
  observe({
    updateSelectInput(session, "stat_choice", choices = stat_choices, selected = "capacity")
  })

  # Enhanced Stadium Capacity Distribution
  output$capacityHist <- renderPlotly({
    data <- teams
    if (input$conference != "All") {
      data <- data %>% filter(conference == input$conference)
    }
    data <- data %>% filter(!is.na(capacity))
    mean_cap <- mean(data$capacity, na.rm = TRUE)
    median_cap <- median(data$capacity, na.rm = TRUE)
    adv_cap <- 80000

    # Identify stadiums above the crowd noise threshold
    big_stadiums <- data %>% filter(capacity >= adv_cap)

    # Get histogram data for y positioning
    hist_data <- ggplot_build(
      ggplot(data, aes(x = capacity)) +
        geom_histogram(binwidth = 5000)
    )$data[[1]]
    y_max <- max(hist_data$count, na.rm = TRUE)

    p <- ggplot(data, aes(x = capacity)) +
      geom_histogram(binwidth = 5000, fill = 'steelblue', color = 'white', alpha = 0.8) +
      geom_vline(xintercept = median_cap, color = 'red', linetype = 'dashed', size = 1) +
      geom_vline(xintercept = mean_cap, color = 'orange', linetype = 'dashed', size = 1) +
      geom_vline(xintercept = adv_cap, color = 'purple', linetype = 'dashed', size = 1) +
      geom_boxplot(aes(y = -0.1), width = 0.1, fill = 'gray', outlier.color = 'black', outlier.size = 2, alpha = 0.7, orientation = 'x') +
      # Highlight big stadiums with hoverable points
      geom_point(data = big_stadiums, aes(x = capacity, y = 0, text = school), color = "purple", size = 3, alpha = 0.8, inherit.aes = FALSE) +
      labs(title = "Stadium Capacity Distribution", x = "Capacity (Number of People)", y = "Count") +
      theme_minimal() +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
    plt <- ggplotly(p, tooltip = c("x", "text"))
    plt <- plt %>%
      add_annotations(x = median_cap, y = y_max, text = "Median", showarrow = FALSE, font = list(color = "red", size = 12), yshift = 10) %>%
      add_annotations(x = mean_cap, y = y_max, text = "Mean", showarrow = FALSE, font = list(color = "orange", size = 12), yshift = 30) %>%
      add_annotations(x = adv_cap, y = y_max, text = "Crowd Noise Advantage", showarrow = FALSE, font = list(color = "purple", size = 12), yshift = 50)
    plt
  })

  # Enhanced Stadium Elevation Distribution
  output$elevationHist <- renderPlotly({
    data <- teams
    if (input$conference != "All") {
      data <- data %>% filter(conference == input$conference)
    }
    data <- data %>% filter(!is.na(elevation))
    high_alt <- 1219
    mean_elev <- mean(data$elevation, na.rm = TRUE)
    median_elev <- median(data$elevation, na.rm = TRUE)

    # Identify stadiums above the high altitude threshold
    high_stadiums <- data %>% filter(elevation >= high_alt)

    # Get histogram data for y positioning
    hist_data <- ggplot_build(
      ggplot(data, aes(x = elevation)) +
        geom_histogram(binwidth = 250)
    )$data[[1]]
    y_max <- max(hist_data$count, na.rm = TRUE)

    p <- ggplot(data, aes(x = elevation)) +
      geom_histogram(binwidth = 250, fill = 'darkgreen', color = 'white', alpha = 0.8) +
      geom_vline(xintercept = high_alt, color = 'red', linetype = 'dashed', size = 1) +
      geom_vline(xintercept = mean_elev, color = 'orange', linetype = 'dashed', size = 1) +
      geom_vline(xintercept = median_elev, color = 'red', linetype = 'dashed', size = 1) +
      geom_boxplot(aes(y = -0.1), width = 0.1, fill = 'gray', outlier.color = 'black', outlier.size = 2, alpha = 0.7, orientation = 'x') +
      # Highlight high altitude stadiums with hoverable points
      geom_point(data = high_stadiums, aes(x = elevation, y = 0, text = school), color = "red", size = 3, alpha = 0.8, inherit.aes = FALSE) +
      labs(title = "Stadium Elevation Distribution", x = "Elevation (meters)", y = "Count") +
      theme_minimal() +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
    plt <- ggplotly(p, tooltip = c("x", "text"))
    plt <- plt %>%
      add_annotations(x = high_alt, y = y_max, text = "High Altitude Competitive Advantage", showarrow = FALSE, font = list(color = "red", size = 12), yshift = 10) %>%
      add_annotations(x = mean_elev, y = y_max, text = "Mean", showarrow = FALSE, font = list(color = "orange", size = 12), yshift = 30) %>%
      add_annotations(x = median_elev, y = y_max, text = "Median", showarrow = FALSE, font = list(color = "red", size = 12), yshift = 50)
    plt
  })

  # Enhanced Teams by Time Zone with Win Percentage Overlay
  output$timezoneBar <- renderPlotly({
    data <- teams
    if (input$conference != "All") {
      data <- data %>% filter(conference == input$conference)
    }
    data <- data %>% filter(!is.na(timezone))

    # Join time zone info to games for both home and away teams
    tz_lookup <- teams %>% select(school, timezone)
    games_tz <- games %>%
      left_join(tz_lookup, by = c("homeTeam" = "school")) %>%
      rename(homeTimezone = timezone) %>%
      left_join(tz_lookup, by = c("awayTeam" = "school")) %>%
      rename(awayTimezone = timezone)

    # Only keep games where time zones are different
    cross_tz_games <- games_tz %>%
      filter(!is.na(homeTimezone), !is.na(awayTimezone), homeTimezone != awayTimezone)

    # For each team, calculate win percentage in cross-time-zone games
    # Home team
    home_cross <- cross_tz_games %>%
      mutate(team = homeTeam, opp = awayTeam, teamTimezone = homeTimezone, oppTimezone = awayTimezone, win = homePoints > awayPoints) %>%
      select(team, teamTimezone, win)
    # Away team
    away_cross <- cross_tz_games %>%
      mutate(team = awayTeam, opp = homeTeam, teamTimezone = awayTimezone, oppTimezone = homeTimezone, win = awayPoints > homePoints) %>%
      select(team, teamTimezone, win)
    # Combine
    cross_games_long <- bind_rows(home_cross, away_cross)

    # Calculate win pct for each team in cross-time-zone games
    team_cross_win_pct <- cross_games_long %>%
      group_by(team, teamTimezone) %>%
      summarise(
        games_played = n(),
        wins = sum(win, na.rm = TRUE),
        win_pct = ifelse(games_played > 0, wins / games_played, NA_real_),
        .groups = "drop"
      )

    # Join win_pct to teams data
    data <- data %>% left_join(team_cross_win_pct, by = c("school" = "team", "timezone" = "teamTimezone"))

    # Average win percentage by timezone (cross-time-zone games only)
    tz_win_pct <- data %>%
      group_by(timezone) %>%
      summarise(avg_win_pct = mean(win_pct, na.rm = TRUE), n = n(), .groups = "drop")

    # Bar chart for team counts
    p <- ggplot(data, aes(x = timezone, fill = timezone)) +
      geom_bar() +
      scale_fill_viridis_d() +
      labs(title = "Teams by Time Zone (with Cross-TZ Avg Win %)", x = "Time Zone", y = "Number of Teams") +
      theme_minimal() +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
            axis.text.x = element_blank(), axis.ticks.x = element_blank())

    # Overlay line for average win percentage (cross-time-zone games only)
    p <- p +
      geom_line(data = tz_win_pct, aes(x = timezone, y = avg_win_pct * max(table(data$timezone)), group = 1), color = "red", size = 1, inherit.aes = FALSE) +
      geom_point(data = tz_win_pct, aes(x = timezone, y = avg_win_pct * max(table(data$timezone)), text = paste0("Cross-TZ Avg Win %: ", scales::percent(avg_win_pct, accuracy = 0.1))), color = "red", size = 2, inherit.aes = FALSE)

    # Use plotly and add secondary y-axis for win percentage
    plt <- ggplotly(p, tooltip = c("x", "y", "text"))
    plt <- plt %>% layout(
      yaxis2 = list(overlaying = "y", side = "right", title = "Cross-TZ Avg Win %", tickformat = ".0%", showgrid = FALSE),
      title = list(text = "Teams by Time Zone (Bar) and Cross-TZ Avg Win % (Line)")
    )
    plt
  })

  # Home vs Neutral vs Away Win Rates (Horizontal Bar Chart)
  output$homeWinPctPlot <- renderPlotly({
    req(input$year)
    games_filtered <- games %>% filter(year == input$year)
    if (input$conference != "All") {
      games_filtered <- games_filtered %>%
        filter(homeConference == input$conference | awayConference == input$conference)
    }
    # Home win
    home <- games_filtered %>%
      mutate(win = homePoints > awayPoints) %>%
      summarise(win_pct = mean(win, na.rm = TRUE), n = n()) %>%
      mutate(type = "Home")
    # Away win
    away <- games_filtered %>%
      mutate(win = awayPoints > homePoints) %>%
      summarise(win_pct = mean(win, na.rm = TRUE), n = n()) %>%
      mutate(type = "Away")
    # Neutral site win (treat as home team at neutral site)
    neutral <- games_filtered %>%
      filter(neutralSite) %>%
      mutate(win = homePoints > awayPoints) %>%
      summarise(win_pct = mean(win, na.rm = TRUE), n = n()) %>%
      mutate(type = "Neutral Site (Home)")
    win_df <- bind_rows(home, away, neutral)
    plot_ly(
      data = win_df,
      y = ~type,
      x = ~win_pct,
      type = 'bar',
      orientation = 'h',
      text = ~scales::percent(win_pct, accuracy = 0.1),
      textposition = 'auto',
      marker = list(color = c('steelblue', 'firebrick', 'gray'))
    ) %>%
      layout(
        xaxis = list(title = "Win Percentage", tickformat = ".0%"),
        yaxis = list(title = ""),
        title = "Win Rates: Home vs Away vs Neutral"
      )
  })

  # Completion Rate Over Time (reactive to conference)
  output$completionRatePlot <- renderPlotly({
    req(input$conference)
    games_filtered <- games
    if (input$conference != "All") {
      games_filtered <- games_filtered %>%
        filter(homeConference == input$conference | awayConference == input$conference)
    }
    completion_by_year <- games_filtered %>%
      group_by(year) %>%
      summarise(
        total_games = n(),
        completed_games = sum(completed, na.rm = TRUE),
        completion_rate = ifelse(total_games > 0, completed_games / total_games, NA_real_),
        .groups = "drop"
      )
    plot_ly(
      data = completion_by_year,
      x = ~year,
      y = ~completion_rate,
      type = 'scatter',
      mode = 'lines+markers',
      text = ~scales::percent(completion_rate, accuracy = 0.1),
      textposition = 'top middle',
      line = list(color = 'darkgreen', width = 3),
      marker = list(size = 8, color = 'darkgreen')
    ) %>%
      layout(
        yaxis = list(title = "Completion Rate", tickformat = ".0%"),
        xaxis = list(title = "Year"),
        title = "Completion Rate Over Time"
      )
  })
})