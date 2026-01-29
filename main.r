library(baseballr)
library(dplyr)
library(DBI)
library(RSQLite)





# Initialize database connection
init_database <- function(db_path = "baseball_data.db") {
  conn <- dbConnect(RSQLite::SQLite(), db_path)
  return(conn)
}

# Save 4-year spans to database
save_spans_to_db <- function(spans_df, conn, table_name = "four_year_spans") {
  if (is.null(spans_df) || !is.data.frame(spans_df) || nrow(spans_df) == 0) {
    message("No data to save - dataframe is empty or invalid")
    return(FALSE)
  }
  dbWriteTable(conn, table_name, spans_df, overwrite = TRUE)
  message(paste("Saved", nrow(spans_df), "records to", table_name))
  return(TRUE)
}

# Query from database
query_spans <- function(conn, sql_query) {
  result <- dbGetQuery(conn, sql_query)
  return(result)
}

# Get all 4-year spans for a player from database
get_player_spans <- function(conn, player_name) {
  query <- paste0("SELECT * FROM four_year_spans WHERE player = '", player_name, "'")
  result <- dbGetQuery(conn, query)
  return(result)
}

# Get top performers from database
get_top_performers <- function(conn, stat = "HR", n = 10, order = "DESC") {
  query <- paste0("SELECT * FROM four_year_spans ORDER BY ", stat, " ", order, " LIMIT ", n)
  result <- dbGetQuery(conn, query)
  return(result)
}



# Function to get a player's cumulative stats over a 4-year span
# Uses playerid directly instead of looking up by name
get_4year_cumulative_stats <- function(playerid, player_name, start_year, all_data, stat_type = "batting") {
  
  # Define the 4-year range
  years <- start_year:(start_year + 3)
  
  # Filter the existing data for this player and years
  player_stats <- all_data %>%
    filter(playerid == !!playerid, Season %in% years)
  
  if (nrow(player_stats) == 0) {
    stop("No stats found for this player in the specified years")
  }
  
  # Calculate cumulative stats
  TB <- player_stats$H + (2 * player_stats$`2B`) + (3 * player_stats$`3B`) + (4 * player_stats$HR)
  player_stats$TB <- TB
  
  cumulative <- player_stats %>%
    summarize(
      player = player_name,
      years = paste(min(years), "-", max(years)),
      games = sum(G, na.rm = TRUE),
      AB = sum(AB, na.rm = TRUE),
      H = sum(H, na.rm = TRUE),
      HR = sum(HR, na.rm = TRUE),
      RBI = sum(RBI, na.rm = TRUE),
      R = sum(R, na.rm = TRUE),
      SB = sum(SB, na.rm = TRUE),
      BB = sum(BB, na.rm = TRUE),
      SO = sum(SO, na.rm = TRUE),
      AVG = round(sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE), 3),
      OBP = round(sum(H + BB, na.rm = TRUE) / sum(AB + BB, na.rm = TRUE), 3),
      SLG = round(sum(TB, na.rm = TRUE) / sum(AB, na.rm = TRUE), 3),
      WAR = round(sum(WAR, na.rm = TRUE), 1)
    )
  
  return(cumulative)
}

# Function to create a database of all players and their first/last season
# Also returns the raw data for calculating spans
get_all_players_seasons <- function(start_year = 1980, end_year = 2025) {
  
  message(paste("Fetching all players' season data from", start_year, "to", end_year))
  
  all_seasons_data <- data.frame()
  
  # Fetch data year by year (more reliable)
  for (yr in start_year:end_year) {
    message(paste("Fetching data for", yr))
    
    tryCatch({
      year_data <- fg_batter_leaders(startseason = yr, endseason = yr)
      
      if (!is.null(year_data) && nrow(year_data) > 0) {
        year_data$Season <- yr
        all_seasons_data <- bind_rows(all_seasons_data, year_data)
      }
    }, error = function(e) {
      message(paste("  Could not fetch", yr, "data"))
    })
    
    # Small delay to avoid rate limiting
    Sys.sleep(0.5)
  }
  
  if (nrow(all_seasons_data) == 0) {
    stop("Could not fetch any batter data")
  }
  
  # Process batters
  all_players <- all_seasons_data %>%
    group_by(playerid, PlayerName) %>%
    summarize(
      first_season = min(Season, na.rm = TRUE),
      last_season = max(Season, na.rm = TRUE),
      stat_type = "batting",
      career_length = max(Season, na.rm = TRUE) - min(Season, na.rm = TRUE) + 1,
      .groups = 'drop'
    ) %>%
    filter(career_length >= 4) %>%
    arrange(PlayerName)
  
  message(paste("Successfully created database with", nrow(all_players), "players, all with career length >= 4 years."))
  
  # Return both the player list and the raw data
  return(list(
    players = all_players,
    raw_data = all_seasons_data
  ))
}







# Function to get all 4-year spans using pre-fetched data (much faster!)
# Saves progress every 50 players in case of crash
get_all_4year_spans <- function(player_db, raw_data, conn) {
  
  message("Creating database of all possible 4-year spans...")
  
  all_spans <- data.frame()
  
  for (i in 1:nrow(player_db)) {
    pid <- player_db$playerid[i]
    player_name <- player_db$PlayerName[i]
    first_season <- player_db$first_season[i]
    last_season <- player_db$last_season[i]
    
    message(paste("Processing:", player_name, "(", i, "of", nrow(player_db), ")"))
    
    for (start_yr in first_season:(last_season - 3)) {
      tryCatch({
        result <- get_4year_cumulative_stats(pid, player_name, start_yr, raw_data)
        all_spans <- bind_rows(all_spans, result)
      }, error = function(e) {
        message(paste("  Error for", player_name, start_yr, ":", e$message))
      })
    }
    
    # Save progress every 50 players
    if (i %% 50 == 0 && nrow(all_spans) > 0) {
      message(paste("Saving progress...", nrow(all_spans), "spans so far"))
      save_spans_to_db(all_spans, conn, "four_year_spans")
    }
  }
  
  message(paste("Total spans collected:", nrow(all_spans)))
  
  # Final save to database
  save_spans_to_db(all_spans, conn, "four_year_spans")
  
  return(all_spans)
}


sorted_by <- function(data, stat){
    sorted <- data %>%
        filter(AB>1900) %>%
        arrange(desc(!!sym(stat)))
    print(head(sorted, 20))
    return(sorted)
}





# Initialize database
conn <- init_database("baseball_data.db")


# ### database creation ###
# # Create the player database (returns both players list and raw data)
# player_data <- get_all_players_seasons()

# # Check if data was fetched successfully
# if (is.null(player_data) || is.null(player_data$players) || is.null(player_data$raw_data)) {
#   stop("Failed to fetch player data. Check your internet connection and try again.")
# }

# player_database <- player_data$players
# raw_stats <- player_data$raw_data

# message(paste("Loaded", nrow(player_database), "players and", nrow(raw_stats), "season records"))

# # Generate ALL 4-year spans for ALL players
# all_spans <- get_all_4year_spans(player_database, raw_stats, conn)

# # Show results
# message("Results:")
# print(head(all_spans, 20))

# # Query from database to verify it saved
# message("Verifying database:")
# db_check <- dbGetQuery(conn, "SELECT COUNT(*) as total FROM four_year_spans")
# print(paste("Total records in database:", db_check$total))
# ###


# sorted_by(all_spans, "AVG")

# Close connection
dbDisconnect(conn)



