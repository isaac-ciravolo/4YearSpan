# =============================================================================
# 4-Year Span Analysis
# Analyzes MLB player performance over 4-year spans using FanGraphs data
# =============================================================================

# --- Load Required Packages ---
library(baseballr)
library(dplyr)
library(DBI)
library(RSQLite)
library(ggplot2)
library(gridExtra)

# --- Configuration ---
# Create output directories for plots and pictures
OUTPUT_DIR <- "pictures_and_plots"
PLAYER_SPAN_DIR <- file.path(OUTPUT_DIR, "player_span_plots")
LEADERBOARD_DIR <- file.path(OUTPUT_DIR, "leaderboards")
if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR)
}
if (!dir.exists(PLAYER_SPAN_DIR)) {
  dir.create(PLAYER_SPAN_DIR)
}
if (!dir.exists(LEADERBOARD_DIR)) {
  dir.create(LEADERBOARD_DIR)
}

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
  if (stat_type == "batting") {
    TB <- player_stats$`1B` + (2 * player_stats$`2B`) + (3 * player_stats$`3B`) + (4 * player_stats$HR)
    player_stats$TB <- TB
    BIP <- player_stats$AB - player_stats$SO - player_stats$HR + player_stats$SF
    player_stats$BIP <- BIP

    # calculate cumulative stats
    cumulative <- player_stats %>%
      summarize(
        player = player_name,
        years = paste(min(years), "-", max(years)),
        games = sum(G, na.rm = TRUE),
        AB = sum(AB, na.rm = TRUE),
        PA = sum(AB, na.rm = TRUE) + sum(BB, na.rm = TRUE) + sum(HBP, na.rm = TRUE) + sum(SF, na.rm = TRUE),
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
        OPS = round(sum(H + BB, na.rm = TRUE) / sum(AB + BB, na.rm = TRUE) + sum(TB, na.rm = TRUE) / sum(AB, na.rm = TRUE), 3),
        WAR = round(sum(WAR, na.rm = TRUE), 1),
        # wOBA calculated from summed totals using 2024 linear weights
        # Formula: (0.69*uBB + 0.72*HBP + 0.88*1B + 1.24*2B + 1.57*3B + 2.01*HR) / (AB + BB - IBB + SF + HBP)
        WOBA = round(
          (0.69 * sum(BB - IBB, na.rm = TRUE) +
            0.72 * sum(HBP, na.rm = TRUE) +
            0.88 * sum(`1B`, na.rm = TRUE) +
            1.24 * sum(`2B`, na.rm = TRUE) +
            1.57 * sum(`3B`, na.rm = TRUE) +
            2.01 * sum(HR, na.rm = TRUE)) /
            (sum(AB, na.rm = TRUE) + sum(BB, na.rm = TRUE) - sum(IBB, na.rm = TRUE) + sum(SF, na.rm = TRUE) + sum(HBP, na.rm = TRUE)), 3
        ),
        BARREL_PCT = if (start_year >= 2016) round((sum(Barrels, na.rm = TRUE) / sum(BIP, na.rm = TRUE)), 3) else NA,
        WPA = if (start_year >= 1980) round(sum(WPA, na.rm = TRUE), 2) else NA,
        CLUTCH = if (start_year >= 1980) round(sum(Clutch, na.rm = TRUE), 2) else NA,
        BB_K = round(sum(BB, na.rm = TRUE) / sum(SO, na.rm = TRUE), 3),
        BSR = round(sum(BaseRunning, na.rm = TRUE), 1),
        DEF = round(sum(Defense, na.rm = TRUE), 1)
      )
  }
}

# Function to create a database of all players and their first/last season
# Also returns the raw data for calculating spans
get_all_players_seasons <- function(start_year = 1980, end_year = 2025) {
  message(paste("Fetching all players' season data from", start_year, "to", end_year))

  all_seasons_data <- data.frame()

  # Fetch data year by year (more reliable)
  for (yr in start_year:end_year) {
    message(paste("Fetching data for", yr))

    tryCatch(
      {
        year_data <- fg_batter_leaders(startseason = yr, endseason = yr)

        if (!is.null(year_data) && nrow(year_data) > 0) {
          year_data$Season <- yr
          all_seasons_data <- bind_rows(all_seasons_data, year_data)
        }
      },
      error = function(e) {
        message(paste("  Could not fetch", yr, "data"))
      }
    )

    # Small delay to avoid rate limiting
    Sys.sleep(0.2)
  }

  if (nrow(all_seasons_data) == 0) {
    stop("Could not fetch any batter data")
  }

  # Filter out pitchers by excluding players with low PA per season
  all_seasons_data <- all_seasons_data %>%
    filter(PA >= 50) # Exclude pitchers and players with minimal playing time

  # Process batters
  all_players <- all_seasons_data %>%
    group_by(playerid, PlayerName) %>%
    summarize(
      first_season = min(Season, na.rm = TRUE),
      last_season = max(Season, na.rm = TRUE),
      stat_type = "batting",
      career_length = max(Season, na.rm = TRUE) - min(Season, na.rm = TRUE) + 1,
      .groups = "drop"
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
      tryCatch(
        {
          result <- get_4year_cumulative_stats(pid, player_name, start_yr, raw_data)
          all_spans <- bind_rows(all_spans, result)
        },
        error = function(e) {
          message(paste("  Error for", player_name, start_yr, ":", e$message))
        }
      )
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




# --- Analysis Functions ---

# Leaderboard function - creates a clean ranked table and saves as PNG
leaderboard <- function(stat = "WAR", n = 10, save_png = TRUE) {
  # Create WAR rankings for reference when stat != WAR
  war_ranked <- all_spans %>%
    filter(PA > 1900) %>%
    arrange(desc(WAR)) %>%
    mutate(war_rank = row_number()) %>%
    distinct(player, years, .keep_all = TRUE) %>%
    select(player, years, war_rank)

  leaders <- all_spans %>%
    filter(PA > 1900) %>%
    arrange(desc(!!sym(stat))) %>%
    head(n) %>%
    mutate(Rank = row_number()) %>%
    left_join(war_ranked, by = c("player", "years"))

  # Select columns based on whether stat is WAR or not
  if (stat == "WAR") {
    leaders <- leaders %>%
      select(Rank, Player = player, Years = years, !!sym(stat), AB, PA, OPS, HR, WAR, AVG, BSR, DEF, WPA)
  } else {
    leaders <- leaders %>%
      select(Rank, WAR_Rank = war_rank, Player = player, Years = years, !!sym(stat), AB, PA, OPS, HR, WAR, AVG, BSR, DEF, WPA)
  }

  print(leaders, row.names = FALSE)

  if (save_png) {
    # Create a table plot
    library(gridExtra)

    p <- tableGrob(leaders,
      rows = NULL,
      theme = ttheme_minimal(
        core = list(fg_params = list(fontsize = 10)),
        colhead = list(fg_params = list(fontsize = 11, fontface = "bold"))
      )
    )

    filename <- file.path(LEADERBOARD_DIR, paste0("leaderboard_", stat, "_top", n, ".png"))
    ggsave(filename, p, width = 10, height = 0.5 + n * 0.4, dpi = 150)
    message(paste("Saved to", filename))
  }

  return(invisible(leaders))
}

# Get top 10 unique players by WAR (each player's best span only)
top_10_unique <- function(stat = "WAR", n = 10, save_png = TRUE) {
  # First, rank all spans by the stat to get overall leaderboard rank
  ranked_all <- all_spans %>%
    arrange(desc(!!sym(stat))) %>%
    mutate(overall_rank = row_number())

  # Also create WAR rankings for reference when stat != WAR
  war_ranked <- all_spans %>%
    arrange(desc(WAR)) %>%
    mutate(war_rank = row_number()) %>%
    distinct(player, years, .keep_all = TRUE) %>%
    select(player, years, war_rank)

  # Join WAR rank to the main data
  ranked_all <- ranked_all %>%
    left_join(war_ranked, by = c("player", "years"))

  # Get each player's best span and their overall rank
  leaders <- ranked_all %>%
    group_by(player) %>%
    slice_max(!!sym(stat), n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    arrange(desc(!!sym(stat))) %>%
    mutate(unique_rank = row_number()) %>%
    head(n)

  # Select columns based on whether stat is WAR or not
  if (stat == "WAR") {
    leaders <- leaders %>%
      select(Rank = unique_rank, Overall_Rank = overall_rank, Player = player, Years = years, !!sym(stat), AB, PA, OPS, HR, WAR, AVG, BSR, DEF, WPA)
  } else {
    leaders <- leaders %>%
      select(Rank = unique_rank, Overall_Rank = overall_rank, WAR_Rank = war_rank, Player = player, Years = years, !!sym(stat), AB, PA, OPS, HR, WAR, AVG, BSR, DEF, WPA)
  }

  print(leaders, row.names = FALSE)

  if (save_png) {
    library(gridExtra)

    p <- tableGrob(leaders,
      rows = NULL,
      theme = ttheme_minimal(
        core = list(fg_params = list(fontsize = 10)),
        colhead = list(fg_params = list(fontsize = 11, fontface = "bold"))
      )
    )

    filename <- file.path(LEADERBOARD_DIR, paste0("leaderboard_unique_", stat, "_top", n, ".png"))
    ggsave(filename, p, width = 12, height = 0.5 + n * 0.4, dpi = 150)
    message(paste("Saved to", filename))
  }

  return(invisible(leaders))
}


# --- Query Functions ---

check_player <- function(name_pattern) {
  all_spans %>%
    filter(grepl(name_pattern, player, ignore.case = TRUE)) %>%
    arrange(desc(WAR)) %>%
    head(10)
}

# Get a player's best 4-year span by WAR
best_span <- function(player_name) {
  # Create ranked leaderboard by WAR
  ranked_spans <- all_spans %>%
    arrange(desc(WAR)) %>%
    mutate(war_rank = row_number())

  result <- ranked_spans %>%
    filter(grepl(player_name, player, ignore.case = TRUE)) %>%
    arrange(desc(WAR)) %>%
    slice(1)

  if (nrow(result) == 0) {
    message(paste("No player found matching:", player_name))
    return(NULL)
  }

  return(result)
}




# --- Visualization Functions ---

# plot function
plot_top_10 <- function(stat) {
  top_10 <- all_spans %>%
    filter(AB > 1900) %>%
    arrange(desc(!!sym(stat))) %>%
    head(10) %>%
    mutate(label = paste(player, years, sep = "\n"))

  ggplot(top_10, aes(x = reorder(label, !!sym(stat)), y = !!sym(stat), fill = !!sym(stat))) +
    geom_bar(stat = "identity") +
    coord_flip() +
    scale_fill_gradient(low = "steelblue", high = "darkred") +
    labs(
      title = paste("Top 10 Four-Year Spans by", stat),
      x = NULL,
      y = stat
    ) +
    theme_minimal() +
    theme(legend.position = "none")
}

# Player span percentile chart - works for any player
plot_player_span <- function(player_name, span_years = NULL, stat = "WAR", save_png = TRUE) {
  # Filter to the player
  player_spans <- all_spans %>%
    filter(grepl(player_name, player, ignore.case = TRUE), PA > 1900)

  if (nrow(player_spans) == 0) {
    stop(paste("No player found matching:", player_name))
  }

  # If specific years provided, filter to those
  if (!is.null(span_years)) {
    selected_span <- player_spans %>%
      filter(years == span_years)

    if (nrow(selected_span) == 0) {
      stop(paste("No span found for", player_name, "in years", span_years))
    }
  } else {
    # Otherwise use their best span by the specified stat
    selected_span <- player_spans %>%
      arrange(desc(!!sym(stat))) %>%
      slice(1)
  }

  # Get the rank of this span in the overall leaderboard
  rank <- all_spans %>%
    filter(PA > 1900) %>%
    arrange(desc(!!sym(stat))) %>%
    mutate(rank = row_number()) %>%
    filter(player == selected_span$player, years == selected_span$years) %>%
    pull(rank)

  if (nrow(selected_span) == 0) {
    stop("No player found")
  }

  # Calculate percentiles for all players, then filter to selected
  span_stats <- all_spans %>%
    filter(PA > 1900) %>%
    mutate(
      WAR_pct = percent_rank(WAR),
      OPS_pct = percent_rank(OPS),
      AVG_pct = percent_rank(AVG),
      HR_pct = percent_rank(HR),
      RBI_pct = percent_rank(RBI),
      SB_pct = percent_rank(SB),
      DEF_pct = percent_rank(DEF),
      BSR_pct = percent_rank(BSR),
      WPA_pct = percent_rank(WPA)
    ) %>%
    filter(player == selected_span$player, years == selected_span$years)

  # Reshape for plotting
  stats_long <- data.frame(
    stat = c("WAR", "OPS", "AVG", "HR", "RBI", "SB", "DEF", "BSR", "WPA"),
    percentile = c(
      span_stats$WAR_pct, span_stats$OPS_pct, span_stats$AVG_pct,
      span_stats$HR_pct, span_stats$RBI_pct, span_stats$SB_pct,
      span_stats$DEF_pct, span_stats$BSR_pct, span_stats$WPA_pct
    ) * 100
  )

  # Calculate total of all percentile ranks
  total_pct <- sum(stats_long$percentile)
  pct_per_war <- round(total_pct / selected_span$WAR, 1)

  p <- ggplot(stats_long, aes(x = reorder(stat, percentile), y = percentile)) +
    geom_segment(aes(xend = stat, y = 0, yend = percentile), color = "gray70", linewidth = 1) +
    geom_point(aes(color = percentile), size = 9) +
    geom_text(aes(label = round(percentile, 0)), color = "white", size = 2.5, fontface = "bold") +
    coord_flip() +
    scale_color_gradient2(low = "steelblue", mid = "gray70", high = "darkred", midpoint = 50, limits = c(0, 100)) +
    geom_hline(yintercept = 50, linetype = "dashed", color = "gray50") +
    labs(
      title = paste0(
        "#", rank, " by ", stat, ": ",
        selected_span$player, " (", selected_span$years, ")"
      ),
      subtitle = paste(
        "WAR:", selected_span$WAR, "| OPS:",
        selected_span$OPS, "| HR:", selected_span$HR,
        "| Total Pct:", round(total_pct, 0),
        "| Pct/WAR:", pct_per_war
      ),
      x = NULL,
      y = "Percentile Rank (vs all 4-year spans)"
    ) +
    theme_minimal() +
    theme(legend.position = "none") +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25))

  if (save_png) {
    # Create filename from player name and years (remove spaces and special characters)
    player_name_clean <- gsub("[^A-Za-z0-9]", "_", selected_span$player)
    years_clean <- gsub("[^0-9]", "_", selected_span$years)
    filename <- file.path(PLAYER_SPAN_DIR, paste0("player_span_", player_name_clean, "_", years_clean, "_", stat, ".png"))
    ggsave(filename, p, width = 8, height = 6, dpi = 150)
    message(paste("Saved to", filename))
  }

  print(p)
  return(invisible(p))
}


# --- Startup ---
# Initialize database
conn <- init_database("baseball_data.db")

# Load data directly from database (data already exists!)
all_spans <- dbGetQuery(conn, "SELECT * FROM four_year_spans")
message(paste("Loaded", nrow(all_spans), "spans from database"))


# =============================================================================
# EXAMPLES
# =============================================================================

# Generate percentile charts for specific players
# plot_player_span("Barry Bonds")
# plot_player_span("Barry Bonds", "2001 - 2004")
# plot_player_span("Mike Trout", stat = "OPS")


# Generate bar plots for top 10 by various stats
# plot_top_10("WAR")

# View leaderboards
# leaderboard("WAR", 10)
# leaderboard("HR", 10)
# top_10_unique("AVG")

# Search for players
# check_player("Albert Pujols")
# best_span("Mike Trout")

# =============================================================================
# CLEANUP
# =============================================================================
# Uncomment to close database connection when done:
# dbDisconnect(conn)


# =============================================================================
# DATABASE REGENERATION (Uncomment to re-fetch from FanGraphs - takes ~10-15 min)
# =============================================================================
# player_data <- get_all_players_seasons()
#
# if (is.null(player_data) || is.null(player_data$players) || is.null(player_data$raw_data)) {
#   stop("Failed to fetch player data. Check your internet connection and try again.")
# }
#
# player_database <- player_data$players
# raw_stats <- player_data$raw_data
# message(paste("Loaded", nrow(player_database), "players and", nrow(raw_stats), "season records"))
#
# all_spans <- get_all_4year_spans(player_database, raw_stats, conn)
#
# message("Results:")
# print(head(all_spans, 20))
#
# db_check <- dbGetQuery(conn, "SELECT COUNT(*) as total FROM four_year_spans")
# print(paste("Total records in database:", db_check$total))
# =============================================================================
