# 4-Year Span Analysis

An R project that analyzes MLB player performance over 4-year spans using data from FanGraphs. This tool calculates cumulative statistics for all possible 4-year periods in a player's career and provides rankings, visualizations, and comparisons.

## Features

- **Cumulative Stats Calculation**: Aggregates batting statistics over 4-year windows including WAR, OPS, wOBA, HR, RBI, and more
- **Leaderboards**: Generate ranked tables of the best 4-year spans by any stat
- **Unique Player Rankings**: Show each player's best span only (no duplicates)
- **Player Percentile Charts**: Visualize how a player's span ranks across multiple categories
- **Database Storage**: SQLite database for fast queries without re-fetching data

## Requirements

### R Packages

```r
install.packages(c("baseballr", "dplyr", "DBI", "RSQLite", "ggplot2", "gridExtra"))
```

## Usage

### Quick Start (Using Existing Database)

```r
source("main.r")

# View top 10 by WAR
leaderboard("WAR", 10)

# View top 10 by home runs
leaderboard("HR", 10)

# Get a specific player's best span
best_span("Mike Trout")

# Check all spans for a player
check_player("Barry Bonds")

# Generate percentile chart for the #1 ranked player by WAR
plot_player_span(1, "WAR")
```

### Key Functions

| Function                       | Description                                  |
| ------------------------------ | -------------------------------------------- |
| `leaderboard(stat, n)`         | Top n 4-year spans ranked by stat            |
| `top_10_unique(stat, n)`       | Top n players (each player's best span only) |
| `best_span(player_name)`       | Find a player's best 4-year span by WAR      |
| `check_player(pattern)`        | Search for players matching a pattern        |
| `plot_player_span(rank, stat)` | Percentile chart for player at given rank    |
| `plot_top_10(stat)`            | Bar chart of top 10 spans                    |

### Regenerating the Database

To fetch fresh data from FanGraphs (takes ~10-15 minutes):

```r
# Uncomment the database creation section in main.r, then run:
player_data <- get_all_players_seasons()
player_database <- player_data$players
raw_stats <- player_data$raw_data
all_spans <- get_all_4year_spans(player_database, raw_stats, conn)
```

## Stats Included

- **Counting**: G, AB, PA, H, HR, RBI, R, SB, BB, SO
- **Rate**: AVG, OBP, SLG, OPS, wOBA, BB/K
- **Advanced**: WAR, WPA, Clutch, Barrel%, BSR (baserunning), DEF (defense)

## Output

All generated plots and leaderboard images are saved to the `pictures_and_plots/` directory.

## Data Source

Player statistics are fetched from [FanGraphs](https://www.fangraphs.com/) via the `baseballr` package.

## License

MIT License
