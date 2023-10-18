
# Libraries used

library(ggplot2)
library(stringr)
library(dplyr)
library(rvest)
library(scales)
library(ggradar)

# URLs from teams that played the USA during the FIFA 2022 World Cup

# USA FIFA World Cup 2022 Matches
murl1 <- "https://fbref.com/en/matches/9de4c4cc/United-States-Wales-November-21-2022-FIFA-World-Cup"
murl2 <- "https://fbref.com/en/matches/cde0c30b/England-United-States-November-25-2022-FIFA-World-Cup"
murl3 <- "https://fbref.com/en/matches/dcd3b239/IR-Iran-United-States-November-29-2022-FIFA-World-Cup"
murl4 <- "https://fbref.com/en/matches/249ccad4/Netherlands-United-States-December-3-2022-FIFA-World-Cup"

# Wales FIFA World Cup 2022 Matches
murl5 <- "https://fbref.com/en/matches/ff2407b2/Wales-IR-Iran-November-25-2022-FIFA-World-Cup"
murl6 <- "https://fbref.com/en/matches/e51b7436/Wales-England-November-29-2022-FIFA-World-Cup"

# England FIFA World Cup 2022 Matches
murl7 <- "https://fbref.com/en/matches/d8a472c1/England-IR-Iran-November-21-2022-FIFA-World-Cup"
murl8 <- "https://fbref.com/en/matches/a323f701/England-Senegal-December-4-2022-FIFA-World-Cup"
murl9 <- "https://fbref.com/en/matches/6d4b58f5/England-France-December-10-2022-FIFA-World-Cup"

# Netherlands FIFA World Cup 2022 Matches
murl10 <- "https://fbref.com/en/matches/21eb041c/Senegal-Netherlands-November-21-2022-FIFA-World-Cup"
murl11 <- "https://fbref.com/en/matches/dd581756/Netherlands-Ecuador-November-25-2022-FIFA-World-Cup"
murl12 <- "https://fbref.com/en/matches/af95872a/Netherlands-Qatar-November-29-2022-FIFA-World-Cup"
murl13 <- "https://fbref.com/en/matches/9fd14983/Netherlands-Argentina-December-9-2022-FIFA-World-Cup"

# Read in tables from the linked urls

selected_urls <- rbind(murl1, murl2, murl3, murl4, murl5, murl6, murl7, murl8, 
                       murl9, murl10, murl11, murl12, murl13)


full_stat <- NULL

for (g in 1:length(selected_urls)) {
  game_data <- substr(selected_urls[g], 39, nchar(selected_urls[g]) - 15)
  game_data <- str_replace(game_data, "United-States", "United States")
  parts <- strsplit(game_data, "-")[[1]]
  teamA <- parts[1]
  teamB <- parts[2]
  date <- paste(parts[3:5], collapse = "-")
  
  statA = read_html(selected_urls[g],as.data.frame=TRUE,stringAsFactors=TRUE) %>% html_nodes("table") %>% .[[4]] %>% html_table(fill=TRUE)
  statA <- head(statA, -1)
  colnames(statA) <- as.character(unlist(statA[1, ]))
  statA = statA[-1, ]
  statA = cbind(date, Team = teamA, Opponent = teamB, statA)
  statB = read_html(selected_urls[g],as.data.frame=TRUE,stringAsFactors=TRUE) %>% html_nodes("table") %>% .[[11]] %>% html_table(fill=TRUE)
  statB <- head(statB, -1)
  colnames(statB) <- as.character(unlist(statB[1, ]))
  statB = statB[-1, ]
  statB = cbind(date, Team = teamB, Opponent = teamA, statB)
  stat_both <- rbind(statA, statB)
  
  full_stat <- rbind(full_stat, stat_both)
  Sys.sleep(1)
}

all_stat_full <- unique(full_stat)
write.csv(all_stat_full, "Fifa2022.csv")

# summary data frame

summaryDF <- read.csv("Fifa2022.csv")
summaryDF$Pos <-NULL
summaryDF$Age <- NULL
summaryDF$X <- NULL
summaryDF$X. <- NULL

summaryDF <- summaryDF %>%
  group_by(Player) %>%
  summarise_if(is.numeric, function(x) sum(as.numeric(x), na.rm = TRUE))

# Select Players - Focusing on comparing Sophia Smith to other women who play 
# her position she was marked as a LW and FW so I selected a few around her age 
# and skill level/ playing time to fairly compare her to

selected_players <- subset(summaryDF, Player == "Christian Pulisic" | 
                                Player == "Cody Gakpo" | 
                                Player == "Gareth Bale" | 
                                Player == "Mehdi Taremi" | 
                                Player == "Phil Foden" | 
                                Player == "Marcus Rashford" | 
                                Player == "Ismaila Sarr" | 
                                Player == "Harry Wilson")

# Create Radar Plots

player_subset <- selected_players %>%
  select(Player, xG, Cmp, Sh, SoT, Touches, Succ)

create_radar <- function(subset, indices, name) {
  result <- subset %>%
    mutate(across(-Player, ~ rescale(.))) %>%
    slice(indices) %>%
    select(1:7)
  
  assign(paste0(name, "_radar"), result, envir = .GlobalEnv)
}

create_radar(player_subset, c(1, 2), "CPCG")
create_radar(player_subset, c(1, 3), "CPGB")
create_radar(player_subset, c(1, 5), "CPIS")
create_radar(player_subset, c(1, 6), "CPMR")
create_radar(player_subset, c(1, 7), "CPMT")
create_radar(player_subset, c(1, 8), "CPPF")

# Fix Aesthetics of Graphs

create_radar_plot <- function(data, font, group_colors, title, filename) {
  plot <- data %>%
    ggradar(
      font.radar = font,
      grid.label.size = 4,
      axis.label.size = 3, 
      group.point.size = 2,  
      grid.line.width = 0.5,
      group.colours = group_colors,
      gridline.mid.colour = "#8b8c8c",
      group.line.width = 1
    ) +
    theme(
      legend.position = c(1, 0),  
      legend.justification = c(1, 0),
      legend.text = element_text(size = 8, family = font),
      legend.key = element_rect(fill = NA, color = NA),
      legend.background = element_blank()
    ) +
    labs(
      title = title
    ) + 
    theme(
      plot.title.position = "panel", 
      plot.title = element_text(
        family = font, 
        size = 12, 
        hjust = 0.5
      )
    )
  
  ggsave(
    filename = filename,
    plot = plot,
    width = 5.7,
    height = 5,
    device = "png"
  )
}

create_radar_plot(CPCG_radar, "sans", c("#1F2742", "#F36C21"), "Radar Plot Comparing Christian Pulisic and Cody Gakpo Statistics", "ChristianPulisic_CodyGakpo.png")
create_radar_plot(CPGB_radar, "sans", c("#1F2742", "#CF1E26"), "Radar Plot Comparing Christian Pulisic and Gareth Bale Statistics", "ChristianPulisic_GarethBale.png")
create_radar_plot(CPIS_radar, "sans", c("#1F2742", "#FFDC00"), "Radar Plot Comparing Christian Pulisic and Ismaila Sarr Statistics", "ChristianPulisic_IsmailaSarr.png")

create_radar_plot(CPMR_radar, "sans", c("#1F2742", "#2B57AC"), "Radar Plot Comparing Christian Pulisic and Marcus Rashford Statistics", "ChristianPulisic_MarcusRashford.png")
create_radar_plot(CPMT_radar, "sans", c("#1F2742", "#239F40"), "Radar Plot Comparing Christian Pulisic and Mehdi Taremi Statistics", "ChristianPulisic_MehdiTaremi.png")
create_radar_plot(CPPF_radar, "sans", c("#1F2742", "#2B57AC"), "Radar Plot Comparing Christian Pulisic and Phil Foden Statistics", "ChristianPulisic_PhilFoden .png")















