library(plyr)
library(tidyverse)
library(stringr)
library(baseballr)
library(discordr)
library(emojifont)
library(janitor)
library(tidyr)
library(ggplot2)
library(tools)
library(pandoc)
library(cfbplotR)
library(ggforce)
library(cowplot)
library(gridExtra)
# THIS DOCUMNET WILL ALLOW YOU TO MAKE POST GAME CATCHER REPORTS USING TrackMan DATA. 

# OPTIONAL - CREATE A DISCORD WEBHOOK FOR AUTOMATED NOTIFICATIONS
conn_obj <- create_discord_connection(webhook = 'https://discord.com/api/webhooks/1278067437127864401/2Wr-ZtbBT8wK-sbKBbFYrSDFo-46WKutGEc_VxL-xcbP5pM8_jONvO3ar5tHTMfM4isT',
                                      username = paste('Report Maker', emoji('robot')), set_default = TRUE)

send_webhook_message(paste("Generating Chanticleers Catcher Game Report(s)"))

#this loads the new CSV and transforms it to add more columns and clean up some player names
ccu_24 <-  read.csv("/Users/aaronstaehely/Downloads/20241026-SpringsBrooksStadium-Private-1_unverified.csv")

#rename the dataset and filter by the teams most recent game, optional if just importint one file at a time
Track <- ccu_24 %>%
  # FILTER
  filter(PitcherTeam %in% c('COA_CHA','CCU_PRA')) |>
  # CHANGE DATE WHEN NECESSARY
  filter(
    Date == max(Date))

mound_curve <- data.frame(
  x = c(seq(-3, 3, length.out = 100), rev(seq(-3, 3, length.out = 100))),
  y = c(rep(0, 100), sqrt(9 - seq(-3, 3, length.out = 100)^2) * 0.15)  # Adjust multiplier for mound height
)

pitch_colors = c("Fastball" = '#FA8072', 
                 "Four-Seam" = '#FA8072', 
                 "Sinker" = "#fdae61", 
                 "Slider" = "#A020F0", 
                 "Sweeper" = "magenta",
                 "Curveball" = '#2c7bb6', 
                 "ChangeUp" = '#90EE90',
                 "Splitter" = '#90EE32',
                 "Cutter" = "red")
#Indicators used for this report and other reports
Track <- Track %>%
  mutate(
    EarlyIndicator = ifelse(
      ((Balls == 0 & Strikes == 0 & PitchCall == "InPlay") |
         (Balls == 1 & Strikes == 0 & PitchCall == "InPlay") |
         (Balls == 0 & Strikes == 1 & PitchCall == "InPlay") |
         (Balls == 1 & Strikes == 1 & PitchCall == "InPlay")), 
      1, 0),
    AheadIndicator = ifelse(
      ((Balls == 0 & Strikes == 1) & (PitchCall %in% c("StrikeCalled", "StrikeSwinging", "FoulBall"))) |
        ((Balls == 1 & Strikes == 1) & (PitchCall %in% c("StrikeCalled", "StrikeSwinging", "FoulBall"))), 
      1, 0),
    StrikeZoneIndicator = ifelse(
      PlateLocSide >= -0.8333 & PlateLocSide <= 0.8333 & 
        PlateLocHeight >= 1.5 & PlateLocHeight <= 3.37467, 
      1, 0),
    EdgeHeightIndicator = ifelse(
      ((PlateLocHeight > 14/12 & PlateLocHeight < 22/12) |
         (PlateLocHeight > 38/12 & PlateLocHeight < 46/12)), 
      1, 0),
    EdgeZoneHtIndicator = ifelse(
      PlateLocHeight > 16/12 & PlateLocHeight < 45.2/12, 
      1, 0),
    EdgeZoneWIndicator = ifelse(
      PlateLocSide > -13.4/12 & PlateLocSide < 13.4/12, 
      1, 0),
    EdgeWidthIndicator = ifelse(
      ((PlateLocSide > -13.3/12 & PlateLocSide < -6.7/12) |
         (PlateLocSide < 13.3/12 & PlateLocSide > 6.7/12)), 
      1, 0),
    HeartIndicator = ifelse(
      PlateLocSide >= -0.5583 & PlateLocSide <= 0.5583 & 
        PlateLocHeight >= 1.83 & PlateLocHeight <= 3.5, 
      1, 0),
    StrikeIndicator = ifelse(
      PitchCall %in% c("StrikeSwinging", "StrikeCalled", "FoulBallNot", "InPlay"), 
      1, 0),
    WhiffIndicator = ifelse(
      PitchCall == 'StrikeSwinging',1,0
    ),
    SwingIndicator = ifelse(
      PitchCall %in% c("StrikeSwinging", "FoulBall", "InPlay"), 
      1, 0),
    LHHindicator = ifelse(
      BatterSide == 'Left', 1,0
    ),
    RHHindicator = ifelse(
      BatterSide == 'Right', 1,0
    ),
    ABindicator = ifelse(
      PlayResult %in% c("Error", "FieldersChoice", "Out", "Single", "Double", "Triple", "Homerun") | 
        KorBB == "Strikeout", 
      1, 0),
    HitIndicator = ifelse(
      PlayResult %in% c("Single", "Double", "Triple", "Homerun"), 
      1, 0),
    FPindicator = ifelse(Balls == 0 & Strikes == 0, 1,0),
    PAindicator = ifelse(
      PitchCall %in% c("InPlay", "HitByPitch", "CatchersInterference") | 
        KorBB %in% c("Walk", "Strikeout"), 
      1, 0),
    LeadOffIndicator = ifelse(
      (PAofInning == 1 & (PlayResult != "Undefined" | KorBB != "Undefined")) | 
        PitchCall == "HitByPitch", 
      1, 0),
    HBPIndicator = ifelse(
      PitchCall == 'HitByPitch',1,0),
    WalkIndicator = ifelse(
      KorBB == 'Walk',1,0
    ),
  StolenStrike = ifelse(
      PitchCall == "StrikeCalled" & 
        (PlateLocHeight > 3.37467 | 
           PlateLocHeight < 1.5 | 
           PlateLocSide < -0.83083 | 
           PlateLocSide > 0.83083), 
      1, 
      0
    ),
  StrikeLost = ifelse(
    PitchCall == "BallCalled" & 
      (PlateLocHeight < 3.37467 & 
         PlateLocHeight > 1.5 & 
         PlateLocSide > -0.83083 & 
         PlateLocSide < 0.83083), 
    1, 
    0)
  )
  

Track <- Track %>%
  mutate(
    EdgeIndicator = ifelse(
      (EdgeHeightIndicator == 1 & EdgeZoneWIndicator == 1) | 
        (EdgeWidthIndicator == 1 & EdgeZoneHtIndicator == 1), 
      1, 0),
    QualityPitchIndicator = ifelse(
      StrikeZoneIndicator == 1 | EdgeIndicator == 1, 
      1, 0),
    FPSindicator = ifelse(
      PitchCall %in% c("StrikeCalled", "StrikeSwinging", "FoulBall", "InPlay") &
        (FPindicator == 1),
      1, 0),
    OutIndicator = ifelse(
      (PlayResult %in% c("Out", "FieldersChoice") | KorBB == "Strikeout") & HBPIndicator == 0, 
      1, 0),
    LOOindicator = ifelse(
      LeadOffIndicator == 1 & OutIndicator == 1, 
      1, 0)
  )

Track <- Track %>%
  mutate(PlayResult = ifelse(is.na(PlayResult) | PlayResult == "Undefined", PitchCall, PlayResult))



# ------------------------
unique(Track$PitcherTeam)
# set a factor to manually order the pitch types
Track$TaggedPitchType <- factor(Track$TaggedPitchType, levels = c("Fastball", "Sinker", "Cutter","Curveball", "Slider", "ChangeUp", "Splitter", 'Knuckleball', 'Other'))

# extract the date for the title of the report and change to different format
game_date <- unique(format(as.Date(Track$Date, format = "%m/%d/%y"), "%d/%m/%y"))
#each catcher that caught in the previous game
catchers <- unique(Track$Catcher)
# this will send a message telling you how many reports are being made
send_webhook_message(paste(length(catchers),"new catcher reports being generated."))

# LOOP ---------

# For each pitcher in the dataset, run through the following code
for (catcher in catchers) {
  # Filter the data for the current catcher
  catcher_data <- Track[Track$Catcher == catcher, ]
  
  IP <- catcher_data |>
    dplyr::summarize(
      total_outs = sum(OutsOnPlay) + sum(KorBB == 'Strikeout'),
      innings_pitched = total_outs %/% 3 + (total_outs %% 3)/10)
  # pull the opponent's name for the report
  opponent <- catcher_data$BatterTeam[1]
  
  #  IP <- pitcher_data |>
  #   dplyr::summarize(
  #    total_outs = sum(OutsOnPlay) + sum(KorBB == 'Strikeout'),
  #  innings_pitched = total_outs %/% 3 + (total_outs %% 3)/10)
  
  # Generate the catcher +/- table
  game_summary_table <- 
    catcher_data %>%
    dplyr::summarize(
      'Strikes Stolen' = sum(StolenStrike),
      'Strikes Lost' = sum(StrikeLost),
      'Game +/-' = sum(StolenStrike)-sum(StrikeLost)
      )
  
  
  # Stolen Strikes Plot
  StolenStrikes <- catcher_data %>% 
    filter(
      StolenStrike == 1
    ) %>%
    ggplot( 
      aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType)) +
    xlim(-2,2) + ylim(-.25,5) + labs(color = "")+
    geom_rect(aes(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5), alpha = 0, size = .5, color = "red") + 
    geom_rect(aes(xmin = -0.5583, xmax = 0.5583, ymin = 1.83, ymax = 3.17), alpha = 0, size = .5, color = "black", linetype = 'dotdash') +
    geom_rect(aes(xmin = -1.108, xmax = 1.108, ymin = 1.167, ymax = 3.83), alpha = 0, size = .5, color = "black", linetype = 'dotdash') +
    # Home Plate Outline Below
    geom_segment(aes(x = -0.708, y = 0.15, xend = 0.708, yend = 0.15), size = .5, color = "black") +
    geom_segment(aes(x = -0.708, y = 0.3, xend = -0.708, yend = 0.15), size = .5, color = "black") +
    geom_segment(aes(x = -0.708, y = 0.3, xend = 0, yend = 0.5), size = .5, color = "black") +
    geom_segment(aes(x = 0, y = 0.5, xend = 0.708, yend = 0.3), size = .5, color = "black") +
    geom_segment(aes(x = 0.708, y = 0.3, xend = 0.708, yend = 0.15), size = .5, color = "black") +
    geom_point(size =3, alpha = .95) +
    #stat_ellipse(aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType, fill = TaggedPitchType),
    #geom = 'polygon', level = 0.4, alpha = 0.1) +
    scale_color_manual(values = pitch_colors) + # , na.rm = TRUE)+
    theme_bw() + theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5)) +
    theme(legend.position = "none", axis.title = element_blank())  +
    theme(strip.text = element_text(size = 7, face = 'bold'),
          axis.text.x=element_blank(), #remove x axis labels
          axis.text.y=element_blank(),  #remove y axis labels
    ) + labs(title = 'Strikes Stolen')+
  coord_fixed()
  
  # Lost Strikes Plot
  LostStrikes <- catcher_data %>% 
    filter(
      StrikeLost == 1
    ) %>%
    ggplot( 
      aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType)) +
    xlim(-2,2) + ylim(-.25,5) + labs(color = "")+
    geom_rect(aes(xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5), alpha = 0, size = .5, color = "red") + 
    geom_rect(aes(xmin = -0.5583, xmax = 0.5583, ymin = 1.83, ymax = 3.17), alpha = 0, size = .5, color = "black", linetype = 'dotdash') +
    geom_rect(aes(xmin = -1.108, xmax = 1.108, ymin = 1.167, ymax = 3.83), alpha = 0, size = .5, color = "black", linetype = 'dotdash') +
    # Home Plate Outline Below
    geom_segment(aes(x = -0.708, y = 0.15, xend = 0.708, yend = 0.15), size = .5, color = "black") +
    geom_segment(aes(x = -0.708, y = 0.3, xend = -0.708, yend = 0.15), size = .5, color = "black") +
    geom_segment(aes(x = -0.708, y = 0.3, xend = 0, yend = 0.5), size = .5, color = "black") +
    geom_segment(aes(x = 0, y = 0.5, xend = 0.708, yend = 0.3), size = .5, color = "black") +
    geom_segment(aes(x = 0.708, y = 0.3, xend = 0.708, yend = 0.15), size = .5, color = "black") +
    geom_point(size =3, alpha = .95) +
    #stat_ellipse(aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType, fill = TaggedPitchType),
    #geom = 'polygon', level = 0.4, alpha = 0.1) +
    scale_color_manual(values = pitch_colors) + # , na.rm = TRUE)+
    theme_bw() + theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5)) +
    theme(legend.position = "none", axis.title = element_blank())  +
    theme(strip.text = element_text(size = 7, face = 'bold'),
          axis.text.x=element_blank(), #remove x axis labels
          axis.text.y=element_blank(),  #remove y axis labels
    ) + labs(title = 'Strikes Lost')+
    coord_fixed()


  # Throwdown plot, colored by throw speed
  ThrowPlot <- catcher_data %>% 
    filter(
      Notes == '2b out' | Notes == '2b safe'
    ) %>%
    ggplot() +
    geom_polygon(data = data.frame(x = c(-8,8,8,-8), y = c(0.25,0.25,7.5,7.5)), aes(x = x, y = y), fill = '#14a6a8', color = '#14a6a8', inherit.aes = FALSE) +
    geom_polygon(data = data.frame(x = c(-8,8,8,-8), y = c(-1,-1,0.25,0.25)), aes(x = x, y = y), fill = 'brown', color = 'brown', inherit.aes = FALSE) +
    geom_polygon(data = data.frame(x = c(-8,8,8,-8), y = c(-3,-3,-1,-1)), aes(x = x, y = y), fill = 'darkgreen', color = 'darkgreen', inherit.aes = FALSE) +
    geom_polygon(data = data.frame(x = c(-1, 1, 1, -1), y = c(0, 0, 0.45, 0.45)),
                 aes(x = x, y = y), fill = 'white', color = 'black', inherit.aes = FALSE) +
    geom_polygon(data=data.frame(x = c(-1,0,0,-1),y=c(0,0,0.45,0.45)),aes(x=x, y=y), fill = 'lightgrey',color = 'black',inherit.aes = F)+
    geom_point(aes(x = BasePositionZ, y = BasePositionY, fill = ThrowSpeed), color = 'white',pch=21,na.rm = TRUE, alpha = .99, size = 2.5) + 
    scale_fill_gradient(high = 'red', low = 'blue') +
    scale_x_continuous(
      limits = c(-8,8)
    ) +
    scale_y_continuous(
      limits = c(-3,7.5)
    ) + 
    theme_bw() + theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5)) +
    theme(legend.position = "none", axis.title = element_blank())  +
    theme(strip.text = element_text(size = 7, face = 'bold'),
          axis.text.x=element_blank(), #remove x axis labels
          axis.text.y=element_blank(),  #remove y axis labels
    ) + coord_fixed()

  # OPTIONAL: Create Pitch Log of the stolen or lost strikes, good for video review
  pitch_log <- catcher_data %>%
    filter(
      StolenStrike == 1 | StrikeLost == 1
    ) %>%
    select(PitchNo, Pitcher, Catcher, Batter, TaggedPitchType, PitchCall, StrikeZoneIndicator) %>%
    mutate(
      'Pitch' = TaggedPitchType,
      'Actual' = ifelse(StrikeZoneIndicator == 1, "STRIKE", "BALL"),
    ) %>%
    select(PitchNo, 'Pitch', Pitcher , Catcher , Batter , 'Pitch', PitchCall, 'Actual')


  #Throwdown log
  Throwlog <- catcher_data %>%
    filter(
      Notes == '2b out' | Notes == '2b safe' | Notes == '3b out' | Notes == '3b safe') %>%
    select(PitchNo,Pitcher,Catcher,ThrowSpeed,PopTime,ExchangeTime,Notes)
  
  
  

  # SET THE PARAMETERS FOR THE R MARKDOWN FILE
  params <- list(
    pitch_colors = pitch_colors,
    game_summary_table = game_summary_table,
    catcher = catcher,
    StolenStrikes = StolenStrikes,
    LostStrikes = LostStrikes,
    ThrowPlot = ThrowPlot,
    Throwlog = Throwlog,
    date = Track$Date[1],
    opponent = catcher_data$BatterTeam[1]
  )
  
  # SETS THE DATE FOR THE FILE NAME
  file_date <- format(as.Date(catcher_data$Date[1], format = "%m/%d/%y"), "%m-%d")
  # Knit the R Markdown file to PDF
  rmarkdown::render(input = "/Users/aaronstaehely/Documents/CCU Analytics/R:Rmd/CatcherReport.Rmd",
                    output_file = paste0("/Users/aaronstaehely/Documents/CCU Analytics/R reports/",file_date," ",catcher, " report",".pdf"),
                    params = params)
}

send_webhook_message(paste("New reports for", paste(unique(catchers), collapse = ", "),"from", game_date, "created."))
