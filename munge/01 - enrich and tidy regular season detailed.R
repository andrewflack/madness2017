# enrich detailed regular season results

rs_detailed <- read.csv("data/RegularSeasonDetailedResults.csv")

# add game-level rates
rs_detailed <- rs_detailed %>% 
  mutate(Wfgpct = Wfgm/Wfga,
         Lfgpct = Lfgm/Lfga,
         Wfgpct3 = Wfgm3/Wfga3,
         Lfgpct3 = Lfgm3/Lfga3,
         Wftpct = Wftm/Wfta,
         Lftpct = Lftm/Lfta)

# add season-to-date rates