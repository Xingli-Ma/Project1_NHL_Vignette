Project1
================
Xingli Ma
June 20, 2021

-   [Introduction](#introduction)
-   [NFL Records API and NFL Stats
    API](#nfl-records-api-and-nfl-stats-api)
    -   [Retriving Tables and Quering Information from NFL Records
        API](#retriving-tables-and-quering-information-from-nfl-records-api)
    -   [Retriving Tables and Quering Information from NFL Stats
        API](#retriving-tables-and-quering-information-from-nfl-stats-api)
-   [Exploratory Data Analysis](#exploratory-data-analysis)
    -   [Tables Retrieved from NHL Records
        API](#tables-retrieved-from-nhl-records-api)
    -   [Create Plots for NHL Records](#create-plots-for-nhl-records)

The following R packages are reqired to run the code to create this
vignette.

``` r
require(rmarkdown)
require(knitr)
require(dplyr)
require(tidyverse)
require(ggplot2)
require(DT)
library(jsonlite)
library(httr)
```

## Introduction

I created a vignette for reading and summarizing data from the National
Hockey League’s (NHL) API. Three main components are present in this
vignette: extracting tables and querying information from NFL Records
API, extracting tables and querying information from NFL Stats API, and
exploratory data analysis.

## NFL Records API and NFL Stats API

### Retriving Tables and Quering Information from NFL Records API

Create a wrapper function to return the records for a given team ID or
franchise ID.

``` r
team_Record <- function(teamId_OR_franchiseId, ...){
  
  # construct URLs
  full_url_records <- function(table_name, ...){
  base_url <- "https://records.nhl.com/site/api"
  return(paste0(base_url, table_name, ...))
}
  franchise <- full_url_records("/franchise?cayenneExp=mostRecentTeamId=", teamId_OR_franchiseId)
  franchise_teams <- full_url_records("/franchise-team-totals?cayenneExp=teamId=", teamId_OR_franchiseId)
  franchise_season <- full_url_records("/franchise-season-records?cayenneExp=franchiseId=", teamId_OR_franchiseId)
  goalie <- full_url_records("/franchise-goalie-records?cayenneExp=franchiseId=", teamId_OR_franchiseId)
  skater <- full_url_records("/franchise-skater-records?cayenneExp=franchiseId=", teamId_OR_franchiseId)
  franchise_detail <- full_url_records("/franchise-detail?cayenneExp=mostRecentTeamId=", teamId_OR_franchiseId)
  
 # retrieve the NHL records information and convert it to data frames  
  franchise_record <- jsonlite::fromJSON(content(GET(franchise),"text"),       flatten=TRUE)$data
  franchise_teams_record <- jsonlite::fromJSON(content(GET(franchise_teams),"text"), flatten=TRUE)$data
  franchise_season_record <- jsonlite::fromJSON(content(GET(franchise_season),"text"), flatten=TRUE)$data
  goalie_record <- jsonlite::fromJSON(content(GET(goalie),"text"), flatten=TRUE)$data
  skater_record <- jsonlite::fromJSON(content(GET(skater),"text"), flatten=TRUE)$data
  franchise_detail_record <- jsonlite::fromJSON(content(GET(franchise_detail),"text"), flatten=TRUE)$data 
  return(list(df1_franchise=franchise_record, df2_teams=franchise_teams_record,   df3_season=franchise_season_record, df4_goalie=goalie_record, df5_skater=skater_record, df6_adminDetail=franchise_detail_record))
}

# Query NHL records information according to the team ID or franchise ID given by a user
team_Record(2)
```

    ## $df1_franchise
    ##   id firstSeasonId           fullName lastSeasonId mostRecentTeamId teamAbbrev teamCommonName
    ## 1 22      19721973 New York Islanders           NA                2        NYI      Islanders
    ##   teamPlaceName
    ## 1      New York
    ## 
    ## $df2_teams
    ##   id activeFranchise firstSeasonId franchiseId gameTypeId gamesPlayed goalsAgainst goalsFor
    ## 1  3               1      19721973          22          2        3788        11907    12045
    ## 2  4               1      19721973          22          3         310          899      986
    ##   homeLosses homeOvertimeLosses homeTies homeWins lastSeasonId losses overtimeLosses
    ## 1        678                 84      170      963           NA   1587            166
    ## 2         53                  1       NA       95           NA    139              0
    ##   penaltyMinutes pointPctg points roadLosses roadOvertimeLosses roadTies roadWins
    ## 1          57792    0.5133   3889        909                 82      177      725
    ## 2           5693    0.0129      8         86                  2       NA       76
    ##   shootoutLosses shootoutWins shutouts teamId           teamName ties triCode wins
    ## 1             70           86      177      2 New York Islanders  347     NYI 1688
    ## 2              0            0       12      2 New York Islanders   NA     NYI  171
    ## 
    ## $df3_season
    ##   id fewestGoals fewestGoalsAgainst fewestGoalsAgainstSeasons fewestGoalsSeasons fewestLosses
    ## 1 41          NA                 NA                        NA                 NA           NA
    ##   fewestLossesSeasons fewestPoints fewestPointsSeasons fewestTies fewestTiesSeasons fewestWins
    ## 1                  NA           NA                  NA         NA                NA         NA
    ##   fewestWinsSeasons franchiseId      franchiseName homeLossStreak homeLossStreakDates
    ## 1                NA           2 Montreal Wanderers             NA                  NA
    ##   homePointStreak homePointStreakDates homeWinStreak homeWinStreakDates homeWinlessStreak
    ## 1              NA                   NA            NA                 NA                NA
    ##   homeWinlessStreakDates lossStreak           lossStreakDates mostGameGoals
    ## 1                     NA          5 Dec 22 1917 - Jan 05 1918            10
    ##             mostGameGoalsDates mostGoals mostGoalsAgainst mostGoalsAgainstSeasons
    ## 1 Dec 19 1917 - TAN 9 @ MWN 10        17               37            1917-18 (22)
    ##   mostGoalsSeasons mostLosses mostLossesSeasons mostPenaltyMinutes mostPenaltyMinutesSeasons
    ## 1     1917-18 (22)          5      1917-18 (22)                 27              1917-18 (22)
    ##   mostPoints mostPointsSeasons mostShutouts mostShutoutsSeasons mostTies mostTiesSeasons
    ## 1          2      1917-18 (22)            0        1917-18 (22)        0    1917-18 (22)
    ##   mostWins mostWinsSeasons pointStreak pointStreakDates roadLossStreak
    ## 1        1    1917-18 (22)          NA               NA              3
    ##         roadLossStreakDates roadPointStreak roadPointStreakDates roadWinStreak
    ## 1 Dec 29 1917 - Jan 05 1918              NA                   NA            NA
    ##   roadWinStreakDates roadWinlessStreak roadWinlessStreakDates winStreak winStreakDates
    ## 1                 NA                NA                     NA        NA             NA
    ##   winlessStreak        winlessStreakDates
    ## 1             5 Dec 22 1917 - Jan 05 1918
    ## 
    ## $df4_goalie
    ##    id activePlayer firstName franchiseId      franchiseName gameTypeId gamesPlayed lastName
    ## 1 250        FALSE      Bert           2 Montreal Wanderers          2           4  Lindsay
    ##   losses mostGoalsAgainstDates mostGoalsAgainstOneGame mostSavesDates mostSavesOneGame
    ## 1      3            1917-12-22                      11             NA               NA
    ##   mostShotsAgainstDates mostShotsAgainstOneGame mostShutoutsOneSeason mostShutoutsSeasonIds
    ## 1                    NA                      NA                     0              19171918
    ##   mostWinsOneSeason mostWinsSeasonIds overtimeLosses playerId positionCode rookieGamesPlayed
    ## 1                 1          19171918             NA  8450014            G                 4
    ##   rookieShutouts rookieWins seasons shutouts ties wins
    ## 1              0          1       1        0    0    1
    ## 
    ## $df5_skater
    ##       id activePlayer assists firstName franchiseId      franchiseName gameTypeId gamesPlayed
    ## 1  16897        FALSE       0     Gerry           2 Montreal Wanderers          2           4
    ## 2  16903        FALSE       0      Jack           2 Montreal Wanderers          2           1
    ## 3  16908        FALSE       0    George           2 Montreal Wanderers          2           4
    ## 4  16917        FALSE       0    Raymie           2 Montreal Wanderers          2           1
    ## 5  16920        FALSE       0       Ken           2 Montreal Wanderers          2           1
    ## 6  16889        FALSE       0     Billy           2 Montreal Wanderers          2           2
    ## 7  16914        FALSE       0       Art           2 Montreal Wanderers          2           3
    ## 8  16919        FALSE       0      Phil           2 Montreal Wanderers          2           4
    ## 9  16904        FALSE       1      Jack           2 Montreal Wanderers          2           4
    ## 10 16912        FALSE       2      Dave           2 Montreal Wanderers          2           4
    ## 11 16901        FALSE       1     Harry           2 Montreal Wanderers          2           4
    ##    goals lastName                           mostAssistsGameDates mostAssistsOneGame
    ## 1      0    Geran 1917-12-19, 1917-12-22, 1917-12-26, 1917-12-29                  0
    ## 2      0    Marks                                     1917-12-29                  0
    ## 3      0  O'Grady 1917-12-19, 1917-12-22, 1917-12-26, 1917-12-29                  0
    ## 4      0  Skilton 1917-12-19, 1917-12-22, 1917-12-26, 1917-12-29                  0
    ## 5      0 Thompson 1917-12-19, 1917-12-22, 1917-12-26, 1917-12-29                  0
    ## 6      1     Bell                         1917-12-19, 1917-12-29                  0
    ## 7      1     Ross 1917-12-19, 1917-12-22, 1917-12-26, 1917-12-29                  0
    ## 8      1  Stevens 1917-12-19, 1917-12-22, 1917-12-26, 1917-12-29                  0
    ## 9      3 McDonald                                     1917-12-29                  1
    ## 10     5  Ritchie                                     1917-12-19                  2
    ## 11     6   Hyland                                     1917-12-29                  1
    ##    mostAssistsOneSeason mostAssistsSeasonIds                             mostGoalsGameDates
    ## 1                     0             19171918 1917-12-19, 1917-12-22, 1917-12-26, 1917-12-29
    ## 2                     0             19171918                                     1917-12-29
    ## 3                     0             19171918 1917-12-19, 1917-12-22, 1917-12-26, 1917-12-29
    ## 4                     0             19171918 1917-12-19, 1917-12-22, 1917-12-26, 1917-12-29
    ## 5                     0             19171918 1917-12-19, 1917-12-22, 1917-12-26, 1917-12-29
    ## 6                     0             19171918                                     1917-12-19
    ## 7                     0             19171918                                     1917-12-19
    ## 8                     0             19171918                                     1917-12-22
    ## 9                     1             19171918             1917-12-19, 1917-12-22, 1917-12-26
    ## 10                    2             19171918                         1917-12-19, 1917-12-29
    ## 11                    1             19171918                                     1917-12-19
    ##    mostGoalsOneGame mostGoalsOneSeason mostGoalsSeasonIds mostPenaltyMinutesOneSeason
    ## 1                 0                  0           19171918                           0
    ## 2                 0                  0           19171918                           0
    ## 3                 0                  0           19171918                           0
    ## 4                 0                  0           19171918                           0
    ## 5                 0                  0           19171918                           0
    ## 6                 1                  1           19171918                           0
    ## 7                 1                  1           19171918                          12
    ## 8                 1                  1           19171918                           3
    ## 9                 1                  3           19171918                           3
    ## 10                2                  5           19171918                           3
    ## 11                5                  6           19171918                           6
    ##    mostPenaltyMinutesSeasonIds                            mostPointsGameDates
    ## 1                     19171918 1917-12-19, 1917-12-22, 1917-12-26, 1917-12-29
    ## 2                     19171918                                     1917-12-29
    ## 3                     19171918 1917-12-19, 1917-12-22, 1917-12-26, 1917-12-29
    ## 4                     19171918 1917-12-19, 1917-12-22, 1917-12-26, 1917-12-29
    ## 5                     19171918 1917-12-19, 1917-12-22, 1917-12-26, 1917-12-29
    ## 6                     19171918                                     1917-12-19
    ## 7                     19171918                                     1917-12-19
    ## 8                     19171918                                     1917-12-22
    ## 9                     19171918 1917-12-19, 1917-12-22, 1917-12-26, 1917-12-29
    ## 10                    19171918                                     1917-12-19
    ## 11                    19171918                                     1917-12-19
    ##    mostPointsOneGame mostPointsOneSeason mostPointsSeasonIds penaltyMinutes playerId points
    ## 1                  0                   0            19171918              0  8446580      0
    ## 2                  0                   0            19171918              0  8447616      0
    ## 3                  0                   0            19171918              0  8448052      0
    ## 4                  0                   0            19171918              0  8449048      0
    ## 5                  0                   0            19171918              0  8449231      0
    ## 6                  1                   1            19171918              0  8445044      1
    ## 7                  1                   1            19171918             12  8448456      1
    ## 8                  1                   1            19171918              3  8449181      1
    ## 9                  1                   4            19171918              3  8447761      4
    ## 10                 4                   7            19171918              3  8448336      7
    ## 11                 5                   7            19171918              6  8447013      7
    ##    positionCode rookieGamesPlayed rookiePoints seasons
    ## 1             C                 4            0       1
    ## 2             L                 1            0       1
    ## 3             D                 4            0       1
    ## 4             D                 1            0       1
    ## 5             L                 1            0       1
    ## 6             C                 2            1       1
    ## 7             D                 3            1       1
    ## 8             C                 4            1       1
    ## 9             L                 4            4       1
    ## 10            D                 4            7       1
    ## 11            R                 4            7       1
    ## 
    ## $df6_adminDetail
    ##   id active
    ## 1 22   TRUE
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        captainHistory
    ## 1 <ul class="striped-list">\r\n\t<li>Anders Lee: 2018-19 &ndash;&nbsp;Present</li>\r\n\t<li>John Tavares: 2013-14 &ndash;&nbsp;2017-18</li>\r\n\t<li>Mark Streit: 2011-12 &ndash;&nbsp;2012-13</li>\r\n\t<li>Doug Weight: 2009-10 &ndash;&nbsp;2010-11</li>\r\n\t<li>Bill Guerin and (No Captain): 2008-09</li>\r\n\t<li>Bill Guerin: 2007-08</li>\r\n\t<li>Alexei Yashin: 2005-06 &ndash;&nbsp;2006-07</li>\r\n\t<li>Michael Peca: 2001-02 &ndash;&nbsp;2003-04</li>\r\n\t<li>Kenny Jonsson: 1999-00 &ndash;&nbsp;2000-01</li>\r\n\t<li>Trevor Linden: 1998-99</li>\r\n\t<li>Bryan McCabe and Trevor Linden: 1997-98</li>\r\n\t<li>(No Captain): 1996-97</li>\r\n\t<li>Patrick Flatley: 1992-93 &ndash;&nbsp;1995-96</li>\r\n\t<li>Brent Sutter and Patrick Flatley: 1991-92</li>\r\n\t<li>Brent Sutter: 1987-88 &ndash;&nbsp;1990-91</li>\r\n\t<li>Denis Potvin: 1979-80 &ndash;&nbsp;1986-87</li>\r\n\t<li>Clark Gillies: 1977-78 &ndash;&nbsp;1978-79</li>\r\n\t<li>Ed Westfall and Clark Gillies: 1976-77</li>\r\n\t<li>Ed Westfall: 1972-73 &ndash;&nbsp;1975-76</li>\r\n</ul>\r\n
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         coachingHistory
    ## 1 <ul class="striped-list">\r\n\t<li>Barry Trotz: Oct. 4, 2018 &ndash; Present</li>\r\n\t<li>Doug Weight: Jan. 19, 2017 &ndash; April 7, 2018</li>\r\n\t<li>Jack Capuano: Nov. 17, 2010 &ndash; Jan. 16, 2017</li>\r\n\t<li>Scott Gordon: Oct. 10, 2008 &ndash; Nov. 13, 2010</li>\r\n\t<li>Ted Nolan: Nov. 3, 2007 &ndash; April 4, 2008</li>\r\n\t<li>Al Arbour: Nov. 1, 2007</li>\r\n\t<li>Ted Nolan: Oct. 5, 2006 &ndash; Oct. 27, 2007</li>\r\n\t<li>Brad Shaw: Jan. 12&nbsp;&ndash; April 18, 2006</li>\r\n\t<li>Steve Stirling: Oct. 9, 2003 &ndash; Jan. 10, 2006</li>\r\n\t<li>Peter Laviolette: Oct. 5, 2001 &ndash; April 17, 2003</li>\r\n\t<li>Lorne Henning: March 5&nbsp;&ndash; April 7, 2001</li>\r\n\t<li>Butch Goring: Oct. 2, 1999 &ndash; March 3, 2001</li>\r\n\t<li>Bill Stewart: Jan. 21&nbsp;&ndash; April 17, 1999</li>\r\n\t<li>Mike Milbury: March 12, 1998 &ndash; Jan. 20, 1999</li>\r\n\t<li>Rick Bowness: Jan. 24, 1997 &ndash; March 10, 1998</li>\r\n\t<li>Mike Milbury: Oct. 7, 1995 &ndash; Jan. 22, 1997</li>\r\n\t<li>Lorne Henning: Jan. 21&nbsp;&ndash; May 2, 1995</li>\r\n\t<li>Al Arbour: Dec. 9, 1988 &ndash; April 24, 1994</li>\r\n\t<li>Terry Simpson: Oct. 9, 1986 &ndash; Dec. 6, 1988</li>\r\n\t<li>Al Arbour: Oct. 10, 1973 &ndash; April 12, 1986</li>\r\n\t<li>Earl Ingarfield: Jan. 31&nbsp;&ndash; April 1, 1973</li>\r\n\t<li>Phil Goyette: Oct. 7, 1972 &ndash; Jan. 26, 1973</li>\r\n\t<li>* <em>Date range indicates first and last games coached during tenure (regular season or playoffs)</em></li>\r\n</ul>\r\n
    ##           dateAwarded                                          directoryUrl firstSeasonId
    ## 1 1972-06-06T00:00:00 https://www.nhl.com/islanders/team/business-directory      19721973
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    generalManagerHistory
    ## 1 <ul class="striped-list">\r\n\t<li>Lou Lamoriello: June 5, 2018 &ndash; Present</li>\r\n\t<li>Garth Snow: July 18, 2006 &ndash; June 5, 2018</li>\r\n\t<li>Neil Smith: June 6&nbsp;&ndash; July 18, 2006</li>\r\n\t<li>Mike Milbury: Dec. 12, 1995 &ndash; June 6, 2006</li>\r\n\t<li>Darcy Regier: Dec. 2-12, 1995</li>\r\n\t<li>Don Maloney: Aug. 17, 1992 &ndash; Dec. 2, 1995</li>\r\n\t<li>Bill Torrey: Feb. 14, 1972 &ndash; Aug. 17, 1992</li>\r\n\t<li>* <em>Date range indicates first and last days of tenure</em></li>\r\n</ul>\r\n
    ##                                                                   heroImageUrl
    ## 1 https://records.nhl.com/site/asset/public/ext/hero/Team Pages/NYI/Barzal.jpg
    ##   mostRecentTeamId
    ## 1                2
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                   retiredNumbersSummary
    ## 1 <ul class="striped-list">\r\n\t<li>5 &ndash;&nbsp;Denis Potvin (1973-88)</li>\r\n\t<li>9 &ndash;&nbsp;Clark Gillies (1974-86)</li>\r\n\t<li>19 &ndash;&nbsp;Bryan Trottier (1975-90)</li>\r\n\t<li>22 &ndash;&nbsp;Mike Bossy (1977-87)</li>\r\n\t<li>23 &ndash;&nbsp;Bobby Nystrom (1972-86)</li>\r\n\t<li>27 &ndash;&nbsp;John Tonelli (1978-86)</li>\r\n\t<li>31 &ndash;&nbsp;Billy Smith (1972-89)</li>\r\n\t<li>91 &ndash;&nbsp;Butch Goring (1980-84)</li>\r\n</ul>\r\n
    ##   teamAbbrev       teamFullName
    ## 1        NYI New York Islanders

### Retriving Tables and Quering Information from NFL Stats API

Create a wrapper function to return the stats for a given team ID.

``` r
team_Stats <- function(teamID, ...) {
  # construct URLs
  full_url_stats <- function(teamID){
  base_url <- "https://statsapi.web.nhl.com/api/v1/teams/"
  tab_name <- "/?expand=team.stats"
  return(paste0(base_url, teamID, tab_name))
  }

  # retrieve the NHL stats information and convert it to data frames
  NHLstats <- function(full_url_stats){
  return(jsonlite::fromJSON(content(GET(full_url_stats),"text"), flatten=TRUE))
  }
  
  teamStatsDf <- NHLstats(full_url_stats(teamID))$teams[[8]][[1]]
  teamStatsDf$splits[[1]]
}

# Query NHL stats information according to the team ID or franchise ID given by a user
team_Stats(1)
```

    ##   stat.gamesPlayed stat.wins stat.losses stat.ot stat.pts stat.ptPctg stat.goalsPerGame
    ## 1               56        19          30       7       45        40.2             2.589
    ## 2               NA      28th        29th    15th     29th        29th              26th
    ##   stat.goalsAgainstPerGame stat.evGGARatio stat.powerPlayPercentage stat.powerPlayGoals
    ## 1                    3.375          0.8293                     14.2                  22
    ## 2                     28th            21st                     28th                28th
    ##   stat.powerPlayGoalsAgainst stat.powerPlayOpportunities stat.penaltyKillPercentage
    ## 1                         43                         155                       71.0
    ## 2                       30th                        23rd                       31st
    ##   stat.shotsPerGame stat.shotsAllowed stat.winScoreFirst stat.winOppScoreFirst
    ## 1           28.7857           31.0179              0.552                 0.111
    ## 2              24th              22nd               22nd                  31st
    ##   stat.winLeadFirstPer stat.winLeadSecondPer stat.winOutshootOpp stat.winOutshotByOpp
    ## 1                0.737                 0.733               0.211                0.417
    ## 2                 19th                  28th                31st                 31st
    ##   stat.faceOffsTaken stat.faceOffsWon stat.faceOffsLost stat.faceOffWinPercentage
    ## 1               3180             1481              1699                      46.6
    ## 2                8th             27th              30th                      27th
    ##   stat.shootingPctg stat.savePctg stat.penaltyKillOpportunities stat.savePctRank
    ## 1                 9         0.891                          <NA>             <NA>
    ## 2                NA            NA                           6th             29th
    ##   stat.shootingPctRank team.id         team.name       team.link
    ## 1                 <NA>       1 New Jersey Devils /api/v1/teams/1
    ## 2                 24th       1 New Jersey Devils /api/v1/teams/1

## Exploratory Data Analysis

### Tables Retrieved from NHL Records API

``` r
library(jsonlite)
library(httr)
# construct URLs
full_url_records <- function(tabl_name){
  base_url <- "https://records.nhl.com/site/api"
  return(paste0(base_url, tabl_name))
}
franchise <- full_url_records("/franchise")
franchise_totals <- full_url_records("/franchise-team-totals")
franchise_season <- full_url_records("/franchise-season-records")
goalie <- full_url_records("/franchise-goalie-records")
skater <- full_url_records("/franchise-skater-records")
admin <- full_url_records("/franchise-detail")

# retrieve the NHL records information and convert it to data frames
NHLrecords <- function(full_url_records){
  return(jsonlite::fromJSON(content(GET(full_url_records),"text"), flatten=TRUE))
}
franchiseDf <- NHLrecords(franchise)$data
franchise_totalsDf <- NHLrecords(franchise_totals)$data
franchise_seasonsDf <- NHLrecords(franchise_season)$data
goalieDf <- NHLrecords(goalie)$data
skaterDf <- NHLrecords(skater)$data
adminDf <- NHLrecords(admin)$data
```

``` r
#franchiseDf
#franchise_totalsDf
#franchise_seasonsDf
#goalieDf
#skaterDf
#adminDf
```

### Create Plots for NHL Records

Bar plot on goalie and skater information.

``` r
dim(goalieDf)
```

    ## [1] 1078   29

``` r
dim(skaterDf)
```

    ## [1] 17209    31

``` r
goalieDf_new <- cbind(goalieDf,df=rep(1,1078))
skaterDf_new <- cbind(skaterDf,df=rep(2,17209))

goalie_skater <- bind_rows(goalieDf_new, skaterDf_new)
dim(goalie_skater)
```

    ## [1] 18287    49

``` r
goalie_skater_new <- filter(goalie_skater, gameTypeId == 2, activePlayer == TRUE)
goalie_skater_new
```

    ##     id activePlayer  firstName franchiseId         franchiseName gameTypeId gamesPlayed
    ## 1  272         TRUE      Craig          30       Ottawa Senators          2         435
    ## 2  273         TRUE       Ryan          19        Buffalo Sabres          2         540
    ## 3  278         TRUE Marc-Andre          17   Pittsburgh Penguins          2         691
    ## 4  279         TRUE Marc-Andre          38  Vegas Golden Knights          2         192
    ## 5  280         TRUE      Pekka          34   Nashville Predators          2         683
    ## 6  282         TRUE   Jonathan          14     Los Angeles Kings          2         666
    ## 7  284         TRUE     Sergei          36 Columbus Blue Jackets          2         374
    ## 8  285         TRUE     Maxime          38  Vegas Golden Knights          2          17
    ## 9  286         TRUE      Oscar          38  Vegas Golden Knights          2           6
    ## 10 287         TRUE    Malcolm          38  Vegas Golden Knights          2          63
    ## 11 288         TRUE      Dylan          38  Vegas Golden Knights          2           1
    ## 12 292         TRUE     Henrik          10      New York Rangers          2         887
    ## 13 294         TRUE      Carey           1    Montréal Canadiens          2         707
    ## 14 295         TRUE        Ben          31   Tampa Bay Lightning          2         227
    ## 15 318         TRUE      Corey          11    Chicago Blackhawks          2         488
    ## 16 320         TRUE   Jaroslav          18       St. Louis Blues          2         159
    ## 17 321         TRUE     Tuukka           6         Boston Bruins          2         560
    ## 18 324         TRUE      James           5   Toronto Maple Leafs          2         207
    ## 19 325         TRUE     Semyon          27    Colorado Avalanche          2         389
    ## 20 326         TRUE     Braden          24   Washington Capitals          2         468
    ##     lastName losses                                      mostGoalsAgainstDates
    ## 1   Anderson    168 2020-03-03, 2016-12-05, 2015-12-29, 2015-11-10, 2014-04-04
    ## 2     Miller    186                         2011-02-13, 2008-11-19, 2003-12-10
    ## 3     Fleury    216                                                 2006-01-28
    ## 4     Fleury     60                                                 2019-12-23
    ## 5      Rinne    213                                                 2020-03-02
    ## 6      Quick    249                                                 2019-10-09
    ## 7  Bobrovsky    130                                     2018-12-04, 2018-10-13
    ## 8     Lagace      8                                                 2017-11-14
    ## 9      Dansk      1                                                 2019-10-21
    ## 10    Subban     21                                                 2018-11-19
    ## 11  Ferguson      0                                                 2017-11-14
    ## 12 Lundqvist    310                                     2008-12-12, 2006-12-16
    ## 13     Price    257                                     2019-03-08, 2011-02-09
    ## 14    Bishop     64                                     2016-10-30, 2013-04-11
    ## 15  Crawford    162 2020-02-25, 2019-03-02, 2018-11-29, 2018-11-27, 2018-10-27
    ## 16     Halak     47                                                 2010-11-17
    ## 17      Rask    163                                                 2014-12-04
    ## 18    Reimer     76                                                 2016-02-15
    ## 19  Varlamov    156                                                 2013-12-05
    ## 20    Holtby    122                         2020-02-08, 2019-01-22, 2018-10-04
    ##    mostGoalsAgainstOneGame                                 mostSavesDates mostSavesOneGame
    ## 1                        7                                     2018-10-28               49
    ## 2                        7                                     2013-12-29               49
    ## 3                        7                                     2008-10-11               47
    ## 4                        7                                     2017-10-06               45
    ## 5                        8                                     2011-02-15               48
    ## 6                        8                                     2010-12-13               51
    ## 7                        8                                     2014-12-04               52
    ## 8                        7                                     2017-12-09               36
    ## 9                        6                                     2017-10-27               32
    ## 10                       7                                     2018-03-22               42
    ## 11                       1                                     2017-11-14                1
    ## 12                       8             2018-03-02, 2018-02-28, 2010-03-04               50
    ## 13                       8                                     2009-11-14               53
    ## 14                       6                                     2014-01-19               48
    ## 15                       6                                     2019-03-16               48
    ## 16                       7                                     2012-03-13               43
    ## 17                       7                                     2014-03-30               49
    ## 18                       7 2015-04-04, 2013-11-23, 2013-04-20, 2012-02-04               49
    ## 19                       8                                     2017-11-02               57
    ## 20                       7                                     2013-11-17               46
    ##                             mostShotsAgainstDates mostShotsAgainstOneGame
    ## 1                          2018-11-23, 2018-10-28                      53
    ## 2                                      2013-11-05                      51
    ## 3                                      2006-01-28                      51
    ## 4                                      2018-12-22                      47
    ## 5                          2011-02-15, 2009-02-10                      50
    ## 6                                      2010-12-13                      51
    ## 7                                      2014-12-04                      55
    ## 8                                      2017-12-09                      39
    ## 9                                      2019-10-21                      37
    ## 10                         2018-03-22, 2017-12-08                      44
    ## 11                                     2017-11-14                       2
    ## 12                         2018-02-28, 2010-03-04                      55
    ## 13                                     2009-11-14                      55
    ## 14                                     2014-01-19                      51
    ## 15                                     2019-03-16                      48
    ## 16                                     2012-03-13                      46
    ## 17                                     2014-03-30                      52
    ## 18 2015-04-04, 2013-12-07, 2013-11-23, 2013-04-20                      50
    ## 19                                     2017-11-02                      60
    ## 20                         2013-11-17, 2013-10-22                      47
    ##    mostShutoutsOneSeason mostShutoutsSeasonIds mostWinsOneSeason mostWinsSeasonIds
    ## 1                      5              20162017                33          20112012
    ## 2                      6              20112012                41          20092010
    ## 3                     10              20142015                42          20112012
    ## 4                      8              20182019                35          20182019
    ## 5                      8              20172018                43          20112012
    ## 6                     10              20112012                40          20152016
    ## 7                      9              20182019                41          20162017
    ## 8                      0    20172018, 20182019                 6          20172018
    ## 9                      1              20172018                 3          20172018
    ## 10                     1              20182019                13          20172018
    ## 11                     0              20172018                 0          20172018
    ## 12                    11              20102011                39          20112012
    ## 13                     9              20142015                44          20142015
    ## 14                     6              20152016                40          20142015
    ## 15                     7              20152016                35          20152016
    ## 16                     7              20102011                27          20102011
    ## 17                     8              20162017                37          20162017
    ## 18                     4              20122013                20          20102011
    ## 19                     5              20142015                41          20132014
    ## 20                     9    20142015, 20162017                48          20152016
    ##    overtimeLosses playerId positionCode rookieGamesPlayed rookieShutouts rookieWins seasons
    ## 1              46  8467950            G                NA             NA         NA      10
    ## 2              56  8468011            G                48              1         30      11
    ## 3              66  8470594            G                50              1         13      13
    ## 4              14  8470594            G                NA             NA         NA       4
    ## 5              75  8471469            G                52              7         29      15
    ## 6              69  8471734            G                44              4         21      14
    ## 7              27  8475683            G                NA             NA         NA       7
    ## 8               1  8476509            G                16              0          6       2
    ## 9               0  8476861            G                 4              1          3       3
    ## 10              7  8476876            G                22              1         13       3
    ## 11              0  8480263            G                 1              0          0       1
    ## 12             96  8468685            G                53              2         30      15
    ## 13             79  8471679            G                41              3         24      14
    ## 14             20  8471750            G                NA             NA         NA       5
    ## 15             53  8470645            G                57              4         33      13
    ## 16             19  8470860            G                NA             NA         NA       4
    ## 17             66  8471695            G                45              5         22      14
    ## 18             23  8473503            G                37              3         20       6
    ## 19             38  8473575            G                NA             NA         NA       8
    ## 20             46  8474651            G                14              2         10      10
    ##    shutouts ties wins df assists goals mostAssistsGameDates mostAssistsOneGame
    ## 1        28    0  202  1      NA    NA                 <NA>                 NA
    ## 2        28    1  284  1      NA    NA                 <NA>                 NA
    ## 3        44    2  375  1      NA    NA                 <NA>                 NA
    ## 4        23    0  117  1      NA    NA                 <NA>                 NA
    ## 5        60    0  369  1      NA    NA                 <NA>                 NA
    ## 6        54    0  336  1      NA    NA                 <NA>                 NA
    ## 7        33    0  213  1      NA    NA                 <NA>                 NA
    ## 8         0    0    6  1      NA    NA                 <NA>                 NA
    ## 9         1    0    4  1      NA    NA                 <NA>                 NA
    ## 10        1    0   30  1      NA    NA                 <NA>                 NA
    ## 11        0    0    0  1      NA    NA                 <NA>                 NA
    ## 12       64    0  459  1      NA    NA                 <NA>                 NA
    ## 13       49    0  360  1      NA    NA                 <NA>                 NA
    ## 14       17   NA  131  1      NA    NA                 <NA>                 NA
    ## 15       26    0  260  1      NA    NA                 <NA>                 NA
    ## 16       20   NA   83  1      NA    NA                 <NA>                 NA
    ## 17       52    0  306  1      NA    NA                 <NA>                 NA
    ## 18       11   NA   85  1      NA    NA                 <NA>                 NA
    ## 19       21    0  183  1      NA    NA                 <NA>                 NA
    ## 20       35    0  282  1      NA    NA                 <NA>                 NA
    ##    mostAssistsOneSeason mostAssistsSeasonIds mostGoalsGameDates mostGoalsOneGame
    ## 1                    NA                 <NA>               <NA>               NA
    ## 2                    NA                 <NA>               <NA>               NA
    ## 3                    NA                 <NA>               <NA>               NA
    ## 4                    NA                 <NA>               <NA>               NA
    ## 5                    NA                 <NA>               <NA>               NA
    ## 6                    NA                 <NA>               <NA>               NA
    ## 7                    NA                 <NA>               <NA>               NA
    ## 8                    NA                 <NA>               <NA>               NA
    ## 9                    NA                 <NA>               <NA>               NA
    ## 10                   NA                 <NA>               <NA>               NA
    ## 11                   NA                 <NA>               <NA>               NA
    ## 12                   NA                 <NA>               <NA>               NA
    ## 13                   NA                 <NA>               <NA>               NA
    ## 14                   NA                 <NA>               <NA>               NA
    ## 15                   NA                 <NA>               <NA>               NA
    ## 16                   NA                 <NA>               <NA>               NA
    ## 17                   NA                 <NA>               <NA>               NA
    ## 18                   NA                 <NA>               <NA>               NA
    ## 19                   NA                 <NA>               <NA>               NA
    ## 20                   NA                 <NA>               <NA>               NA
    ##    mostGoalsOneSeason mostGoalsSeasonIds mostPenaltyMinutesOneSeason
    ## 1                  NA               <NA>                          NA
    ## 2                  NA               <NA>                          NA
    ## 3                  NA               <NA>                          NA
    ## 4                  NA               <NA>                          NA
    ## 5                  NA               <NA>                          NA
    ## 6                  NA               <NA>                          NA
    ## 7                  NA               <NA>                          NA
    ## 8                  NA               <NA>                          NA
    ## 9                  NA               <NA>                          NA
    ## 10                 NA               <NA>                          NA
    ## 11                 NA               <NA>                          NA
    ## 12                 NA               <NA>                          NA
    ## 13                 NA               <NA>                          NA
    ## 14                 NA               <NA>                          NA
    ## 15                 NA               <NA>                          NA
    ## 16                 NA               <NA>                          NA
    ## 17                 NA               <NA>                          NA
    ## 18                 NA               <NA>                          NA
    ## 19                 NA               <NA>                          NA
    ## 20                 NA               <NA>                          NA
    ##    mostPenaltyMinutesSeasonIds mostPointsGameDates mostPointsOneGame mostPointsOneSeason
    ## 1                         <NA>                <NA>                NA                  NA
    ## 2                         <NA>                <NA>                NA                  NA
    ## 3                         <NA>                <NA>                NA                  NA
    ## 4                         <NA>                <NA>                NA                  NA
    ## 5                         <NA>                <NA>                NA                  NA
    ## 6                         <NA>                <NA>                NA                  NA
    ## 7                         <NA>                <NA>                NA                  NA
    ## 8                         <NA>                <NA>                NA                  NA
    ## 9                         <NA>                <NA>                NA                  NA
    ## 10                        <NA>                <NA>                NA                  NA
    ## 11                        <NA>                <NA>                NA                  NA
    ## 12                        <NA>                <NA>                NA                  NA
    ## 13                        <NA>                <NA>                NA                  NA
    ## 14                        <NA>                <NA>                NA                  NA
    ## 15                        <NA>                <NA>                NA                  NA
    ## 16                        <NA>                <NA>                NA                  NA
    ## 17                        <NA>                <NA>                NA                  NA
    ## 18                        <NA>                <NA>                NA                  NA
    ## 19                        <NA>                <NA>                NA                  NA
    ## 20                        <NA>                <NA>                NA                  NA
    ##    mostPointsSeasonIds penaltyMinutes points rookiePoints
    ## 1                 <NA>             NA     NA           NA
    ## 2                 <NA>             NA     NA           NA
    ## 3                 <NA>             NA     NA           NA
    ## 4                 <NA>             NA     NA           NA
    ## 5                 <NA>             NA     NA           NA
    ## 6                 <NA>             NA     NA           NA
    ## 7                 <NA>             NA     NA           NA
    ## 8                 <NA>             NA     NA           NA
    ## 9                 <NA>             NA     NA           NA
    ## 10                <NA>             NA     NA           NA
    ## 11                <NA>             NA     NA           NA
    ## 12                <NA>             NA     NA           NA
    ## 13                <NA>             NA     NA           NA
    ## 14                <NA>             NA     NA           NA
    ## 15                <NA>             NA     NA           NA
    ## 16                <NA>             NA     NA           NA
    ## 17                <NA>             NA     NA           NA
    ## 18                <NA>             NA     NA           NA
    ## 19                <NA>             NA     NA           NA
    ## 20                <NA>             NA     NA           NA
    ##  [ reached 'max' / getOption("max.print") -- omitted 2096 rows ]

``` r
g <- ggplot(data=franchise_totalsDf, aes(x=gameTypeId))
g + geom_bar(aes(fill=wins)) +
  labs(x="game Type Id")
```

![](README_files/figure-gfm/franchise_totals%20bar%20plot-1.png)<!-- -->

Histogram plot on goalie and skater information.

Box plot on goalie and skater information.

Scatter plot on goalie and skater information.
