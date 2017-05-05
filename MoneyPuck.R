library(readr)
NHL_Skaters <- read_csv("~/Documents/NHL_Stats.csv")
View(NHL_Skaters)
##Skaters

NHL_Goalies <- read_csv("~/Documents/NHL_Stats_Goalies.csv")
View(NHL_Goalies)
##Goalies

summary(NHL_Skaters)
summary(NHL_Goalies)
#Summary Statistics by position

'+/-mean' <- (mean(NHL_Skaters$`+/-`))
'+/-sd' <- (sd(NHL_Skaters$`+/-`))
##Coding mean and SD for single variables, in this case +/-

z.score <- function(x){
  (x-mean(x))/sd(x)
}
##Coding z-score as a function

z.score(NHL_Skaters$`+/-`)
##Using that function to create z-scores for a single variable

scale(NHL_Skaters$`+/-`)
##Different code to find the z-scores of a variable

Age.z <- scale(NHL_Skaters$Age)
Games.z <- scale(NHL_Skaters$GP)
Goals.z <- scale(NHL_Skaters$G)
Assists.z <- scale(NHL_Skaters$A)
PlusMinus.z <- scale(NHL_Skaters$`+/-`)
PlusMinusAdj.z <- scale(NHL_Skaters$PlusMinusAdj)
PenMins.z <- scale(NHL_Skaters$PIM)
Shots.z <- scale(NHL_Skaters$S)
ShotPerc.z <- scale(NHL_Skaters$`S%`)
IceTime.z <- scale(NHL_Skaters$TOI)
TimePerGame.z <- scale(NHL_Skaters$ATOI)
Blocks.z <- scale(NHL_Skaters$BLK)
Hits.z <- scale(NHL_Skaters$HIT)
FaceOffs.z <- scale(NHL_Skaters$`FO%`)
FaceOffWins.z <- scale(NHL_Skaters$FOW)
SkaterSalary.z <- scale(NHL_Skaters$Salary)
TeamPoints.z <- scale(NHL_Skaters$TmPoints)
GoalPerGame.z <- scale(NHL_Skaters$GPG)
AsstsPerGame.z <- scale(NHL_Skaters$APG)
PenMinsPerGame.z <- scale(NHL_Skaters$PenPG)
ShotPerGame.z <- scale(NHL_Skaters$SPG)
BlocksPerGame.z <- scale(NHL_Skaters$BPG)
HitsPerGame.z <- scale(NHL_Skaters$HPG)
##Z-scores for all relevant skater data

GoalieAge.z <- scale(NHL_Goalies$Age)
Starts.z <- scale(NHL_Goalies$GS)
Wins.z <- scale(NHL_Goalies$W)
Saves.z <- scale(NHL_Goalies$SV)
SavePerc.z <- scale(NHL_Goalies$`SV%`)
GoalsAgainstAvg.z <- scale(NHL_Goalies$GAA)
Shutouts.z <- scale(NHL_Goalies$SO)
ShutoutPerc.z <- scale(NHL_Goalies$SOPerc)
GoalieSalary.z <- scale(NHL_Goalies$Salary)
SavePerGame.z <- scale(NHL_Goalies$SVPG)
QualityStartPerc.z <- scale(NHL_Goalies$`QS%`)
##Z-scores for all relevant goalie data

NHL_Skaters$Salary <- c(0.6, 0.675, 0.8325, 0.8325, 0.8325, 0.8325, 0.775, 2.6, 0.8325, 1.7,
                           0.9, 3.25, 1, 0.8325, 0.7, 0.65, 0.8325, 0.66, 4, 0.575, 0.825,
                           3, 5, 1.75, 0.8325, 3.1, 4, 3, 3.8, 6, 7.875, 9.25, 3.2, 0.65,
                           0.8325, 0.874125, 0.95, 0.8075, 0.675, 0.725, 1, 2.25, 0.8325,
                           0.8325, 0.8325, 2.4, 3, 0.8325, 0.8325, 0.625, 2.5, 0.7, 2, 5.375,
                           0.8325, 6, 1, 0.8325, 0.675, 0.7, 0.8075, 0.8, 0.6, 0.6, 0.7, 2,
                           2, 0.8, 4, 3, 2.5, 0.75, 0.9, 0.6, 0.625, 0.9, 0.75, 0.9, 5, 5,
                           1.1, 4, 8.75, 7.25, 0.8325, 5, 0.65, 0.8325, 0.575, 0.575, 0.575,
                           0.8325, 0.6, 0.7, 0.8325, 0.8325, 0.775, 0.6, 0.65, 4.5, 3.9, 0.75,
                           0.6, 0.65, 0.6525, 5.25, 0.95, 2.65, 1.15, 2, 1.25, 2.25, 3, 2,
                           6, 5.4, 6, 0.8325, 7.5, 0.8325, 0.7, 0.6, 0.65, 0.65, 0.575, 0.635,
                           4.5, 0.6, 0.75, 0.95, 0.675, 0.75, 0.775, 1.1, 1.25, 0.8, 0.7,
                           0.8325, 0.6, 0.8325, 5.5, 2.5, 0.8325, 2.5, 6, 4, 0.8325, 6, 0.6,
                           0.8325, 0.8325, 0.65, 0.6325, 0.7, 2.825, 0.6, 0.6, 2.2, 5, 3.5,
                           1, 0.687333, 2.9, 5, 5.85, 5.4, 0.8325, 0.65, 5.5, 5.75, 0.925,
                           3.5, 0.625, 0.7, 0.8325, 0.575, 0.65, 0.8325, 0.6125, 0.575, 0.6,
                           0.65, 2.2, 2.75, 6, 3, 0.8, 4.5, 0.95, 0.8325, 4.8375, 0.95, 4.75,
                           5, 0.8325, 5.5, 3.575, 6.5, 3.75, 0.575, 0.8, 0.6, 0.75, 0.8325,
                           0.675, 0.8325, 0.6075, 0.575, 0.575, 0.75, 0.575, 2, 1.5, 3.6,
                           0.8325, 0.8325, 6.875, 0.875, 4.55, 4, 6, 10.5, 0.8325, 10.5,
                           0.874125, 0.575, 0.8, 0.8325, 0.675, 1.65, 0.8325, 0.575, 0.8,
                           0.8325, 1.9, 2.5, 0.8325, 0.8, 2, 4.75, 6, 4.5, 0.65, 2.4, 1.3,
                           5.5, 5, 0.8325, 6, 5, 0.6, 0.8325, 0.625, 0.8325, 0.9, 0.8325,
                           0.67, 0.675, 4, 0.91875, 0.65, 2, 3.65, 3.5, 0.625357, 0.6, 0.71,
                           5, 0.875, 2.2, 0.8325, 0.775, 2.5, 7.5, 5.75, 6, 0.8325, 0.6,
                           0.6, 0.65, 0.6, 0.575, 0.6, 0.575, 0.575, 0.6, 1.025, 4, 0.715,
                           4, 1.875, 5.5, 0.6, 0.675, 4, 5, 0.575, 0.8325, 6, 0.8325, 5, 2.75,
                           4.25, 7.75, 0.925, 0.585, 0.8325, 3.5, 1.05, 0.725, 0.95, 1.85,
                           0.8325, 0.8325, 0.8325, 0.725, 3.1, 4, 0.8325, 0.8325, 3, 1.5,
                           6.5, 1.8, 3, 2, 6, 6, 6, 0.8325, 0.8325, 0.705, 0.8325, 0.575,
                           0.8325, 0.65, 3, 0.6, 0.8325, 0.575, 1.2, 3.35, 1.3, 1.25, 0.8325,
                           0.8325, 0.95, 4, 3.5, 4, 3.5, 6.35, 4, 0.75, 4.5, 3.75, 1.6,
                           0.8325, 0.6, 2.5, 1.55, 1.4, 0.8325, 0.575, 0.925, 1, 0.575, 1.25,
                           0.6, 0.65, 6.075, 0.63, 2, 4, 3.9, 7, 3.9, 7.45, 0.735833, 10,
                           7, 0.8325, 0.64, 0.6, 0.65, 0.65, 0.8325, 0.925, 0.6, 0.65, 0.8325,
                           0.85, 0.6, 3.75, 0.65, 1.2, 4, 1.05, 2.375, 4.25, 9, 9, 6.25, 2,
                           2.6, 2.66667, 5.4, 3.5, 3.2, 0.8325, 0.55, 0.6, 0.6, 0.575, 0.8325,
                           0.8325, 0.575, 0.6, 0.575, 0.8325, 0.625, 4.2, 0.95, 1.3, 1, 6,
                           7, 0.8325, 4.5, 3, 4.25, 0.875, 7.857143, 1, 2.85, 5.75, 5, 0.635,
                           0.575, 0.625, 0.8325, 0.7, 0.675, 0.6, 0.6, 0.575, 0.75, 0.8, 0.7,
                           1, 2.5, 0.8325, 0.75, 0.625, 1.25, 1.1, 5, 0.8325, 0.725, 1.75,
                           0.8325, 0.6, 5, 4.5, 6.5, 6, 4.5, 0.575, 0.575, 0.575, 0.575, 0.65,
                           0.575, 0.575, 0.6, 0.575, 1.833333, 0.8425, 0.575, 0.6125, 0.575,
                           0.575, 0.575, 0.8325, 0.575, 3, 3.5, 4, 2, 4, 2.5, 9, 5, 4, 4.25,
                           6, 6, 0.575, 0.8325, 0.8, 0.575, 0.8325, 0.575, 0.575, 0.8325,
                           0.8325, 4.875, 0.575, 0.8, 2.2, 3.5, 1, 7, 4.25, 0.8325, 2.5, 2.5,
                           2, 5.5, 2, 2.5, 4.5, 3.5, 3.7, 6, 0.65, 0.6, 0.6, 1, 5, 0.6, 2.75,
                           5, 1.1, 0.7, 0.8325, 1, 0.8325, 1.65, 3.25, 8, 0.8325, 1.65, 4.7,
                           2.1, 4, 8, 2.5, 4.5, 0.925, 0.7, 0.75, 0.6, 0.65, 4.5, 0.575, 0.65,
                           0.575, 0.8325, 0.725, 0.874125, 1.1, 1.5, 0.7, 4.9, 0.9, 2.25,
                           0.8, 0.8, 7, 6.5, 0.75, 2.35, 1.1, 4, 3.75, 4, 3.8, 7, 0.925,
                           0.8325, 0.8325, 0.8325, 0.625, 0.95, 0.76815, 2.25, 0.8325, 0.725,
                           2.35, 0.85, 0.65, 0.725, 2.75, 0.84, 4.75, 4, 4, 3.35, 0.8325,
                           0.8325, 2, 0.8325, 4.3, 5.125, 9, 4.25, 0.7, 0.8325, 0.575, 0.575,
                           0.575, 0.575, 0.12776, 0.575, 0.6675, 0.575, 4.083, 0.575, 0.575,
                           0.8, 0.625, 2.7, 3.667, 2.3, 0.59, 0.62, 3.85, 1, 0.575, 7.25,
                           2.1, 4.75, 1.4, 0.575, 7, 9.5, 10.9, 0.675, 0.575, 0.7, 0.8325,
                           0.925, 0.6, 0.8325, 0.6, 2.75, 0.625, 3.8, 0.8325, 2.25, 0.575,
                           2.8, 1.85, 0.7, 5, 5.2, 4.25, 3, 6.5, 6.75, 6, 6, 5.76, 0.575,
                           0.6, 2.725, 0.575, 0.8325, 0.575, 1.05, 2.9, 2.5, 1, 0.65, 1.125,
                           0.7, 5.6, 0.9, 1, 0.9, 4.4, 0.8325, 3.7, 0.8325, 7, 3.75, 7,
                           6.5, 5.25, 8, 0.575, 1.25, 0.8325, 0.575, 0.575, 0.8325, 0.575,
                           0.8325, 0.575, 0.575, 6.5, 1.25, 0.6, 3.6, 0.8, 0.615, 3.7, 1.6,
                           8.5, 4.5, 1.9, 4.45, 0.575, 4, 4, 0.8325, 4.25, 4.25, 0.8325,
                           4, 0.8325, 1.2, 0.65, 0.65, 2.5, 0.575, 2.25, 1.2, 5, 0.925,
                           2.95, 0.8325, 0.65, 4.05, 4.2, 0.8325, 0.8325, 4.5, 5, 0.8325,
                           0.575, 0.6, 0.65, 0.8, 0.95, 0.65, 0.625, 0.8125, 0.296102,
                           0.8325, 0.650, 0.20556, 0.675, 1.25, 0.625, 0.27776, 0.2, 0.6,
                           1.25, 0.17776, 0.7, 0.14224, 1.1, 0.7825, 0.574986, 0.6, 3.75,
                           2.75, 0.9, 0.7425, 0.715, 0.6, 2, 0.6, 1.3, 2.6, 0.8, 0.735,
                           2.8, 4, 1.25, 1.55, 1, 2.1, 1.5, 0.725, 2.5, 1.1, 1, 2, 4.35,
                           0.335843, 2, 2.501625, 2.75, 1.25, 4.5, 3.5, 5, 1.3, 1, 4.25,
                           0.6325, 0.575, 0.8325, 0.575, 0.6, 0.575, 0.8325, 0.95, 0.6, 0.7,
                           2.5, 0.8325, 0.8325, 1.025, 3.5, 0.6, 0.6, 0.925, 0.7, 3.5, 3.1,
                           0.8325, 5, 0.8325, 6, 0.85, 3.3, 1.5, 7, 7, 0.8325, 0.8325, 0.575,
                           0.65, 0.6, 0.6, 0.874125, 2.7, 1.2, 5, 0.8325, 0.8325, 0.85,
                           2.125, 0.75, 0.925, 5.75, 0.8325, 0.875, 0.8325, 1, 2.5, 3, 5,
                           7, 0.8325, 0.8325, 5.8, 5, 0.8425, 0.575, 0.575, 0.575, 0.575,
                           0.575, 0.775, 0.8325, 2.8, 5.5, 0.875, 1.75, 0.85, 2, 4.25, 1.75,
                           2.75, 0.8325, 4, 5.75, 3.25, 4.5, 4.25, 3.4, 10, 7)
##Salary Data for all skaters

NHL_Goalies$Salary <- c(0.6, 1.5, 4.15, 6.5, 0.6, 0.6, 1, 0.8325, 7.5, 0.8325, 1.2, 0.8,
                           1, 3.15, 3.5, 0.7, 0.7, 2.55, 0, 0.7, 0.65, 8.5, 0.8325, 1.7,
                           0.8325, 2.7, 0.6, 6, 6, 0.675, 0.925, 0.65, 4.5, 6, 3.85, 5.5,
                           0.575, 4.166666, 0.8, 0.75, 5, 6.714, 1.5, 0.9, 7, 0.575, 0.65,
                           1.55, 5, 7, 0.95, 0.8325, 6, 0.75, 0.575, 7, 0.575, 4.75, 2, 0.675,
                           1, 9.5, 0.6, 1.35, 0.78, 4.75, 0.725, 1.75, 4.1, 0.575, 0.55,
                           5.75, 3, 0.575, 1.25, 0.70875, 2.2, 0.8325, 0.575, 0.75, 0.68,
                           5, 0.8, 0.575, 0.6, 5.95, 6, 1.6, 0.575, 4.75, 1, 0.6925, 0.575,
                           7, 0.85)
##Salary Data for all goalies

summary(NHL_Skaters$Salary)
summary(NHL_Goalies$Salary)
##Summary Stats for both sets of salary data

NHL_Skaters$TmPoints <- c(105, 105, 105, 105, 105, 105, 105, 105, 105, 105, 105, 105, 105,
                             105, 105, 105, 105, 105, 105, 105, 105, 105, 105, 105, 105,
                             105, 105, 105, 105, 105, 105, 105, 70, 70, 70, 70, 70, 70, 70,
                             70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70,
                             70, 70, 70, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
                             95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
                             95, 95, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78,
                             78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78,
                             78, 78, 78, 78, 78, 78, 87, 87, 87, 87, 87, 87, 87, 87, 87,
                             87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87,
                             87, 87, 87, 87, 108, 108, 108, 108, 108, 108, 108, 108, 108,
                             108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108,
                             108, 108, 108, 94, 94, 94, 94, 94, 94, 94, 94, 94, 94, 94,
                             94, 94, 94, 94, 94, 94, 94, 94, 94, 94, 94, 94, 94, 94, 94, 94,
                             109, 109, 109, 109, 109, 109, 109, 109, 109, 109, 109, 109,
                             109, 109, 109, 109, 109, 109, 109, 109, 109, 109, 109, 109, 
                             109, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48,
                             48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 79, 79, 79,
                             79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79,
                             79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79,
                             79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79,
                             79, 79, 79, 79, 79, 79, 103, 103, 103, 103, 103, 103, 103,
                             103, 103, 103, 103, 103, 103, 103, 103, 103, 103, 103, 103,
                             103, 103, 103, 103, 103, 103, 103, 103, 81, 81, 81, 81, 81,
                             81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81,
                             81, 81, 81, 81, 81, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86,
                             86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86,
                             106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106,
                             106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106,
                             106, 106, 106, 106, 103, 103, 103, 103, 103, 103, 103, 103,
                             103, 103, 103, 103, 103, 103, 103, 103, 103, 103, 103, 103,
                             103, 103, 103, 103, 103, 103, 103, 103, 70, 70, 70, 70, 70,
                             70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70,
                             70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 94, 94, 94, 94, 94,
                             94, 94, 94, 94, 94, 94, 94, 94, 94, 94, 94, 94, 94, 94, 94,
                             94, 94, 94, 94, 94, 94, 94, 94, 94, 94, 94, 94, 94, 94, 94,
                             94, 94, 94, 94, 94, 94, 94, 94, 94, 94, 94, 94, 94, 94, 94,
                             94, 94, 94, 94, 94, 94, 94, 94, 102, 102, 102, 102, 102, 102,
                             102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102,
                             102, 102, 102, 102, 102, 102, 98, 98, 98, 98, 98, 98, 98, 98,
                             98, 98, 98, 98, 98, 98, 98, 98, 98, 98, 98, 98, 98, 98, 98,
                             98, 98, 98, 98, 98, 98, 98, 88, 88, 88, 88, 88, 88, 88, 88,
                             88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88,
                             88, 88, 88, 88, 88, 111, 111, 111, 111, 111, 111, 111, 111,
                             111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111,
                             111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 99, 99,
                             99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
                             99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
                             99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
                             99, 99, 99, 94, 94, 94, 94, 94, 94, 94, 94, 94, 94, 94, 94, 94,
                             94, 94, 94, 94, 94, 94, 94, 94, 94, 94, 94, 94, 94, 94, 94, 95,
                             95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
                             95, 95, 95, 95, 95, 94.3, 103, 102.6, 82.7, 93, 92.55, 99.9, 94,
                             103, 71, 80.28, 96.5, 91.2, 78, 82.6, 89, 97.6, 97.75, 72, 85,
                             85.3, 55, 84.3, 111.7, 95, 87, 85, 87, 100.5, 99, 71.2, 81.5,
                             110.9, 77, 72.5, 98.5, 73.5, 61, 103, 77, 80, 78, 80, 89, 89.7,
                             97, 92.5, 83.5, 83, 79, 90.5, 81, 94.3, 94, 57, 73, 74.5, 79.5,
                             90, 80.5, 85.5, 103.5, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69,
                             69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69,
                             69, 69, 69, 69, 69, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87,
                             87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87,
                             87, 118, 118, 118, 118, 118, 118, 118, 118, 118, 118, 118, 118,
                             118, 118, 118, 118, 118, 118, 118, 118, 118, 118, 118, 118, 118,
                             118)
##Points (standings) for each player's team
##Players who played for more than one got the weighted average depending on how many games
##they played for each

lm(NHL_Skaters$Salary ~ NHL_Skaters$Age + NHL_Skaters$GP + NHL_Skaters$G + NHL_Skaters$A
   + NHL_Skaters$PIM + NHL_Skaters$S + NHL_Skaters$`S%` + NHL_Skaters$TOI + NHL_Skaters$BLK
   + NHL_Skaters$HIT + NHL_Skaters$FOW + NHL_Skaters$`+/-`)
summary(lm(NHL_Skaters$Salary ~ NHL_Skaters$Age + NHL_Skaters$GP + NHL_Skaters$G + NHL_Skaters$A
           + NHL_Skaters$PIM + NHL_Skaters$S + NHL_Skaters$`S%` + NHL_Skaters$TOI + NHL_Skaters$BLK
           + NHL_Skaters$HIT + NHL_Skaters$FOW + NHL_Skaters$`+/-`))
##Initial Multiple Regression for statistics on salary (all skaters)

lm(NHL_Skaters$TmPoints ~ NHL_Skaters$Age + NHL_Skaters$GP + NHL_Skaters$G + NHL_Skaters$A
   + NHL_Skaters$PIM + NHL_Skaters$S + NHL_Skaters$`S%` + NHL_Skaters$TOI + NHL_Skaters$BLK
   + NHL_Skaters$HIT + NHL_Skaters$FOW + NHL_Skaters$`+/-`)
summary(lm(NHL_Skaters$TmPoints ~ NHL_Skaters$Age + NHL_Skaters$GP + NHL_Skaters$G + NHL_Skaters$A
           + NHL_Skaters$PIM + NHL_Skaters$S + NHL_Skaters$`S%` + NHL_Skaters$TOI + NHL_Skaters$BLK
           + NHL_Skaters$HIT + NHL_Skaters$FOW + NHL_Skaters$`+/-`))
##Initial Multiple Regression for statistics on team points (all skaters)

lm(NHL_Goalies$Salary ~ NHL_Goalies$Age + NHL_Goalies$GS + NHL_Goalies$W
   + NHL_Goalies$SV + NHL_Goalies$`SV%` + NHL_Goalies$GAA + NHL_Goalies$SO)
summary(lm(NHL_Goalies$Salary ~ NHL_Goalies$Age + NHL_Goalies$GS + NHL_Goalies$W 
           + NHL_Goalies$SV + NHL_Goalies$`SV%` + NHL_Goalies$GAA + NHL_Goalies$SO))
##Initial Multiple Regression for statistics on salary (all goalies)

lm(NHL_Goalies$W ~ NHL_Goalies$Age + NHL_Goalies$`SV%` + NHL_Goalies$GAA + NHL_Goalies$`QS%`
   + NHL_Goalies$SO)
summary(lm(NHL_Goalies$W ~ NHL_Goalies$Age + NHL_Goalies$`SV%` + NHL_Goalies$GAA 
           + NHL_Goalies$`QS%` + NHL_Goalies$SO))
##Initial Multiple regression for statistics on wins (all goalies)

RightWing <- NHL_Skaters$GP > 16 & NHL_Skaters$Pos=='RW'
LeftWing <- NHL_Skaters$GP > 16 & NHL_Skaters$Pos=='LW'
Center <- NHL_Skaters$GP > 16 & NHL_Skaters$Pos=='C'
Forward <- RightWing | LeftWing | Center
##Defining Forward as an LW, C, or RW playing more than 16 games

Ducks <- NHL_Skaters$Tm=='ANA'
Coyotes <- NHL_Skaters$Tm=='ARI'
Bruins <- NHL_Skaters$Tm=='BOS'
Sabres <- NHL_Skaters$Tm=='BUF'
Hurricanes <- NHL_Skaters$Tm=='CAR'
BlueJackets <- NHL_Skaters$Tm=='CBJ'
Flames <- NHL_Skaters$Tm=='CGY'
BlackHawks <- NHL_Skaters$Tm=='CHI'
Avalanche <- NHL_Skaters$Tm=='COL'
DallasStars <- NHL_Skaters$Tm=='DAL'
RedWings <- NHL_Skaters$Tm=='DET'
Oilers <- NHL_Skaters$Tm=='EDM'
Panthers <- NHL_Skaters$Tm=='FLA'
Kings <- NHL_Skaters$Tm=='LAK'
Wild <- NHL_Skaters$Tm=='MIN'
Canadiens <- NHL_Skaters$Tm=='MTL'
Devils <- NHL_Skaters$Tm=='NJD'
Predators <- NHL_Skaters$Tm=='NSH'
Islanders <- NHL_Skaters$Tm=='NYI'
Rangers <- NHL_Skaters$Tm=='NYR'
Senators <- NHL_Skaters$Tm=='OTT'
Flyers <- NHL_Skaters$Tm=='PHI'
Penguins <- NHL_Skaters$Tm=='PIT'
Sharks <- NHL_Skaters$Tm=='SJS'
Blues <- NHL_Skaters$Tm=='STL'
Lightning <- NHL_Skaters$Tm=='TBL'
MapleLeafs <- NHL_Skaters$Tm=='TOR'
FreeAgents <- NHL_Skaters$Tm=='TOT'
Canucks <- NHL_Skaters$Tm=='VAN'
Jets <- NHL_Skaters$Tm=='WPG'
Capitals <- NHL_Skaters$Tm=='WSH'
#Defining the teams

NHL_Skaters$TmAvgPlusMinus <- c(2.72, 2.72, 2.72, 2.72, 2.72, 2.72, 2.72, 2.72, 2.72, 2.72, 2.72, 2.72, 2.72, 2.72, 2.72, 
                              2.72, 2.72, 2.72, 2.72, 2.72, 2.72, 2.72, 2.72, 2.72, 2.72, 2.72, 2.72, 2.72, 2.72, 2.72, 
                              2.72, 2.72, -7.52, -7.52, -7.52, -7.52, -7.52, -7.52, -7.52, -7.52, -7.52, -7.52, -7.52, 
                              -7.52, -7.52, -7.52, -7.52, -7.52, -7.52, -7.52, -7.52, -7.52, -7.52, -7.52, -7.52, -7.52, 
                              -7.52, 0.31, 0.31, 0.31, 0.31, 0.31, 0.31, 0.31, 0.31, 0.31, 0.31, 0.31, 0.31, 0.31, 0.31, 
                              0.31, 0.31, 0.31, 0.31, 0.31, 0.31, 0.31, 0.31, 0.31, 0.31, 0.31, 0.31, 0.31, 0.31, 0.31,
                              -5.29, -5.29, -5.29, -5.29, -5.29, -5.29, -5.29, -5.29, -5.29, -5.29, -5.29, -5.29, -5.29, 
                              -5.29, -5.29, -5.29, -5.29, -5.29, -5.29, -5.29, -5.29, -5.29, -5.29, -5.29, -5.29, -5.29, 
                              -5.29, -5.29, -5.29, -5.29, -5.29, -5.29, -5.29, -5.29, -4.29, -4.29, -4.29, -4.29, -4.29, 
                              -4.29, -4.29, -4.29, -4.29, -4.29, -4.29, -4.29, -4.29, -4.29, -4.29, -4.29, -4.29, -4.29, 
                              -4.29, -4.29, -4.29, -4.29, -4.29, -4.29, -4.29, -4.29, -4.29, -4.29, 8.88, 8.88, 8.88, 
                              8.88, 8.88, 8.88, 8.88, 8.88, 8.88, 8.88, 8.88, 8.88, 8.88, 8.88, 8.88, 8.88, 8.88, 8.88, 
                              8.88, 8.88, 8.88, 8.88, 8.88, 8.88, -0.59, -0.59, -0.59, -0.59, -0.59, -0.59, -0.59, -0.59, 
                              -0.59, -0.59, 
                              -0.59, -0.59, -0.59, -0.59, -0.59, -0.59, -0.59, -0.59, -0.59, -0.59, -0.59, -0.59, -0.59, 
                              -0.59, -0.59, -0.59, -0.59, 6.72, 6.72, 6.72, 6.72, 6.72, 6.72, 6.72, 6.72, 6.72, 6.72, 
                              6.72, 6.72, 6.72, 6.72, 6.72, 6.72, 6.72, 6.72, 6.72, 6.72, 6.72, 6.72, 6.72, 6.72, 6.72, 
                              -13.85, -13.85, -13.85, -13.85, -13.85, -13.85, -13.85, -13.85, -13.85, -13.85, -13.85, 
                              -13.85, -13.85, -13.85, -13.85, -13.85, -13.85, -13.85, -13.85, -13.85, -13.85, -13.85, 
                              -13.85, -13.85, -13.85, -13.85, -3.04, -3.04, -3.04, -3.04, -3.04, -3.04, -3.04, -3.04, 
                              -3.04, -3.04, -3.04, -3.04, -3.04, -3.04, -3.04, -3.04, -3.04, -3.04, -3.04, -3.04, -3.04, 
                              -3.04, -3.04, -3.04, -3.04, -3.04, -6.43, -6.43, -6.43, -6.43, -6.43, -6.43, -6.43, -6.43,
                              -6.43, -6.43, -6.43, -6.43, -6.43, -6.43, -6.43, -6.43, -6.43, -6.43, -6.43, -6.43, -6.43,
                              -6.43, -6.43, -6.43, -6.43, -6.43, -6.43, -6.43, 3.67, 3.67, 3.67, 3.67, 3.67, 3.67, 3.67,
                              3.67, 3.67, 3.67, 3.67, 3.67, 3.67, 3.67, 3.67, 3.67, 3.67, 3.67, 3.67, 3.67, 3.67, 3.67,
                              3.67, 3.67, 3.67, 3.67, 3.67, -6.88, -6.88, -6.88, -6.88, -6.88, -6.88, -6.88, -6.88, -6.88,
                              -6.88, -6.88, -6.88, -6.88, -6.88, -6.88, -6.88, -6.88, -6.88, -6.88, -6.88, -6.88, -6.88,
                              -6.88, -6.88, -6.88, -2.6, -2.6, -2.6, -2.6, -2.6, -2.6, -2.6, -2.6, -2.6, -2.6, -2.6, -2.6,
                              -2.6, -2.6, -2.6, -2.6, -2.6, -2.6, -2.6, -2.6, -2.6, -2.6, -2.6, -2.6, -2.6, 8.64, 8.64,
                              8.64, 8.64, 8.64, 8.64, 8.64, 8.64, 8.64, 8.64, 8.64, 8.64, 8.64, 8.64, 8.64, 8.64, 8.64,
                              8.64, 8.64, 8.64, 8.64, 8.64, 8.64, 8.64, 8.64, 8.64, 8.64, 8.64, 4.32, 4.32, 4.32, 4.32,
                              4.32, 4.32, 4.32, 4.32, 4.32, 4.32, 4.32, 4.32, 4.32, 4.32, 4.32, 4.32, 4.32, 4.32, 4.32,
                              4.32, 4.32, 4.32, 4.32, 4.32, 4.32, 4.32, 4.32, 4.32, -8.13, -8.13, -8.13, -8.13, -8.13,
                              -8.13, -8.13, -8.13, -8.13, -8.13, -8.13, -8.13, -8.13, -8.13, -8.13, -8.13, -8.13, -8.13,
                              -8.13, -8.13, -8.13, -8.13, -8.13, -8.13, -8.13, -8.13, -8.13, -8.13, -8.13, -8.13, 2.1, 
                              2.1,
                              2.1, 2.1, 2.1, 2.1, 2.1, 2.1, 2.1, 2.1, 2.1, 2.1, 2.1, 2.1, 2.1, 2.1, 2.1, 2.1, 2.1, 2.1,
                              2.1, 2.1, 2.1, 2.1, 2.1, 2.1, 2.1, 2.1, 2.1, 2.1, 2.1, 0.89, 0.89, 0.89, 0.89, 0.89, 0.89,
                              0.89, 0.89, 0.89, 0.89, 0.89, 0.89, 0.89, 0.89, 0.89, 0.89, 0.89, 0.89, 0.89, 0.89, 0.89,
                              0.89, 0.89, 0.89, 0.89, 0.89, 0.89, 6.79, 6.79, 6.79, 6.79, 6.79, 6.79, 6.79, 6.79, 6.79,
                              6.79, 6.79, 6.79, 6.79, 6.79, 6.79, 6.79, 6.79, 6.79, 6.79, 6.79, 6.79, 6.79, 6.79, 6.79,
                              1.37, 1.37, 1.37, 1.37, 1.37, 1.37, 1.37, 1.37, 1.37, 1.37, 1.37, 1.37, 1.37, 1.37, 1.37,
                              1.37, 1.37, 1.37, 1.37, 1.37, 1.37, 1.37, 1.37, 1.37, 1.37, 1.37, 1.37, 1.37, 1.37, 1.37,
                              -4.25, -4.25, -4.25, -4.25, -4.25, -4.25, -4.25, -4.25, -4.25, -4.25, -4.25, -4.25, -4.25,
                              -4.25, -4.25, -4.25, -4.25, -4.25, -4.25, -4.25, -4.25, -4.25, -4.25, -4.25, -4.25, -4.25,
                              -4.25, -4.25, 6.1, 6.1, 6.1, 6.1, 6.1, 6.1, 6.1, 6.1, 6.1, 6.1, 6.1, 6.1, 6.1, 6.1, 6.1, 
                              6.1,
                              6.1, 6.1, 6.1, 6.1, 6.1, 6.1, 6.1, 6.1, 6.1, 6.1, 6.1, 6.1, 6.1, 6.1, 6.1, 3.19, 3.19, 3.19,
                              3.19, 3.19, 3.19, 3.19, 3.19, 3.19, 3.19, 3.19, 3.19, 3.19, 3.19, 3.19, 3.19, 3.19, 3.19,
                              3.19, 3.19, 3.19, 3.19, 3.19, 3.19, 3.19, 3.19, 1.3, 1.3, 1.3, 1.3, 1.3, 1.3, 1.3, 1.3, 1.3,
                              1.3, 1.3, 1.3, 1.3, 1.3, 1.3, 1.3, 1.3, 1.3, 1.3, 1.3, 1.3, 1.3, 1.3, 1.3, 1.3, 1.3, 1.3,
                              -1.75, -1.75, -1.75, -1.75, -1.75, -1.75, -1.75, -1.75, -1.75, -1.75, -1.75, -1.75, -1.75,
                              -1.75, -1.75, -1.75, -1.75, -1.75, -1.75, -1.75, -1.75, -1.75, -1.75, -1.75, -1.75, -1.75,
                              -1.75, -1.75, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55,
                              0.55, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55, -4.16, -4.16, -4.16, -4.16, -4.16, 
                              -4.16,
                              -4.16, -4.16, -4.16, -4.16, -4.16, -4.16, -4.16, -4.16, -4.16, -4.16, -4.16, -4.16, -4.16, 
                              -4.16,
                              -4.16, -4.16, -4.16, -4.16, -4.16, -4.16, -4.16, -4.16, -4.16, -4.16, -4.16, -4.16, -4.16, 
                              -4.16,
                              -4.16, -4.16, -4.16, -4.16, -4.16, -4.16, -4.16, -4.16, -4.16, -4.16, -4.16, -4.16, -4.16, 
                              -4.16,
                              -4.16, -4.16, -4.16, -4.16, -4.16, -4.16, -4.16, -4.16, -4.16, -4.16, -4.16, -4.16, -4.16, 
                              -4.16,
                              -7.26, -7.26, -7.26, -7.26, -7.26, -7.26, -7.26, -7.26, -7.26, -7.26, -7.26, -7.26, -7.26,
                              -7.26, -7.26, -7.26, -7.26, -7.26, -7.26, -7.26, -7.26, -7.26, -7.26, -7.26, -7.26, -7.26,
                              -7.26, -7.26, -7.26, -7.26, -7.26, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25,
                              0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25,
                              0.25, 0.25, 0.25, 0.25, 13.12, 13.12, 13.12, 13.12, 13.12, 13.12, 13.12, 13.12, 13.12, 
                              13.12,
                              13.12, 13.12, 13.12, 13.12, 13.12, 13.12, 13.12, 13.12, 13.12, 13.12, 13.12, 13.12, 13.12,
                              13.12, 13.12, 13.12)
#Deefine Variable Average Plus Minus per team

NHL_Skaters$PlusMinusAdj <- c(NHL_Skaters$`+/-` - NHL_Skaters$TmAvgPlusMinus)
#Defining Variable for Adjusted Plus/Minus

NHL_Skaters$GPG <- c(NHL_Skaters$G/NHL_Skaters$GP)
NHL_Skaters$APG <- c(NHL_Skaters$A/NHL_Skaters$GP)
NHL_Skaters$PenPG <- c(NHL_Skaters$PIM/NHL_Skaters$GP)
NHL_Skaters$SPG <- c(NHL_Skaters$S/NHL_Skaters$GP)
NHL_Skaters$BPG <- c(NHL_Skaters$BLK/NHL_Skaters$GP)
NHL_Skaters$HPG <- c(NHL_Skaters$HIT/NHL_Skaters$GP)
#Defining Per Game Statistics

summary(lm(NHL_Skaters$Salary[Forward] ~ NHL_Skaters$Age[Forward] + NHL_Skaters$GP[Forward] 
           + NHL_Skaters$G[Forward] + NHL_Skaters$A[Forward] + NHL_Skaters$PIM[Forward] 
           + NHL_Skaters$S[Forward] + NHL_Skaters$`S%`[Forward] + NHL_Skaters$TOI[Forward] 
           + NHL_Skaters$BLK[Forward] + NHL_Skaters$HIT[Forward] + NHL_Skaters$FOW[Forward] 
           + NHL_Skaters$`+/-`[Forward]))
##Multiple Regression for Forwards who played more than 16 games (salary as y)

summary(lm(NHL_Skaters$TmPoints[Forward] ~ NHL_Skaters$Age[Forward] + NHL_Skaters$GP[Forward] 
           + NHL_Skaters$G[Forward] + NHL_Skaters$A[Forward] + NHL_Skaters$PIM[Forward] 
           + NHL_Skaters$S[Forward] + NHL_Skaters$`S%`[Forward] + NHL_Skaters$TOI[Forward] 
           + NHL_Skaters$BLK[Forward] + NHL_Skaters$HIT[Forward] + NHL_Skaters$FOW[Forward] 
           + NHL_Skaters$`+/-`[Forward]))
##Multiple Regression for Forwards who played more than 16 games (Team Points as y)

summary(lm(NHL_Skaters$Salary[Forward] ~ NHL_Skaters$Age[Forward] + NHL_Skaters$GP[Forward] 
           + NHL_Skaters$G[Forward] + NHL_Skaters$A[Forward] + NHL_Skaters$PIM[Forward] 
           + NHL_Skaters$`S%`[Forward] + NHL_Skaters$TOI[Forward] 
           + NHL_Skaters$BLK[Forward] + NHL_Skaters$HIT[Forward] + NHL_Skaters$FOW[Forward] 
           + NHL_Skaters$`+/-`[Forward]))
##Multiple Regression (salary as y) without Shots (Shots and Goals are cross-correlated) for Forwards

Defender <- NHL_Skaters$GP > 16 & NHL_Skaters$Pos=='D'
##Defining Defender as a D playing more than 16 Games

Other <- !Forward & !Defender
## Define Others as skaters who played fewer than 16 games (neither forwards nor defenders in this set)

summary(lm(NHL_Skaters$Salary[Defender] ~ NHL_Skaters$Age[Defender] + NHL_Skaters$GP[Defender] 
           + NHL_Skaters$G[Defender] + NHL_Skaters$A[Defender] + NHL_Skaters$PIM[Defender] 
           + NHL_Skaters$S[Defender] + NHL_Skaters$`S%`[Defender] + NHL_Skaters$TOI[Defender] 
           + NHL_Skaters$BLK[Defender] + NHL_Skaters$HIT[Defender] + NHL_Skaters$`+/-`[Defender]))
##Multiple Regression for Defenders who play more than 16 games (salary as y)

summary(lm(NHL_Skaters$TmPoints[Defender] ~ NHL_Skaters$Age[Defender] + NHL_Skaters$GP[Defender] 
           + NHL_Skaters$G[Defender] + NHL_Skaters$A[Defender] + NHL_Skaters$PIM[Defender] 
           + NHL_Skaters$S[Defender] + NHL_Skaters$`S%`[Defender] + NHL_Skaters$TOI[Defender] 
           + NHL_Skaters$BLK[Defender] + NHL_Skaters$HIT[Defender] 
           + NHL_Skaters$`+/-`[Defender]))
##Multiple Regression for Defenders who play more than 16 games (Team Points as y)

Goalies <- NHL_Goalies$GS > 12
##Defining Goalies as those who started more than 12 games

NHL_Goalies$SVPG <- NHL_Goalies$SV/NHL_Goalies$GP
##Creation of the SavePerGame statistic

NHL_Goalies$WinPerc <- NHL_Goalies$W/NHL_Goalies$GS
##Creation of the Win Percentage statistic for goalies

NHL_Goalies$SOPerc <- NHL_Goalies$SO/NHL_Goalies$GS
##Creation of the Shutout Percentage statistic for goalies

summary(lm(NHL_Goalies$Salary[Goalies] ~ NHL_Goalies$Age[Goalies] + NHL_Goalies$GS[Goalies]
           + NHL_Goalies$W[Goalies] + NHL_Goalies$SVPG[Goalies] + NHL_Goalies$`SV%`[Goalies]
           + NHL_Goalies$GAA[Goalies] + NHL_Goalies$SO[Goalies] + NHL_Goalies$`QS%`[Goalies]))
##Multiple Regression (salary as y) for all goalies who started more than 12 games

summary(lm(NHL_Goalies$W[Goalies] ~ NHL_Goalies$Age[Goalies] + NHL_Goalies$`SV%`[Goalies] 
           + NHL_Goalies$GAA[Goalies] + NHL_Goalies$`QS%`[Goalies]
           + NHL_Goalies$SVPG[Goalies]))
##Multiple Regression (Wins as y) for all goalies who started more than 12 games

summary(lm(NHL_Goalies$W[Goalies] ~ NHL_Goalies$Age[Goalies] + NHL_Goalies$`SV%`[Goalies] 
           + NHL_Goalies$`QS%`[Goalies] + NHL_Goalies$SVPG[Goalies]))
##Multiple Regression Without GAA (it and Sv% are cross-correlated)

summary(lm(NHL_Goalies$W[Goalies] ~ NHL_Goalies$Age[Goalies] 
           + NHL_Goalies$GAA[Goalies] + NHL_Goalies$`QS%`[Goalies]
           + NHL_Goalies$SVPG[Goalies]))
##Multiple Regression without Sv% (it and GAA are cross-correlated)

GoalieSalarySVPerc.MR <- summary(lm(NHL_Goalies$Salary[Goalies] ~ NHL_Goalies$Age[Goalies] + NHL_Goalies$GS[Goalies]
                              + NHL_Goalies$WinPerc[Goalies] + NHL_Goalies$`SV%`[Goalies]
                              + NHL_Goalies$SOPerc[Goalies] + NHL_Goalies$`QS%`[Goalies]))

GoalieSalaryGAA.MR <- summary(lm(NHL_Goalies$Salary[Goalies] ~ NHL_Goalies$Age[Goalies] + NHL_Goalies$GS[Goalies]
                                 + NHL_Goalies$WinPerc[Goalies] + NHL_Goalies$GAA[Goalies]
                                 + NHL_Goalies$SOPerc[Goalies] + NHL_Goalies$`QS%`[Goalies]))

GoalieSalarySVPG.MR <- summary(lm(NHL_Goalies$Salary[Goalies] ~ NHL_Goalies$Age[Goalies] + NHL_Goalies$GS[Goalies]
                                    + NHL_Goalies$WinPerc[Goalies] + NHL_Goalies$SVPG[Goalies]
                                    + NHL_Goalies$SOPerc[Goalies] + NHL_Goalies$`QS%`[Goalies]))

GoalieWinsGAA.MR <- summary(lm(NHL_Goalies$WinPerc[Goalies] ~ NHL_Goalies$Age[Goalies]
                               + NHL_Goalies$GAA[Goalies]  
                               + NHL_Goalies$SOPerc[Goalies] + NHL_Goalies$`QS%`[Goalies]))

GoalieWinsSVPG.MR <- summary(lm(NHL_Goalies$WinPerc[Goalies] ~ NHL_Goalies$Age[Goalies]
                                + NHL_Goalies$SVPG[Goalies]
                                + NHL_Goalies$SOPerc[Goalies] + NHL_Goalies$`QS%`[Goalies]))

GoalieWinsSVPerc.MR <- summary(lm(NHL_Goalies$WinPerc[Goalies] ~ NHL_Goalies$Age[Goalies] 
                                  + NHL_Goalies$`SV%`[Goalies] + NHL_Goalies$SOPerc[Goalies] 
                                  + NHL_Goalies$`QS%`[Goalies]))

##Defining multiple regression summaries for Goalie Regressions
##Note: GAA and 'SV%' are cross-correlated

ForwardsSalaryShots.MR <- summary(lm(NHL_Skaters$Salary[Forward] ~ NHL_Skaters$Age[Forward] + NHL_Skaters$GP[Forward] 
                                     + NHL_Skaters$SPG[Forward] + NHL_Skaters$APG[Forward] + NHL_Skaters$PenPG[Forward] 
                                     + NHL_Skaters$`S%`[Forward] + NHL_Skaters$ATOI[Forward] 
                                     + NHL_Skaters$BPG[Forward] + NHL_Skaters$HPG[Forward] 
                                     + NHL_Skaters$PlusMinusAdj[Forward]))

ForwardsSalaryGoals.MR <- summary(lm(NHL_Skaters$Salary[Forward] ~ NHL_Skaters$Age[Forward] + NHL_Skaters$GP[Forward] 
                                     + NHL_Skaters$GPG[Forward] + NHL_Skaters$APG[Forward] + NHL_Skaters$PenPG[Forward] 
                                     + NHL_Skaters$`S%`[Forward] + NHL_Skaters$ATOI[Forward] 
                                     + NHL_Skaters$BPG[Forward] + NHL_Skaters$HPG[Forward] 
                                     + NHL_Skaters$PlusMinusAdj[Forward]))

ForwardsTmPointsShots.MR <- summary(lm(NHL_Skaters$TmPoints[Forward] ~ NHL_Skaters$Age[Forward] + NHL_Skaters$GP[Forward] 
                                       + NHL_Skaters$APG[Forward] + NHL_Skaters$PenPG[Forward] 
                                       + NHL_Skaters$SPG[Forward] + NHL_Skaters$`S%`[Forward] + NHL_Skaters$ATOI[Forward] 
                                       + NHL_Skaters$BPG[Forward] + NHL_Skaters$HPG[Forward]
                                       + NHL_Skaters$PlusMinusAdj[Forward]))

ForwardsTmPointsGoals.MR <- summary(lm(NHL_Skaters$TmPoints[Forward] ~ NHL_Skaters$Age[Forward] + NHL_Skaters$GP[Forward] 
                                       + NHL_Skaters$APG[Forward] + NHL_Skaters$PenPG[Forward] 
                                       + NHL_Skaters$GPG[Forward] + NHL_Skaters$`S%`[Forward] + NHL_Skaters$ATOI[Forward] 
                                       + NHL_Skaters$BPG[Forward] + NHL_Skaters$HPG[Forward]
                                       + NHL_Skaters$PlusMinusAdj[Forward]))
##Defining multiple regression summaries for forward regressions
##Note: Goals and Shots are cross-correlated

DefenderSalaryGoals.MR <- summary(lm(NHL_Skaters$Salary[Defender] ~ NHL_Skaters$Age[Defender] + NHL_Skaters$GP[Defender] 
                                     + NHL_Skaters$GPG[Defender] + NHL_Skaters$APG[Defender] 
                                     + NHL_Skaters$PenPG[Defender] 
                                     + NHL_Skaters$`S%`[Defender] + NHL_Skaters$ATOI[Defender] 
                                     + NHL_Skaters$BPG[Defender] + NHL_Skaters$HPG[Defender] 
                                     + NHL_Skaters$PlusMinusAdj[Defender]))

DefenderSalaryShots.MR <- summary(lm(NHL_Skaters$Salary[Defender] ~ NHL_Skaters$Age[Defender] + NHL_Skaters$GP[Defender] 
                                     + NHL_Skaters$SPG[Defender] + NHL_Skaters$APG[Defender] 
                                     + NHL_Skaters$PenPG[Defender] 
                                     + NHL_Skaters$`S%`[Defender] + NHL_Skaters$ATOI[Defender] 
                                     + NHL_Skaters$BPG[Defender] + NHL_Skaters$HPG[Defender] 
                                     + NHL_Skaters$PlusMinusAdj[Defender]))

DefenderTmPointsGoals.MR <- summary(lm(NHL_Skaters$TmPoints[Defender] ~ NHL_Skaters$Age[Defender] 
                                       + NHL_Skaters$GP[Defender] 
                                       + NHL_Skaters$GPG[Defender] + NHL_Skaters$APG[Defender] 
                                       + NHL_Skaters$PenPG[Defender] 
                                       + NHL_Skaters$`S%`[Defender] + NHL_Skaters$ATOI[Defender] 
                                       + NHL_Skaters$BPG[Defender] + NHL_Skaters$HPG[Defender] 
                                       + NHL_Skaters$PlusMinusAdj[Defender]))

DefenderTmPointsShots.MR <- summary(lm(NHL_Skaters$TmPoints[Defender] ~ NHL_Skaters$Age[Defender] 
                                       + NHL_Skaters$GP[Defender] 
                                       + NHL_Skaters$SPG[Defender] + NHL_Skaters$APG[Defender] 
                                       + NHL_Skaters$PenPG[Defender] 
                                       + NHL_Skaters$`S%`[Defender] + NHL_Skaters$ATOI[Defender] 
                                       + NHL_Skaters$BPG[Defender] + NHL_Skaters$HPG[Defender] 
                                       + NHL_Skaters$PlusMinusAdj[Defender]))
##Defining multiple regressions for defender regressions
##Note: Goals and Shots are cross-correlated

JeffrickasFWD <- (1.151*Age.z[Forward] + 1.707*Games.z[Forward] + 3.027*GoalPerGame.z[Forward]
                    + 6.993*AsstsPerGame.z[Forward] - 1.685*PenMinsPerGame.z[Forward] 
                    + 3.78*ShotPerc.z[Forward] + 2.863*ShotPerGame.z[Forward]
                    + 3.818*BlocksPerGame.z[Forward] + 1.735*HitsPerGame.z[Forward]
                    + 1.398*PlusMinusAdj.z[Forward] - 5.09281)
##Beginning of aggregate value statistic, the 'Jeffrickas Index', for forwards
                      
JeffrickasDEF <- (0.622*Age.z[Defender] + 1.493*Games.z[Defender] - 0.192*GoalPerGame.z[Defender] 
                    + 2.993*AsstsPerGame.z[Defender] - 0.791*PenMinsPerGame.z[Defender] 
                  + 1.867*ShotPerc.z[Defender] 
                    + 0.448*ShotPerGame.z[Defender] + 1.507*BlocksPerGame.z[Defender] 
                  - 0.075*HitsPerGame.z[Defender] 
                    + 2.92*PlusMinusAdj.z[Defender] - 2.441374)
##Beginning of aggregate value statistic, the 'Jeffrickas Index', for defenders

JeffrickasGOL <- (2.093*SavePerc.z[Goalies] - 0.852*GoalieAge.z[Goalies] - 4.192*GoalsAgainstAvg.z[Goalies] 
                  + 0.754*QualityStartPerc.z[Goalies] + 1.723*ShutoutPerc.z[Goalies]
                  - 1.607*SavePerGame.z[Goalies] - 0.724624)
##Beginning of aggregate value statistic, the 'Jeffrickas Index', for goalies

#Correlation Plots
string.free.goalies <- NHL_Goalies[,-c(1:2,4,13:14,18,20:22,28:29)]
string.free.skaters <- NHL_Skaters[,-c(1:4,5,27,20)]
Goalies.Cor <- cor(string.free.goalies)
Skaters.Cor <- cor(string.free.skaters)
corrplot(Goalies.Cor, method = "circle")
corrplot(Skaters.Cor, method = "circle")  

#Correlations
cor(NHL_Skaters$Salary[Forward], NHL_Skaters$TmPoints[Forward])
cor(NHL_Skaters$Salary[Forward], JeffrickasFWD)
cor(NHL_Skaters$TmPoints[Forward], JeffrickasFWD)
cor(NHL_Skaters$`+/-`[Forward], NHL_Skaters$TmPoints[Forward])

cor(NHL_Skaters$Salary[Defender], NHL_Skaters$TmPoints[Defender])
cor(NHL_Skaters$Salary[Defender], JeffrickasDEF)
cor(NHL_Skaters$TmPoints[Defender], JeffrickasDEF)
cor(NHL_Skaters$`+/-`[Defender], NHL_Skaters$TmPoints[Defender])

cor(NHL_Goalies$Salary[Goalies], NHL_Goalies$WinPerc[Goalies])
cor(JeffrickasGOL, NHL_Goalies$Salary[Goalies])
cor(JeffrickasGOL, NHL_Goalies$WinPerc[Goalies])
cor(NHL_Goalies$GAA[Goalies], NHL_Goalies$WinPerc[Goalies])

write.csv(NHL_Goalies, "/Users/Daniel/Documents/NHL_Goalies.csv")
write.csv(NHL_Skaters, "/Users/Daniel/Documents/NHL_Skaters.csv")

ResidualsFWD <- resid(lm(NHL_Skaters$Salary[Forward]~JeffrickasFWD))
ResidualsDEF <- resid(lm(NHL_Skaters$Salary[Defender]~JeffrickasDEF))
ResidualsGOL <- resid(lm(NHL_Goalies$Salary[Goalies]~JeffrickasGOL))

lm(log(NHL_Skaters$Salary[Forward])~JeffrickasFWD)
