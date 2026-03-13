library(tidyverse)
library("scales")
game1<-read.csv('2-24Dickinson1.csv')
game2<-read.csv('2-24Dickinson2.csv')
game3<-read.csv('2-26Elizabeth1.csv')
game4<-read.csv('2-26Elizabeth2.csv')
game5<-read.csv('3-4Brockport.csv')
game6<-read.csv('3-4Franklin.csv')
game7<-read.csv('3-5Marietta.csv')
game8<-read.csv('3-18g1Lynchburg.csv')
game9<-read.csv('3-18g2Lynchburg.csv')
game10<-read.csv('3-21Pfeiffer.csv')
game11<-read.csv('3-25g1HSydney.csv')
game12<-read.csv('3-25g2HSydney.csv')
game13<-read.csv('4-3Ferrum.csv')
game14<-read.csv('4-4Maryville.csv')
game15<-read.csv('4-11Averett.csv')
game16<-read.csv('4-25Guilford.csv')
game17<-read.csv('2-22Greensboro.csv')


df_list <- list(game1, game2, game3, game4, game5, game6, game7, game8, game9, game10, game11, game12, game13, game14, game15, game16, game17)

baseball <- df_list %>% reduce(full_join)

roanoke_pitching <- filter(baseball,PitcherTeam=="ROA_COL")
roanoke_pitching <-select(roanoke_pitching,Pitcher,Inning,Top.Bottom,Outs,Balls,Strikes,TaggedPitchType,AutoPitchType,PitchCall,KorBB, PlayResult)

roanoke_pitching<- mutate(roanoke_pitching, c00 = TRUE, c10 = FALSE, c01 = FALSE, c20 = FALSE, c11 = FALSE, c02 = FALSE, c30 = FALSE, c21 = FALSE, c12 = FALSE, c31 = FALSE, c22 = FALSE, c32 = FALSE)



for (i in 1:nrow(roanoke_pitching)){
  if(roanoke_pitching$Balls[i]!=0 | roanoke_pitching$Strikes[i]!=0) {
    roanoke_pitching$c10[i]=roanoke_pitching$c10[i-1]
    roanoke_pitching$c01[i]=roanoke_pitching$c01[i-1]
    roanoke_pitching$c20[i]=roanoke_pitching$c20[i-1]
    roanoke_pitching$c11[i]=roanoke_pitching$c11[i-1]
    roanoke_pitching$c02[i]=roanoke_pitching$c02[i-1]
    roanoke_pitching$c30[i]=roanoke_pitching$c30[i-1]
    roanoke_pitching$c21[i]=roanoke_pitching$c21[i-1]
    roanoke_pitching$c12[i]=roanoke_pitching$c12[i-1]
    roanoke_pitching$c31[i]=roanoke_pitching$c31[i-1]
    roanoke_pitching$c22[i]=roanoke_pitching$c22[i-1]
    roanoke_pitching$c32[i]=roanoke_pitching$c32[i-1]
    if(roanoke_pitching$Balls[i]==0){
      if(roanoke_pitching$Strikes[i]==1){
        roanoke_pitching$c01[i]=TRUE
      }
      else{
        if(roanoke_pitching$Strikes[i]==2){
          roanoke_pitching$c02[i]=TRUE
        }
      }
    }
    else if(roanoke_pitching$Balls[i]==1){
      if(roanoke_pitching$Strikes[i] == 0)
      {
        roanoke_pitching$c10[i]= TRUE 
      }
      else if(roanoke_pitching$Strikes[i]==1){
        roanoke_pitching$c11[i]=TRUE
      }
      else{
        if(roanoke_pitching$Strikes[i]==2){
          roanoke_pitching$c12[i]=TRUE
        }
      }
    }
    else if(roanoke_pitching$Balls[i]==2){
      if(roanoke_pitching$Strikes[i] == 0)
      {
        roanoke_pitching$c20[i]= TRUE 
      }
      else if(roanoke_pitching$Strikes[i]==1){
        roanoke_pitching$c21[i]=TRUE
      }
      else{
        if(roanoke_pitching$Strikes[i]==2){
          roanoke_pitching$c22[i]=TRUE
        }
      }
    }
    else if(roanoke_pitching$Balls[i]==3){
      if(roanoke_pitching$Strikes[i]== 0)
      {
        roanoke_pitching$c30[i]= TRUE 
      }
      else if(roanoke_pitching$Strikes[i]==1){
        roanoke_pitching$c31[i]=TRUE
      }
      else{
        if(roanoke_pitching$Strikes[i]==2){
          roanoke_pitching$c32[i]=TRUE
        }
      }
    }
  }
}

#The line below gets rid of every pitch that doesn't end an at bat.
roanoke_pitching<-filter(roanoke_pitching,PitchCall=="HitByPitch" | KorBB!="Undefined" | PlayResult!="Undefined")
roanoke_pitching<-mutate(roanoke_pitching, OBP = 0)
for (i in 1:nrow(roanoke_pitching)){
  if(roanoke_pitching$PitchCall[i]=="HitByPitch" | roanoke_pitching$KorBB[i]=="Walk" | roanoke_pitching$PlayResult[i]=="Single"| roanoke_pitching$PlayResult[i]=="Double"| roanoke_pitching$PlayResult[i]=="Triple"| roanoke_pitching$PlayResult[i]=="HomeRun") {
    roanoke_pitching$OBP[i]=1
  }
}


#roanoke_pitching<-filter(roanoke_pitching$PlayResult=="Single"| roanoke_pitching$PlayResult=="Double"| roanoke_pitching$PlayResult=="Triple"| roanoke_pitching$PlayResult=="HomeRun")
roanoke_pitching<-mutate(roanoke_pitching, Slugging = 0)
for (i in 1:nrow(roanoke_pitching)){
  if(roanoke_pitching$PlayResult[i]=="Single"){
    roanoke_pitching$Slugging[i]=1
  }
  else if(roanoke_pitching$PlayResult[i]=="Double"){
    roanoke_pitching$Slugging[i]=2
  }
  else if(roanoke_pitching$PlayResult[i]=="Triple"){
    roanoke_pitching$Slugging[i]=3
  } 
  else if(roanoke_pitching$PlayResult[i]=="HomeRun") {
    roanoke_pitching$Slugging[i]=4
  }
}

roanoke_pitching<-mutate(roanoke_pitching, AtBat = !(PitchCall == "HitByPitch" | KorBB == "Walk" | PlayResult == "Sacrifice"))
roanoke_pitching<-filter(roanoke_pitching, Pitcher=="Turner, Will")
RC2023Pitching_byCount <- roanoke_pitching %>% summarize(total_bases_slg = sum(Slugging), 
                                    PA = n(), 
                                    TotalAB = nrow(filter(roanoke_pitching, AtBat)),
                                    SLG = total_bases_slg/TotalAB,
                                    times_on_base = sum(OBP),
                                    OBP = times_on_base/PA)

RCPitching_Simplified <- roanoke_pitching %>% select(starts_with("c"), OBP, Slugging, AtBat)


RCPitching_Tidy <- RCPitching_Simplified %>%
  gather(key = "count", value = "pass_through", -c(OBP, Slugging, AtBat))

RCPitching_OBP_by_count <- RCPitching_Tidy %>% filter(pass_through) %>% group_by(count) %>% 
  summarize(total_bases_slg = sum(Slugging), 
            PA = n(), 
            TotalAB = sum(AtBat),
            SLG = total_bases_slg/TotalAB,
            times_on_base = sum(OBP),
            OBP = times_on_base/PA,
            OPS = SLG + OBP)

RCPitching_OBP_by_count<-mutate(RCPitching_OBP_by_count, balls = str_sub(count,2,2), strikes = str_sub(count,3,3))

OnBase<-RCPitching_OBP_by_count %>%
  ggplot(aes(x=strikes,y=balls,fill=OBP)) +
  geom_tile() +
  geom_text(aes(label=round(OBP,3))) +
  ggtitle(roanoke_pitching$Pitcher) +
  scale_fill_gradient2("OBP",low="skyblue",high="red",
                       mid="white",midpoint=.361)
OnBase

slug<-RCPitching_OBP_by_count %>%
  ggplot(aes(x=strikes,y=balls,fill=SLG)) +
  geom_tile() +
  geom_text(aes(label=round(SLG,3))) +
  ggtitle(roanoke_pitching$Pitcher) +
  scale_fill_gradient2("Slugging",low="skyblue",high="red",
                       mid="white",midpoint=.332)
slug

addem<-RCPitching_OBP_by_count %>%
  ggplot(aes(x=strikes,y=balls,fill=OPS)) +
  geom_tile() +
  geom_text(aes(label=round(OPS,3))) +
  ggtitle(roanoke_pitching$Pitcher) +
  scale_fill_gradient2("OPS",low="skyblue",high="red",
                       mid="white",midpoint=.692)

addem


group_by(roanoke_pitching, TaggedPitchType) %>% summarize(TotalPA = n(),
                                                        TotalAB = sum(AtBat),
                                                        TotalReachBase = sum(OBP),
                                                        Hits = sum(OBP*AtBat),
                                                        TotalBases = sum(Slugging),
                                                        OBP = TotalReachBase/TotalPA,
                                                        SLG = TotalBases/TotalAB,
                                                        AVG = Hits/TotalAB,
                                                        OPS = OBP + SLG) %>% head(10)
                                                        
#PitchTypeByCount
roanoke_pitching<-select(roanoke_pitching, Outs, Balls, Strikes, TaggedPitchType, AutoPitchType, Pitcher)
roanoke_pitching<-roanoke_pitching %>% mutate(Count = paste(Balls, Strikes, sep = "-"))

PitchCountPercentage <- group_by(roanoke_pitching, Count) %>% summarize(N = n(),
                                                           Fastball = sum(TaggedPitchType == "Fastball")/N,
                                                           Sinker = sum(TaggedPitchType == "Sinker")/N,
                                                           Changeup = sum(TaggedPitchType == "ChangeUp")/N,
                                                           Slider = sum(TaggedPitchType == "Slider")/N,
                                                           Curveball = sum(TaggedPitchType == "Curveball")/N,
                                                           Cutter = sum(TaggedPitchType == "Cutter")/N,
                                                           Other = sum(TaggedPitchType == "Other")/N)


if(nrow(filter(PitchCountPercentage, Count == "0-0")) == 0 ){
  PitchCountPercentage[nrow(PitchCountPercentage) + 1, ] <- list("0-0", 0, 0, 0, 0, 0, 0, 0, 0)
}else if (nrow(filter(PitchCountPercentage, Count == "0-1")) == 0 ){
  PitchCountPercentage[nrow(PitchCountPercentage) + 1, ] <- list("0-1", 0, 0, 0, 0, 0, 0, 0, 0)
}else if (nrow(filter(PitchCountPercentage, Count == "0-2")) == 0 ){
  PitchCountPercentage[nrow(PitchCountPercentage) + 1, ] <- list("0-2", 0, 0, 0, 0, 0, 0, 0, 0)
}else if (nrow(filter(PitchCountPercentage, Count == "1-0")) == 0 ){
  PitchCountPercentage[nrow(PitchCountPercentage) + 1, ] <- list("1-0", 0, 0, 0, 0, 0, 0, 0, 0)
}else if (nrow(filter(PitchCountPercentage, Count == "1-1")) == 0 ){
  PitchCountPercentage[nrow(PitchCountPercentage) + 1, ] <- list("1-1", 0, 0, 0, 0, 0, 0, 0, 0)
}else if (nrow(filter(PitchCountPercentage, Count == "1-2")) == 0 ){
  PitchCountPercentage[nrow(PitchCountPercentage) + 1, ] <- list("1-2", 0, 0, 0, 0, 0, 0, 0, 0)
}else if (nrow(filter(PitchCountPercentage, Count == "2-0")) == 0 ){
  PitchCountPercentage[nrow(PitchCountPercentage) + 1, ] <- list("2-0", 0, 0, 0, 0, 0, 0, 0, 0)
}else if (nrow(filter(PitchCountPercentage, Count == "2-1")) == 0 ){
  PitchCountPercentage[nrow(PitchCountPercentage) + 1, ] <- list("2-1", 0, 0, 0, 0, 0, 0, 0, 0)
}else if (nrow(filter(PitchCountPercentage, Count == "2-2")) == 0 ){
  PitchCountPercentage[nrow(PitchCountPercentage) + 1, ] <- list("2-2", 0, 0, 0, 0, 0, 0, 0, 0)
}else if (nrow(filter(PitchCountPercentage, Count == "3-0")) == 0 ){
  PitchCountPercentage[nrow(PitchCountPercentage) + 1, ] <- list("3-0", 0, 0, 0, 0, 0, 0, 0, 0)
}else if (nrow(filter(PitchCountPercentage, Count == "3-1")) == 0 ){
  PitchCountPercentage[nrow(PitchCountPercentage) + 1, ] <- list("3-1", 0, 0, 0, 0, 0, 0, 0, 0)
}else if (nrow(filter(PitchCountPercentage, Count == "3-2")) == 0 ){
  PitchCountPercentage[nrow(PitchCountPercentage) + 1, ] <- list("3-2", 0, 0, 0, 0, 0, 0, 0, 0)
}

  
PitchCountPercentage <- PitchCountPercentage %>% arrange(Count)

group_by(roanoke_pitching, TaggedPitchType) %>% summarize(N = n()) %>% head(12) 

# Pie Chart from data frame with All Roanoke Pitches
mypitch <- table(roanoke_pitching$TaggedPitchType)
bb <- paste(names(mypitch), "\n", mypitch, sep="")
pie(mypitch, labels = bb)
pitcher <- title(roanoke_pitching$Pitcher[i])

# Bar Plot for pitches by count
counts <- table(roanoke_pitching$TaggedPitchType, roanoke_pitching$Count)
barplot(counts,
        xlab="Number of Pitches by Count", col=c("green","darkgreen","blue","red","orange","gold","pink"),
        legend = rownames(counts))
pitcher <- title(roanoke_pitching$Pitcher[i])

# library
library(ggplot2)

# create a datase
count <- c(rep("0-0" , 7) , rep("0-1" , 7) , rep("0-2" , 7) , rep("1-0" , 7), rep("1-1" , 7), rep("1-2" , 7), rep("2-0" , 7), rep("2-1" , 7), rep("2-2" , 7), rep("3-0" , 7), rep("3-1" , 7), rep("3-2" , 7))
PitchTypes <- rep(c("Fastball" , "Sinker" , "Changeup" , "Slider", "Curveball" , "Cutter" , "Other") , 12)
#value <- abs(rnorm(12 , 0 , 15))

value <- c()
for (i in 1:12){
  value<- c(value, PitchCountPercentage$Fastball[i], PitchCountPercentage$Sinker[i], PitchCountPercentage$Changeup[i], PitchCountPercentage$Slider[i], PitchCountPercentage$Curveball[i], PitchCountPercentage$Cutter[i], PitchCountPercentage$Other[i])
}

data <- data.frame(count,PitchTypes,value)
# Stacked + percent
ggplot(data, aes(fill=PitchTypes, y=value, x=count)) + 
  geom_bar(position="fill", stat="identity") +
  ggtitle(roanoke_pitching$Pitcher) +
  scale_fill_manual(values = c("green","darkgreen","blue","red","pink","orange","gold"))

K_zone_plot <- ggplot(roanoke_pitching, aes(x=PlateLocSide, y=PlateLocHeight))+
    geom_rect(xmin=-0.947, xmax=0.947, ymin=1.5,
              ymax=3.6, fill="lightblue", alpha=0.1)+
    coord_equal()+
    scale_x_continuous("Horizontal location (ft.)",
                      limits=c(-2,2))+
    scale_y_continuous("Vertical location (ft.)",
                      limits=c(0,5))
K_zone_plot +
  geom_point(aes(color=factor(TaggedPitchType))) +
  scale_color_manual("Pitch Type", values= c("green","darkgreen","blue","red","pink","orange","gold"))
    
                     

