# Testing plot to make visualizations

#finding all attacks to code
data <- read_csv("~/COSC_6050_project/big_east_plays.csv")
data$AttackPlay <- ifelse(data$AttackPlay == "Hut", "5", data$AttackPlay)
data$AttackPlay <- ifelse(data$AttackPlay == "Red", "9", data$AttackPlay)
data$AttackPlay <- ifelse(data$AttackPlay == "X", "Red", data$AttackPlay)
data$AttackPlay <- ifelse(data$AttackPlay == "O hits a 2nd step 4", "Red", data$AttackPlay)
data$AttackPlay <- ifelse(data$AttackPlay == "Quick in front (4)", "C", data$AttackPlay)
data$AttackPlay <- ifelse(data$AttackPlay == "Quick in Center", "C", data$AttackPlay)
data$AttackPlay <- ifelse(data$AttackPlay == "Quick ball back(5)", "C", data$AttackPlay)
data$AttackPlay <- ifelse(data$AttackPlay == "O hits in middle", "2", data$AttackPlay)
data$AttackPlay <- ifelse(data$AttackPlay == "2", "B", data$AttackPlay)
data$AttackPlay <- ifelse(data$AttackPlay == "Slide moved from S", "Slide", data$AttackPlay)
data$AttackPlay <- ifelse(data$AttackPlay == "Slide near S", "Slide", data$AttackPlay)
data$AttackPlay <- ifelse(data$AttackPlay == "Slide by the opposite", "Slide", data$AttackPlay)

all_hits <- subset(data, select = c(Name, attack_code,AttackPlay, evaluation_code, start_zone, end_zone, skill_subtype)) 
all_hits <- all_hits|> count(attack_code, AttackPlay, start_zone, sort = TRUE)
all_hits <- subset(all_hits, !is.na(attack_code))

#make court visual
x = c(1:20)
y= c(20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20)
plot(x,y,type = "l", xlim =c(2,19), ylim = c(0,29), xlab = "", ylab="") #xaxt='n',yaxt='n'

# add pct to top 4 for line colors
top_4$pct <- c(top1_pct,top2_pct,top3_pct,top4_pct)
# add to top_4 start zone-- probably should go back and have this by default for lines
top_4 <- top_4 |> mutate(start_x = case_when(
  AttackPlay=="Go" ~ 19.5,
  AttackPlay=="5" ~ 16,
  AttackPlay=="Red" ~ 1.5,
  AttackPlay=="9" ~ 5,
  AttackPlay=="Slide" ~ 1.5,
  AttackPlay=="Red" ~ 1.5,
  AttackPlay=="B" ~ 12.5,
  AttackPlay=="C" ~ 10,
  AttackPlay=="2" ~ 10
))
start_y = 30
top_4 <- top_4 |> mutate(end_x = case_when(
  end_zone==1 | end_zone==9 | end_zone==2 ~ 16,
  end_zone==6 | end_zone==8 | end_zone==3 ~ 10,
  end_zone==5 | end_zone==7 | end_zone==4 ~ 4
), end_y = case_when(
  end_zone == 5 | end_zone == 6 | end_zone == 1 ~ 5,
  end_zone == 7 | end_zone == 8 | end_zone == 9 ~ 15,
  end_zone == 4 | end_zone == 3 | end_zone == 2 ~ 25
), color = case_when(
  pct <= 0.150 ~ 'darkred',
  pct >0.15 & pct <= 0.3 ~ 'yellow',
  pct > 0.3 ~ 'darkgreen'
))

# add lines to plot
arrows(top_4[[1,6]], start_y, top_4[[1,7]],top_4[[1,8]], length = 0.05, lwd=2, col = top_4[[1,9]])
arrows(top_4[[2,6]], start_y, top_4[[2,7]],top_4[[2,8]], length = 0.05, lwd=2, col = top_4[[2,9]])
arrows(top_4[[3,6]], start_y, top_4[[3,7]],top_4[[3,8]], length = 0.05, lwd=2, col = top_4[[3,9]])
arrows(top_4[[4,6]], start_y, top_4[[4,7]],top_4[[4,8]], length = 0.05, lwd=2, col = top_4[[4,9]])

