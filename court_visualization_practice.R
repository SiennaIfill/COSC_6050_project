# Testing plot to make visualizations

#make court visual
x = c(1:20)
y= c(20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20)
plot(x,y,type = "l", xlim =c(2,19), ylim = c(0,29), xlab = "", ylab="") #xaxt='n',yaxt='n'

# add pct to top 4 for line colors
top_4$pct <- c(top1_pct,top2_pct,top3_pct,top4_pct)
# add to top_4 start zone-- probably should go back and have this by default for lines
top_4 <- top_4 |> mutate(start_x = case_when(
  AttackPlay=="Go" ~ 19.5,
  AttackPlay=="Hut" ~ 16,
  AttackPlay=="X" ~ 1.5
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

