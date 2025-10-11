# Testing plot to make visualizations

#make court visual
x = c(1:20)
y= c(20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20)
plot(x,y,type = "l", xlim =c(2,19), ylim = c(0,29), xlab = "", ylab="") #xaxt='n',yaxt='n'

# add to top_4 start zone-- probably should go back and have this by default for lines
top_4 <- top_4 |> mutate(start_x = case_when(
  AttackPlay=="Go" ~ 19.5,
  AttackPlay=="Hut" ~ 16
))
start_y = 30
top_4 <- top_4 |> mutate(end_x = case_when(
  end_zone==1 ~ 16,
  end_zone==6 ~ 10
), end_y = case_when(
  end_zone == 5 | end_zone == 6 | end_zone == 1 ~ 5,
  end_zone == 7 | end_zone == 8 | end_zone == 9 ~ 15,
  end_zone == 4 | end_zone == 3 | end_zone == 2 ~ 25
))

# add lines to plot
arrows(top_4[[1,5]], start_y, top_4[[1,6]],top_4[[1,7]], length = 0.05, lwd=2)

