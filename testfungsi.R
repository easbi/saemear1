# tes fungsi penduga y ----------------------------------------------------

dt_test=import("Data_2 - test.xlsx") %>% mutate(your_x2=scale(your_x2))
colnames(dt_test)

# Application without Measurement Error
out=saeryme1(
  formula = your_y_star~your_x1+your_x2,
  t = ~your_t,
  area = ~your_Area,
  w = ~your_w,
  data = dt_test, 
  ME = F
)

out$est
out$REML
out$beta

# Application with Measurement Error
out_ME=saeryme1(
  formula = your_y_star~your_x1+your_x2,
  t = ~your_t,
  area = ~your_Area,
  w = ~your_w,
  data = dt_test, 
  ME = T
)

out_ME$est
out_ME$REML
out_ME$beta

plot(out$est$est, type="l")
lines(out_ME$est$est, col="red")


# tes fungsi mse --------------------------------------------------------------
mydata= import("Data_2.xlsx") %>% mutate(x2=scale(x2))
mse_me=msesaemear1(
  formula = y_star~x1+x2,
  t = ~t,
  area = ~Area,
  w = ~w,
  data = mydata, 
  ME = T,
  B=10
)
mse_nme=msesaemear1(
  formula = y_star~x1+x2,
  t = ~t,
  area = ~Area,
  w = ~w,
  data = mydata, 
  ME = F,
  B=10
)

