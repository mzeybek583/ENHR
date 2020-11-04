

## Plot R^2

library(ggplot2)

gam <- read.csv(file = "Data/Results/GAM_r2.txt")
spline <- read.csv(file = "Data/Results/spline_r2.txt")
poly <- read.csv(file = "Data/Results/poly_r2.txt")

df <- data.frame(gam, spline, poly)
colnames(df) <- c("GAM", "SPLINES", "POLYNOMIAL")
x <- seq(from=1, to= nrow(poly), by=1)

ggplot()+
  geom_point(data = df, mapping = aes(x=x, y=GAM), colour="red")+
  geom_line(data = df, mapping = aes(x=x, y=GAM), colour="red")+
  geom_point(data = df, mapping = aes(x=x, y=SPLINES), colour="green")+
  geom_line(data = df, mapping = aes(x=x, y=SPLINES), colour="green")+
  geom_point(data = df, mapping = aes(x=x, y=POLYNOMIAL), colour="blue")+
  geom_line(data = df, mapping = aes(x=x, y=POLYNOMIAL), colour="blue")+
  xlab("Tel no") +ylab(expression(paste(R^{2}," ", "Değeri")))+
  theme_bw(base_size = 20)
  

ggsave("./Data/Results/tum_r2.png")


        