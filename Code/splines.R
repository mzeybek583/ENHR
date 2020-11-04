## Splines


# Coder: Assist. Prof. Dr. Mustafa Zeybek
# Artvin Coruh University

#
# Load libraries ----------------------------------------------------------
library(splines) 
library(ggplot2)
library(MASS)


#Time
time <- proc.time()

df <- read.csv(file = "Data/Results/dbscan.out.txt")[,2:5]

CID <- length(unique(df$Classification))
CID
st <- unique(df$Classification)[-1]
st
CID <- length(st)

CID

out <- list()
#i=20
i=1
for (i in st[1]:max(st)) {
  print(i)
  ind <- df$Classification==i
  df.fit <- df[ind,]
  #  plot(df.fit$X, df.fit$Y)
  
  fitxy <- lm(Y~X, data=df.fit)
  
  knots <- quantile(df.fit$Z, p = c(0.25, 0.5, 0.75))
  
  fit <- lm(Z ~ bs(X, knots=knots, degree=3) + bs(Y, knots=knots, degree=3), data=df.fit)
  out[[i]] <- fit
  # Starts from 9
  if (i==11) {
    min.x <- min(df.fit$X)
    max.x <- max(df.fit$X)
    min.y <- min(df.fit$Y)
    max.y <- max(df.fit$Y)
    min.z <- min(df.fit$Z)
    max.z <- max(df.fit$Z)
    
    q.x <- seq(from=min.x, to=max.x, by=0.05)
    q.y <- predict(fitxy, data.frame(X=q.x))
    
    df.predict <-data.frame(X=q.x, Y=q.y) 
    predicted.Z <- data.frame(predict(fit,df.predict,interval='confidence',
                                      level=0.99))
    export <- data.frame(X=q.x, Y=q.y, Z=predicted.Z$fit)
    colors <- c("Gözlem" = "red", "Model" = "black")
    print(ggplot(df.fit, aes(x=X, y=Z))+
            geom_point(color="red")+
            theme_bw(base_size = 20)+
            geom_point(data=export, mapping=aes(x=X, y=Z), color="black")
    )
    ggsave(paste("./Data/Results/spline_tel-", i, ".png", sep = ""))
    #plot(df.fit$X, df.fit$Z, col="red", cex=0.5 )
    #points(df.predict$X, predicted.Z$fit, cex=0.4, add=TRUE)
    
    df.p <- out[[i]] 
    print(ggplot(df.p, aes(df.p$fitted.values, df.p$residuals))+
            geom_point(alpha = 0.5)+
            geom_hline(yintercept = mean(df.p$residuals), col="red", lwd=1.5, lty=2)+
            xlab("Kestirim Değerleri")+ ylab("Artık Hatalar")+
            theme_bw(base_size = 20))
    ggsave(paste("./Data/Results/spline_residual-", i, ".png", sep = ""))
  }
}
out <- out[!sapply(out,is.null)]  

s <- list()
for (i in 1: CID) {
  sum.out <- summary(out[[i]])
  s[[i]] <- sum.out
}


# Test Plots --------------------------------------------------------------

summary(out[[16]])
par(mfrow = c(2,2))
plot(out[[16]])
par(mfrow=c(1,1))

confint(out[[16]], level=0.95)


r.2 <- data.frame()
for (i in 1:CID) {
  t <- s[[i]][["r.squared"]]
  r.2[i,1] <-t
}
ggplot(r.2, aes(x = seq(1, length(V1)),y=V1))+
  geom_line()+
  geom_point()+
  xlab("Tel no") +ylab(expression(paste(R^{2}," ", "Değeri")))+
  theme_bw(base_size = 20)
ggsave("./Data/Results/spline_r2.png")

sprintf("Processing time %.2f second", (proc.time()-time)[3])



