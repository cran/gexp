## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(gexp)
dic <- gexp(mu   = 15,
            err  = matrix(rep(0, 6),
                          nrow = 6),
            r    = 3,
            fe   = list(alpha = c(1, -2)))
summary(dic)

## ------------------------------------------------------------------------
dic_mv <- gexp(mu  = c(15, 6),
               err = matrix(c(0, 0,
                              0, 0,
                              0, 0,
                              0, 0,
                              0, 0,
                              0, 0), 
                            ncol = 2),
               r   = 3L,
               fe  = list(alpha = matrix(c(1, -2,
                                           2,  3), 
                                         ncol = 2)))
summary(dic_mv)

## ------------------------------------------------------------------------
rcbd <- gexp(mu   = 15,
             r    = 1,
             err  = matrix(rep(0, 6),
                           nrow = 6),
             fe   = list(alpha = c(1, -2)),
             blke = c(2, 4, 6),
             type = 'RCBD')
summary(rcbd)

## ------------------------------------------------------------------------
lsd <- gexp(mu    = 15,
            err   = matrix(rep(0, 9), 
                           nrow = 9),
            fe    = list(alpha = c(1, -2, 3)),
            rowe  = c(2, 3, 4),
            cole  = c(6, 7, 8),
            nrand = 0,
            type  = 'LSD')
summary(lsd)

## ------------------------------------------------------------------------
fat_dic <- gexp(mu   = 15,
                r    = 2, 
                err  = matrix(rep(0, 12),
                              nrow = 12),
                fe   = list(alpha = c(1, -2),
                            tau = c(1, -1, 1)),
                inte = c(3, 1, 1, 
                         -5, 1, 1),
                type = 'FE')
summary(fat_dic)

## ------------------------------------------------------------------------
split_dic <- gexp(mu   = 15,
                  err  = matrix(rep(0, 12),
                                nrow = 12),
                  errp = matrix(rep(0, 6),
                                nrow = 6),
                  r    = 3,
                  fe   = list(alpha = c(1, -2),
                              tau   = c(1, -1)),
                  inte = c(3, 1, 1, -5),
                  type = 'SPE')
summary(split_dic)

## ------------------------------------------------------------------------
level <- seq(0, 15, 5)
cont_crd <- matrix(c(level,
                     level^2,
                     level^3),
                   ncol=3)

crd_l <- gexp(mu        = NULL,
              r         = 3,
              err       = matrix(rep(0,12),
                                 nrow = 12),
              fe        = list(f1 = c(2, 3, 0, 0)),
              contrasts = list(f1 = cont_crd))

summary(crd_l)

## ----echo=FALSE, results='hide'------------------------------------------
with(crd_l$dfm,
     plot(Y1 ~ rep(level, 3),
          axes = FALSE,
          ylim = c(0,50),
          xlab = 'X1'))
axis(1, pos = 0)
axis(2, pos = 0)
abline(h = 2, col = 'red')
abline(2, 3, col = 'red', lty = 2)
text(-0.5,2,'2',col = 'red', xpd = TRUE) 

## ------------------------------------------------------------------------
crd_q <- gexp(mu        = NULL,
              r         = 3,
              err       = matrix(rep(0,12),
                                 nrow = 12),
              fe        = list(f1 = c(2, 3, 4, 0)),
              contrasts = list(f1 = cont_crd))

summary(crd_q)

## ----echo=FALSE, results='hide'------------------------------------------
with(crd_q$dfm,
     plot(Y1 ~ rep(level, 3),
          axes = FALSE,
          #ylim = c(0,50),
          xlab = 'X1'))
axis(1, pos = 0)
axis(2, pos = 0)
curve(2+3*x+4*x^2, col = 'red', lty = 2, add=T)
text(-0.5,2,'2',col = 'red', xpd = TRUE) 

## ------------------------------------------------------------------------
level2 <- seq(2,8,2)
cont_crd2 <- matrix(c(level2,level2^2,level2^3),ncol=3)

crd_hb <- gexp(mu = NULL,
               r  = 2,
               err = matrix(rep(0,32),
                            nrow = 32),
               fe  = list(f1 = c(1, 3, 0, 0),
                          f2 = c(0, 2, 0, 0)),
               fl = list(N = ordered(level),
                         P = ordered(level2)),
               contrasts = list(f1 = cont_crd,
                                f2 = cont_crd2))

summary(crd_hb)

## ----echo=FALSE, results='hide'------------------------------------------
z <- matrix(crd_hb$dfm$Y1,
            ncol=4,
            byrow=TRUE)[1:4,]
persp(level,
      level2, 
      z, 
      theta=30,
      xlab='N',
      ylab='P',
      col='red')

## ------------------------------------------------------------------------
crd_hb2 <- gexp(mu = NULL,
               r  = 2,
               err = matrix(rep(0,32),
                            nrow = 32),
               fe  = list(f1 = c(1, 3, 2, 0),
                          f2 = c(0, 2, 3, 0)),
               fl = list(N = ordered(level),
                         P = ordered(level2)),
               contrasts = list(f1 = cont_crd,
                                f2 = cont_crd2))

summary(crd_hb2)

## ----echo=FALSE, results='hide'------------------------------------------
z1 <- matrix(crd_hb2$dfm$Y1,
            ncol=4,
            byrow=TRUE)[1:4,]
persp(level,
      level2, 
      z1, 
      theta=30,
      xlab='N',
      ylab='P',
      col='red')

## ------------------------------------------------------------------------
crd_design <- gexp(random = TRUE)
summary(crd_design)

## ------------------------------------------------------------------------
plot(crd_design)

## ------------------------------------------------------------------------
rcbd_designs <- gexp(r = 1, type = 'RCBD', random = TRUE)
summary(rcbd_designs)

## ----fig.width = 7, fig.height = 7---------------------------------------
plot(rcbd_designs)

## ------------------------------------------------------------------------
rcbd_designc <- gexp(r = 2, type = 'RCBD', random = TRUE)
summary(rcbd_designc)

## ----fig.width = 7, fig.height = 7---------------------------------------
plot(rcbd_designc)

## ------------------------------------------------------------------------
lsd_design <- gexp(type = 'LSD', random = TRUE)
summary(lsd_design)

## ----fig.width = 7, fig.height = 7---------------------------------------
plot(lsd_design)

## ------------------------------------------------------------------------
fat_design <- gexp(r = 3, type = 'FE', random = TRUE)
summary(fat_design)

## ----fig.width = 7, fig.height = 7---------------------------------------
plot(fat_design)

## ------------------------------------------------------------------------
split_design <- gexp(r = 3, type = 'SPE', random = TRUE)
summary(split_design)

## ----fig.width = 7, fig.height = 7---------------------------------------
plot(split_design)

## ----eval=FALSE----------------------------------------------------------
#  dic_plot <- gexp(r = 3, fe = list(f1=rep(1,2)))
#  plot(dic_plot,
#       dynamic = TRUE)

