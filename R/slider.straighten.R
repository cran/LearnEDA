slider.straighten=function (x, y, ...) 
{
    x.name <- deparse(substitute(x))
    if (missing(x) || length(x) < 2) 
        return("Error: x must be a vector")
    y.name <- deparse(substitute(y))
    if (missing(y) || length(y) < 2) 
        return("Error: y must be a vector")
    power.x = 1
    power.y = 1
    args <- list(...)
    refresh <- function(...) {
        power.x <- slider(no = 1)
        power.y <- slider(no = 2)
        power.x <- ifelse(power.x == 0, 1e-04, power.x)
        power.y <- ifelse(power.y == 0, 1e-04, power.y)
        reexpressed.x = (x^power.x - 1)/power.x
        reexpressed.y = (y^power.y - 1)/power.y
        fit = rline(reexpressed.x, reexpressed.y, iter = 3)

        xlb="Reexpressed x"
        ylb="Reexpressed y"
        tit=paste("Power.x =", 
          round(power.x, 1), "Power.y =", round(power.y, 
          1), "(Half-slope ratio =", round(fit$half.slope.ratio, 
                  2), ")")
        par(mfrow=c(2,1))
        do.call("plot", 
           c(alist(reexpressed.x, reexpressed.y, ylab=ylb,
              xlab=xlb, main=tit), args))
        abline(a = fit$a - fit$b * fit$xC, b = fit$b)
        do.call("plot",
           c(alist(reexpressed.x, fit$residual, 
              ylab = "Residual", 
              main = "Residual Plot"), args)) 
        abline(h = 0)

    }
    slider(refresh, sl.names = c("Power of X Reexpression", "Power of Y Reexpression"), 
        sl.mins = c(-2, -2), sl.maxs = c(2, 2), sl.deltas = c(0.1, 
            0.1), sl.defaults = c(1, 1))
    refresh()
    par(mfrow = c(1, 1))
}
