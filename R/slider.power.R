slider.power=function (x, ...) 
{
    if (missing(x) || length(x) < 2) 
        return("Error: x must be a vector")
    Power = 1
    args <- list(...)
    refresh <- function(...) {
        power <- slider(no = 1)
        power <- ifelse(power == 0, 1e-04, power)
        reexpressed = (x^power - 1)/power
        xlb="Reexpressed Data"
        tit=paste("Power =", round(power, 1), 
        ", d =", round(hinkley(reexpressed), 2))
        do.call("hist", 
           c(alist(x = reexpressed, xlab=xlb, main=tit), args))
    }
    slider(refresh, "Power of Reexpression", -2, 2, 0.1, Power)
    refresh()
    "use slider to select the power of the reexpression"
}
