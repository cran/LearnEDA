slider.match=function (x, ...) 
{
    x.name <- deparse(substitute(x))
    if (missing(x) || length(x) < 2) 
        return("Error: x must be a vector")
    Power = 1
    args <- list(...)
    refresh <- function(...) {
        power <- slider(no = 1)
        power <- ifelse(power == 0, 1e-04, power)
        reexpressed = mtrans(x, power)
        xlb="Reexpressed Data"
        tit=paste("Power =", round(power, 1), ", d =", 
                round(hinkley(reexpressed), 2))
        do.call("boxplot", 
           c(alist(data.frame(raw = x, reexpressed), 
              main=tit, horizontal=TRUE), args))
    }
    slider(refresh, "Power of Reexpression", -2, 2, 0.1, Power)
    refresh()
    "use slider to select the power of the reexpression"
}
