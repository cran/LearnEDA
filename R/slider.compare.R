slider.compare=function (x, group, ...) 
{
    Power = 1
    args <- list(...)
    refresh <- function(...) {
        power <- slider(no = 1)
        power <- ifelse(power == 0, 1e-04, power)
        reexpressed = (x^power - 1)/power
        xlb="Reexpressed Data"
        tit=paste("Power =", round(power, 1))
        do.call("boxplot", 
           c(alist(reexpressed ~ group, 
              xlab=xlb, main=tit, horizontal=TRUE), args))
    }
    slider(refresh, "Power of Reexpression", -2, 2, 0.1, Power)
    refresh()
    "use slider to select the power of the reexpression"
}
