hinkley=function(d)
{
md=median(d)
mn=mean(d)
s=diff(quantile(d,probs=c(.25,.75)))
(mn-md)/s
}