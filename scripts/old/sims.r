source('functions.r')

#surface = MLE.surface(data6$radD + pi, seq(1, 20, .1), seq(1, 100, 1), L2vonmixed.fixmew)

#WHy are our algorithms returning different results?
#BROKEN: need to load data6 for it to run.. also, this issue was resolved.
if (TRUE){
par(mfrow = c(2,2))
hist(data6$radD + pi, freq = FALSE)$breaks
hist(genvonmixed(10000, pi, 1.853, 1), freq = FALSE, breaks = breaks)
hist(genvonmixed(10000, pi, 11.869, .218), freq = FALSE, breaks = breaks)
hist(genvonmixed(10000, pi, 11.869, 1-.218), freq = FALSE, breaks = breaks)
##Try 2
par(mfrow = c(2,2))
plot(density(data6$radD + pi))
plot(density(genvonmixed(10000, pi, 1.853, 1)))
plot(density(genvonmixed(10000, pi, 11.869, .218)))
plot(density(genvonmixed(10000, pi, 11.869, 1-.218)))
}