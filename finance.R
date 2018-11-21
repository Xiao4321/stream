library(quantmod)
library(stream)

plot(c(), xlim = c(0, 100), ylim = c(-100, 100), xlab = "X", ylab = "Y", main = "GE")
while (TRUE) {
    quotes <- getQuote("GE")
    points(quotes$Last)
    ## Sys.sleep(time = 5)
}

plot(c(), xlim = c(0, 100), ylim = c(-100, 100), xlab = "X", ylab = "Y", main = "SPY")
while (TRUE) {
    quotes <- getQuote("SPY")
    points(quotes$Last)
    Sys.sleep(time = 5)
}

getQuote("GE")

stream <- DSD_Gaussians(k=3, noise=0)

sample <- DSC_Sample(k=20)
update(sample, stream, 500)
sample
kmeans <- DSC_Kmeans(k=3)
recluster(kmeans, sample)
plot(kmeans, stream, type="both")



## Not run:
stream <- DSD_Benchmark(1)
animate_data(stream, horizon=100, n=5000, xlim=c(0,1), ylim=c(0,1))
### animations can be replayed with the animation package
library(animation)
animation::ani.options(interval=.1) ## change speed
ani.replay()
### animations can also be saved as HTML, animated gifs, etc.
saveHTML(ani.replay())
### animate the clustering process with evaluation
### Note: we choose to exclude noise points from the evaluation
###
measure calculation, even if the algorithm would assign
###
them to a cluster.
reset_stream(stream)
dbstream <- DSC_DBSTREAM(r=.04, lambda=.1, gaptime=100, Cm=3,
                         shared_density=TRUE, alpha=.2)
animate_cluster(dbstream, stream, horizon=100, n=5000,
                measure="crand", type="macro", assign="micro", noise = "exclude",
                plot.args = list(xlim=c(0,1), ylim=c(0,1), shared = TRUE))
## End(Not run)




## create data stream with three clusters in 3-dimensional data space
stream1 <- DSD_Gaussians(k=3, d=2)
plot(stream1)

## create data stream with specified clusterpositions, 20% noise in a given
## bounding box and with different densities (1 to 9 between the two clusters)
stream2 <- DSD_Gaussians(k=2, d=2,
                         mu=rbind(c(-.5,-.5), c(.5,.5)),
                         noise=0.2, noise_range=rbind(c(-1,1),c(-1,1)),
                         p=c(.1,.9))
plot(stream2)

stream2$noise










## store 10000 points from a stream
stream <- DSD_Gaussians(k=3, d=2)
replayer <- DSD_Memory(stream, k=3, n=10000)
replayer
plot(replayer)


## creating 2 clusterers of different algorithms
dsc1 <- DSC_DBSTREAM(r=0.1)
dsc2 <- DSC_DStream(gridsize=0.1, Cm=1.5)

## clustering the same data in 2 DSC objects
reset_stream(replayer) # resetting the replayer to the first position
update(dsc1, replayer, 500)
reset_stream(replayer)
update(dsc2, replayer, 500)

## plot the resulting clusterings
reset_stream(replayer)
plot(dsc1, replayer, main="DBSTREAM")
reset_stream(replayer)
plot(dsc2, replayer, main="D-Stream")

## use a data.frame to create a stream (3rd col. contains the assignment)
df <- data.frame(x = runif(1000), y = runif(1000), class = sample(1:3, 1000, replace = TRUE))
head(df)

stream <- DSD_Memory(df[,c("x", "y")], class=df[,"class"])
stream
plot(stream)

DSD_mlbenchData(data = "DNA", loop = FALSE, random = FALSE, scale = FALSE)

DSD_mlbenchData(data = "Ionosphere", loop = FALSE, random = FALSE, scale = FALSE)
