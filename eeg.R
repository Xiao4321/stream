library(tidyverse)
library(stream)

my.files <- list.files("./Data/EEG/S/")

read.eeg <- function(file)
{
  read_csv(paste("./Data/EEG/S/", file, sep=""), col_names=F)$X1
}

## Cheating: I want low standard deviations
mean <- lapply(my.files, (function(file) mean(read.eeg(file))))
sd <- lapply(my.files, (function(file) sd(read.eeg(file))))

epsilon <- 3
low.sd <- which(sd < min(unlist(sd)) + epsilon)
data.list <- lapply(my.files[low.sd], read.eeg)
names(data.list) <- "1":"9"
df <- as.tibble(data.list)

mean(df$`1`)
mean(df$`2`)
mean(df$`9`)

km.df <- as.tibble(list("eeg1"=df$`1`, "eeg2"=df$`2`, "eeg3"=df$`9`))

x <- 1:4097
plot(x=x, y=km.df$eeg3)

km <- kmeans(km.df, centers=3, iter.max=10000)

plot(km.df, col=km$cluster)

plot(km.df$eeg1)
plot(km.df$eeg2)
plot(km.df$eeg3)

hist(km.df$eeg1)
hist(km.df$eeg2)
hist(km.df$eeg3)

## Terrible:
data.stream.candidate <- unlist(data)
km <- kmeans(data.stream.candidate, centers=3)
plot(x=1:length(data.stream.candidate), y=data.stream.candidate, col=km$cluster)
## End Terrible

## TODO : Have three simultaneous streams...

## Try a simulated version and then the real version.
stream <- DSD_Gaussians(k=3, d=1, mu=unlist(lapply(km.df, mean)), noise=1)

## Real version
stream <- DSD_Memory(km.df)

sample <- DSC_Sample(k=3)
update(sample, stream, 96)
sample

kmeans <- DSC_Kmeans(k=3)
recluster(kmeans, sample)
plot(kmeans, stream, type="both")

animate_cluster(kmeans, stream, horizon=102, n=4097*3)
