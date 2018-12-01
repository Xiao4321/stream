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
mean(df$`9`)

km.df <- as.tibble(list("eeg1"=df$`1`, "eeg9"=df$`9`))

x <- 1:4097
plot(x=x, y=km.df$eeg9)

plot(x=x, y=km.df$eeg1)

km <- kmeans(km.df, centers=2, iter.max=10000)

hist(km.df$eeg1)
hist(km.df$eeg9)

## Real version
stream <- DSD_Memory(km.df)

sample <- DSC_Sample(k=2)
update(sample, stream, 96)
kmeans <- DSC_Kmeans(k=2)
recluster(kmeans, sample)
plot(kmeans, stream, type="both")

birch <- DSC_BIRCH(treshold = .1, branching = 8, maxLeaf = 20)

animate_cluster(birch, km.df, horizon=102, n=4097*3)
