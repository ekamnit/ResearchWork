#----------------Om NamahShivaaya-----------------#
#----------------ShivaayaNamah Om-----------------#

#-----------------Data collection-----------------#
#-------------------1.PROMISE---------------------#

setwd('/media/sharma/6E88CA0788C9CE31/Research/Sharma/Work/DataSets/PROMISE/')

# Reading all file names from the current directory
filenames <- list.files(full.names=TRUE)
All <- lapply(filenames,function(i){read.csv(i,header=FALSE, skip=1)})
PROMISE <- do.call(rbind.data.frame, All)

#-------------------2.AEEEM---------------------#

setwd('/media/sharma/6E88CA0788C9CE31/Research/Sharma/Work/DataSets/AEEEM/')

# Reading all file names from the current directory
filenames <- list.files(full.names=TRUE)
All <- lapply(filenames,function(i){read.csv(i,header=FALSE, skip=1)})
PROMISE <- do.call(rbind.data.frame, All)
