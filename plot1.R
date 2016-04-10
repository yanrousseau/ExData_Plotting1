################################################################################
# Builds a series of plots regarding electric power consumption in an unknown
# city on February 1st and 2nd 2007. This function builds the following plot:
# Plot #1: Histogram of global active power
# Author : Yannick Rousseau
# Date   : 2016-04-09
################################################################################

buildPlot1 <- function() {
    
    library(lubridate)
    # setwd("D:\\doc\\Education\\JH-DataScience\\4. Exploratory data analysis\\Week #1\\Lesson #7 - Course project 1")
    
    # Parameters.
    fn  <- "household_power_consumption.txt"
    dtA <- strptime("1/2/2007", format="%d/%m/%Y")
    dtB <- strptime("2/2/2007", format="%d/%m/%Y")
    
    # Create the data frame that will hold the data.
    df  <- as.data.frame(matrix(0, ncol=9, nrow=0))
    
    # Read the lines in the data file corresponding to the dates of interest and
    # dynamically insert each relevant record into a data frame.
    # The records not in the range [dtA-dtB] are not added to the data frame.
    # The incomplete records (containing the symbol '?') are ignored.
    cn <- file(fn, 'r')
    i = 1
    while (length(ln <- readLines(cn, n=1)) > 0) {
        if (grepl("\\?", ln)) next
        tokens <- strsplit(ln, ";")[[1]]
        # Column variables are read and stored.
        if (i == 1) {
            varNames <- gsub("_", "", tokens)
            varNames <- tolower(varNames)
        }
        # Records are read and stored.
        else  {
            # After reading the first record, the variable names are assigned to
            # the data frame.
            if (i == 2) {
                names(df) <- varNames
                df[,1:2] <- sapply(df[,1:2],as.character)
                df[,3:9] <- sapply(df[,3:9],as.numeric)
            }
            # The record is inserted if it meets the date criterion.
            dt <- strptime(tokens[1], format="%d/%m/%Y")
            if ((year(dt)  >= year(dtA))  && (year(dt)  <= year(dtB)) &&
                (month(dt) >= month(dtA)) && (month(dt) <= month(dtB)) &&
                (day(dt)   >= day(dtA))   && (day(dt)   <= day(dtB))) {
                newRec        <- as.data.frame(matrix(0, ncol=9, nrow=1))
                newRec[1,]    <- tokens[1:9]
                newRec[,3:9]  <- as.numeric(newRec[,3:9])
                names(newRec) <- varNames
                df <- rbind(df, newRec)
            }
        }
        i <- i + 1
    }
    close(cn)
    
    # Comnbine the date and time information into a single date-time object.
    dt        <- strptime(paste(df$date, df$time), format="%d/%m/%Y %H:%M:%S")
    df        <- cbind(dt, df[,3:9])
    names(df) <- c("datetime", names(df[,2:ncol(df)]))
    
    # Build plot #1.
    par(mfrow=c(1,1))
    hist(df$globalactivepower, main="Global Active Power",
        xlab="Global Active Power (kilowatts)", ylab="Frequency", col="red")
    dev.copy(png, file="plot1.png", width = 480, height = 480)
    dev.off()
}