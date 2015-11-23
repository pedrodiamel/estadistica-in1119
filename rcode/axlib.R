## If you want to source() a bunch of files, something like
## the following may be useful:
sourceDir <- function(path, trace = TRUE, ...) {
    for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
       if(trace) cat(nm,":")
       source(file.path(path, nm), ...)
       if(trace) cat("\n")
    }
}

##Loading data
loadData <- function(path)
{
	#Load data
	con = file(path, "r");
	DB <- read.csv(con, head=T, sep=";");
	#DB <- read.table(con, sep=",");
	close(con);	
	return(DB);
}

