#plotHeightFunctions.R
#Portfolio Visualziation Sketches
#by Matt Brehmer
#2013-2014

#determine chart height based on number of name items in chart, number of year facets
plotHeight <- function() 
{
	result.dt <- Query()
	
	if (nrow(result.dt) == 0 || is.null(result.dt))
		return(0)
	else
		return(200 + 100*min(length(unique(result.dt$name)),getNSpaces()))
}

#determine chart height based on maximum number of unique ranks across all intervals
plotHeightLineUp <- function() 
{		
	result.dt <- Query()
	
	#check for empty data, and don't plot 4h intervals
	if (nrow(result.dt) == 0 || is.null(result.dt))
		return(0)
	
	#subset the data to the highest/lowest n spaces
	result.dt <- uniqueSubset(result.dt,nSpaces=getNSpaces(),sortDir=(getSortDir()=="desc"))
	
	#rank the data
	ranked.dt <- assignRanks(result.dt,sortDir=getSortDir()=="desc") 
	
	return(200 + 75*max(ranked.dt$UniqueRanks))
}

#determine chart height based on number of unique name items
plotHeightTimeSeries <- function() 
{
	result.dt <- Query()
	
	#check for empty data
	if (nrow(result.dt) == 0 || is.null(result.dt))
		return(0)
	
	return(200 + 325*ceiling(min(length(unique(result.dt$name)),getNSpaces()) / 4))
}