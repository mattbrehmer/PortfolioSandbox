#dataFunctions.R
#Portfolio Visualziation Sketches
#by Matt Brehmer
#2013-2014

# this file is sourced by server.R for manipulating data
# Tidying metadata
# performs generic filter functions for strings, numerical ranges, and options
# computes differentials

# tidy metadata ------------------------------------------------------------------------------
#' TidyMetadata
#' @author Matt Brehmer
#' @export
#' @description tidying the space metadata 
#' @param metadata, space metadata for a portfolio
#' @return metadataDT, clean metadata data table
TidyMetadata = function(metadata) {
	
	metadata$area = gsub("NULL","",metadata$area)
	metadata$area = as.numeric(metadata$area)
	
	if (unique(metadata$area_unit)[1] == "SquareFeet")
		metadata$area <- metadata$area / 10.76391
	
	#throw out these metadata attributes
	metadata = subset(metadata, select=-c(
		time_zone_id,
		address,
		description,
		premise_id,
		area_unit,
		naics_code,
		enrollment_date,
		disabled,
		name_hash,
		name_encrypted,
		baseline_end_date,
		fields_encrypted,
		update_timestamp,
		hide_children,
		group_id
	))
	
	#coerce data types
	metadata$name = gsub(",","",metadata$name)
	metadata$name = as.factor(metadata$name)
	metadata$id = as.factor(metadata$id)
	metadata$organization_id = as.factor(metadata$organization_id)
	metadata$use_type = gsub("NULL","",metadata$use_type)
	metadata$use_type = as.factor(metadata$use_type)
	metadata$occupant_count = gsub("NULL","",metadata$occupant_count)
	metadata$occupant_count = as.integer(metadata$occupant_count)
	# metadata$address = as.character(metadata$address)
	metadata$city = gsub("é","e",metadata$city)
	metadata$city = gsub("NULL","",metadata$city)
	metadata$city = as.factor(metadata$city)
	metadata$state_province_id = gsub("NULL","",metadata$state_province_id)
	metadata$state_province_id = as.factor(metadata$state_province_id)
	metadata$longitude = gsub("NULL","",metadata$longitude)
	metadata$longitude = as.double(metadata$longitude)
	metadata$latitude = gsub("NULL","",metadata$latitude)
	metadata$latitude = as.double(metadata$latitude)
	metadata$weather_station = as.factor(metadata$weather_station)
	metadata$year_constructed = gsub("NULL","",metadata$year_constructed)
	metadata$building_age = as.numeric(format(Sys.Date(), "%Y")) - as.numeric(metadata$year_constructed)
	metadata$weekly_operating_hours = gsub("NULL","",metadata$weekly_operating_hours)
	metadata$weekly_operating_hours = as.numeric(metadata$weekly_operating_hours)
	metadata$parent_id = as.factor(metadata$parent_id)
	metadata$tmy_weather_station = as.factor(metadata$tmy_weather_station)
	
	metadataDT = as.data.table(metadata)
	setnames(metadataDT,1,"space_id")
	setkey(metadataDT,space_id)
	
	return(metadataDT)
}

#in_range ------------------------------------------------------------------------------
#' in_range
#' @author Matt Brehmer
#' @export
#' @description Returns a logical vector of which values in `x` are within the min and max values of `range`.
#' @param x, a vector
#' @param range, max and min values from a sliderInput UI component
#' @return Returns a logical vector of which values in `x` are within the min and max values of `range`.
in_range <- function(x, range) {
	if (is.null(range))
		T
	else
		x >= min(range) & x <= max(range)
}

#string_match ------------------------------------------------------------------------------
#' string_match
#' @author Matt Brehmer
#' @export
#' @description Returns a logical vector of which values in `x` partially match the textInput query
#' @param x, a vector
#' @param nameQuery, text from a textInput UI component
#' @return Returns a logical vector of which values in `x` partially match the textInput query
string_match <- function(x, nameQuery) {
	if (is.null(nameQuery))
		T
	else
		grepl(nameQuery,x, ignore.case=T)
}

#option_match ------------------------------------------------------------------------------
#' option_match
#' @author Matt Brehmer
#' @export
#' @description Returns a logical vector of which values in `x` match the selected options.
#' @param x, a vector
#' @param selected, a list of selected options from a selectInput UI component where multiple = TRUE
#' @return Returns a logical vector of which values in `x` match the selected option
option_match <- function(x, selected) {
	if (is.null(selected) || selected == "all")
		T
	else
		x %in% selected
	# 		grepl(paste(selected,collapse="|"),x)
}

#exact_match ------------------------------------------------------------------------------
#' exact_match
#' @author Matt Brehmer
#' @export
#' @description Returns a logical vector of which values in `x` match the selected option.
#' @param x, a vector
#' @param selected, a list of selected options from a selectInput UI component where multiple = FALSE
#' @return Returns a logical vector of which values in `x` match the selected option
exact_match <- function(x, selected) {
	if (is.null(selected) || selected == "all")
		T
	else
		selected == x
}

#computeDifferential ------------------------------------------------------------------------------
#' computeDifferential
#' @author Matt Brehmer
#' @export
#' @description for each unique space-interval-resource combination in a data table, compute a differential between 2 years (or actual:baselineQuantity)
#' @param data.dt, a data table containing multiple time period (or actual and baselineQuantity), containing space name, resource, interval, Year, quantity
#' @param omit_outages, logical value for omitting outages (±100% changes), useful when space shut-downs or start-ups in either year skew the scale
#' @return data.dt, data table containing unique combinations of space name, resource, interval, and differential quantity (absolute, normalized)
computeDifferential <- function(data.dt,omit_outages) 
{	
	data.dt <- data.dt[quantity>=0 & baselineQuantity>=0,]
	
	#compute diffs
	data.dt[,quantityAbsDiff:=quantity-baselineQuantity]
	
	#if quantity != 0 and baselineQuantity != 0, diff is absolute difference / quantity b
	data.dt <- data.dt[quantity!=0 & baselineQuantity!=0,quantityRelDiff:=(quantityAbsDiff/baselineQuantity)*100]
	
	#if quantity != 0 and baselineQuantity == 0, diff is +100%, if quantity == 0 and baselineQuantity != 0, diff is -100%
	if (!omit_outages) 
	{
		data.dt <- data.dt[quantity==0 & baselineQuantity==0,quantityRelDiff:=0]
		data.dt <- data.dt[quantity!=0 & baselineQuantity==0,quantityRelDiff:=100]	
		data.dt <- data.dt[quantity==0 & baselineQuantity!=0,quantityRelDiff:=-100]
	}
	else # prevent these values from skewing the scale
	{
		data.dt <- data.dt[quantity==0 & baselineQuantity==0,quantityRelDiff:=NA]
		data.dt <- data.dt[quantity!=0 & baselineQuantity==0,quantityRelDiff:=NA]
		data.dt <- data.dt[quantity==0 & baselineQuantity!=0,quantityRelDiff:=NA]
		data.dt <- data.dt[quantity!=0 & baselineQuantity==0,quantityAbsDiff:=NA]
		data.dt <- data.dt[quantity==0 & baselineQuantity!=0,quantityAbsDiff:=NA]
	}
		
	return(data.dt)	
}

#uniqueSubset ------------------------------------------------------------------------------
#' uniqueSubset
#' @author Matt Brehmer
#' @export
#' @description subsets data returned by a reactive function  
#' @param data.dt, a data table returned by Query()
#' @param nSpaces, numeric value indicating of space items to retain
#' @param sortDir, logical indicating whether to subset the head or tail of the data table
#' @return data.dt, a data table with additional rank information assigned by reference
uniqueSubset <- function(data.dt,nSpaces,sortDir){
	
	#subset to unique combinations of name,resource,Year,interval,quantity
	data.dt <- unique(data.dt[, list(name,resource,Year,interval,quantity,baselineQuantity) ])
	
	#subset to top 10
	data.dt[,meanQuantity:=mean(quantity), by=list(name)]	
	subData.dt <- subset(data.dt, select = c(name,meanQuantity))	
	subData.dt <- unique(subData.dt[, list(name,meanQuantity) ])
	subData.dt <- arrange(subData.dt,desc(meanQuantity))	
	
	#take head or tail or table, based on user input and size specified
	if (sortDir)
		subData.dt <- head(subData.dt,n=nSpaces)
	else
		subData.dt <- tail(subData.dt,n=nSpaces)
	
	data.dt <- data.dt[data.dt$name %in% subData.dt$name,]
	
	return(data.dt)
}

#assignRanks ------------------------------------------------------------------------------
#' assignRanks
#' @author Matt Brehmer
#' @export
#' @description assign ranks and rank deltas 
#' @param data.dt, a data table returned by Query()
#' @param sortDir, logical indicating whether to subset the head or tail of the data table
#' @return data.dt, a data table with additional rank information assigned by reference
assignRanks <- function(data.dt,sortDir){
	
	#sort the intervals
	sortedIntervals <- unique(data.dt$interval)
	
	#compute numeric interval indices
	data.dt[,intervalIndex:=which(sortedIntervals==interval,arr.ind=TRUE),by=list(name)]
	
	#scale the quantity to a range between 0 and 0.5 (for drawing geom_rects between numeric interval indices)
	data.dt[,sQuant:=scale(quantity,center=FALSE, scale=max(quantity, na.rm = TRUE)/0.5)]
	
	#compute the absolute range in quantity over all intervals
	data.dt[,QRange:=length(unique(quantity)), by=list(name)]
	
	#compute the rank of a quantity by year/interval
	data.dt[,Rank:=rank(quantity,na.last=NA,ties.method= "max"), by=list(intervalIndex)]
	
	#compute the range of ranks for each unique name
	data.dt[,RankRange:=max(Rank)-min(Rank)+1, by=list(name)]
	
	#scale the rank ranges between 0 and 1 for opacity/alpha aesthetic
	data.dt[,nRankRange:=scale(RankRange,center=FALSE, scale=max(RankRange, na.rm = TRUE)/1)]
	
	#compute the number of unique ranks by interval (used for computing optimal chart height)
	data.dt[,UniqueRanks:=length(unique(Rank)), by=list(interval)]
	
	data.dt[,nextRank:=Rank]
	
	#arrange the data table by name, then by interval
	data.dt <- arrange(data.dt,name,interval)
	
	for (i in 1:nrow(data.dt))
	{		
		#get next rank for current name, year, interval
		if(data.dt[i]$interval != sortedIntervals[length(sortedIntervals)])
				data.dt[i]$nextRank <- data.dt[i+1]$Rank
	}
		
	if(!sortDir)
	{
		maxRank <- max(data.dt$Rank,na.rm=T)
		data.dt$Rank <- maxRank - data.dt$Rank + 1
	}
	
	return(data.dt)	
}