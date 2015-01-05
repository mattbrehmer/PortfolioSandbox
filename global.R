#global.R
#Portfolio Visualziation Sketches
#by Matt Brehmer
#2013-2014

# this is the main file that server.R sources
# loads and tidies metadata, performance, and tag data for a portfolio from separate local CSV files 

#load packages ------------------------------------------------------------------------------------------
# load required packages

require(shiny)
require(ggplot2)
require(data.table)
require(plyr)
require(foreach)
require(reshape2)
require(grid)
require(rCharts)
require(leaflet)
require(maps)
require(scales)
require(directlabels)
require(rjson)
require(proto)

#wrangle and tidy ------------------------------------------------------------------------------------------
# data wrangling and tidying

source("dataFunctions.R", local=TRUE)

#load space metadata, performance data from CSV in local directory
meta.dt <- read.csv(
	"data/portfolio-metadata.csv",
	stringsAsFactors=TRUE)

#call metadata tidying function from portfolioWrangling.R
meta.dt <- TidyMetadata(meta.dt)

#subset to named buildings
meta.dt <- subset(meta.dt,name!="NULL")
meta.dt$name <- factor(meta.dt$name)
meta.dt$space_type <- factor(meta.dt$space_type)
meta.dt$space_use <- factor(meta.dt$space_use)
meta.dt$use_type <- factor(meta.dt$use_type)

#load portfolio performance data from CSV in local directory
performance.dt <- unique(as.data.table(read.csv(
	"data/portfolio-performance.csv",
	stringsAsFactors=FALSE)
))

#remove duplicate records returned by API call (should they exist)
performance.dt <- performance.dt[!duplicated(performance.dt)]

#don't include any data earlier than Jan 1 2012
performance.dt <- performance.dt[Year > 2011,]

#type coercion: convert ints to ordered factors
performance.dt$space_id <- as.factor(performance.dt$space_id)
performance.dt$date <- as.Date(performance.dt$Time)
performance.dt$Year <- as.ordered(performance.dt$Year)
#correcting weekday date mismatch: sundays are 1st day of the week, saturdays are the last day of the week
performance.dt$WeekOfYear <- as.numeric(performance.dt$WeekOfYear)
performance.dt[DayOfWeek==1 & Year==2013]$WeekOfYear <- performance.dt[DayOfWeek==1 & Year==2013]$WeekOfYear + 1
performance.dt[DayOfWeek==7 & Year==2012]$WeekOfYear <- performance.dt[DayOfWeek==7 & Year==2012]$WeekOfYear - 1
performance.dt$WeekOfYear <- as.ordered(performance.dt$WeekOfYear)
performance.dt$TimeOfDay <- as.ordered(performance.dt$Hour)
performance.dt$DayOfYear <- as.ordered(performance.dt$DayOfYear)
performance.dt$DayOfMonth <- as.ordered(performance.dt$DayOfMonth)
performance.dt$DateTimeStmp <- as.ordered(performance.dt$DateTimeStmp)

performance.dt$season <- as.factor(performance.dt$season)
seasons <- c("winter","spring","summer","fall")
performance.dt$season <- factor(performance.dt$season,levels = seasons, ordered=TRUE)

#create ordered factor weekday from int DayOfWeek
performance.dt$weekday <- as.factor(performance.dt$DayOfWeek)
levels(performance.dt$weekday) <- c(
	"1" = "sun",
	"2" = "mon",
	"3" = "tue",
	"4" = "wed",
	"5" = "thu",
	"6" = "fri",
	"7" ="sat")

#create ordered factor month from int Month
performance.dt$month <- as.factor(performance.dt$Month)
levels(performance.dt$month) <- c(
	"1" = "jan",
	"2" = "feb",
	"3" = "mar",
	"4" = "apr",
	"5" = "may",
	"6" = "jun",
	"7" ="jul",
	"8" = "aug",
	"9" = "sep",
	"10" = "oct",
	"11" = "nov",
	"12" = "dec")

#create ordered week month from int WeekOfYear and Date Time
performance.dt[,week:=paste(min(Time),"-",max(Time)),by=list(Year,WeekOfYear)]
performance.dt$week <- as.ordered(performance.dt$week)

#reshape ------------------------------------------------------------------------------------------
# reshaping performance data based on resource type

#first, set NA = 0 for resource variables
performance.dt$Electricity[is.na(performance.dt$Electricity)] <- 0
performance.dt$NaturalGas[is.na(performance.dt$NaturalGas)] <- 0
performance.dt$Steam[is.na(performance.dt$Steam)] <- 0
performance.dt$Steam[is.na(performance.dt$Steam)] <- 0
performance.dt$Elec_Baseload[is.na(performance.dt$Elec_Baseload)] <- 0
performance.dt$Total_Baseload[is.na(performance.dt$Total_Baseload)] <- 0

#remove duplicate records returned by API call (should they exist)
performance.dt <- performance.dt[!duplicated(performance.dt)]

#set data table keys for reshaping
setkey(performance.dt,space_id,DateTimeStmp)

#melt resource values into single column
performance.melt <- as.data.table(
	melt(
		performance.dt[!duplicated(performance.dt)],
		id.vars=c("space_id","DateTimeStmp"),
		measure.vars=c("Total","Electricity","NaturalGas","Steam","Elec_Baseload","Total_Baseload"),
		variable.name="resource",
		value.name="value"
	)
)

#set keys for new reshaped data table
setkey(performance.melt,space_id,DateTimeStmp)

#join new reshaped data table with performance data
performance.dt <- performance.dt[!duplicated(performance.dt)]
performance.dt <- performance.dt[performance.melt]

#remove the new reshaped data table (no longer needed)
remove(performance.melt)

#set keys for new joined data table
setkey(performance.dt,space_id)

# throw out original resource attributes in performance.dt
performance.dt = subset(performance.dt, select=-c(
	Total,
	Electricity,
	NaturalGas,
	Steam,
	Total_Baseload,
	Elec_Baseload
))

#replace negative / missing values with 0
performance.dt[is.na(value)]$value <- 0

#create baseline and baseline HDD/CDDs
performance.dt[,baseline:=unique(value)[1],by=list(space_id,resource,WeekOfYear,DayOfWeek,Hour)]
performance.dt[,baselineHDD:=unique(HDD)[1],by=list(space_id,resource,WeekOfYear,DayOfWeek,Hour)]
performance.dt[,baselineCDD:=unique(CDD)[1],by=list(space_id,resource,WeekOfYear,DayOfWeek,Hour)]

performance.dt[Year==2012]$baseline <- performance.dt[Year==2012]$value
performance.dt[Year==2012]$baselineHDD <- performance.dt[Year==2012]$HDD
performance.dt[Year==2012]$baselineCDD <- performance.dt[Year==2012]$CDD

#load tags ------------------------------------------------------------------------------------------
# load and tidy the tags for a portfolio of spaces

#load and format portfolio tags from CSV in local directory
spacetags.dt <- as.data.table(read.csv("data/portfolio-tags.csv",stringsAsFactors=TRUE))

if(exists("spacetags.dt")) {
	
	#reshape the table of tags into two columns: space_id and tag
	spacetags.dt <- as.data.table(melt(spacetags.dt,id.vars=c("id","name"),value.name="tag"))
	spacetags.dt <- subset(spacetags.dt, select = -c(variable,name))
	setnames(spacetags.dt,colnames(spacetags.dt)[1],"space_id")
	
	#type coercion
	spacetags.dt$space_id <- factor(spacetags.dt$space_id)
	spacetags.dt$tag <- factor(spacetags.dt$tag)
	
	#set key for tag data table 
	setkey(spacetags.dt,space_id)
	spacetags.dt <- spacetags.dt[spacetags.dt$tag!="",] 
	spacetags.dt$tag  <- factor(spacetags.dt$tag)
	
	#generate master tag list (used by input$tag_filter)
	taglist.dt <- data.table(unique(spacetags.dt$tag))
	setnames(taglist.dt,1,"tag")
	setkey(taglist.dt,tag)
	taglist.dt <- arrange(taglist.dt,tag)
	taglist.dt$tag <- as.factor(taglist.dt$tag)
}