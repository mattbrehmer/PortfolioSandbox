#selectOptions.R
#Portfolio Visualziation Sketches
#by Matt Brehmer
#2013-2014

#options ------------------------------------------------------------------------------

#specify the resource (selectInput)
output$resource_select <- renderUI({
	
	selectInput(
		inputId = "resource_select", 
		label = "Resource Type:",
		choices=c(
			"Total" = "Total",
			"Total Baseload" = "Total_Baseload",
			"Electricity" = "Electricity",
			"Electricity Baseload" = "Elec_Baseload",
			"Natural Gas" = "NaturalGas",
			"Steam" = "Steam"
		),
		selected = "Electricity",
		multiple = FALSE)
})

#specify the quantity to compute (selectInput)
output$quantity_select <- renderUI({
	
	selectInput(
		inputId = "quantity_select", 
		label = "Quantity:",
		choices=c(
			"Average Demand",
			"Min Demand",
			"Max Demand",
			"Total Consumption"
		),
		selected = "Average Demand")
})

# #specify the interval attribute (selectInput)
# output$interval_select <- renderUI({
# 	
# 	selectInput(
# 		inputId = "interval_select", 
# 		label = "Interval:",
# 		choices=c(
# 			"Year" = "Year",
# 			"Season" = "season",
# 			"Month" = "month",
# 			"Week" = "WeekOfYear",
# 			"Day of Year" = "DayOfYear",
# 			"Day of Month" = "DayOfMonth",
# 			"weekday" = "weekday",
# 			"Time of Day" = "TimeOfDay",
# 			"4h Intervals" = "DateTimeStmp"
# 		),
# 		selected = "Year")
# })

#specify the attribtue to aggregate by (selectInput)
output$aggregate_select <- renderUI({
	
	selectInput(
		inputId = "aggregate_select", 
		label = "Group Spaces by:",
		choices=c(
			"None",
			"Space Use",
			"Use Type",
			"Space Type",
			"City",
			"Tags"
		),
		selected = "None")
})

#specify the weather attribute to normalize by (selectInput)
output$weather_normalize_select <- renderUI({
	
	selectInput(
		inputId = "weather_normalize_select", 
		label = "Weather Normalization:",
		choices=c(
			"None",
			"HDDs",
			"CDDs"
		),
		selected = "None")
})	

#specify the attribute to normalize by (selectInput)
output$normalize_select <- renderUI({
	
	selectInput(
		inputId = "normalize_select", 
		label = "Space Normalization:",
		choices=c(
			"None",
			"Area",
			"Occupant Count",
			"Operating Hours"
		),
		selected = "None")
})	

#specify the differential comparison (from / source) (selectInput)
output$differential_a <- renderUI({
	
	selectInput(
		inputId = "differential_a", 
		label = "Compare 'A':",
		choices=c(levels(performance.dt$Year)),
		selected = "2013",
		multiple = FALSE)
})

#specify the differential comparison (to / target) (selectInput)
output$differential_b <- renderUI({
	
	selectInput(
		inputId = "differential_b", 
		label = "to 'B':",
		choices=c(levels(performance.dt$Year)),
		selected = "2012",
		multiple = FALSE)
})

#specify the item to highlight (selectInput)
output$highlight_select <- renderUI({
	
	result.dt <- Query()
	
	if (nrow(result.dt) == 0 || is.null(result.dt$quantity))
		names <- ""
	else
	{
		#subset the data to the highest/lowest n spaces
		result.dt <- uniqueSubset(result.dt,nSpaces=getNSpaces(),sortDir=(getSortDir()=="desc"))
		names <- as.character(unique(result.dt$name))
	}
	
	selectInput(
		inputId = "highlight_select", 
		label = "Highlight:",
		choices=c("None",names),
		selected = "None")
})	