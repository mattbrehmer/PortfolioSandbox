#ui.R
#Portfolio Visualziation Sketches
#by Matt Brehmer
#2013-2014

#header ------------------------------------------------------------------------------------------

shinyUI(pageWithSidebar(
	
	headerPanel("Portfolio Visualization Sandbox"),
	
	#sidebar ------------------------------------------------------------------------------------------
	
	sidebarPanel(
		tags$head(
			tags$style(type="text/css", "input { max-width: 165px; }"),
			tags$style(type="text/css", "select { max-width: 165px; }"),
			tags$style(type="text/css", ".jslider { max-width: 200px; }"),
			tags$style(type='text/css', ".span4 { max-width: 450px; }")
		),		
		h5("Filter and Sort Spaces (by Quantity)"),
		div(class="span20",				
				div(class="span6", numericInput("nSpaces", "# Results:", 5,min=2,max=100)),
				div(class="span2", radioButtons("sortDir", "Sort:", c("Descending" = "desc", "Ascending" = "asc")))
		),
		h5("Filter Time Window"),
		
		uiOutput("date_filter"),	
		div(class="span20",				
				div(class="span5", radioButtons("dayOfWeekSelect", "Day of Week", c("All" = "all", "Weekdays" = "weekday","Weekends" = "weekend"))),
				div(class="span5", radioButtons("timeOfDaySelect", "Time of Day", c("All" = "all", "Day: 08-20h" = "daytime","Night: 20-08h" = "nighttime")))
		),
		checkboxInput("date_filter_cb", "Additional Time Filtering:", FALSE),
		conditionalPanel(
			condition = "input.date_filter_cb == true",
			div(class="span20",
					div(class="span5", uiOutput("season_filter")),
					div(class="span5", uiOutput("month_filter"))
			)		
		),
		
		#leaflet map ------------------------------------------------------------------------------------------
		
		h5("Select a Space on the Map"),
		leafletMap(			
			"map", "100%", 400,
			initialTileLayer = "http://{s}.tile.stamen.com/toner-lite/{z}/{x}/{y}.png",
			initialTileLayerAttribution = HTML(
				'&copy; <a href="http://maps.stamen.com/">Stamen Design</a>, 
				<a href="http://creativecommons.org/licenses/by/3.0/">CC-BY-3.0</a>'),
			options=list(
				center = c(
					(min(meta.dt$latitude,na.rm=TRUE) + max(meta.dt$latitude,na.rm=TRUE)) / 2, 
					(min(meta.dt$longitude,na.rm=TRUE) + max(meta.dt$longitude,na.rm=TRUE)) / 2
				),
				zoom = 7,
				maxBounds = list(list(17, -180), list(59, 180))
			)
			),
		
		#metadata filtering ------------------------------------------------------------------------------------------
		
		h5("Filter by Space Metadata"),
		div(class="span20",
				div(class="span5", uiOutput("name_filter")),
				div(class="span5", uiOutput("city_filter"))
		),
		div(class="span20",
				div(class="span5", uiOutput("tag_filter")),
				div(class="span5", uiOutput("space_type_filter"))
		),
		div(class="span20",
				div(class="span5", uiOutput("use_type_filter")),
				div(class="span5", uiOutput("space_use_filter"))
		),
		
		#additional filtering ------------------------------------------------------------------------------------------
		
		checkboxInput("filter_cb", "Additional Space Filtering:", FALSE),
		conditionalPanel(
			condition = "input.filter_cb == true",
			checkboxInput("area_cb", "Area:", FALSE),
			uiOutput("area_filter"),
			checkboxInput("occupant_count_cb", "Occupant Count:", FALSE),
			uiOutput("occupant_count_filter"),
			checkboxInput("weekly_operating_hours_cb", "Weekly Operating Hours:", FALSE),
			uiOutput("weekly_operating_hours_filter"),
			checkboxInput("building_age_cb", "Building Age:", FALSE),
			uiOutput("building_age_filter")
		)
	),
	
	#main panel ------------------------------------------------------------------------------------------
	
	mainPanel(
		
		wellPanel(
			div(class="span15",
					div(class="span2", uiOutput("aggregate_select")),				
					div(class="span2", uiOutput("resource_select")),
					div(class="span2", uiOutput("quantity_select")),
					div(class="span2", uiOutput("weather_normalize_select")),
					div(class="span2", uiOutput("normalize_select"))
			),
			
			htmlOutput("queryResultNote")
		),
				
		tabsetPanel(
			id="vizTabs",
			
			tabPanel("Heatmaps",
							 
							 div(class="span15",
							 		
							 		div(class="span9", 
							 										 					
							 				conditionalPanel(
							 					condition = "input.heatmap_radio == 'basic'",
							 					plotOutput("heatmapPlot",width="100%",height="auto"),
							 					wellPanel(
							 						h5("Description:"),
							 						p("-- 'Quantity' values are displayed as heatmap tiles for each individual 
							 					 	or aggregate space (y axis) for each interval (x axis).")
							 					)
							 				),
							 				
							 				conditionalPanel(
							 					condition = "input.heatmap_radio == 'cal'",							 					
							 					plotOutput("calendarPlot",width="auto",height="auto"),
							 					wellPanel(
							 						h5("Description:"),
							 						p("-- 'Quantity' values are displayed as heatmap tiles for each individual 
							 					 	or aggregate space (vertical facets) for each calendar date.")
							 					)
							 				),
							 				
							 				conditionalPanel(
							 					condition = "input.diffAbsRel != 'no'",
							 					wellPanel(
							 						p("-- Differential 'Quantity' values, computed as the absolute or relative (%) difference relative to a baseline
							 						(in this case, 2012 values, aligned by week and weekday), are displayed as heatmap tiles for each individual or 
							 						aggregate space (y axis) for each interval (x axis)."), 
							 						p("-- Decreases are blue, increases are red, and unchanged is white; relative differences are capped at ±100%."), 
							 						p("-- Omitting outages is useful when space shut-downs or start-ups in either year skew the scale."),
							 						p("--Grey tiles indicate missing data.")
							 					)
							 				),
							 				
							 				h5("Options:"),							 				
							 				div(class="span15", 
							 						div(class="span2", radioButtons("heatmap_radio", "Select type of heatmap:",c("Basic" = "basic","Calendar" = "cal"))),
							 						div(class="span2", radioButtons("diffAbsRel", "Show Differential?", c("No" = "no","Absolute" = "abs", "Relative (%)" = "rel"))),
							 						div(class="span2", conditionalPanel(
							 							condition = "input.diffAbsRel != 'no'",
							 							radioButtons("omitOutages", "Omit Outages?:", c("No" = "n", "Yes" = "y"))
							 						)
							 						)
							 				)
							 		),
							 		
							 		div(class="span3", 
							 											 									 				
							 				conditionalPanel(
							 					condition = "input.summary_radio == 'box'",	
							 					plotOutput("boxPlot",width="100%",height="auto"),
							 					wellPanel(
							 						h5("Description:"),
							 						p("-- Box plots for individual or aggregate spaces (y axis), sorted by mean 'Quantity' values."),
							 						p("-- They represent the distribution of values along the x-axis: 
							 					 	the median (midline), the interquartile range or IQR (box), the whiskers (1.5*IQR), 
							 					 	and outliers (dots)."), 
							 						p("-- The 'Constrain Range?' option constrains the scale between 0 and the end of largest whisker + 5%;
							 					 	which effectively prevents distant outliers from skewing the scale of the plot.")
							 					)
							 				),							 	
							 				
							 				conditionalPanel(
							 					condition = "input.summary_radio == 'hist'",	
							 					plotOutput("histogram",width="100%",height="auto"),
							 					wellPanel(
							 						h5("Description:"),
							 						p("-- Histograms (density plots) for individual or aggregate spaces."),
							 						p("-- The 'Constrain Range?' option constrains the scale between 0 and the end of largest whisker for the equivalent boxplot + 5%;
							 					 	which effectively prevents distant outliers from skewing the scale of the plot.")
							 					)
							 				),
							 				
							 				conditionalPanel(
							 					condition = "input.summary_radio == 'bar'",	
							 					plotOutput("barPlot",width="100%",height="auto"),
							 					wellPanel(
							 						h5("Description:"),
							 						p("-- Bar charts for individual or aggregate spaces (y axis), sorted by mean 'Quantity' values (x-axis)."),
							 						p("-- Note: The 'Constrain Range?' does not affect bar charts (it applies to box plots and density plots).")
							 					)
							 				),
							 				
							 				h5("Options:"),
							 				div(class="span15", 
							 						div(class="span5", radioButtons("summary_radio", "Select type of summary plot:",c("Box" = "box","Hist" = "hist","Bar" = "bar"))),
							 						div(class="span5", radioButtons("omitOutliers", "Constrain Range?:", c("No" = "n", "Yes" = "y")))
							 				)
							 		)
							 )
			),	
			
			tabPanel("LineUp",
							 							 
# 							 tabsetPanel(
# 							 	id="lineupTabs",
							 	
# 							 	tabPanel("ggplot",
							 					 
							 plotOutput("lineUpPlot",width="100%",height="auto"),
							 wellPanel(
							 	h5("Description:"),
							 	p("-- Lines indicate a set of ranks for individual or aggregate spaces."),
							 	p("-- Ranks are computed for each interval according to the selected 'Quantity' for the selected 'Resource' 
							 	based on mean quantity across current time window."),
							 	p("-- Bar lengths indicate absolute 'Quantity' values. "),
							 	p("-- Alpha values for bars and lines are adjusted such that spaces that change rank 
							 					 	are more salient than those that do not."),
							 	p("-- Inspired by Caleydo LineUp (Gratzl et al. (IEEE TVCG / Proc. InfoVis 2013).")
							 )
# # 							 	tabPanel("rCharts",
# 							 					 
# 							 					 showOutput("intLineUpPlot","polycharts"),
# 							 					 h5("Description:"),
# 							 					 p("-- rCharts implementation of 'LineUp' (Gratzl et al. (IEEE TVCG / Proc. InfoVis 2013); 
# 							 					 	visually not as expressive as the ggplot implementation,but offers mouseover interactivity; a hybrid version would be preferred."),
# 							 					 p("-- Lines indicate a set of ranks for individual or aggregate spaces."),
# 							 					 p("-- Ranks are computed in decreasing order for each interval according to the 
# 							 					 	selected 'Quantity of Interest' for the selected 'Resource'."),
# 							 					 p("-- Point sizes indicate absolute 'Quantity of Interest' values."), 
# 							 					 p("-- Alpha values for bars and lines are adjusted such that spaces that change rank 
# 							 					 	are more salient than those that do not."),
# 							 					 p("-- Mouseover on points reveal tooltip with absolute absolute 'Quantity of Interest' values."),
# 							 					 p("-- Multiple years are faceted when the selected interval is not 'Year'."), 
# 							 					 p("-- Note: this LineUp chart cannot display multiple resources, 
# 							 					 	nor does it behave correctly when the selected 'Interval' is 'Year'.")
# 							 					 #tableOutput("lineUpPlotDebug")# for debugging											 
# 							 	)
# 							 )
			),
			
			tabPanel("Line Plots",
							 
							 plotOutput("stackedTimeSeriesPlot",width="100%",height="600px"),
							 wellPanel(uiOutput("highlight_select")),
							 wellPanel(
							 	h5("Description:"),
							 	p("-- A stacked area plot showing the cumulative values of the visible set of spaces (or groups), 
							 	sorted by mean 'Quantity' value."),
							 	p("-- Select a name from the 'Highlight' dropdown to highlight it.")
							 ),
							 plotOutput("timeSeriesPlot",width="100%",height="auto"),
							 wellPanel(	
							 	h5("Description:"),
							 	p("-- Line plots displaying select 'Quantity' values (y axis) for the 
							 	selected 'Resource' across time intervals (x axis), facted by individual or aggregate spaces."),
							 	p("-- The red and blue dots represent maximum and minimum values, respectively, for the current time window."),
							 	p("-- The dashed line represents a baseline."),
							 	p("-- Select a name from the 'Highlight' dropdown to highlight it.")
							 )
							 #verbatimTextOutput("timeSeriesSummary"), # for debugging
							 #tableOutput("timeSeriesDebug") # for debugging
			),
			
# 			tabPanel("Metadata",
# 							 
# 							 #verbatimTextOutput("summary"),
# # 							 tableOutput("metadata_table")
# 							 #tableOutput("view"), # for debugging
# 							 #numericInput("obs", "Number of observations to view:", 10) # for debugging
# 			),
			selected="Heatmaps"
		),
		wellPanel(
			h4("By Matt Brehmer"),
			p("2013-2014"),
			h5("About:"),
			p("This is a sandbox environment for creating visualization sketches relating to the energy use of a portfolio of buildings over a 2 year period. 
			2012 data serves as a 'baseline' for 2013; there is no baseline for 2012."),		
			p("Contact: matt.brehmer@pulseenergy.com / brehmer@cs.ubc.ca")
		)
	)		
))