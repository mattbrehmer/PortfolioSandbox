#server.R
#Portfolio Visualziation Sketches
#by Matt Brehmer
#2013-2014

# Shiny Server with reactive functions and plot implementation 
# filters, aggregates, facets data loaded by global.R
# calls utility functions in dataFunctions.R

#source ------------------------------------------------------------------------------------------
# source required files

source("global.R", local=TRUE)
source("dataFunctions.R", local=TRUE)
source("eventHandlers.R", local=TRUE)	# event handlers (currently only used by leaflet map)

# Start up Shiny's reactive log for browser debugging of observables/observers
options(shiny.reactlog = TRUE)

#shinyServer ------------------------------------------------------------------------------
#' shinyServer
#' @author Matt Brehmer
#' @export
#' @description reactive functions and plot implementation 
#' @param input
#' @param output
#' @param session
shinyServer(function(input, output, session) {
	
	output$queryResultNote <- renderUI({
		
		#refresh to match user query
		result.dt <- Query()
		
		nSpaces <- getNSpaces()
		
		if(getAggregateSelect()!="None")
			itemType <- "space groups"
		else
			itemType <- "spaces"
		
		numItems <- length(unique(result.dt$name))
		
		if (nrow(result.dt) == 0 || is.null(result.dt$quantity))
			HTML(paste0('<span style="color: red">Your query does not return any results</span>'))    
		else
			HTML(paste0('Displaying ',min(nSpaces,numItems),' of ',numItems, ' ', itemType,' matching query.'))   
	})
	
	#source reactive functions, data filtering and selection options
	source("server/reactiveFunctions.R", local=TRUE) # reactive functions used by plots
	source("server/reactiveValues.R", local=TRUE) # reactive values used by plots
	source("server/filterOptions.R", local=TRUE) # time window and space metadata filtering
	source("server/selectOptions.R", local=TRUE) # aggregation, resource selection, normalization, and granularity selectio
	
	#source plot code
	source("server/plotHeightFunctions.R", local=TRUE) # custom plot height functions
	source("server/plotBar.R", local=TRUE) # bar charts 
	source("server/plotBox.R", local=TRUE) # box plots 
	source("server/plotHistograms.R", local=TRUE) # histograms / density plots
	source("server/plotLineUp.R", local=TRUE) # LineUp plots
# 	source("server/plotRChartsLineUp.R", local=TRUE)	# interactive RCharts implementation of LineUp plots
	source("server/plotHeatmap.R", local=TRUE)	# heatmaps
	source("server/plotCalendars.R", local=TRUE)	# calendar heatmaps
	source("server/plotLinePlots.R", local=TRUE)	# small multiple / faceted time series line plots
	source("server/plotStackedTimeSeries.R", local=TRUE)	# small multiple / faceted time series line plots
	# 	source("server/plotTables.R", local=TRUE)	# metadata tables for debugging
	source("server/plotPortfolioMap.R", local=TRUE)	# portfolio map built with shiny-leaflet wrapper
})