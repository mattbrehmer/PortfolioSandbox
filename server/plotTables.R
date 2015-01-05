#plotTables.R
#Portfolio Visualziation Sketches
#by Matt Brehmer
#2013-2014

#metadata tables ------------------------------------------------------------------------------

# Show space metdata
output$metadata_table <- renderTable({		
	
	submeta.dt <- MapQuery()
# 	submeta.dt = unique(subset(submeta.dt, select=-tag))
	head(submeta.dt, n = nrow(submeta.dt))    
	
})

# # Show a summary of the dataset
# output$summary <- renderPrint({
# 	
# 	query.dt <- Query()
# 	summary(query.dt)
# 	
# })
# 
# # Show a detailed summary of the dataset
# output$details <- renderPrint({
# 	
# 	data.dt <- Data()
# 	summary(data.dt)
# 	
# })	
# 
# # Show the first "n" observations of Query() in a table
# output$view <- renderTable({		
# 	
# 	result.dt <- Query()
# 	head(result.dt, n = 10)    
# 	
# })