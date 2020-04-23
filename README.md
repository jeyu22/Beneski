# Beneski Museum Attendance

Shiny app for Beneski Museum of Natural History (Amherst College) - examining past trends in visitor attendance and using machine learning to predict future attendance based on weather patterns. 

Link to app: https://r.amherst.edu/apps/jeyu22/beneski/

App.r - contains code for the shiny app
attendance.Rds - raw data for attendance from 2011 - 2019, originally in excel format
gtrends.Rds - data from Google Trends used to idenitfy patterns between search queries and attendance
machinel.R - training and creating model to predict future attendance using Random Forest
rfFit.Rds - actual model created from Random Forest used in the Shiny App 
