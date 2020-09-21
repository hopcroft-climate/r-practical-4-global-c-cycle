#---
# title: "R Notebook 5 - the global carbon cycle"

# Tom Pugh
# 10.12.19

# Peter Hopcroft
# 09.20
#---

# The aim of this practical is to explore how the global carbon budget is constructed and assess its uncertainties. You will also build upon the R skills that you have been developing with timeseries datasets.

# As discussed in the lecture, the global carbon budget can be summarised by the equation,

# d[Catm]/dt = EFF + ELUC - SO - SL - NO 	(1),

# where Catm is the stock of carbon in the atmosphere, 
# EFF is carbon emission from fossil fuel and cement emissions, 
# ELUC is net carbon emissions from land-use change, 
# SO is uptake of carbon by the ocean, 
# SL is uptake of carbon by the land and 
# NO is a net term incorporating all other emissions or uptake. 

# We often lump NO with SL, as these other fluxes are also land-based, to simply give,

# d[Catm]/dt = EFF + ELUC - SO - SL	(2).

# Some parts of this equation we know better than others. The change in Catm is very precisely measured and EFF is quite well constrained. SO and ELUC are much less certain, and SL is also very uncertain. Because there are no direct measurements of SL and modelling it has very many uncertainties, it has often been calculated as a residual of the other fluxes. 

# Your first task here is to calculate SL using the information on the other fluxes that is provided in the folder Global_C_budget. Most of the operations here build on R Notebook 1.


###--- The historical carbon cycle ---

# First read in the data that you need.
# - EFF data is in Global_C_budget/Fossil_fuel_and_industry_emissions.csv
# - ELUC data is in Global_C_budget/Land_use_change_emissions.csv
# - SO data is in Global_C_budget/Ocean_C_uptake.csv
# - d[Catm]/dt is in Atmospheric_C_growth.csv
# Add the code you need to do this below. Note that you need to use the "read.csv" command to read these files, as the columns are separated by commas.
setwd("~/shared/data/Global_C_budget/") 
EFF=read.csv("Fossil_fuel_and_industry_emissions.csv")
ELUC=read.csv("Land_use_change_emissions.csv")
SO=read.csv("Ocean_C_uptake.csv")
dCatm=read.csv("Atmospheric_C_growth.csv")

# Plot the data to make sure that is makes sense. Are the units consistent? Does it look at you would expect? Why does some data have higher interannual variability than others? (this can link back to either the process itself, or the way it is quantified.
plot(x=EFF$Year,y=EFF$fossil.fuel.and.industry.GtC.y.,type="l",col="black",ylim=c(-5,10))
lines(x=ELUC$Year,y=ELUC$land.use.change.emissions.GtC.y.,col="red")
lines(x=ELUC$Year,y=-SO$ocean.sink.GtC.y.,col="blue")
lines(x=ELUC$Year,y=-dCatm$atmospheric.growth.GtC.y.,col="purple")
legend(1960, 9, legend=c("EFF", "ELUC","SO","-dCatm/dt"),
       col=c("black","red", "blue","purple"), lty=1)


# Now rearrange equation 2 above, so that SL is the subject. Use the vectors you have read in above for the different components to calculate SL
SL=EFF$fossil.fuel.and.industry.GtC.y.+ELUC$land.use.change.emissions.GtC.y.-SO$ocean.sink.GtC.y.-dCatm$atmospheric.growth.GtC.y.

# Plot the SL vector that you have calculated. You may want to use the "lines()" function (see Notebook 1) to plot all of the fluxes on the same plot in order to better compare them.

lines(x=ELUC$Year,y=-SL,col="green")


# How does the SL flux compare to the others fluxes in the calculation? What is different about its behaviour and why? Make some notes below as comments.

# ...


# What do you think might be driving the behaviour of SL? Some possible drivers (but not the only ones) are available in the folder Historical_global_timeseries, for instance:
# - Global_temp_anomaly_1880-2018_annual.csv (global temperature anomalies relative to 1901-2000)
# - ENSO_mei_index_annual.txt (An index charting ENSO cycles, positive is El Nino, negative La Nina, with magnitude representing intensity)
# Make an initial assessment of whether the variables are linked by making scatter plots of SL against them.
# Note that these timeseries have different lengths to the C flux datasets you have been working with above. You will need to take appropriate subsets of each vector using the [:] syntax (Notebook 1).

temp=read.csv("../Historical_global_timeseries/Global_temp_anomaly_1880-2018_annual.csv", header=T, skip=5)
enso=read.table("../Historical_global_timeseries/ENSO_mei_index_annual.txt",header=T,skip=1)

plot(x=temp$Value[80:139], y=SL)
R_temp=cor(x=temp$Value[80:139], y=SL)
R2_temp=R_temp^2

plot(x=enso$MEI_annual[10:69], y=SL)
R_enso=cor(x=enso$MEI_annual[10:69], y=SL)
R2_enso=R_enso^2

# Dynamic Global Vegetation Models are often used to make a process-based estimate of the uptake of C by the land. Compare the flux from DGVMs (given in Global_C_budget/Land_sink_DGVMs_individual.csv, each column is a different DGVM, with MMM being the multi-model mean) to that which you have calculated from the budget. To what extent are DGVMs able to simulate the budget well? You can show this statistically by calculating the R2 of the SL from the DGVM versus the SL from the budget residual that you have calculated.

# First read in the DGVM data
dgvm_land_sink=read.csv("../Global_C_budget/Land_sink_DGVMs_individual.csv", header=T, skip=0)

# To calculate R2 in R you need to first calculate R, the correlation coefficient,
# R=cor(x,y)
# and then take the square of R
# R2=R^2

# first check the DGVM data looks ok:
plot(x=dgvm_land_sink[,1],y=dgvm_land_sink[,2],type="l",col="black") 
lines(x=dgvm_land_sink[,1],y=dgvm_land_sink[,3],type="l",col="blue") 
lines(x=dgvm_land_sink[,1],y=dgvm_land_sink[,4],type="l",col="red") 

R=cor(SL,dgvm_land_sink[,2])
R2=R^2
print(R2)

R=cor(SL,dgvm_land_sink[,3])
R2=R^2
print(R2)

# etc


###--- The future carbon cycle ---

# Finally, use information in the folder GHG_emissions to calculate the SL implied by different future climate scenarios. This is an important value. Future emissions scenarios assume that the terrestrial biosphere takes up this amount of carbon. If it takes up more or less then this will result in a different amount of C in the atmosphere and thus a different level of climate change for a give anthropogenic emission.

# Let's try RCP85: (not use read.table for .txt files and read.csv for .csv files).
# fossil fuel emissions
rcp85_fossil_ems=read.table("../GHG_emissions/RCP85/RCP85_EMISSIONS_FOSSILCO2.txt", header=T, skip=0)
# land-use change
rcp85_luc_ems=read.table("../GHG_emissions/RCP85/RCP85_EMISSIONS_FOSSILCO2.txt", header=T, skip=0)
# sum these two (but not the first column which is the year)
rcp85_ems<-rcp85_fossil_ems
rcp85_ems[,2]<-rcp85_fossil_ems[,2]+rcp85_luc_ems[,2]

plot(x=rcp85_ems[,1],y=rcp85_ems[,2],type="l",col="red")

# We can assume that 55% of these emissions are absorbed by the ocean or by the land

# Thus the atmospheric increase is equal to 45% of these emissions
rcp85_atm<-0.45*rcp85_ems

# You will need the conversion factor of Pg C emitted into the atmosphere to CO2 mixing ratio, this is 2.12 Pg C ppm-1.
rcp85_atm_conc<-rcp85_atm/2.12

plot(x=rcp85_ems[,1],y=rcp85_atm_conc[,2],type="l",col="red")
# This is the increase per year
# Now convert to cmulative increase:
rcp85_atm_conc<-cumsum(rcp85_atm_conc)
plot(x=rcp85_ems[,1],y=rcp85_atm_conc[,2],type="l",col="red")

# ----------------------------------------------------------------------
# Now let's briefly look at some DGVM model outputs spatially.
# DGVMs (dynamic global vegetation model)
# First we'llompare the vegetation carbon outputs from the vegetation model at different levels of CO2 concentration in the atmosphere.
# Note that the model outputs are for uniform vegetation types, either 100% cover of "NATURAL" vegetation (this is known as Potential Natural Vegetation, PNV) or 100% cover of "Pasture". If you want to turn these into realistic estimates, you will first have to mask the outputs appropriately using the relevant categories from either current landcover from ESA CCI or the landcover estimates used in the RCP scenarios that you looked at in Practical 4. i.e.
# - Use PNV to represent all non-agricultural areas
# - Use Pasture to represent all agricultural areas
# (or another scheme that you can justify)
# Note: You are unlikely to have a sensitivity simulation that matches exactly the change in climate or CO2 that you would like to test the effect of. You can estimate intermediate effects by interpolation, e.g. for a CO2 increment of 225 ppm, you could take the average of the effect at 150 ppm and 300 ppm (this assumes linearity...).


library("ncdf4")
library("fields")
library("RColorBrewer")
setwd("~/shared/data/DGVM_simulations/Natural_vegetation_everywhere/baseline") 
file_baseline <- nc_open('lpj-guess_CVeg_annual_1970_1999_mean.nc4')
lat_lpjg=ncvar_get(nc=file_baseline, varid='latitude')
lon_lpjg=ncvar_get(nc=file_baseline,varid='longitude')
cveg_baseline_pnv = ncvar_get(nc=file_baseline, varid='CVeg')

setwd("~/shared/data/DGVM_simulations/Natural_vegetation_everywhere/CO2plus150") 
file_co2plus150 <- nc_open('lpj-guess_CVeg_annual_1970_1999_mean.nc4')
cveg_co2plus150_pnv = ncvar_get(nc=file_co2plus150, varid='CVeg')

del_cveg_co2plus150_pnv<-cveg_co2plus150_pnv - cveg_baseline_pnv
image.plot(lon_lpjg, lat_lpjg, del_cveg_co2plus150_pnv,main="Veg C",sub="CO2plus150-baseline", col = rev(brewer.pal(9, "RdBu")))



