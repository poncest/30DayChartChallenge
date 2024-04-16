          NATIONAL CENTERS FOR ENVIRONMENTAL INFORMATION
          GLOBAL SURFACE SUMMARY OF DAY DATA (GSOD)
          (OVER 9000 WORLDWIDE STATIONS)
             
         10/28/2020

********************************************************************
 
SPECIAL NOTES 

The data summaries provided here are based on data exchanged under the World 
Meteorological Organization (WMO) World Weather Watch Program according to WMO 
Resolution 40 (Cg-XII). This allows WMO member countries to place restrictions on the 
use or re-export of their data  for commercial purposes outside of the receiving country.  
Data for selected countries may, at times, not be available through this system.   

Those countries' data summaries and products which are available here are intended for
free and unrestricted use in research, education, and other non-commercial activities.  
However, for non-U.S. locations' data, the data or any derived product shall not be provided 
to other users or be used for the re-export of commercial services.

To determine off-line availability of any country's data, please contact NCEI at

ncei.orders@noaa.gov
828-271-4800

See our contact page at:
www.ncei.noaa.gov/contact

The data are available via:
 
Web Accessible Folder -- https://www.ncei.noaa.gov/data/global-summary-of-the-day/
Common Access -- https://www.ncei.noaa.gov/access/search/data-search/global-summary-of-
the-day



********************************************************************


OVERVIEW

The following is a description of the global surface summary of day product produced by the 
National Centers for Environmental Information (NCEI) in Asheville, NC.  The input data used in 
building these daily summaries are the Integrated Surface Data (ISD), which includes global 
data obtained from the USAF Climatology Center, located in the Federal Climate Complex with 
NCEI. The latest daily summary data are normally available a few days after the date-time of the 
observations used in the daily summaries. The online data files begin with 1929. Over 9000 
stations' data are typically available.  

The headers used in csv files and definition of each daily element included in the dataset (as 
available from each station) are as follows:

TEMP - Mean temperature (.1 Fahrenheit)
DEWP - Mean dew point (.1 Fahrenheit)
SLP - Mean sea level pressure (.1 mb)
STP - Mean station pressure (.1 mb)
VISIB - Mean visibility (.1 miles)
WDSP – Mean wind speed (.1 knots)
MXSPD - Maximum sustained wind speed (.1 knots)
GUST - Maximum wind gust (.1 knots)
MAX - Maximum temperature (.1 Fahrenheit)
MIN - Minimum temperature (.1 Fahrenheit)
PRCP - Precipitation amount (.01 inches)
SNDP - Snow depth (.1 inches)
FRSHTT – Indicator for occurrence of:
		      Fog		
                              Rain or Drizzle
                              Snow or Ice Pellets
                              Hail
                              Thunder
                              Tornado/Funnel Cloud

For more detailed descriptions of data elements and contents of the dataset, see the format 
documentation shown below.  


********************************************************************

DETAILS/FORMAT

Global summary of day data for 18 surface meteorological elements
are derived from the synoptic/hourly observations contained in
USAF DATSAV3 Surface data and Federal Climate Complex Integrated 
Surface Data (ISD). Historical data are generally available for 1929 to
the present, with data from 1973 to the present being the most complete.  
For some periods, one or more countries' data may not be available due to
data restrictions or communications problems. In deriving the summary of 
day data, a minimum of 4 observations for the day must be present (allows 
for stations which report 4 synoptic observations/day). Since the data are
converted to constant units (e.g, knots), slight rounding error from the
originally reported values may occur (e.g, 9.9 instead of 10.0).

The mean daily values described below are based on the hours of
operation for the station.  For some stations/countries, the
visibility will sometimes 'cluster' around a value (such as 10
miles) due to the practice of not reporting visibilities greater
than certain distances. The daily extremes and totals--maximum
wind gust, precipitation amount, and snow depth--will only appear
if the station reports the data sufficiently to provide a valid value.
Therefore, these three elements will appear less frequently than 
other values. Also, these elements are derived from the stations'
reports during the day, and may comprise a 24-hour period which
includes a portion of the previous day. The data are reported and
summarized based on Greenwich Mean Time (GMT, 0000Z - 2359Z) since
the original synoptic/hourly data are reported and based on GMT.

As for quality control (QC), the input data undergo extensive
automated QC to correctly 'decode' as much of the synoptic data as
possible, and to eliminate many of the random errors found in the
original data. Then, these data are QC'ed further as the summary of
day data are derived. However, we expect that a very small percent of 
the errors will remain in the summary of day data.

The data are strictly ASCII, with a mixture of character data, real
values, and integer values. 

Following is the data format:

All data records are described below.
All 9's in a field (e.g., 99.99 for PRCP) indicates no report or insufficient data.

FIELD            DESCRIPTION

STATION - Station number (WMO/DATSAV3 possibly combined w/WBAN number) 

DATE - Given in mm/dd/yyyy format

LATITUDE - Given in decimated degrees (Southern Hemisphere values are negative)

LONGITUDE - Given in decimated degrees (Western Hemisphere values are negative)

ELEVATION - Given in meters

NAME - Name of station/airport/military base

TEMP - Mean temperature for the day in degrees Fahrenheit to tenths. Missing = 9999.9

TEMP_ATTRIBUTES - Number of observations used in calculating mean temperature.

DEWP - Mean dew point for the day in degrees Fahrenheit to tenths. Missing = 9999.9

DEWP_ATTRIBUTES - Number of observations used in calculating mean dew point.  

SLP - Mean sea level pressure for the day in millibars to tenths. Missing = 9999.9

SLP_ATTRIBUTES - Number of observations used in calculating mean sea level pressure.

STP - Mean station pressure for the day in millibars to tenths. Missing = 9999.9

STP_ATTRIBUTES - Number of observations used in calculating mean station pressure.  

VISIB - Mean visibility for the day in miles to tenths. Missing = 999.9

VISIB_ATTRIBUTES - Number of observations used in calculating mean visibility.      

WDSP - Mean wind speed for the day in knots to tenths.  Missing = 999.9 

WDSP_ATTRIBUTES - Number of observations used in calculating mean wind speed.

MXSPD - Maximum sustained wind speed reported for the day in knots to tenths. Missing = 
999.
GUST - Maximum wind gust reported for the day in knots to tenths.  Missing = 999.9

MAX - Maximum temperature reported during the day in Fahrenheit to tenths. Missing = 9999.9

Note: Time of maximum temperature report varies by country and region, so this will sometimes 
not be the maximum for the calendar day.    

MAX_ATTRIBUTES – 
Blank indicates maximum temperature was taken from the explicit maximum 
temperature report and not from the 'hourly' data.  
                    
       * indicates maximum temperature was derived from the hourly data
(i.e. highest hourly or synoptic-reported temperature).

MIN - Minimum temperature reported during the day in Fahrenheit to tenths. Missing = 9999.9

Note: Time of minimum temperature report varies by country and region, so this will sometimes 
not be the maximum for the calendar day.

MIN_ATTRIBUTES    
Blank indicates minimum temperature was taken from the explicit minimum 
temperature report and not from the 'hourly' data.  
                    
       * indicates minimum temperature was derived from the hourly data
(i.e. highest hourly or synoptic-reported temperature).

PRCP - Total precipitation (rain and/or melted snow) reported during the day in inches
 and hundredths; will usually not end with the midnight observation (i.e. may include
latter part of previous day). “0” indicates no measurable precipitation (includes a trace).   
Missing = 99.99
                         
Note: Many stations do not report “0” on days with no precipitation, therefore “99.99” will often 
appear on these days. Also, for example, a station may only report a 6-hour amount for the 
period during which rain fell. See attribute field for source of data.

PRCP_ATTRIBUTES -     
          A = 1 report of 6-hour precipitation amount.
               B = Summation of 2 reports of 6-hour precipitation amount.
               C = Summation of 3 reports of 6-hour precipitation amount.
               D = Summation of 4 reports of 6-hour precipitation amount.
               E = 1 report of 12-hour precipitation amount.
               F = Summation of 2 reports of 12-hour precipitation amount.
               G = 1 report of 24-hour precipitation amount.
               H = Station reported '0' as the amount for the day (eg, from 6-hour reports),
but also reported at least one occurrence of precipitation in hourly observations. 
This could indicate a trace occurred, but should be considered as incomplete 
data for the day.
                I = Station did not report any precipitation data for the day and did not report any
                     occurrences of precipitation in its hourly observations. It's still possible that
                     precipitation occurred but was not reported.

SNDP - Snow depth in inches to tenths. It is the last report for the day if reported more than
                         once. Missing = 999.9
                         
Note: Most stations do not report “0” on days with no snow on the ground, therefore, “999.9” will 
often appear on these days.

FRSHTT - Indicators (1 = yes, 0 = no/not reported) for the occurrence during the day of:
                         Fog ('F' - 1st digit).
                         Rain or Drizzle ('R' - 2nd digit).
                         Snow or Ice Pellets ('S' - 3rd digit).
                         Hail ('H' - 4th digit).
                         Thunder ('T' - 5th digit).
                         Tornado or Funnel Cloud ('T' - 6th digit).

********************************************************************

REFERENCE

The NCEI Climate Services Branch (CSB) is responsible for distribution of NCEI products to 
users. NCEI's CSB can be contacted via the following phone number, internet address, or 
fax number.  

Telephone Number:   828-271-4800
Fax Number:         828-271-4876
Internet Address:   ncei.orders@noaa.gov
Website:            www.ncei.noaa.gov

********************************************************************

Mark Lackey
Meteorologist 
NOAA’s National Centers for Environmental Information (NCEI)
Center for Weather and Climate (CWC)
151 Patton Ave
Asheville, NC 28801

