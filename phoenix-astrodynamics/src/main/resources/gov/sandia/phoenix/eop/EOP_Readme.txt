The files in this directory contain Earth specific data.  The directory includes files 
containing gravity field models (*.grv files), files containing solar flux indices 
(SolFlx* files), files containing both solar and geomagnetic flux indices (*.fxm files,
SpaceWeather* files), and files containing Earth orientation parameters (EOP* files).

EOP FILES

These files contain Earth Orientation Parameters which include the pole wander values and 
the UT1-UTC time corrections.  These parameters are used when transforming between Earth
Fixed and Earth Mean J2000 coordinate frames.

With the release of STK 8.0 in the Fall of 2006, AGI has changed the default EOP file
that is used for new Scenarios.  The new file, EOP-v1.1.txt, is format version 1.1 of
the EOP file available from http://www.celestrak.com/SpaceData (and is also available from
the AGI public ftp site at ftp://ftp.stk.com/pub/DynamicEarthData/).  This new file
contains additional data needed for implementing frames according to the IAU2000
standards.  See http://celestrak.com/SpaceData/EOP-format.asp for format details of the file.

The data for the EOP*.dat files comes from the USNO series 7 / IERS Bulletin A.  
The format for the data is as seen in the IERS rapid service section of bulletin A.

	MJD      x    error     y    error   UT1-UTC   error
	         "      "       "      "        s        s

The total number of data records is the first entry in the file.  

The EOP.dat file contains recent observed data (about 5 years worth) and about one year 
of predict data. This file is updated with every release of STK, and so the actual dates
spanned by this file has changed with every release. Older versions of the EOP.dat file
have also been included in this directory for users who desire to maintain backward
compatibility to previous versions--version identifiers are included in the filename.
Users who choose to use the more recent file (EOP.dat) may notice small differences over
the previous version of STK. Position differences are generally less than a few meters.  
Timely updates of the EOP.dat file can be found out our ftpsite ftp.stk.com/pub.

The files EOP.dat.all and EOP-All-v1.1.txt are provided for users who have a need for 
more historical data; these files contains the more recent data but in addition contain
observed data from as far back as about 2 Jan 1973 / 1 Jan 1962.  Updates of these
files are also available on our website.

NOTE: The EOP files that were shipped with version 4.2 and 4.2.1 were dated 1 Nov 2000. 
The 4.3 file was dated from Jan 2002. Between Nov 2000 and Jan 2002, the USNO re-fit 
the historical EOP parameters. Thus, the EOP.dat.1976 file shipped with 4.2 and 4.2.1 
has different historical values from that contained in the EOP.dat.all file. Users who had
previously used the EOP.dat.1976 file and now use EOP.dat.all will now notice differences, 
even for times in the past. 

Moreover, USNO continues to regularly re-fit about a year duration of historical data along 
with adding new daily values. In Jan 2002, this re-fitting occurred about twice a week.

NOTE: The EOP file shipped with version 6.2 (and subsequent versions) now reflects the leap 
second that occurred just before 01 Jan 2006 00:00:00 UTC.

Change in EOP as of Jan 02, 2002:

In older versions of this file, the table was designed so that values past the
end of the known data were held constant by duplicating the last line and
setting the final time very large.

In the latest version, the table does not hold the last values constant.
Instead, the values are set to zero one day after the end of the data.

The reason for this change was to stabilize results for scenarios occurring far
past the last valid EOP date.


