## Experiments in El Nino analysis and prediction

This software is connected to the Azimuth Code Project [Experiments in El Nino analysis and prediction](http://www.azimuthproject.org/azimuth/show/Experiments%20in%20El%20Ni%C3%B1o%20analysis%20and%20prediction).  

#### R / netcdf-convertor-ludescher.R.

This script written by Graham Jones converts netCDF files containing daily mean surface temperature data from NOAA (e.g. air.sig995.1951.nc) into a format more easily readable by other programs in other languages. You may need to edit it to your requirements:

* Change the ranges of latitude and longitude
* Change the range of years

As supplied, it converts the data from 1948 to 1949 in a region of the Pacific used in a [paper by Ludescher *et al*](http://www.pnas.org/content/early/2013/06/26/1309353110.full.pdf+html).  Then start R, and copy and paste the 
whole file into the R console. (There are other ways of running R scripts but this is simplest for novices.)

More instructions are in the script itself.  For detailed explanations, see [Part 4](http://johncarlosbaez.wordpress.com/2014/07/08/el-nino-project-part-4/) and [Part 5](http://johncarlosbaez.wordpress.com/2014/07/12/el-nino-project-part-5/) of the El Ni&ntilde;o Project series on the Azimuth blog.

#### R / grj / ludescher.R

This program is aimed at replicating the  [paper by Ludescher *et al*](http://www.pnas.org/content/early/2013/06/26/1309353110.full.pdf+html).  Instructions are in the script itself.  For detailed explanations, see [Part 3](http://johncarlosbaez.wordpress.com/2014/07/01/el-nino-project-part-3/), [Part 4](http://johncarlosbaez.wordpress.com/2014/07/08/el-nino-project-part-4/) and [Part 5](http://johncarlosbaez.wordpress.com/2014/07/12/el-nino-project-part-5/) of the El Ni&ntilde;o Project series on the Azimuth blog.

#### R / grj / covariances-basin-vs-rest.R

Makes maps of the Pacific, one per quarter from 1951 to 1979, showing covariances of grid points with the "Ludescher et al basin"

```
