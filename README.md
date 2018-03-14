[![Build Status](https://travis-ci.org/mpio-be/sdb.svg?branch=master)](https://travis-ci.org/mpio-be/sdb)


sdb
------------
An interface to MariaDB databases hosted on scidb.mpio.orn.mpg.de

There are methods for:

- regular database support through [RMySQL]( https://CRAN.R-project.org/package=RMySQL ) 
- spatial query through  [gdalUtils]( https://CRAN.R-project.org/package=gdalUtils ) with 
  ``gdal`` `ogr2ogr('SQLite', 'SPATIALITE=yes', 'mysql_query')`            
- bulk DB operations (dump, restore, ...) through `mysql CLI`
