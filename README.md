
sdb
------------
An interface to databases hosted on scidb.mpio.orn.mpg.de


|            Function                            |                    Description                |
| -----------------------------------------------|-----------------------------------------------|
| `saveCredentials()                       `     |  of different hosts and for different users within a host.|
| `dbq(con, SELECT * FROM)                 `     | a `data.table` |
| `dbq(user = 'user', q = 'SELECT * FROM') `     | on-the-fly connection  |
| `dbq(con, SELECT * FROM, enhance = TRUE) `     | a keyed `data.table` with recognized `POSIX` columns |
| `dbq(con, SELECT * FROM, geom = 'SHAPE') `     | a `sf` spatial data.frame  |




Installation
------------

``` r

install.packages("devtools")
devtools::install_github("mpio-be/sdb")

```
