## ------------------------------------------------------------------------
require(sdb)
Host = 'localhost'

id = snipSave(query = 'SELECT * FROM BTatWESTERHOLZ.BREEDING limit 1', 
            description = 'select the first line of the table', user = 'valcu', host = Host)

## ------------------------------------------------------------------------
s = snipSearch("BTatWESTERHOLZ.BREEDING", user = 'valcu', host = Host)

## ------------------------------------------------------------------------
s1 = snipFetch(1, user = 'valcu', host = Host)


## ------------------------------------------------------------------------
id

## ------------------------------------------------------------------------
snipDrop(id, user = 'valcu', host = Host)

