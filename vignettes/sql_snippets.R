## ------------------------------------------------------------------------
require(sdb)
Host = '127.0.0.1'

id = snipSave(query = 'SELECT * FROM BTatWESTERHOLZ.BREEDING limit 1',
            description = 'select the first line of the table', user = 'mihai', host = Host)

## ------------------------------------------------------------------------
s = snipSearch("BTatWESTERHOLZ.BREEDING", user = 'mihai', host = Host)

## ------------------------------------------------------------------------
s1 = snipFetch(1, user = 'mihai', host = Host)


## ------------------------------------------------------------------------
id

## ------------------------------------------------------------------------
snipDrop(id, user = 'mihai', host = Host)

