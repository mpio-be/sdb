


Changes in sdb 2017.1
---------------------
	* saveCredentials() is updated because it did not return the correct password in some particular cases. 
	* dbq got an `enhance` argument. When `enhance` is set to true then the data.table output contains 
	  POSIXct times and indices for relevant columns. Alternatively, use enhanceOutput() on a database output or
	  on any data.table.
