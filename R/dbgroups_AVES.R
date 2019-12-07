#' Synonyms merge
#' @param ttax     table containing the taxonomy we wish to keep. 
#' @param tdata    table containing the data to add to ttax.
#' @param sy       the synonyms table. A table with two columns (scinam, syid), see examples. 
#' @param clean    default to TRUE; remove scinam_tdata, .pk and renames scinam_ttax to scinam
#' @export
#' @examples
#' ttax  = data.table(scinam = c('a', 'b', 'c', 'd', 'm', 'n'), V1 = rnorm(6))
#' tdata  = data.table(scinam = c('a', 'b', 'j', 'k', 'n', 'm'), V2 = rnorm(6))
#' sy = data.table(scinam = c('a', 'b', 'c', 'j', 'd', 'k', 'm', 'n'), syid = c(1,2,3,3,4,4,5,5))
#' symerge(ttax, tdata, sy)
#' ############
#' require(sdb)   
#' tdata = dbq(user ='mihai', q = 'select * from AVES_lifeHistory.sociality')
#' ttax = dbq(user ='mihai', q = 'select scinam, prop_epp_young from AVES_lifeHistory.promiscuity_v2')
#' sy = dbq(user = 'mihai', q= 'select scinam, syid from  AVES_taxonomy.synonyms_v2')
#'
#' x = symerge(ttax, tdata, sy)


symerge <- function(ttax, tdata, sy, clean = TRUE) { 

  ttax[, `.pk` := .I]

  # plain merge
  o1 = merge(ttax, tdata, by = 'scinam')
  setnames(o1, 'scinam', 'scinam_ttax')
  o1[, scinam_tdata := scinam_ttax]

  # syn merge
  ttax_rest = ttax[! `.pk` %in% o1$`.pk`]

  ttax_syn  = merge(ttax_rest, sy,  by = 'scinam', all.x = TRUE)
  tdata_syn = merge(tdata, sy, by = 'scinam')

  x = merge(ttax_syn, tdata_syn,by = 'syid',  suffixes = c("_ttax", "_tdata"), all.x = TRUE)

  gnam = c('.pk', 'scinam_ttax', 'scinam_tdata')
  nn   = c( gnam, setdiff(names(x), gnam) )

  o2 = 
    try(
    unique(x, by = c('scinam_ttax', setdiff(names(x), gnam) ) ), 
    silent =  TRUE)
  if(inherits(o2, 'try-error')) {
    warning('unique does not work on some of the columns')
    o2 = copy(x)
  }




  setcolorder(o2, nn)
 
  # final set
  o = rbind(o1, o2, fill = TRUE)
  setorder(o, `.pk`)


  # checks
  z = o[, .N, .pk][N > 1, .pk]
  if(length(z) > 0) warning(paste('duplicated rows in ttax:', paste(z, collapse = ',')))


  # clean   
  if(clean) {
    setnames(o, 'scinam_ttax', 'scinam')  
    o[, ':=' (scinam_tdata = NULL, .pk = NULL, syid = NULL)]
    }  



  o


  }

