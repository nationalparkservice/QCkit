#assign global package variables

#initiate new environment accessible from within package:
.pkgglobalenv <- new.env(parent=emptyenv())

#data_store API base URL:
assign("ds_api", "https://irmaservices.nps.gov/datastore/v6/rest/", envir=.pkgglobalenv)

#data_store secure API base URL:
assign("ds_secure_api", "https://irmaservices.nps.gov/datastore-secure/v6/rest/", envir=.pkgglobalenv)

#data_store dev api (requires secure)
assign("ds_dev_api", "https://irmadevservices.nps.gov/datastore-secure/v6/rest/", envir = .pkgglobalenv)

.ds_api <- function(x){
  get("ds_api", envir = .pkgglobalenv)
}

.ds_secure_api <- function(x){
  get("ds_secure_api", envir = .pkgglobalenv)
}

.ds_dev_api <- function(x){
  get("ds_dev_api", envir = .pkgglobalenv)
}