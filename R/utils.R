#assign global package variables

#initiate new environment accessible from within package:
.pkgglobalenv <- new.env(parent=emptyenv())

#data_store API base URL:
assign("QC_ds_api", "https://irmaservices.nps.gov/datastore/v6/rest/", envir=.pkgglobalenv)

#data_store secure API base URL:
assign("QC_ds_secure_api", "https://irmaservices.nps.gov/datastore-secure/v6/rest/", envir=.pkgglobalenv)

#data_store dev api (requires secure)
assign("QC_ds_dev_api", "https://irmadevservices.nps.gov/datastore-secure/v6/rest/", envir = .pkgglobalenv)

.QC_ds_api <- function(x){
  get("QC_ds_api", envir = .pkgglobalenv)
}

.QC_ds_secure_api <- function(x){
  get("QC_ds_secure_api", envir = .pkgglobalenv)
}

.QC_ds_dev_api <- function(x){
  get("QC_ds_dev_api", envir = .pkgglobalenv)
}