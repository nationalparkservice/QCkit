


create_datastore_script <- function(owner,
                                    repo,
                                    path = here::here(),
                                    force = FALSE,
                                    dev = FALSE) {
  url <- paste0("https://api.github.com/repos/",
                owner,
                "/",
                repo,
                "releases/lastest")

  #GitHub API request for latest releast of a given repo:
  gh_req <- httr::GET("https://api.github.com/repos/nationalparkservice/EMLeditor/releases/latest",
                     httr::add_headers('Accept'='application/vnd.github+json'))

  status_code <- httr::stop_for_status(gh_req)$status_code

  #if API call fails, alert user and remind them to log on to VPN:
  if(!status_code == 200){
    stop("ERROR: GitHub connection failed. Are you connected to the internet?\n")
  }

  #Make the json R friendly:
  gh_req_json <- httr::content(gh_req, "text")
  gh_req_rjson <- jsonlite::fromJSON(gh_req_json)

  #check DataStore for existing references with the same title
  #Title is auto generated as repo + version; replace space with %20
  new_ref_title <- paste0(gh_req_rjson$tag_name)
  dynamic_title <- gsub(" ", "%20", new_ref_title)

  #quick search of datastore for the string "dynamic title"
  post_url <- paste0(.ds_secure_api(), "QuickSearch?q=", "EMLeditor")
  req <- httr::GET(post_url,
                    httr::authenticate(":", "", "ntlm"),
                    httr::add_headers('accept'='application/json'))

    #check status code; suggest{ logging in to VPN if errors occur:
  status_code <- httr::stop_for_status(req)$status_code
  if(!status_code == 200){
    stop("ERROR: DataStore connection failed. Are you logged in to the VPN?\n")
  }
  #get title list:
  json <- httr::content(req, "text")
  rjson <- jsonlite::fromJSON(json)
  items <- as.data.frame(rjson$items)
  #search for title in title list, if force == false:
  if(force == FALSE){
    matches <- items %>% filter(stringr::str_detect(items$title, "EMLeditor"))
    if(length(seq_along(matches$title) > 0)){
      cat("One or more DataStore references with title containing: ",
          new_ref_title,
          " already exists:", sep="")
      cat("Reference ID: ", matches$referenceId, "; Title: ", matches$title, sep="")
      cat("Are you sure you want to create a new draft reference for ",
          new_ref_title, "?", sep = "")


    }
  }


  #get download link for .zip file
  gh_zip_url <- gh_req_rjson$zipball_url

  #in case someone uses non-default directory
  orig_wd <- getwd()
  #set directory back to original working directory on exit.
  on.exit(setwd(orig_wd), add=TRUE)
  #set wd to path; defaults to wd.
  setwd(path)

  #create "releases" directory (to store .zip), if necessary:
  if (!file.exists("releases")) {
    dir.create("releases")
  }

  #create file name:
  file_name <- paste0(repo, "_", gh_req_rjson$tag_name, ".zip")
  #create file path
  download_file_path <- paste0("releases/", file_name)

  #download the file (.zip) from github:
  invisible(capture.output(
    suppressMessages(httr::content(
      httr::GET(
        gh_zip_url,
        httr::progress(),
        httr::write_disk(download_file_path,
                         overwrite = TRUE))))))
  if(force == FALSE){
    cat("Writing: ",
        crayon::blue$bold(download_file_path),
        ".\n", sep="")
  }

  #generate json body for rest api call to create the reference:
  mylist <- list(referenceTypeId = "Script",
                 title = new_ref_title,
                 location = "",
                 issuedDate = list(year = 0,
                                   month = 0,
                                   day = 0,
                                   precision = ""),
                 Version = "0.1.3")
  bdy <- jsonlite::toJSON(mylist, pretty = TRUE, auto_unbox = TRUE)

  if(dev == TRUE){
    post_url <- paste0(.ds_dev_api(), "Reference/CreateDraft")
  } else {
    post_url <- paste0(.ds_secure_api(), "Reference/CreateDraft")
  }

  #create the draft reference:
  req <- httr::POST(post_url,
                    httr::authenticate(":", "", "ntlm"),
                    httr::add_headers('Content-Type'='application/json'),
                    body = bdy)
  #check status code; suggest logging in to VPN if errors occur:
  status_code <- httr::stop_for_status(req)$status_code
  if(!status_code == 200){
    stop("ERROR: DataStore connection failed. Are you logged in to the VPN?\n")
  }
  #get newly created reference id:
  json <- httr::content(req, "text")
  rjson <- jsonlite::fromJSON(json)
  ds_ref <- rjson$referenceCode

  if(dev == TRUE){
    ds_profile_link <- paste0(
      "https://irmadev.nps.gov/DataStore/Reference/Profile/",
      ds_ref)}
  else{
    ds_profile_link <- paste0(
      "https//irma.nps.gov/DataStore/Reference/Profile/",
      ds_ref)
  }

  if(force == FALSE){
    cat("A draft reference has been created on DataStore.")
    cat("Your reference can be accessed and activated at:\n", ds_profile_link)
  }

  #check for files that are too big!
  if(file.size(download_file_path) > 33554432){
    #warn for each file >32Mb
    if(force == TRUE){
    cat(crayon::blue$bold(file_name),
        " is greater than 32Mb and cannot be uploaded with this funcion.\n",
        "please use the DataStore website to upload your files manually.",
        sep = "")
      }
    stop()
  }

  #use reference id to put the file:
  if(dev == TRUE){
    api_url <- paste0(.ds_dev_api(), "Reference/", ds_ref, "/UploadFile")
  } else {
    api_url <- paste0(.ds_secure_api(), "Reference/", ds_ref, "/UploadFile")
  }

  #upload the zip file
  req <- httr::POST(
    url = api_url,
    httr::add_headers('Content-Type' = 'multipart/form-data'),
    httr::authenticate(":", "", "ntlm"),
    body = list(addressFile = httr::upload_file(download_file_path)),
    encode = "multipart",
    httr::progress(type = "up", con = ""))

    status_code <- httr::stop_for_status(req)$status_code

      if(status_code != 201){
      stop("ERROR: DataStore connection failed. Your file was not successfully uploaded.")
    }
    else{
      cat("Your file, ", crayon::blue$bold(file_name),
          ", has been uploaded to:\n", sep = "")
      cat(req$headers$location, "\n", sep="")
    }

  #add a web link:
  #release url:
  weblink <- gh_req_rjson$html_url
  #last verified date/time:
  sys_date <- Sys.time()
  sys_date_iso8601 <- strptime(sys_date, format="%Y-%m-%d %H:%M:%S")
  mylist <- list(resourceID = "0",
                 userSort = "0",
                 description = "GitHub.com url for the release",
                 uri = weblink,
                 lastVerified = sys_date_iso8601)
  bdy <- jsonlite::toJSON(mylist, pretty = TRUE, auto_unbox = TRUE)

  #use reference id to put the weblink:
  if(dev == TRUE){
    api_url <- paste0(.ds_dev_api(), "Reference/", ds_ref, "/ExternalLinks")
  } else {
    api_url <- paste0(.ds_secure_api(), "Reference/", ds_ref, "/ExternalLinks")
  }

  #upload the weblink:
  req <- httr::POST(
    url = api_url,
    httr::add_headers('Content-Type'='application/json'),
    httr::authenticate(":", "", "ntlm"),
    body = bdy)

  status_code <- httr::stop_for_status(req)$status_code

  json <- httr::content(req, "text")
  rjson <- jsonlite::fromJSON(json)

}




  #generate json body for rest api call to create the reference:
  mylist <- list(referenceTypeId = "Script",
                 title = dynamic_title,
                 location = "",
                 issuedDate = list(year = 0,
                                   month = 0,
                                   day = 0,
                                   precision = ""))
bdy <- jsonlite::toJSON(mylist, pretty = TRUE, auto_unbox = TRUE)


GH token:
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

curl -L -H "Accept: application/vnd.github+json" \
-H "Authorization: Bearer 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" \
-H "X-GitHub-Api-Version: 2022-11-28" \
https://api.github.com/repos/nationalparkservice/EMLeditor/releases/latest


curl  https://api.github.com/repos/nationalparkservice/EMLeditor/releases/latest -L -H 'Accept: application/vnd.github+json' -H 'X-GitHub-Api-Version: 2022-11-28'

[
  {
    "url": ["https://api.github.com/repos/nationalparkservice/EMLeditor/releases/latest"],
    "method": ["GET"],
    "cookies": {},
    "username": {},
    "password": {},
    "user_agent": {},
    "referer": {},
    "data": {},
    "headers": {
      "Accept": ["application/vnd.github+json"],
      "X-GitHub-Api-Version": ["2022-11-28"]
    },
    "verbose": [false],
    "url_parts": {
      "scheme": ["https"],
      "hostname": ["api.github.com"],
      "port": {},
      "path": ["repos/nationalparkservice/EMLeditor/releases/latest"],
      "query": {},
      "params": {},
      "fragment": {},
      "username": {},
      "password": {}
    },
    "orig_curl": ["curl  https://api.github.com/repos/nationalparkservice/EMLeditor/releases/latest -L -H 'Accept: application/vnd.github+json' -H 'X-GitHub-Api-Version: 2022-11-28'"]
  }
]

req <- httr::GET("https://api.github.com/repos/nationalparkservice/EMLeditor/releases/latest",
                  #httr::authenticate(":", "", "ntlm"),
                  httr::add_headers('Accept'='application/vnd.github+json'),
                  httr::verbose()
)

req <- httr2:
