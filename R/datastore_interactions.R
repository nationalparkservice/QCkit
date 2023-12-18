#' Turn a GitHub release into a DataStore Script Reference
#'
#' @description Given a GitHub owner ("nationalparkservice") and public repo ("EMLeditor"), the function uses the GitHub API to access the latest release version on GitHub and generate a corresponding draft Script reference on DataStore.
#'
#' WARNING: if you are not an author of the repo on GitHub, you should probably NOT be the one adding it to DataStore unless you have very good reason. If you want to cite a GitHub release/repo and need a DOI, contact the repo maintainer and suggest they use this function to put it on DataStore for you.
#'
#' It searches DataStore for references with a similar title (where the title is repo + release tag). If `force = FALSE` and there are similarly titled references, the function will return a list of them and ask if the user really wants a new DataStore reference generated. Assuming yes (or if there are no existing DataStore references with a similar title or `force = TRUE`), the function will: 1) download the .zip of the latest GitHub release for the repo, 2) initiate a draft reference on DataStore, 3) give the draft reference a title (repo + release tag), 4) upload the .zip from GitHub 5) add a web link to the release on GitHub.
#'
#' `create_datastore_script()` will also access the keywords from your GitHub repo ("Topics") and add them to the draft references as keywords. It will automatically set your reference and all the files and links to public, allow data managers to edit the reference, and set the quality to "Operational". Unless you have good reason backed by a policy that specifically includes information from your release, please do not change these settings (and perhaps reconsider using public github repositories).
#'
#' The user will still need to go access the draft Script reference on DataStore to fill in the remaining fields (which are not accessible via API and so cannot be automated through this function) and activate the reference (thereby generating and registering a citeable DOI).
#'
#' If the Reference is a version of an older reference, the user will have to access the older version and indicate that it is an older version of the current Reference. The user will also have to manually add the new Reference to a Project for the repo, if desired.
#'
#' @param owner String. The owner of the account where the GitHub repo resides. For example, "nationalparkservice"
#' @param repo String. The repo with a release that should be turned into a DataStore Script reference. For example, "EMLeditor"
#' @param path String. The location where the release .zip from GitHub should be downloaded to (and uploaded from). Defaults to the working directory of the R Project (i.e. `here::here()`).
#' @param force Logical. Defaults to FALSE. In the default status the function has a number of interactive components, such as searching DataStore for similarly titled References and asking if a new Reference is really what the user wants. When set to TRUE, all interactive components are turned off and the function will proceed unless it hits an error. Setting force = TRUE may be useful for scripting purposes.
#' @param dev Logical. Defaults to FALSE. In the default status, the function generates and populates a new draft Script reference on the DataStore production server. If set to TRUE, the draft Script reference will be generated and populated on the DataStore development server. Setting dev = TRUE may be useful for testing the function without generating excessive references on the DataStore production server.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' create_datastore_script("nationalparkservice", "EMLeditor")
#' }
create_datastore_script <- function(owner,
                                    repo,
                                    path = here::here(),
                                    force = FALSE,
                                    dev = FALSE) {
  gh_url <- paste0("https://api.github.com/repos/",
                   owner,
                   "/",
                   repo,
                   "/releases/latest")

  #GitHub API request for latest release of a given repo:
  gh_req <- httr::GET(
    gh_url,
    httr::add_headers('Accept' = 'application/vnd.github+json'))

  status_code <- httr::stop_for_status(gh_req)$status_code

  #if API call fails, alert user and remind them to log on to VPN:
  if (!status_code == 200) {
    stop("ERROR: GitHub connection failed. Are you connected to the internet?")
  }

  #Make the json R friendly:
  gh_req_json <- httr::content(gh_req, "text")
  gh_req_rjson <- jsonlite::fromJSON(gh_req_json)

  #check DataStore for existing references with the same title
  #Title is auto generated as repo + version; replace space with %20
  new_ref_title <- paste0(repo, " ", gh_req_rjson$tag_name)
  dynamic_title <- gsub(" ", "%20", new_ref_title)

  #quick search of DataStore for the string "dynamic title"
  post_url <- paste0(.QC_ds_secure_api(), "QuickSearch?q=", dynamic_title)
  req <- httr::GET(post_url,
                   httr::authenticate(":", "", "ntlm"),
                   httr::add_headers('accept' = 'application/json'))

  #check status code; suggest logging in to VPN if errors occur:
  status_code <- httr::stop_for_status(req)$status_code
  if (!status_code == 200) {
    stop("ERROR: DataStore connection failed. Are you logged in to the VPN?\n")
  }

  #get search results and turn into a dataframe:
  json <- httr::content(req, "text")
  rjson <- jsonlite::fromJSON(json)
  items <- as.data.frame(rjson$items)

  #search for title in title list, if force == false:
  if (force == FALSE) {
    if (length(items) > 0) {
      matches <- items %>% filter(stringr::str_detect(items$title,
                                                      new_ref_title))
      if (length(seq_along(matches$title) > 0)) {
        cat("One or more DataStore references with title containing: ",
            new_ref_title,
            " already exists:", sep = "")
        cat("Reference ID: ",
            matches$referenceId,
            "; Title: ",
            matches$title, sep = "")
        cat("Are you sure you want to create a new draft reference for ",
            new_ref_title, "?", sep = "")
        var1 <- readline(prompt = cat("\n\n1: Yes\n2: No\n\n"))
        if (var1 == 2) {
          cat("Your have not generated a new DataStore refernce.")
          return()
        }
      }
    }
  }

  #get download link for .zip file
  gh_zip_url <- gh_req_rjson$zipball_url

  #in case someone uses non-default directory
  orig_wd <- getwd()
  #set directory back to original working directory on exit.
  on.exit(setwd(orig_wd), add = TRUE)
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
    suppressMessages(
      httr::content(
        httr::GET(gh_zip_url,
        httr::progress(),
        httr::write_disk(download_file_path,
        overwrite = TRUE))))))
  if (force == FALSE) {
    cat("Writing: ",
        crayon::blue$bold(download_file_path),
        ".\n", sep = "")
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

  if (dev == TRUE) {
    post_url <- paste0(.QC_ds_dev_api(), "Reference/CreateDraft")
  } else {
    post_url <- paste0(.QC_ds_secure_api(), "Reference/CreateDraft")
  }

  #create the draft reference:
  req <- httr::POST(post_url,
                    httr::authenticate(":", "", "ntlm"),
                    httr::add_headers('Content-Type' = 'application/json'),
                    body = bdy)
  #check status code; suggest logging in to VPN if errors occur:
  status_code <- httr::stop_for_status(req)$status_code
  if (!status_code == 200) {
    stop("ERROR: DataStore connection failed. Are you logged in to the VPN?\n")
  }
  #get newly created reference id:
  json <- httr::content(req, "text")
  rjson <- jsonlite::fromJSON(json)
  ds_ref <- rjson$referenceCode

  if (dev == TRUE) {
    ds_profile_link <- paste0(
      "https://irmadev.nps.gov/DataStore/Reference/Profile/",
      ds_ref)
    } else {
    ds_profile_link <- paste0(
      "https//irma.nps.gov/DataStore/Reference/Profile/",
      ds_ref)
  }
  #inform user a new reference has been generated:
  if (force == FALSE) {
    cat("A draft reference has been created on DataStore.\n")
  }

  #check for files that are too big!
  if (file.size(download_file_path) > 33554432) {
    #warn for each file >32Mb
    if (force == FALSE) {
    cat(crayon::blue$bold(file_name),
        " is greater than 32Mb and cannot be uploaded with this funcion.\n",
        "please use the DataStore website to upload your files manually.",
        sep = "")
      }
    stop()
  }

  #use reference id to put the file:
  if (dev == TRUE) {
    api_url <- paste0(.QC_ds_dev_api(), "Reference/", ds_ref, "/UploadFile")
  } else {
    api_url <- paste0(.QC_ds_secure_api(), "Reference/", ds_ref, "/UploadFile")
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
  if (status_code != 201) {
    stop("ERROR: DataStore connection failed. Your file was not uploaded.")
  }
  ds_resource_url <- req$headers$location
    if (force == FALSE) {
      cat("Your file, ", crayon::blue$bold(file_name),
          ", has been uploaded to:\n", sep = "")
      cat(ds_resource_url, "\n", sep = "")
  }
  #add a web link:
  #release url:
  weblink <- gh_req_rjson$html_url
  #last verified date/time:
  sys_date <- Sys.time()
  sys_date_iso8601 <- strptime(sys_date, format = "%Y-%m-%d %H:%M:%S")
  mylist <- list(resourceID = "0",
                 userSort = "0",
                 description = "GitHub.com url for the release",
                 uri = weblink,
                 lastVerified = sys_date_iso8601)
  bdy <- jsonlite::toJSON(mylist, pretty = TRUE, auto_unbox = TRUE)

  #use reference id to put the weblink:
  if (dev == TRUE) {
    api_url <- paste0(.QC_ds_dev_api(),
                      "Reference/",
                      ds_ref,
                      "/ExternalLinks")
  } else {
    api_url <- paste0(.QC_ds_secure_api(),
                      "Reference/",
                      ds_ref,
                      "/ExternalLinks")
  }

  #upload the weblink:
  req <- httr::POST(
    url = api_url,
    httr::add_headers('Content-Type' = 'application/json'),
    httr::authenticate(":", "", "ntlm"),
    body = bdy)

  status_code <- httr::stop_for_status(req)$status_code
  if (status_code != 200) {
    stop("ERROR: DataStore connection failed. Your web link was not added.")
  }
  if (force == FALSE) {
    cat("The following web link has been added to your Script Reference: \n")
    cat(weblink, "\n", sep = "")
  }

  #add keywords
  #get keywords from repo
  gh_topics_url <- paste0("https://api.github.com/repos/",
                          owner,
                          "/",
                          repo,
                          "/topics")
  headers <- c(`Accept` = "application/vnd.github.mercy-preview+json")
  res <- httr::GET(url = gh_topics_url, httr::add_headers(.headers = headers))

  status_code <- httr::stop_for_status(res)$status_code
  if (status_code != 200) {
    stop("ERROR: GitHub connection failed. Your topics were not retrieved.")
  }
  json <- httr::content(res, "text")
  rjson <- jsonlite::fromJSON(json)

  #add keywords to reference:
  if (length(seq_along(rjson$names)) > 0) {
    keywords <- sort(rjson$names)
    keyword_string <- paste(as.character(keywords), collapse = ", ")

    if (dev == TRUE) {
      ds_kw_url <- paste0(.QC_ds_dev_api(), "Reference/", ds_ref, "/Keywords")
    } else {
      ds_kw_url <- paste0(.QC_ds_secure_api(),
                          "Reference/", ds_ref, "/Keywords")
    }
    #make json body
    mylist <- keywords
    bdy <- jsonlite::toJSON(mylist, pretty = TRUE, auto_unbox = TRUE)

    #api request to DataStore to add keywords (does not replace, just adds)
    req_kw <- httr::POST(
      url = ds_kw_url,
      httr::add_headers('accept' = "application/json"),
      httr::add_headers('Content-Type' = "application/json"),
      httr::authenticate(":", "", "ntlm"),
      body = bdy
    )
    #Check request status
    status_code <- httr::stop_for_status(req_kw)$status_code
    if (status_code != 200) {
      stop("ERROR: DataStore connection failed. Keywords were not added.")
    }
    #make response pretty
    json_kw <- httr::content(req_kw, "text")
    rjson_kw <- jsonlite::fromJSON(json_kw)
    #inform user of new keywords
    if (force == FALSE) {
      cat("Your DataStore Reference now has the following keywords:\n")
      print(rjson_kw)
    }
  }

  if (force == FALSE) {
    if (length(seq_along(rjson$names)) < 1) {
      cat("The ", repo, "repository at github.com/",
          repo, "does not have any topics.\n", sep = "")
      cat("No keywords will be added to the DataStore reference.")
    }
  }

  #set permissions to public
  #generate json body for rest api call to create the reference:
  mylist <- list(internal = FALSE,
                 filesInternal = FALSE,
                 FileRestricted = FALSE,
                 sensitivity = "Not Sensitive",
                 distribution = "No Restriction",
                 quality = "Operational",
                 additionalUserDefined = "",
                 noDataMgrEdit = FALSE,
                 linksRestricted = FALSE)
  bdy <- jsonlite::toJSON(mylist, pretty = TRUE, auto_unbox = TRUE)

  if (dev == TRUE) {
    post_url <- paste0(.QC_ds_dev_api(), "Reference/",
                       ds_ref, "/AccessConstraints")
  } else {
    post_url <- paste0(.QC_ds_secure_api(), "Reference/",
                       ds_ref, "/AccessConstraints")
  }

  #create the draft reference:
  req <- httr::PUT(post_url,
                   httr::authenticate(":", "", "ntlm"),
                   httr::add_headers('Content-Type' = 'application/json'),
                   body = bdy)
  #check status code; suggest logging in to VPN if errors occur:
  status_code <- httr::stop_for_status(req)$status_code
  if (!status_code == 200) {
    stop("ERROR: DataStore connection failed. Are you logged in to the VPN?\n")
  }
  #get newly created reference id:
  json <- httr::content(req, "text")
  rjson <- jsonlite::fromJSON(json)

  if (force == FALSE) {
    cat("Your reference has been set to unrestricted with no sensitivity.\n")
    cat("Your reference has been set to \"Operational\" quality.\n")
    cat("Unless you have a documented reason to do so, please leave the reference public.\n")

    cat("Your draft reference can be accessed at:\n")
    if (dev == TRUE) {
      ds_ref_url <- paste0(
        "https://irmadev.nps.gov/DataStore/Reference/Profile/",
        ds_ref)
    } else {
      ds_ref_url <- paste0("https://irma.nps.gov/DataStore/Reference/Profile/",
                  ds_ref)
    }
    cat(crayon::blue$bold(ds_ref_url))
  }
}