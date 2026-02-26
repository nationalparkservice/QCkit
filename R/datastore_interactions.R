#' Turn a GitHub release into a DataStore Script Reference
#'
#' @description Given a GitHub owner ("nationalparkservice") and public repo ("EMLeditor"), the function uses the GitHub API to access the latest release version on GitHub and generate a corresponding draft public Script reference on DataStore.
#'
#' WARNING: if you are not an author of the repo on GitHub, you should probably NOT be the one adding it to DataStore unless you have very good reason. If you want to cite a GitHub release/repo and need a DOI, contact the repo maintainer and suggest they use this function to put it on DataStore for you.
#'
#' The function searches DataStore for references with a similar title (where the title is repo + release tag). If `force = FALSE` and there are similarly titled references, the function will return a list of them and ask if the user really wants a new DataStore reference generated. Assuming yes (or if there are no existing DataStore references with a similar title or if `force = TRUE`), the function will:
#' 1) download the .zip of the latest GitHub release for the repo,
#' 2) initiate a draft reference on DataStore,
#' 3) give the draft reference a title (repo + release tag),
#' 4) upload the .zip from GitHub
#' 5) add a web link to the release on GitHub.
#' 6) add the items listed under GitHub repo "Topics" as keywords to the DataStore Script reference
#' 7) Set for or by NPS flag
#' 8) Set the issued date
#' 9) If you indicate it is an R package, the authors, steward, description, and other fields will be filled out on the Core tab
#'#'
#' The user will still need to go access the draft Script reference on DataStore to fill in the remaining fields (which are not accessible via API and so cannot be automated through this function) and activate the reference (thereby generating and registering a citeable DOI).
#'
#' If the Reference is a version of an older reference, the user will have to access the older version and indicate that it is an older version of the current Reference. The user will also have to manually add the new Reference to a Project for the repo, if desired.
#'
#' @param owner String. The owner of the account where the GitHub repo resides. For example, "nationalparkservice"
#' @param repo String. The repo with a release that should be turned into a DataStore Script reference. For example, "EMLeditor"
#' @param lib_type String. Can be one of three values: generic_script, R, or python. Defaults to "generic_script".
#' @param path String. The location where the release .zip from GitHub should be downloaded to (and uploaded from). Defaults to the working directory of the R Project (i.e. `here::here()`).
#' @param force Logical. Defaults to FALSE. In the default status the function has a number of interactive components, such as searching DataStore for similarly titled References and asking if a new Reference is really what the user wants. When set to TRUE, all interactive components are turned off and the function will proceed unless it hits an error. Setting force = TRUE may be useful for scripting purposes.
#' @param dev Logical. Defaults to FALSE. In the default status, the function generates and populates a new draft Script reference on the DataStore production server. If set to TRUE, the draft Script reference will be generated and populated on the DataStore development server. Setting dev = TRUE may be useful for testing the function without generating excessive references on the DataStore production server.
#' @param for_or_by_NPS Logical. Was the code, script, or software created either for or by NPS? Defaults to TRUE.
#' @param chunk_size_mb The "chunk" size to break the file into for upload. If your network is slow and your uploads are failing, try decreasing this number (e.g. 0.5 or 0.25).
#' @param retry How many times to retry uploading a file chunk if it fails on the first try.
#'
#' @return Invisibly returns the URL to the DataStore draft reference that was created.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' create_datastore_script("nationalparkservice", "EMLeditor")
#' }
create_datastore_script <- function(owner,
                                    repo,
                                    lib_type = c("generic_script",
                                                 "R",
                                                 "python"),
                                    path = here::here(),
                                    force = FALSE,
                                    dev = FALSE,
                                    for_or_by_NPS = TRUE,
                                    chunk_size_mb = 1,
                                    retry = 1) {

  #check "lib_type" for valid values; if no value supplied defaults to "generic_script":
  lib_type <- match.arg(lib_type)

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
  req_search <- httr::GET(post_url,
                   httr::authenticate(":", "", "ntlm"),
                   httr::add_headers('accept' = 'application/json'))

  #check status code; suggest logging in to VPN if errors occur:
  status_code <- httr::stop_for_status(req_search)$status_code
  if (!status_code == 200) {
    stop("ERROR: DataStore connection failed. Are you logged in to the VPN?\n")
  }

  #get search results and turn into a dataframe:
  json <- httr::content(req_search, "text")
  rjson <- jsonlite::fromJSON(json)
  items <- as.data.frame(rjson$items)

  #search for title in title list, if force == false:
  if (force == FALSE) {
    if (length(items) > 0) {
      matches <- dplyr::filter(items, grepl(new_ref_title, title))
      if (length(seq_along(matches$title) > 0)) {
        cat("One or more DataStore references with title containing: ",
            new_ref_title,
            " already exists:\n", sep = "")
        cat("Reference ID: ",
            matches$referenceId,
            "; Title: ",
            matches$title,
            "\n\n", sep = "")
        cat("Are you sure you want to create a new draft reference for ",
            new_ref_title, "?", sep = "")
        var1 <- readline(prompt = cat("\n\n1: Yes\n2: No\n\n"))
        if (var1 == 2) {
          cat("You have not generated a new DataStore reference.")
          return(invisible(NULL))
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
    cat("Downloading and Writing: ",
        crayon::blue$bold(download_file_path),
        ".\n\n", sep = "")
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
  req_draft <- httr::POST(post_url,
                    httr::authenticate(":", "", "ntlm"),
                    httr::add_headers('Content-Type' = 'application/json'),
                    body = bdy)
  #check status code; suggest logging in to VPN if errors occur:
  status_code <- httr::stop_for_status(req_draft)$status_code
  if (!status_code == 200) {
    stop("ERROR: DataStore connection failed. Are you logged in to the VPN?\n")
  }
  #get newly created reference id:
  json <- httr::content(req_draft, "text")
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
    cat("A draft reference has been created on DataStore.\n\n")
  }

  #use reference id to put the file:
  if (dev == TRUE) {
    api_url <- .QC_ds_dev_api()
  } else {
    api_url <- .QC_ds_secure_api()
  }

# ----- Begin code copied from DataStore API wrapper -----

  # Get a token, which we need for a multi-chunk upload
  upload_token <- httr2::request(api_url) |>
    httr2::req_options(httpauth = 4L, userpwd = ":::") |>
    httr2::req_url_path_append("Reference", ds_ref, "UploadFile", "TokenRequest") |>
    httr2::req_body_json(list(Name = file_name),
                         type = "application/json") |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()

  upload_url <- upload_token$headers$Location
  file_size_bytes <- file.size(download_file_path)
  chunk_size_bytes <- round(chunk_size_mb * 1024 * 1024)
  n_chunks <- ceiling(file_size_bytes/chunk_size_bytes)

  # Open file connection in binary mode
  file_con <- file(download_file_path, "rb")

  # Initialize variables and progress bar to track upload progress
  status <- NA
  total_bytes <- 0
  progress_bar_msg <- paste("Uploading", file_name)
  cli::cli_progress_bar(progress_bar_msg, total = n_chunks)

  # Upload one chunk at a time
  for (i in 0:(n_chunks - 1)) {

    # Starting byte and ending byte for this chunk
    start <- i * chunk_size_bytes
    end <- start + chunk_size_bytes - 1

    # If we've exceeded the file size, reset ending byte
    if (end >= file_size_bytes) {
      end <- file_size_bytes - 1
    }

    n_bytes <- length(start:end)  # this should be chunk_size_bytes except on the last iteration
    total_bytes <- total_bytes + n_bytes  # total bytes uploaded so far

    # Reset the number of retries for each new chunk
    n_retries <- retry

    # Upload a single chunk. Potentially try again if it fails (retry > 0)
    while (n_retries >= 0) {
      upload_resp <- httr2::request(upload_url) |>
        httr2::req_method("PUT") |>
        httr2::req_headers(`Content-Length` = n_bytes,
                           `Content-Range` = glue::glue("bytes {start}-{end}/{file_size_bytes}")) |>
        httr2::req_body_raw(readBin(file_con, raw(), n = n_bytes)) |>
        httr2::req_options(httpauth = 4L, userpwd = ":::") |>
        httr2::req_error(is_error = \(resp) FALSE) |>
        httr2::req_perform()

      if (!httr2::resp_is_error(upload_resp) || httr2::resp_status(upload_resp) == 410) {
        # If upload is successful, or if error is due to token problem, don't retry
        n_retries <- -1
      } else {
        # Decrement retries remaining
        n_retries <- n_retries - 1
      }
    }

    # Throw an error if the chunk ultimately fails
    if (httr2::resp_status(upload_resp) == 410) {
      cli::cli_abort("Your upload token is invalid or has expired. Please try again. If the problem persists, contact the package maintainer or the DataStore helpdesk.")
    } else if (httr2::resp_is_error(upload_resp)) {
      cli::cli_abort("File upload was unsuccessful. Please try again. If the problem persists, contact the package maintainer or the DataStore helpdesk.")
    }

    cli::cli_progress_update()
  }
  cli::cli_progress_done()

  close(file_con)

# ------ End code copied from DataStore API wrapper -----

  ds_resource_url <- upload_resp$headers$location
    if (force == FALSE) {
      cat("Your file, ", crayon::blue$bold(file_name),
          ", has been uploaded to:\n", sep = "")


      #### ds_resource_url is not a good url; needs fixing

            cat(ds_resource_url, "\n\n", sep = "")
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
  req_weblink <- httr::POST(
    url = api_url,
    httr::add_headers('Content-Type' = 'application/json'),
    httr::authenticate(":", "", "ntlm"),
    body = bdy)

  status_code <- httr::stop_for_status(req_weblink)$status_code
  if (status_code != 200) {
    stop("ERROR: DataStore connection failed. Your web link was not added.")
  }
  if (force == FALSE) {
    cat("The following web link has been added to your Script Reference: \n")
    cat(weblink, "\n\n", sep = "")
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
      cat("\n")
    }
  }

  if (force == FALSE) {
    if (length(seq_along(rjson$names)) < 1) {
      cat("The ", repo, " repository at github.com/",
          repo, " does not have any topics.\n", sep = "")
      cat("No keywords will be added to the DataStore reference.\n")
    }
  }

  #set permissions to public
  #generate json body for rest api call to create the reference:
  mylist <- list(internal = FALSE,
                 filesInternal = FALSE,
                 FileRestricted = FALSE,
                 additionalConstraints = "none",
                 legalAuthority = list("none"),
                 otherAuthority = "No Restriction",
                 justification = "none",
                 contactEmail = "none")
  bdy <- jsonlite::toJSON(mylist, pretty = TRUE, auto_unbox = TRUE)

  if (dev == TRUE) {
    post_url <- paste0(.QC_ds_dev_api(), "Reference/",
                       ds_ref, "/AccessConstraints")
  } else {
    post_url <- paste0(.QC_ds_secure_api(), "Reference/",
                       ds_ref, "/AccessConstraints")
  }

  #set reference to public:
  req_public <- httr::PUT(post_url,
                   httr::authenticate(":", "", "ntlm"),
                   httr::add_headers('Content-Type' = 'application/json'),
                   body = bdy)
  #check status code; suggest logging in to VPN if errors occur:
  status_code <- httr::stop_for_status(req_public)$status_code
  if (!status_code == 200) {
    stop("ERROR: DataStore connection failed. Are you logged in to the VPN?\n")
  }

  #set bibliography patch URL
  if (dev == TRUE) {
    patch_url <- paste0(.QC_ds_dev_api(),
                        "Reference/",
                        ds_ref,
                        "/Bibliography")
  } else {
    patch_url <- paste0(.QC_ds_secure_api(),
                        "Reference/",
                        ds_ref,
                        "/Bibliography")
  }

  #Set issuedDate & Agency originated:
  date <- list(year = format(Sys.time(), "%Y"),
               month = format(Sys.time(), "%m"),
               day = format(Sys.time(), "%d"))
  bdy <- list(issuedDate = date,
              isAgencyOriginated = for_or_by_NPS)

  bdy <- jsonlite::toJSON(bdy, pretty = TRUE, auto_unbox = TRUE)

  NPS_req <- httr::PATCH(patch_url,
                     httr::authenticate(":", "", "ntlm"),
                     httr::add_headers('Content-Type' = 'application/json'),
                     body = bdy)

  #check status code; suggest logging in to VPN if errors occur:
  status_code <- httr::stop_for_status(NPS_req)$status_code
  if (!status_code == 200) {
    stop("ERROR: DataStore connection failed. Are you logged in to the VPN?\n")
  }

  # get and use R DESCRIPTION file:
  if (lib_type == "R") {

    descript_url <- paste0("https://raw.githubusercontent.com/",
                           owner, "/",
                           repo, "/",
                           "master/DESCRIPTION")

    #create file name:
    file_name <- paste0(repo, "_", gh_req_rjson$tag_name, "_DESCRIPTION")
    #create file path
    download_file_path <- paste0("releases/", file_name)

    #download the file (.zip) from github:
    invisible(capture.output(
      suppressMessages(
        httr::content(
          httr::GET(descript_url,
                    httr::progress(),
                    httr::write_disk(download_file_path,
                                     overwrite = TRUE))))))
  #  read in the DESCRIPTION file
    desc2 <- tryCatch(desc::description$new(file = paste0("releases/",
                                                          file_name)),
                      error = function(e){},
                      #warnings for bad or missing DESCRIPTION files
                      warning = function(w){
                        msg <- paste0("Warning: your DESCRIPTION file is ",
                                      "missing or malformatted. Some ",
                                      "DataStore fields could not be ",
                                      "automatically entered. Are you sure ",
                                      "the repo contains a valid R package?")
                        cat(msg)
                      })

    if (!is.null(desc2)) {

      #create authors (contact1)
      #authors <- desc::desc_get_author("aut",
      #                                 file = paste0("releases/", file_name))
      authors <- desc2$get_author("aut")
      #if(!is.null(authors)) {
        aut_list <- list()
        for (i in 1:length(seq_along(authors))) {
          aut <- list(title = NULL,
                      primaryName = authors[[i]]$family,
                      firstName = authors[[i]]$given,
                      middleName = NULL,
                      suffix = NULL,
                      affiliation = NULL,
                      isCorporate = FALSE,
                      ORCID = authors[[i]]$comment[[1]])
          aut_list <- append(aut_list, list(aut))
        }
        contact1 <- aut_list

        #create contacts (contact2)
        contacts <- desc2$get_author("cre")
        contact2 <- list()
        for (i in 1:length(contacts)) {
          con <- list(title = NULL,
                    primaryName = contacts[[i]]$family,
                    firstName = contacts[[i]]$given,
                    middleName = NULL,
                    suffix = NULL,
                    affiliation = NULL,
                    isCorporate = FALSE,
                    ORCID = contacts[[i]]$comment[[1]])
          contact2 <- append(contact2, list(con))
        }
      }

      package_descript <- desc2$get("Description")[[1]]

      bdy <- list(miscellaneousCode = "R",
                  abstract = package_descript,
                  contacts1 = contact1,
                  contacts2 = contact2)

      bdy <- jsonlite::toJSON(bdy, pretty = TRUE, null = "null", auto_unbox = TRUE)

      contacts_req <- httr::PATCH(patch_url,
                             httr::authenticate(":", "", "ntlm"),
                             httr::add_headers('Content-Type' = 'application/json'),
                             body = bdy)

      #check status code; suggest logging in to VPN if errors occur:
      status_code <- httr::stop_for_status(contacts_req)$status_code
      if (!status_code == 200) {
        stop("ERROR: DataStore connection failed. Are you logged in to the VPN?\n")
      }
    }



  # make reference URL
  if (dev == TRUE) {
    ds_ref_url <- paste0(
      "https://irmadev.nps.gov/DataStore/Reference/Profile/",
      ds_ref)
  } else {
    ds_ref_url <- paste0("https://irma.nps.gov/DataStore/Reference/Profile/",
                         ds_ref)
  }

  if (force == FALSE) {
    cat("Your reference has been set to unrestricted with no sensitivity.\n")
    cat("Unless you have a documented reason to do so, please leave the reference public.\n\n")

    cat("Your draft reference can be accessed at:\n")
    ds_ref_url <- utils::URLencode(ds_ref_url)
    cli::cli_text("{.url {ds_ref_url}}")
  }

  invisible(ds_ref_url)
}
