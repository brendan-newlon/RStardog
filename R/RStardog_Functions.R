## RStardog_Functions.R - compiled by RoxygenReady, a package by @vertesy


#' Install and load multiple packages at once
#'
#' @param pkgs A character vector of package names to install as needed and load to the environment.
#' @param show_loaded_packages If TRUE, echo the list of packages loaded.
#' @param show_package_processes If TRUE, echo messages about the installation processes.
#' @param lib
#' @param repos defaults to 'http://cran.us.r-project.org'
#'
#' @return Installs and loads the packages.
#' @export
#'

source_packages = function(pkgs, show_loaded_packages = F, show_package_processes = F, lib = .libPaths()[[1]], repos='http://cran.us.r-project.org') {
  new.pkgs = pkgs[!(pkgs %in% installed.packages()[, "Package"])]
  if (length(new.pkgs))

    if(show_package_processes){
      install.packages(new.pkgs, dependencies = T, lib = lib, repos=repos)
    } else {
      suppressMessages(suppressPackageStartupMessages( install.packages(new.pkgs, dependencies = T, lib = lib, repos=repos) ))
    }

  if (show_loaded_packages) {
    cat("Packages loaded:", "\n")
    sapply(pkgs, require, character.only = T)
  } else{
    invisible(sapply(pkgs, require, character.only = T))
  } # end of: don't show loaded pkgs


}


#' sourceR
#'
#' Source scripts selectively, recursively
#'
#' @param source_dir
#' @param include_rmd
#' @param exclude_dirs
#' @param exclude_files
#' @param recursive
#' @param exclude_marked_exclude
#' @param exclude_sourceR
#'
#' @return
#' @export
#'
#' @examples
sourceR = function(source_dir = "R",
                   include_rmd = F,
                   exclude_dirs = "",
                   exclude_files = "",
                   recursive = T,
                   exclude_marked_exclude = T,
                   exclude_sourceR = T,
                   echo_files_sourced = T
) {
  sourcing_directory = file.path(source_dir)
  R_scripts = list.files(
    path = sourcing_directory,
    all.files = T,
    include.dirs = F,
    pattern = ".R$",
    recursive = recursive
  )
  rmd_scripts = list.files(
    path = sourcing_directory,
    all.files = T,
    include.dirs = F,
    pattern = ".Rmd$"
  )
  if (include_rmd) {
    sources = append(R_scripts, rmd_scripts)
  } else {
    sources = R_scripts
  }
  if (all(exclude_files != "")) {
    for (s in seq_along(exclude_files)) {
      sources = sources[!grepl(exclude_files[[s]], sources)]
    }
  }
  if(exclude_marked_exclude){sources = sources[!grepl("_exclude.[^.]*$", sources)]}
  if(exclude_sourceR){sources = sources[!grepl("^sourceR.R$", sources)]}
  for (i in seq_along(sources)) {
    if(echo_files_sourced){
      cat("sourcing script:", sources[i], "\n")}
    suppressMessages(
      source(
        file.path(sourcing_directory, sources[i]),
        local = knitr::knit_global(),
        encoding = "UTF-8"
      )
    )
  }
}
## eg.
# sourceR("R",exclude_files=c("sourceR.R"))






#' Summon together the text of many files.
#'
#' @param files Optional - A vector of files to summon
#' @param file_pattern The file pattern to look for if summoning from a directory, eg the file extension ".R$"
#' @param include_rmd Adds "|.Rmd$" to the file_pattern
#' @param exclude_files A vector of files to exclude from being summoned
#' @param exclude_marked_exclude TRUE to exclude any files in the directory with names ending in _exclude before the file extension
#' @param exclude_summon TRUE to exclude the summon.R script file from being summoned
#' @param directory The directory to search for matching files
#' @param recursive Whether to search recursively through subdirectories
#' @param summon_as_filetype What filetype to summon the files into, defaults to .Rmd with a block for each file.
#' @param open_summoned TRUE to immediately open the resulting summoned file.
#'
#' @return Creates a file that pastes together the text content of all of the summoned files. If summon_as_filetype = "Rmd" this will be an Rmd file with one block for each file summoned.
#' @export
#'
summon = function(
  # --- Files --- #
  files = "",
  file_pattern = ".R$",
  include_rmd = T,
  exclude_files = "",
  exclude_marked_exclude = T,
  exclude_summon = T,
  # --- Files in directory etc --- #
  directory = "R",
  recursive = T,
  # exclude_dirs = "", # not yet implemented -- was in sourceR()....?
  # --- Functions --- #
  # functions = "", # feature request !!! (?)
  # could we look back in the session / call stack to find when each function was loaded, and from where, and use that to pull in the text of the function?
  # for functions in loaded packages, can we get that from wherever defined?
  # --- What to do ? --- #
  summon_as_filetype = "Rmd",
  open_summoned = T # FALSE might serve archiving purposes for sets of functions as they existed at a time

) {


  ## --- testing ---
  # files = ""
  # file_pattern = ".R$"
  # include_rmd = T
  # exclude_files = ""
  # exclude_marked_exclude = T
  # exclude_summon = T
  # # --- Files in directory etc --- #
  # directory = "R"
  # all_in_dir = F
  # recursive = T
  # exclude_dirs = ""
  # # --- Functions --- #
  # functions = ""
  # summon_as_filetype = "Rmd"
  # open_summoned =T


  # files = c("happy.txt", "great.R", "ok.Rmd")
  # files = ""
  # file_pattern = ".R$"

  ## What to summon _______________
  if(all(files != "")){
    file_pattern = paste0("^", paste(files, collapse = "$|^"), "$")
  }  else {
    if (include_rmd) {file_pattern = paste0(file_pattern,"|.Rmd$") }
  }
  sources = list.files(path = directory, all.files = T, include.dirs = F, pattern = file_pattern, recursive = recursive)

  ## What not to summon ________________
  if (all(exclude_files != "")) {
    for (s in seq_along(exclude_files)) {
      sources = sources[!grepl(exclude_files[[s]], sources)]
    }
  }
  if(exclude_marked_exclude){sources = sources[!grepl("_exclude.[^.]*$", sources)]}
  if(exclude_summon){sources = sources[!grepl("^summon.R$", sources)]}

  ## Summoning ______________
  source_file_content = readtext::readtext(file.path(directory, sources) ) %>% suppressWarnings()

  # How to divide the sections
  if(isTRUE(summon_as_filetype == "Rmd")){
    block_dividers = c("```{r}\n\n", "```\n\n")
    unblocked_comment_prefix = ""
  } else {
    block_dividers = c("##____________________________________________\n", "##____________________________________________\n##############################################\n\n")
    unblocked_comment_prefix = "# "
  }

  # Create the blank summoned file
  summon_timestamp = capture.output( invisible(paste0(timestamp()) ) )
  summon_time = capture.output(now()) %>% gsub("\"|\\[1\\] |-|:","",.) %>% gsub(" ","_",.)
  summoned = paste0("summoned_",summon_time,".", summon_as_filetype) ;
  invisible(capture.output(file.create(summoned))) ;
  write_file(file = summoned, x = paste0("\n",
                                         summon_timestamp, "\n\n",
                                         unblocked_comment_prefix, "summoned_",summon_time, "\n",
                                         unblocked_comment_prefix, "~~~*%$#&!%$*#@!&^%!#*!*~~~", "\n##############################################\n\n"), append = F)



  # Summon the source files into the summoned file
  for(i in seq_along(source_file_content$doc_id)){
    summoned_block = paste0(
      unblocked_comment_prefix, source_file_content$doc_id[[i]], "\n\n",
      block_dividers[1],
      "#", unblocked_comment_prefix, paste0(source_file_content$doc_id[[i]],"_summoned_",summon_time), "\n\n",
      source_file_content$text[[i]], "\n",
      block_dividers[2]
    )
    write_file(file = summoned, summoned_block, append = TRUE)
  }



  ## Add the casting/banishing block to the end
  banish_cast_block = paste0(
    unblocked_comment_prefix, "Use banish() to close and delete this file or cast() to overwrite the summoned files with edits made to their corresponding blocks above.",
    "\n\n##** BOTH FUNCTIONS ARE NOT ACTUALLY IMPLEMENTED YET - STAY TUNED! **##\n\n",
    "\n\n",
    block_dividers[1],
    unblocked_comment_prefix, "~~~*%$#&!%$*#@!&^%!#*!*~~~", "\n\n",
    "# banish(\"","summoned_",summon_time,"\")", "\n\n",

    "##__________________##", "\n",
    "## --- WARNING! --- ##", "\n",
    "##__________________##", "\n\n",

    "## If you use the function below, the following files will be permanently overwritten:", "\n\t",
    paste("## ",source_file_content$doc_id, collapse = "\n\t"), "\n\n",

    "## Proceed if you dare.", "\n\n",
    # "# cast(\"","summoned_",summon_time,"\")", "\n",
    "# cast(","\n#\t", "c(", "\n#\t",  paste0("\""),
    paste(source_file_content$doc_id,"_summoned_",summon_time, collapse = "\",\n#\t\"", sep = ""),
    paste0("\"\n#\t)"), "\n#"," )", "\n",
    "\n",
    block_dividers[2]
  )
  # cat(banish_cast_block)
  write_file(file = summoned, banish_cast_block, append = TRUE)

  if(open_summoned){file.edit(summoned)}
}
## eg.
# summon(directory = "R", summon_as_filetype = "R")




#' as.df
#'
#' as.df = function(x){as.data.frame(x,strings.as.factors=F)}
#' @param x
#' @examples as.df(x =  )
#' @export
as.df <-function (x) {
	as.data.frame(x, strings.as.factors = F)
}


#' fix_json_encoding
#'
#' fix_json_encoding = function(x){x = gsub("\\\\u003d", "=" , x ) ; x }
#' @param x
#' @examples fix_json_encoding(x =  )
#' @export

fix_json_encoding <-function (x) {
	x = gsub("\\\\u003d", "=", x)
	x
}


#' start_stardog
#'
#' start_stardog = function(server = "localhost"){
#' @param server
#' @examples start_stardog(server = localhost)
#' @export

start_stardog <-function (server = "localhost") {
	if (server == "localhost") {
		cat("Starting Stardog server on Localhost. \nThis may take a couple of minutes...\n")
		stdout = suppressWarnings(system2(command = "cmd", args = c("/c", "stardog-admin server start"),
			wait = F, invisible = F, minimized = F, stdout = T, timeout = 10))
		cat("Started!\n")
		cat(stdout)
	}
}


#' stardog_start
#'
#' @param
#' @examples stardog_start()
#' @export

stardog_start <-function () {
	start_stardog
}


#' stop_stardog
#'
#' stop_stardog = function(server = "localhost"){
#' @param server
#' @examples stop_stardog(server = localhost)
#' @export

stop_stardog <-function (server = "localhost") {
	if (server == "localhost") {
		cat("Stopping Stardog server on Localhost...\n")
		stdout = system2(command = "stardog-admin", args = c("server stop"), wait = F, invisible = F,
			minimized = F, stdout = T)
		cat(stdout)
	}
}


#' stardog_stop
#'
#' @param
#' @examples stardog_stop()
#' @export

stardog_stop <-function () {
	stop_stardog
}


#' handle_keys
#'
#' handle_keys = function(con_service = con_service, Username = "", clear_keys = F, as_password = F){
#' @param con_service
#' @param Username
#' @param clear_keys
#' @param as_password
#' @examples handle_keys(con_service = con_service, Username =  , clear_keys = F, as_password = F)
#' @export

handle_keys <-function (con_service = con_service, Username = "", clear_keys = F, as_password = F) {
	if (!exists("Username") || Username == "" || is_empty(Username)) {
		Username <- readline(prompt = "Enter username: ")
	}
	if (!exists("Username") || Username == "" || is_empty(Username)) {
		stop("Please try again with a valid username. ")
	}
	if (clear_keys) {
		if (con_service %in% key_list(con_service)$service) {
			key_delete(service = con_service, username = Username)
		}
	}
	if (!con_service %in% key_list(con_service)$service || !Username %in% key_list(con_service)$username) {
		if (Username == "anonymous") {
			key_set_with_value(service = con_service, username = Username, password = "anonymous")
		}
		else {
			key_set(service = con_service, username = Username)
		}
	}
	if (as_password) {
		return(key_get(service = con_service, username = Username))
	}
}


#' stardog_
#'
#' stardog_ = function(q = query, d = db, g = graph, U = Username, e = endpoint){
#' @param q
#' @param d
#' @param g
#' @param U
#' @param e
#' @examples stardog_(q = query, d = db, g = graph, U = Username, e = endpoint)
#' @export

stardog_ <-function (q = query, d = db, g = graph, U = Username, e = endpoint) {
	x = stardog(query = q, db = d, graph = g, Username = U, endpoint = e)
	x
}




#' stardog_http
#'
#' @param query
#' @param endpoint
#' @param db
#' @param graph
#' @param Username
#' @param clear_keys
#' @param httr_method
#' @param body
#' @param assign_temp_results
#' @examples
#' @export

stardog_http = function(
  query = "namespaces",
  endpoint = "http://localhost:5820",
  db = "",
  graph = "",
  Username = "admin",
  clear_keys = F,
  httr_method = "GET", # or "POST" etc.
  body = FALSE,
  assign_temp_results = F # used for debugging, to see exactly what was returned by the server
){
  con_service = "stardoghttp"
  handle_keys(con_service = con_service, Username = Username, clear_keys = clear_keys, as_password = F)
  sd_url = paste0(endpoint, paste0("/", db, "/", query) %>% gsub("//","/",.))
  ## Execute the query
  response = eval(parse(text= paste0("httr::",httr_method)))(
    url = sd_url %>% URLencode(),
    add_headers(
      Authorization = paste0("Basic ", base64_enc(paste0(Username,":", key_get(service = con_service, username = Username) )) ),
      accept = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9",
      `accept-encoding` = "gzip, deflate, br"
    ) , # end headers
    body = body
  )
  cat("status: ", response$status_code, "\n")
  assign("Last.status", response$status_code %>% as.character, envir = .GlobalEnv)
  results =  response$content %>% rawToChar()
  if(assign_temp_results){
    assign("temp_results", results, envir = .GlobalEnv)
  }
  if(results != ""){results = results %>% fromJSON()
  df = results[[1]] %>% as.df }
  df
}



#' stardog_http_
#'
#' stardog_http_ = function(q = query, d = db, g = graph, U = Username, e = endpoint, m = httr_method, b = body){
#' @param q
#' @param d
#' @param g
#' @param U
#' @param e
#' @param m
#' @param b
#' @examples stardog_http_(q = query, d = db, g = graph, U = Username, e = endpoint, m = httr_method, b = body)
#' @export

stardog_http_ <-function (q = query, d = db, g = graph, U = Username, e = endpoint, m = httr_method, b = body) {
	x = stardog_http(query = q, db = d, graph = g, Username = U, endpoint = e, httr_method = m, body = b)
	x
}




# _______________________________________________________ stardog.create_db(db_name)


#' stardog_create_db
#'
#' @param db_name
#' @param endpoint
#' @examples stardog.create_db("cars")
#' @export

stardog_create_db = function(
  db_name,
  endpoint = "http://localhost:5820"
){
  r = stardog_http(
    endpoint = endpoint,
    httr_method = "POST",
    query = 'admin/databases',
    body = list(root = paste0('{"dbname": "',db_name,'"}')
                #------------------------------------------------------feature request: support loading an RDF file per docs:
                # https://stardog-union.github.io/http-docs/#operation/createNewDatabase
    )
  ) ;
  # to include files, may need to add encode = ... see httr help on ?POST
  if(Last.status == "201"){cat("Database",db_name, "created!\n")};
  if(Last.status == "400"){cat("Database",db_name, "could not be created! Does it already exist?\n")
    # print(r)
    cat("Stardog says:",r$x[1])
  }
}
## eg.
# stardog.create_db("cars")


## new.R - compiled by RoxygenReady, a package by @vertesy


#' stardog_list_users
#'
#' @param
#' @examples stardog_list_users()
#' @export

stardog_list_users <-function () {
  stardog_http(query = "admin/users")
}


#' stardog_list_user_permissions
#'
#' stardog_list_user_permissions = function(user){
#' @param user
#' @examples stardog_list_user_permissions(user =  )
#' @export

stardog_list_user_permissions <-function (user) {
  stardog_http(query = paste0("admin/permissions/user/", user))
}


#' stardog_list_roles
#'
#' @param
#' @examples stardog_list_roles()
#' @export

stardog_list_roles <-function () {
  stardog_http(query = "admin/roles")
}


#' stardog_add_role
#'
#' stardog_add_role = function(role_name){
#' @param role_name
#' @examples stardog_add_role(role_name =  )
#' @export

stardog_add_role <-function (role_name) {
  r = stardog_http(httr_method = "POST", query = "admin/roles", body = paste0("{\"rolename\": \"",
                                                                              role_name, "\"}"))
  if (Last.status == "201") {
    cat("Role", role_name, "added!\n")
  }
  if (Last.status == "400") {
    cat("Role", role_name, "could not be added! Does that role already exist?\n")
    print(r)
  }
}


#' stardog_delete_role
#'
#' stardog_delete_role = function(role_name){
#' @param role_name
#' @examples stardog_delete_role(role_name =  )
#' @export

stardog_delete_role <-function (role_name) {
  stardog_http(httr_method = "DELETE", query = paste0("admin/roles/", role_name))
  if (Last.status == "204") {
    cat("Role", role_name, "deleted!\n")
  }
  if (Last.status == "404") {
    cat("Role", role_name, "could not be deleted! Does that role exist?\n")
  }
}


#' stardog_derive_classes
#'
#' stardog_derive_classes = function(db, no_instanceless = F){
#' @param db
#' @param no_instanceless
#' @examples stardog_derive_classes(db =  , no_instanceless = F)
#' @export

stardog_derive_classes <-function (db, no_instanceless = F) {
  stardog.list_namespaces(db = db, assign = TRUE)
  classes = stardog(db = db, query = "select distinct ?class {?s a ?class} ") %>% row_as_col("Class")
  classes = classes %>% prefix_col(lookup_df = namespace_prefix_lookup_table, db = db)
  classes
}


#' stardog_list_namespaces
#'
#' stardog_list_namespaces = function(db, assign=FALSE){
#' @param db
#' @param assign
#' @examples stardog_list_namespaces(db =  , assign = FALSE)
#' @export

stardog_list_namespaces <-function (db, assign = FALSE) {
  namespace_prefix_lookup_table = stardog_http(db = db, query = "namespaces") %>% setNames(c("prefix",
                                                                                             "namespace"))
  if (assign)
    assign("namespace_prefix_lookup_table", namespace_prefix_lookup_table, envir = caller_env())
  namespace_prefix_lookup_table
}


#' stardog_virtual_import
#'
#' @param endpoint
#' @param db
#' @param myAuth
#' @param input_file_type
#' @param input_file
#' @param mapping_file
#' @examples
#' @export

stardog_virtual_import = function(
  endpoint = "http://localhost:5820",
  db,
  myAuth = "admin:admin",
  input_file_type = "DELIMITED", # or "JSON"  # ------------ !!!!!!!!!!!!!! FIX ME / TEST !!!!!!!!!!
  input_file,
  mapping_file
){

  # create the db if it doesn't exist yet
  db_list = stardog_http(query = "admin/databases")
  if(isTRUE(db %notin% db_list$x)) stardog.create_db(db)

  # do the virtual import
  my_curl = paste0('curl -u ',myAuth,' -F "database=',db,'" -F "mappings=<',mapping_file,'" -F "input_file_type=',input_file_type,'" -F "input_file=<',input_file,'" ',endpoint,'/admin/virtual_graphs/import' )
  # system2(command = "cmd" , input = c(my_curl) )
  curlr(curl_statement =  my_curl )

  r = stardog.add_namespaces(endpoint = endpoint,db=db, input_file = mapping_file, myAuth = myAuth); r

}
# cat(my_curl)

## eg.
# stardog.virtual_import(
#   endpoint = "http://localhost:5820",
#   db = "cars",
#   input_file_type = "DELIMITED",
#   input_file = file.path("data","cars.csv"),
#   mapping_file = file.path("data", "cars_mappings.sms")
# )


