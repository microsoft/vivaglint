#' Configure Viva Glint API credentials
#'
#' Stores Viva Glint API credentials in environment variables for the current
#' R session. Optionally writes the values to ~/.Renviron for persistence.
#'
#' Required environment variables:
#' - GLINT_TENANT_ID
#' - GLINT_CLIENT_ID
#' - GLINT_CLIENT_SECRET
#' - GLINT_EXPERIENCE_NAME
#'
#' @param tenant_id Azure AD tenant ID
#' @param client_id Azure AD app (client) ID
#' @param client_secret Azure AD app client secret
#' @param experience_name Viva Glint experience name (e.g., "contoso@demo")
#' @param save_to_renviron Logical; if TRUE, append values to ~/.Renviron
#'
#' @return Invisibly returns TRUE after saving credentials
#'
#' @export
#'
#' @examples
#' \dontrun{
#' glint_setup(
#'   tenant_id = "your-tenant-id",
#'   client_id = "your-client-id",
#'   client_secret = "your-client-secret",
#'   experience_name = "your-experience-name",
#'   save_to_renviron = TRUE
#' )
#' }
glint_setup <- function(tenant_id,
                        client_id,
                        client_secret,
                        experience_name,
                        save_to_renviron = FALSE) {
  if (!nzchar(tenant_id)) {
    stop("tenant_id must be provided.", call. = FALSE)
  }
  if (!nzchar(client_id)) {
    stop("client_id must be provided.", call. = FALSE)
  }
  if (!nzchar(client_secret)) {
    stop("client_secret must be provided.", call. = FALSE)
  }
  if (!nzchar(experience_name)) {
    stop("experience_name must be provided.", call. = FALSE)
  }

  Sys.setenv(GLINT_TENANT_ID = tenant_id)
  Sys.setenv(GLINT_CLIENT_ID = client_id)
  Sys.setenv(GLINT_CLIENT_SECRET = client_secret)
  Sys.setenv(GLINT_EXPERIENCE_NAME = experience_name)

  message("Glint credentials saved to environment variables for this session.")

  if (isTRUE(save_to_renviron)) {
    renviron_path <- file.path(Sys.getenv("HOME"), ".Renviron")
    lines <- c(
      "",
      "# --- Viva Glint API credentials ---",
      paste0("GLINT_TENANT_ID=", tenant_id),
      paste0("GLINT_CLIENT_ID=", client_id),
      paste0("GLINT_CLIENT_SECRET=", client_secret),
      paste0("GLINT_EXPERIENCE_NAME=", experience_name)
    )
    cat(paste(lines, collapse = "\n"), "\n",
        file = renviron_path, append = TRUE)
        message("Also written to ", renviron_path,
          " - restart R to auto-load in future sessions.")
  }

  invisible(TRUE)
}


#' Read Viva Glint Survey Data via API
#'
#' Exports a survey cycle through the Microsoft Graph beta API, downloads the
#' resulting ZIP archive, and returns a \code{glint_survey} object. This is an
#' alternative to \code{read_glint_survey()} when you want to pull data directly
#' from Viva Glint instead of importing a local CSV export.
#'
#' @param survey_uuid Survey UUID from Viva Glint
#' @param cycle_id Survey cycle ID
#' @param emp_id_col Character string specifying the employee ID column name
#' @param first_name_col Column name for first name (default: "First Name")
#' @param last_name_col Column name for last name (default: "Last Name")
#' @param email_col Column name for email (default: "Email")
#' @param status_col Column name for status (default: "Status")
#' @param completion_date_col Column name for survey completion date
#'   (default: "Survey Cycle Completion Date")
#' @param sent_date_col Column name for survey sent date
#'   (default: "Survey Cycle Sent Date")
#' @param start_date Optional start date/time for the export window. Can be a
#'   character string in ISO 8601 format, or a Date/POSIXct value.
#' @param end_date Optional end date/time for the export window. Can be a
#'   character string in ISO 8601 format, or a Date/POSIXct value.
#' @param encoding Character string specifying file encoding (default: "UTF-8")
#' @param poll_interval Seconds to wait between status checks (default: 10)
#' @param max_attempts Maximum number of polling attempts (default: 60)
#' @param experience_name Optional Viva Glint experience name to override the
#'   GLINT_EXPERIENCE_NAME environment variable
#'
#' @return A \code{glint_survey} object (same structure as \code{read_glint_survey}).
#'   The \code{metadata$file_path} field is set to \code{NA}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' glint_setup(
#'   tenant_id = "your-tenant-id",
#'   client_id = "your-client-id",
#'   client_secret = "your-client-secret",
#'   experience_name = "your-experience-name"
#' )
#'
#' survey <- read_glint_survey_api(
#'   survey_uuid = "your-survey-uuid",
#'   cycle_id = "your-cycle-id",
#'   emp_id_col = "EMP ID"
#' )
#' }
read_glint_survey_api <- function(survey_uuid,
                                  cycle_id,
                                  emp_id_col = NULL,
                                  first_name_col = "First Name",
                                  last_name_col = "Last Name",
                                  email_col = "Email",
                                  status_col = "Status",
                                  completion_date_col = "Survey Cycle Completion Date",
                                  sent_date_col = "Survey Cycle Sent Date",
                                  start_date = NULL,
                                  end_date = NULL,
                                  encoding = "UTF-8",
                                  poll_interval = 10,
                                  max_attempts = 60,
                                  experience_name = NULL) {
  if (!nzchar(survey_uuid)) {
    stop("survey_uuid must be provided.", call. = FALSE)
  }
  if (!nzchar(cycle_id)) {
    stop("cycle_id must be provided.", call. = FALSE)
  }

  exp_name <- experience_name %||% glint_env(
    "GLINT_EXPERIENCE_NAME",
    "Experience Name"
  )

  export_url <- paste0(
    glint_graph_base, "/", utils::URLencode(exp_name, reserved = TRUE),
    "/surveys/", survey_uuid,
    "/surveyCycles/", cycle_id,
    "/exportSurveys"
  )

  data <- glint_run_export_pipeline(
    export_url,
    start_date = start_date,
    end_date = end_date,
    poll_interval = poll_interval,
    max_attempts = max_attempts,
    encoding = encoding,
    experience_name = exp_name
  )

  if (!is.data.frame(data)) {
    stop(
      "Export returned multiple CSV files with different schemas. ",
      "Please confirm the export content in Viva Glint and retry.",
      call. = FALSE
    )
  }

  build_glint_survey(
    data,
    emp_id_col,
    first_name_col = first_name_col,
    last_name_col = last_name_col,
    email_col = email_col,
    status_col = status_col,
    completion_date_col = completion_date_col,
    sent_date_col = sent_date_col,
    file_path = NA_character_
  )
}


glint_graph_base <- "https://graph.microsoft.com/beta/employeeExperience/sentiment/experiences"


#' Read a required Glint env var, stopping with a helpful message if missing.
#'
#' @keywords internal
glint_env <- function(var_name, label) {
  val <- Sys.getenv(var_name, unset = "")
  if (!nzchar(val)) {
    stop(
      label,
      " is not set. Run glint_setup() first to configure credentials.",
      call. = FALSE
    )
  }
  val
}


glint_format_datetime <- function(value) {
  if (is.null(value)) {
    return(NULL)
  }
  if (is.character(value) && !nzchar(value)) {
    return(NULL)
  }
  if (inherits(value, "Date")) {
    value <- as.POSIXct(value, tz = "UTC")
  }
  if (inherits(value, "POSIXt")) {
    return(format(as.POSIXct(value, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ"))
  }
  as.character(value)
}


glint_get_token <- function() {
  tid <- glint_env("GLINT_TENANT_ID", "Tenant ID")
  cid <- glint_env("GLINT_CLIENT_ID", "Client ID")
  csec <- glint_env("GLINT_CLIENT_SECRET", "Client Secret")

  url <- paste0(
    "https://login.microsoftonline.com/", tid, "/oauth2/v2.0/token"
  )

  resp <- httr::POST(
    url,
    body = list(
      client_id = cid,
      client_secret = csec,
      scope = "https://graph.microsoft.com/.default",
      grant_type = "client_credentials"
    ),
    encode = "form"
  )

  if (httr::http_error(resp)) {
    stop(
      "Token request failed (HTTP ", httr::status_code(resp), "): ",
      httr::content(resp, "text", encoding = "UTF-8"),
      call. = FALSE
    )
  }

  token_data <- httr::content(resp, "parsed", simplifyVector = TRUE)
  if (is.null(token_data$access_token)) {
    stop("Token response did not include an access token.", call. = FALSE)
  }

  expires_at <- as.numeric(Sys.time()) + token_data$expires_in
  Sys.setenv(GLINT_TOKEN = token_data$access_token)
  Sys.setenv(GLINT_TOKEN_EXPIRES = as.character(expires_at))

  invisible(token_data$access_token)
}


glint_ensure_token <- function() {
  token <- Sys.getenv("GLINT_TOKEN", unset = "")
  expires_at <- Sys.getenv("GLINT_TOKEN_EXPIRES", unset = "")

  if (nzchar(token) && nzchar(expires_at)) {
    remaining <- suppressWarnings(as.numeric(expires_at)) - as.numeric(Sys.time())
    if (!is.na(remaining) && remaining > 60) {
      return(token)
    }
  }

  glint_get_token()
  Sys.getenv("GLINT_TOKEN")
}


glint_start_export <- function(export_url,
                               start_date = NULL,
                               end_date = NULL) {
  token <- glint_ensure_token()
  body <- list()

  start_val <- glint_format_datetime(start_date)
  end_val <- glint_format_datetime(end_date)

  if (!is.null(start_val)) {
    body$startDateTime <- start_val
  }
  if (!is.null(end_val)) {
    body$endDateTime <- end_val
  }

  resp <- httr::POST(
    export_url,
    httr::add_headers(
      Authorization = paste("Bearer", token),
      `Content-Type` = "application/json"
    ),
    body = body,
    encode = "json"
  )

  if (httr::http_error(resp)) {
    stop(
      "Export request failed (HTTP ", httr::status_code(resp), "): ",
      httr::content(resp, "text", encoding = "UTF-8"),
      call. = FALSE
    )
  }

  result <- httr::content(resp, "parsed", simplifyVector = TRUE)
  if (is.null(result$id)) {
    stop("Export request did not return a job ID.", call. = FALSE)
  }

  message("Export job started - ID: ", result$id,
          if (!is.null(result$status)) paste0(" | Status: ", result$status) else "")
  result$id
}


glint_poll_status <- function(job_id,
                              experience_name,
                              poll_interval = 10,
                              max_attempts = 60) {
  status_url <- paste0(
    glint_graph_base, "/", utils::URLencode(experience_name, reserved = TRUE),
    "/operations('", job_id, "')"
  )

  for (i in seq_len(max_attempts)) {
    token <- glint_ensure_token()
    resp <- httr::GET(
      status_url,
      httr::add_headers(Authorization = paste("Bearer", token))
    )

    if (httr::http_error(resp)) {
      stop(
        "Status check failed (HTTP ", httr::status_code(resp), "): ",
        httr::content(resp, "text", encoding = "UTF-8"),
        call. = FALSE
      )
    }

    result <- httr::content(resp, "parsed", simplifyVector = TRUE)
    status <- tolower(result$status %||% "")

    if (status == "succeeded") {
      return(invisible(TRUE))
    }
    if (status == "failed") {
      stop("Export job failed: ", result$statusDetail %||% result$status,
           call. = FALSE)
    }

    Sys.sleep(poll_interval)
  }

  stop("Timed out after ", max_attempts * poll_interval, " seconds.", call. = FALSE)
}


glint_download_export <- function(job_id, experience_name) {
  token <- glint_ensure_token()
  download_url <- paste0(
    glint_graph_base, "/", utils::URLencode(experience_name, reserved = TRUE),
    "/operations('", job_id, "')/content"
  )

  resp <- httr::GET(
    download_url,
    httr::add_headers(Authorization = paste("Bearer", token))
  )

  if (httr::http_error(resp)) {
    stop(
      "Download failed (HTTP ", httr::status_code(resp), "): ",
      httr::content(resp, "text", encoding = "UTF-8"),
      call. = FALSE
    )
  }

  resp
}


glint_import_export_zip <- function(resp, encoding = "UTF-8") {
  raw_bytes <- httr::content(resp, "raw")

  tmp_zip <- tempfile(fileext = ".zip")
  tmp_dir <- tempfile("glint_export_")

  on.exit({
    if (file.exists(tmp_zip)) {
      unlink(tmp_zip)
    }
    if (dir.exists(tmp_dir)) {
      unlink(tmp_dir, recursive = TRUE)
    }
  }, add = TRUE)

  writeBin(raw_bytes, tmp_zip)
  dir.create(tmp_dir)
  utils::unzip(tmp_zip, exdir = tmp_dir)

  files <- list.files(tmp_dir, pattern = "\\.csv$",
                      recursive = TRUE, full.names = TRUE)
  if (length(files) == 0) {
    stop("No CSV files found in the ZIP archive.", call. = FALSE)
  }

  dfs <- lapply(files, function(f) {
    readr::read_csv(
      f,
      locale = readr::locale(encoding = encoding),
      show_col_types = FALSE
    )
  })
  names(dfs) <- tools::file_path_sans_ext(basename(files))

  if (length(dfs) == 1) {
    return(dfs[[1]])
  }

  first_cols <- names(dfs[[1]])
  same_schema <- all(vapply(dfs, function(df) {
    identical(names(df), first_cols)
  }, logical(1)))

  if (same_schema) {
    return(dplyr::bind_rows(dfs))
  }

  dfs
}


glint_run_export_pipeline <- function(export_url,
                                      start_date = NULL,
                                      end_date = NULL,
                                      poll_interval = 10,
                                      max_attempts = 60,
                                      encoding = "UTF-8",
                                      experience_name) {
  job_id <- glint_start_export(export_url, start_date = start_date, end_date = end_date)
  glint_poll_status(
    job_id,
    experience_name = experience_name,
    poll_interval = poll_interval,
    max_attempts = max_attempts
  )
  resp <- glint_download_export(job_id, experience_name = experience_name)
  glint_import_export_zip(resp, encoding = encoding)
}
