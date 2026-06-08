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
#' Exports survey data through the Microsoft Graph beta API, downloads the
#' resulting ZIP archive, and returns either a `glint_survey` object or a
#' named list of them depending on what the export contains. This is an
#' alternative to [read_glint_survey()] when you want to pull data directly
#' from Viva Glint instead of importing a local CSV export.
#'
#' # Modes
#'
#' Three export shapes are supported via the `mode` argument:
#'
#' * `"cycle"` — exports a single survey cycle. Requires `survey_uuid` and
#'   `cycle_id`. This is the existing behavior.
#' * `"survey"` — exports every cycle of one survey. Requires `survey_uuid`.
#' * `"daterange"` — exports every survey in the experience that has activity
#'   between `start_date` and `end_date`. Both dates are optional; if both
#'   are omitted, the API applies its default window (about the last six
#'   months).
#'
#' When `mode` is `NULL`, it is read from the `GLINT_MODE` env var; if that
#' is also unset, it is inferred from which identifiers are populated
#' (both IDs => `"cycle"`, survey UUID only => `"survey"`, neither =>
#' `"daterange"`). This keeps the existing positional call
#' `read_glint_survey_api(survey_uuid, cycle_id)` working unchanged.
#'
#' # Environment variable fallbacks
#'
#' Any input not supplied as a function argument is read from the matching
#' environment variable, so the typical call only specifies what differs
#' from the values in `.Renviron`:
#'
#' * `survey_uuid` <- `GLINT_SURVEY_UUID`
#' * `cycle_id` <- `GLINT_CYCLE_ID`
#' * `mode` <- `GLINT_MODE`
#' * `start_date` <- `GLINT_START_DATE`
#' * `end_date` <- `GLINT_END_DATE`
#' * `save_zip_to` <- `GLINT_SAVE_ZIP_TO`
#'
#' Explicit arguments always win over env vars.
#'
#' @param survey_uuid Survey UUID from Viva Glint. Falls back to
#'   `GLINT_SURVEY_UUID`. Required for `"cycle"` and `"survey"` modes.
#' @param cycle_id Survey cycle ID. Falls back to `GLINT_CYCLE_ID`. Required
#'   for `"cycle"` mode.
#' @param mode One of `"cycle"`, `"survey"`, `"daterange"`. Falls back to
#'   `GLINT_MODE`. If still unset, inferred from which identifiers are
#'   populated. Explicit arguments always win.
#' @param emp_id_col Column name for the employee identifier. Defaults to
#'   `"Employment ID"`, the column name that Microsoft Graph emits in API
#'   exports. Override only if your tenant's CSV uses a different name. Pass
#'   `NULL` only via [read_glint_survey()] for CSVs that lack an employee ID
#'   column entirely; for API exports the column is always present.
#' @param first_name_col Column name for first name (default: "First Name")
#' @param last_name_col Column name for last name (default: "Last Name")
#' @param email_col Column name for email (default: "Email")
#' @param status_col Column name for status (default: "Status")
#' @param completion_date_col Column name for survey completion date
#'   (default: "Survey Cycle Completion Date")
#' @param sent_date_col Column name for survey sent date. Defaults to `NULL`
#'   because the API export does not include this column. Set it explicitly
#'   only if your data source provides one.
#' @param manager_id_col Column name for the manager identifier (default:
#'   "Manager ID"). Set to `NULL` to skip manager-based functionality.
#' @param start_date Optional start date/time for the export window. Falls
#'   back to `GLINT_START_DATE`. Can be a character string in ISO 8601
#'   format, or a Date/POSIXct value.
#' @param end_date Optional end date/time for the export window. Falls back
#'   to `GLINT_END_DATE`.
#' @param encoding Character string specifying file encoding (default: "UTF-8")
#' @param poll_interval Seconds to wait between status checks (default: 10)
#' @param max_attempts Maximum number of polling attempts (default: 60)
#' @param save_zip_to Optional path. When set, the raw export zip Microsoft
#'   Graph returns is also written to disk before being parsed into R data
#'   frames — useful for audit trails or feeding other tools. If the path is
#'   an existing directory (or ends with a path separator), the file is
#'   named `glint-export-{job_id}.zip` inside it; otherwise the path is
#'   treated as a full file path. Falls back to `GLINT_SAVE_ZIP_TO` when
#'   the argument is omitted. Defaults to `NULL` (no zip is persisted).
#' @param parse Logical. When `TRUE` (the default), the function unpacks the
#'   downloaded zip into R data frames and wraps the results as
#'   `glint_survey` objects — today's behavior. When `FALSE`, the function
#'   downloads the zip and returns the path it was saved at, skipping the
#'   in-memory parse entirely. `parse = FALSE` requires `save_zip_to` to be
#'   set (otherwise the download has no destination); an error is raised
#'   before any API call is made if it isn't.
#' @param experience_name Optional Viva Glint experience name to override the
#'   GLINT_EXPERIENCE_NAME environment variable
#'
#' @return
#' When `parse = TRUE` (the default):
#' Single-CSV exports (typical for `"cycle"` mode) return a `glint_survey`
#' object with the same structure as [read_glint_survey()] produces.
#' Multi-CSV exports (typical for `"survey"` and `"daterange"` modes) return
#' a named list of `glint_survey` objects keyed by source CSV filename. CSVs
#' that do not fit the standard GlintSurvey schema (e.g. supplementary
#' metadata or attribute files) are returned as plain `data.frame` entries
#' in that list with a warning.
#'
#' When `parse = FALSE`: a character string (invisible) — the path the raw
#' export zip was written to. No data frames are constructed.
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
#' # Cycle mode (also the default if you pass both IDs):
#' cycle_survey <- read_glint_survey_api(
#'   survey_uuid = "your-survey-uuid",
#'   cycle_id = "your-cycle-id"
#' )
#'
#' # Survey mode: every cycle of one survey, returned as a named list.
#' all_cycles <- read_glint_survey_api(
#'   mode = "survey",
#'   survey_uuid = "your-survey-uuid"
#' )
#'
#' # Date-range mode, with everything else read from .Renviron
#' # (GLINT_START_DATE, GLINT_END_DATE):
#' recent <- read_glint_survey_api(mode = "daterange")
#' }
read_glint_survey_api <- function(survey_uuid = NULL,
                                  cycle_id = NULL,
                                  mode = NULL,
                                  emp_id_col = "Employment ID",
                                  first_name_col = "First Name",
                                  last_name_col = "Last Name",
                                  email_col = "Email",
                                  status_col = "Status",
                                  completion_date_col = "Survey Cycle Completion Date",
                                  sent_date_col = NULL,
                                  manager_id_col = "Manager ID",
                                  start_date = NULL,
                                  end_date = NULL,
                                  encoding = "UTF-8",
                                  poll_interval = 10,
                                  max_attempts = 60,
                                  save_zip_to = NULL,
                                  parse = TRUE,
                                  experience_name = NULL) {
  # Resolve env-var fallbacks for each input the caller didn't pass.
  survey_uuid <- survey_uuid %||% glint_env_optional("GLINT_SURVEY_UUID")
  cycle_id    <- cycle_id    %||% glint_env_optional("GLINT_CYCLE_ID")
  start_date  <- start_date  %||% glint_env_optional("GLINT_START_DATE")
  end_date    <- end_date    %||% glint_env_optional("GLINT_END_DATE")
  mode        <- mode        %||% glint_env_optional("GLINT_MODE")
  save_zip_to <- save_zip_to %||% glint_env_optional("GLINT_SAVE_ZIP_TO")

  # parse = FALSE without a save destination would download and discard the
  # zip. Fail fast before any API call so the contract is unambiguous.
  if (!isTRUE(parse) && (is.null(save_zip_to) || !nzchar(save_zip_to))) {
    stop(
      "parse = FALSE requires save_zip_to to be set (or GLINT_SAVE_ZIP_TO ",
      "in .Renviron); otherwise the downloaded zip has nowhere to go.",
      call. = FALSE
    )
  }

  # Pick a mode if nothing said which one.
  if (is.null(mode)) {
    mode <- infer_mode(survey_uuid, cycle_id)
  }
  mode <- match.arg(mode, c("cycle", "survey", "daterange"))

  # Per-mode required-input validation. Errors mention both the function arg
  # and the env-var fallback so callers can fix whichever source they prefer.
  if (mode == "cycle") {
    if (is.null(survey_uuid) || !nzchar(survey_uuid)) {
      stop("survey_uuid must be provided for cycle mode (or set GLINT_SURVEY_UUID).",
           call. = FALSE)
    }
    if (is.null(cycle_id) || !nzchar(cycle_id)) {
      stop("cycle_id must be provided for cycle mode (or set GLINT_CYCLE_ID).",
           call. = FALSE)
    }
  } else if (mode == "survey") {
    if (is.null(survey_uuid) || !nzchar(survey_uuid)) {
      stop("survey_uuid must be provided for survey mode (or set GLINT_SURVEY_UUID).",
           call. = FALSE)
    }
  }

  exp_name <- experience_name %||% glint_env(
    "GLINT_EXPERIENCE_NAME",
    "Experience Name"
  )

  export_url <- build_export_url(
    mode,
    experience_name = exp_name,
    survey_uuid = survey_uuid,
    cycle_id = cycle_id
  )

  # combine=FALSE so multi-CSV exports come back as a named list of data
  # frames; we wrap each entry below. parse=FALSE makes the pipeline return
  # the saved zip path instead, in which case we just return that.
  data <- glint_run_export_pipeline(
    export_url,
    start_date = start_date,
    end_date = end_date,
    poll_interval = poll_interval,
    max_attempts = max_attempts,
    encoding = encoding,
    experience_name = exp_name,
    combine = FALSE,
    save_zip_to = save_zip_to,
    parse = parse
  )

  if (!isTRUE(parse)) {
    return(invisible(data))
  }

  build_args <- list(
    emp_id_col = emp_id_col,
    first_name_col = first_name_col,
    last_name_col = last_name_col,
    email_col = email_col,
    status_col = status_col,
    completion_date_col = completion_date_col,
    sent_date_col = sent_date_col,
    manager_id_col = manager_id_col,
    file_path = NA_character_
  )

  # Single CSV (typical for cycle mode) -> single glint_survey.
  if (is.data.frame(data)) {
    return(do.call(build_glint_survey, c(list(data = data), build_args)))
  }

  # Multi-CSV (typical for survey and daterange modes) -> named list. Each
  # entry is wrapped as a glint_survey when its schema fits; entries that
  # don't fit (supplementary metadata, attribute files, etc.) come back as
  # raw data.frames with a warning so the caller can still inspect them.
  result <- lapply(names(data), function(nm) {
    df <- data[[nm]]
    tryCatch(
      do.call(build_glint_survey, c(list(data = df), build_args)),
      error = function(e) {
        warning(
          "Could not wrap '", nm, "' as a glint_survey object (",
          conditionMessage(e),
          "). Returning the raw data.frame for this entry.",
          call. = FALSE
        )
        df
      }
    )
  })
  names(result) <- names(data)
  result
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


#' Read an optional Glint env var, returning NULL when unset.
#'
#' Sibling to [glint_env()] for inputs that may legitimately be absent
#' (mode-specific parameters, optional date filters). Returning NULL lets
#' the caller decide whether the absence is fatal.
#'
#' @keywords internal
glint_env_optional <- function(var_name) {
  val <- Sys.getenv(var_name, unset = "")
  if (nzchar(val)) val else NULL
}


#' Infer the export mode from which identifiers are provided.
#'
#' Used as a backward-compatible fallback when the caller doesn't pass an
#' explicit `mode` to [read_glint_survey_api()]. Both IDs present => cycle;
#' survey UUID only => survey; neither => daterange.
#'
#' @keywords internal
infer_mode <- function(survey_uuid, cycle_id) {
  has_survey <- !is.null(survey_uuid) && nzchar(survey_uuid)
  has_cycle  <- !is.null(cycle_id)    && nzchar(cycle_id)
  if (has_survey && has_cycle) {
    return("cycle")
  }
  if (has_survey) {
    return("survey")
  }
  "daterange"
}


#' Construct the Microsoft Graph exportSurveys URL for a given mode.
#'
#' Three URL shapes, all rooted at the package-level `glint_graph_base`:
#' * `cycle`: `.../experiences/{exp}/surveys/{uuid}/surveyCycles/{cid}/exportSurveys`
#' * `survey`: `.../experiences/{exp}/surveys/{uuid}/exportSurveys`
#' * `daterange`: `.../experiences/{exp}/exportSurveys`
#'
#' @keywords internal
build_export_url <- function(mode, experience_name, survey_uuid = NULL, cycle_id = NULL) {
  base <- paste0(
    glint_graph_base, "/",
    utils::URLencode(experience_name, reserved = TRUE)
  )
  switch(mode,
    cycle = paste0(
      base, "/surveys/", survey_uuid,
      "/surveyCycles/", cycle_id, "/exportSurveys"
    ),
    survey = paste0(
      base, "/surveys/", survey_uuid, "/exportSurveys"
    ),
    daterange = paste0(
      base, "/exportSurveys"
    ),
    stop("Unknown export mode: '", mode, "'.", call. = FALSE)
  )
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


#' Extract the export ZIP and load its CSVs.
#'
#' @param resp httr response object containing the ZIP bytes.
#' @param encoding Character encoding to pass to [readr::read_csv()].
#' @param combine Controls multi-CSV handling. When `TRUE` (the default and
#'   backward-compatible behavior), CSVs sharing an identical column schema
#'   are stitched together with [dplyr::bind_rows()]. When `FALSE`, every
#'   multi-CSV export is returned as a named list of data frames keyed by
#'   filename (without `.csv`), regardless of schema match. Single-CSV
#'   exports are always returned as a single data frame.
#' @keywords internal
glint_import_export_zip <- function(resp, encoding = "UTF-8", combine = TRUE) {
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

  if (isTRUE(combine)) {
    first_cols <- names(dfs[[1]])
    same_schema <- all(vapply(dfs, function(df) {
      identical(names(df), first_cols)
    }, logical(1)))

    if (same_schema) {
      return(dplyr::bind_rows(dfs))
    }
  }

  dfs
}


glint_run_export_pipeline <- function(export_url,
                                      start_date = NULL,
                                      end_date = NULL,
                                      poll_interval = 10,
                                      max_attempts = 60,
                                      encoding = "UTF-8",
                                      experience_name,
                                      combine = TRUE,
                                      save_zip_to = NULL,
                                      parse = TRUE) {
  job_id <- glint_start_export(export_url, start_date = start_date, end_date = end_date)
  glint_poll_status(
    job_id,
    experience_name = experience_name,
    poll_interval = poll_interval,
    max_attempts = max_attempts
  )
  resp <- glint_download_export(job_id, experience_name = experience_name)

  # Optionally persist the raw zip alongside in-memory parsing. We do this
  # before the import step so a parse error doesn't lose the bytes.
  saved_path <- NULL
  if (!is.null(save_zip_to) && nzchar(save_zip_to)) {
    saved_path <- persist_export_zip(resp, save_zip_to, job_id)
  }

  # When parse = FALSE, skip the in-memory import entirely and return the
  # path to the saved zip. Callers are expected to have set save_zip_to
  # (read_glint_survey_api enforces that contract up front).
  if (!isTRUE(parse)) {
    return(invisible(saved_path))
  }

  glint_import_export_zip(resp, encoding = encoding, combine = combine)
}


#' Write the raw export zip to disk before in-memory parsing.
#'
#' Used by the export pipeline when the caller passes `save_zip_to`. Path
#' semantics: if `path` ends with a separator or is an existing directory,
#' the zip is written as `glint-export-{job_id}.zip` inside it; otherwise
#' `path` is treated as a full file path. Parent directories are created if
#' missing. Any pre-existing file at the destination is overwritten.
#'
#' @keywords internal
persist_export_zip <- function(resp, path, job_id) {
  ends_with_sep   <- grepl("[/\\\\]$", path)
  is_existing_dir <- dir.exists(path)

  dest <- if (ends_with_sep || is_existing_dir) {
    file.path(sub("[/\\\\]$", "", path),
              paste0("glint-export-", job_id, ".zip"))
  } else {
    path
  }

  parent <- dirname(dest)
  if (nzchar(parent) && !dir.exists(parent)) {
    dir.create(parent, recursive = TRUE, showWarnings = FALSE)
  }

  writeBin(httr::content(resp, "raw"), dest)
  message("Export zip saved to: ", dest)
  invisible(dest)
}
