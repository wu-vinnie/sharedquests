# --------------------------------------------------
# 4sendemail.R
# Automated Email via Microsoft Graph
# --------------------------------------------------

# --------------------------------------------------
# ANNUAL UPDATES
# --------------------------------------------------
#
# If running for a different academic year:
#
# 1. Change academic_year below
# 2. Review/edit email content
#    (e.g., Flex Day sessions, links, contacts)
#
# --------------------------------------------------

library(readr)
library(dplyr)
library(stringr)
library(tibble)
library(glue)
library(purrr)


# --------------------------------------------------
# SETTINGS
# --------------------------------------------------

academic_year <- "2026-27"

test_mode <- FALSE

test_email <- "vwu@fullcoll.edu"

send_from <- "ie-noreply@fullcoll.edu"

# Set to TRUE to restrict sending to only the faculty IDs
# listed in subset_faculty_ids below (e.g. for a re-send
# to a handful of people). Leave FALSE to send to everyone
# with a valid PDF.
subset_mode <- TRUE
# NEVER CHANGE THIS TO FALSE

# IDs must match the zero-padded 8-digit format used by
# faculty_id_clean below (e.g. "00012345") -- no "@" prefix.
subset_faculty_ids <- c(
  # STATE EXACT BANNER IDS HERE
  # "01086781",
  # "01849471",
  # "01786837",
  # "00276568",
  # "02049640",
  # "01333333",
  # "02099230"
)

email_subject <- glue(
  "{academic_year} Course Success Report"
)

delay_seconds <- 2


# --------------------------------------------------
# MICROSOFT GRAPH SETTINGS
# --------------------------------------------------

######
# MUST ENTER TENANT ID FOR YOUR INSTITUTION
##########

tenant_id <- "[TENANT ID HERE]"

# Separate test and production logs so test sends
# are never treated as actual faculty sends.

log_file <- if (test_mode) {
  "email_send_log_TEST.csv"
} else {
  "email_send_log_edited.csv"
}


# --------------------------------------------------
# READ FACULTY CONTACTS
# --------------------------------------------------

faculty_contact <- read_csv(
  "data/course_data_edited.csv",
  show_col_types = FALSE
) |>
  select(
    faculty_id = FACULTY_ID,
    faculty_name = FACULTY_NAME,
    faculty_email = FACULTY_EMAIL
  ) |>
  filter(
    !is.na(faculty_id)
  ) |>
  mutate(
    faculty_id_clean = str_remove(
      faculty_id,
      "^@"
    ),
    faculty_id_clean = str_pad(
      as.character(faculty_id_clean),
      width = 8,
      pad = "0"
    )
  ) |>
  distinct()


# --------------------------------------------------
# EMAIL BODY
# --------------------------------------------------

email_body <- function() {
  glue(
    "Hello,<br><br>

<strong>Please do not reply to this email. This is an automated report.</strong><br><br>

Attached is your individualized Course Success report for the {academic_year} academic year, developed in response to a recommendation from the Professional Learning Committee and supported by Faculty Senate.<br><br>

This report is provided only to you as a tool for professional learning and is not intended for evaluation. It includes your course success rates, disaggregated data, and guiding questions to support reflection.<br><br>

<a href='https://youtu.be/Xm0HtCOhqgs'>A video</a> is available to walk you through the report and help you interpret the information presented.
Additional course success data will also be made available to you in a dashboard at a later date so you can explore the data in greater detail.<br><br>

If you have questions about the purpose or use of the report, please contact Faculty Senate President Bridget Kominek at <a href='mailto:bkominek@fullcoll.edu'>bkominek@fullcoll.edu</a>.<br><br>

If you experience technical or data-related issues with your report (e.g., missing data, formatting problems), please contact the Office of Institutional Effectiveness at <a href='mailto:ie@fullcoll.edu'>ie@fullcoll.edu</a>.<br><br>

Thank you,<br><br>

Office of Institutional Effectiveness<br>
Fullerton College"
  )
}


# --------------------------------------------------
# FIND PDF FILES
# --------------------------------------------------

pdf_pattern <- glue(
  "Course_Success_{academic_year}_.*\\.pdf"
)

pdf_index <- list.files(
  path = ".",
  pattern = pdf_pattern,
  recursive = TRUE,
  full.names = TRUE
) |>
  tibble(
    pdf_path = _
  ) |>
  mutate(
    faculty_id_clean = str_extract(
      pdf_path,
      "\\d{8}(?=\\.pdf)"
    )
  )


# --------------------------------------------------
# MATCH CONTACTS TO PDFS
# --------------------------------------------------

faculty_contact_checked <- faculty_contact |>
  left_join(
    pdf_index,
    by = "faculty_id_clean"
  ) |>
  mutate(
    pdf_exists = !is.na(pdf_path)
  )


# --------------------------------------------------
# CHECK PDF MATCHES
# --------------------------------------------------

print(
  count(
    faculty_contact_checked,
    pdf_exists
  )
)


# --------------------------------------------------
# FILTER TO FACULTY WITH VALID PDFS
# --------------------------------------------------

faculty_to_send <- faculty_contact_checked |>
  filter(
    pdf_exists
  )


# --------------------------------------------------
# OPTIONAL: RESTRICT TO SUBSET OF FACULTY_ID
# --------------------------------------------------

if (subset_mode) {
  faculty_to_send <- faculty_to_send |>
    filter(
      faculty_id_clean %in% subset_faculty_ids
    )

  message(
    glue(
      "Subset mode ON: restricting to {length(subset_faculty_ids)} faculty ID(s)."
    )
  )
}


# --------------------------------------------------
# READ SEND LOG
# --------------------------------------------------

if (file.exists(log_file)) {
  send_log <- read_csv(
    log_file,
    show_col_types = FALSE
  )
} else {
  send_log <- tibble(
    faculty_id = character(),
    faculty_email = character(),
    sent_at = character()
  )
}


# --------------------------------------------------
# REMOVE ALREADY-SENT EMAILS
# --------------------------------------------------

faculty_to_send <- faculty_to_send |>
  anti_join(
    send_log,
    by = c(
      "faculty_id",
      "faculty_email"
    )
  )


# --------------------------------------------------
# DETERMINE EMAILS TO PROCESS
# --------------------------------------------------

faculty_to_process <- if (test_mode) {
  faculty_to_send |>
    slice(1:5)
} else {
  faculty_to_send
}


# --------------------------------------------------
# PRE-SEND SUMMARY
# --------------------------------------------------

message(
  glue(
    "Mode: {ifelse(test_mode, 'TEST', 'PRODUCTION')}"
  )
)

message(
  glue(
    "Reports available to send: {nrow(faculty_to_send)}"
  )
)

message(
  glue(
    "Reports being processed: {nrow(faculty_to_process)}"
  )
)

if (test_mode) {
  message(
    glue(
      "All test emails will be sent to: {test_email}"
    )
  )
}


# --------------------------------------------------
# MICROSOFT GRAPH CREDENTIALS
# --------------------------------------------------
#
# Credentials are entered once per run.
# They are not stored in this R script.
#
# IMPORTANT:
# Use the WEB APPLICATION client ID and client secret,
# NOT the service account username/password.
# --------------------------------------------------

client_id <- readline(
  prompt = "Enter Microsoft Graph client ID: "
)

client_secret <- readline(
  prompt = "Enter Microsoft Graph client secret: "
)


# --------------------------------------------------
# SEND EMAILS
# --------------------------------------------------

pwalk(
  faculty_to_process,
  function(
    faculty_name,
    faculty_email,
    faculty_id,
    faculty_id_clean,
    pdf_path,
    pdf_exists
  ) {
    # ----------------------------------------------
    # DETERMINE RECIPIENT
    # ----------------------------------------------

    recipient <- ifelse(
      test_mode,
      test_email,
      faculty_email
    )

    # ----------------------------------------------
    # NORMALIZE ATTACHMENT PATH
    # ----------------------------------------------

    pdf_path_clean <- normalizePath(
      pdf_path,
      winslash = "/",
      mustWork = TRUE
    )

    # ----------------------------------------------
    # MICROSOFT GRAPH EMAIL
    # ----------------------------------------------

    ps_script <- glue(
      '
$ErrorActionPreference = "Stop"

$tenantId = "{tenant_id}"
$clientId = "{client_id}"
$clientSecret = "{client_secret}"

$sender = "{send_from}"
$recipient = "{recipient}"
$subject = "{email_subject}"

$body = @"
{email_body()}
"@


# -----------------------------------------------
# GET MICROSOFT GRAPH ACCESS TOKEN
# -----------------------------------------------

$tokenBody = @{{
    client_id     = $clientId
    client_secret = $clientSecret
    scope         = "https://graph.microsoft.com/.default"
    grant_type    = "client_credentials"
}}

$tokenResponse = Invoke-RestMethod `
    -Method Post `
    -Uri "https://login.microsoftonline.com/$tenantId/oauth2/v2.0/token" `
    -ContentType "application/x-www-form-urlencoded" `
    -Body $tokenBody

$accessToken = $tokenResponse.access_token


# -----------------------------------------------
# PREPARE PDF ATTACHMENT
# -----------------------------------------------

$attachmentBytes = [System.IO.File]::ReadAllBytes(
    "{pdf_path_clean}"
)

$attachmentBase64 = [System.Convert]::ToBase64String(
    $attachmentBytes
)

$attachmentName = [System.IO.Path]::GetFileName(
    "{pdf_path_clean}"
)


# -----------------------------------------------
# BUILD GRAPH MESSAGE
# -----------------------------------------------

$message = @{{
    message = @{{
        subject = $subject

        body = @{{
            contentType = "HTML"
            content = $body
        }}

        toRecipients = @(
            @{{
                emailAddress = @{{
                    address = $recipient
                }}
            }}
        )

        attachments = @(
            @{{
                "@odata.type" = "#microsoft.graph.fileAttachment"
                name = $attachmentName
                contentType = "application/pdf"
                contentBytes = $attachmentBase64
            }}
        )
    }}

    saveToSentItems = $true
}}


# -----------------------------------------------
# CONVERT MESSAGE TO JSON
# -----------------------------------------------

$jsonBody = $message |
    ConvertTo-Json -Depth 10


# -----------------------------------------------
# SEND THROUGH MICROSOFT GRAPH
# -----------------------------------------------

$headers = @{{
    Authorization = "Bearer $accessToken"
}}

$encodedSender = [System.Uri]::EscapeDataString(
    $sender
)

$graphUri = "https://graph.microsoft.com/v1.0/users/$encodedSender/sendMail"

Invoke-RestMethod `
    -Method Post `
    -Uri $graphUri `
    -Headers $headers `
    -ContentType "application/json" `
    -Body $jsonBody

Write-Output "GRAPH_SEND_SUCCESS"
'
    )

    # ----------------------------------------------
    # WRITE TEMPORARY POWERSHELL SCRIPT
    # ----------------------------------------------

    temp_ps1 <- tempfile(
      fileext = ".ps1"
    )

    writeLines(
      ps_script,
      temp_ps1
    )

    # ----------------------------------------------
    # RUN POWERSHELL
    # ----------------------------------------------

    result <- system2(
      "powershell.exe",
      args = c(
        "-ExecutionPolicy",
        "Bypass",
        "-File",
        shQuote(temp_ps1)
      ),
      stdout = TRUE,
      stderr = TRUE
    )

    # ----------------------------------------------
    # CHECK RESULT
    # ----------------------------------------------

    status <- attr(
      result,
      "status"
    )

    success <- (is.null(status) ||
      identical(status, 0L)) &&
      any(
        str_detect(
          result,
          fixed("GRAPH_SEND_SUCCESS")
        )
      )

    # ----------------------------------------------
    # SUCCESS
    # ----------------------------------------------

    if (success) {
      write.table(
        tibble(
          faculty_id = faculty_id,
          faculty_email = faculty_email,
          sent_at = format(
            Sys.time(),
            "%Y-%m-%d %H:%M:%S"
          )
        ),
        file = log_file,
        sep = ",",
        row.names = FALSE,
        col.names = !file.exists(log_file),
        append = TRUE
      )

      message(
        glue(
          "Sent successfully: {recipient}"
        )
      )

      # ----------------------------------------------
      # FAILURE
      # ----------------------------------------------
    } else {
      message(
        glue(
          "FAILED: {recipient}"
        )
      )

      message(
        paste(
          result,
          collapse = "\n"
        )
      )
    }

    # ----------------------------------------------
    # CLEAN UP TEMP FILE
    # ----------------------------------------------

    unlink(
      temp_ps1
    )

    # ----------------------------------------------
    # DELAY BETWEEN EMAILS
    # ----------------------------------------------

    Sys.sleep(
      delay_seconds
    )
  }
)


# --------------------------------------------------
# REMOVE CREDENTIALS FROM R SESSION
# --------------------------------------------------

rm(
  client_id,
  client_secret
)


# --------------------------------------------------
# DONE
# --------------------------------------------------

message(
  glue(
    "Email processing complete. Mode: {ifelse(test_mode, 'TEST', 'PRODUCTION')}."
  )
)
