# --------------------------------------------------
# fix_faculty_emails.R
# Apply corrected emails from GOREMAL lookup to
# course_data.csv, saving the result separately as
# course_data_edited.csv (source file is left untouched)
# --------------------------------------------------
#
# For each faculty that was missed, take the most recent ACTIVE row and
# use it to overwrite FACULTY_EMAIL in course_data.csv.


library(readr)
library(dplyr)
library(stringr)
library(lubridate)


# --------------------------------------------------
# READ SOURCE DATA
# --------------------------------------------------

course_data <- read_csv(
  "data/course_data.csv",
  show_col_types = FALSE
)


# --------------------------------------------------
# BUILD CORRECTED EMAIL LOOKUP
# --------------------------------------------------

active_emails <- read_csv(
  "data/missed_emails.csv",
  show_col_types = FALSE
) |>
  filter(
    GOREMAL_STATUS_IND == "A"
  ) |>
  mutate(
    activity_dt = parse_date_time(
      GOREMAL_ACTIVITY_DATE,
      orders = c("mdY HMS", "Ymd HMS", "dby", "Ymd")
    )
  )


# Drop rows that match the bounced address (FN_RESULT) so the next
# step falls through to the next most recent active email instead
# of "correcting" to the same broken address.
alternate_emails <- active_emails |>
  filter(
    GOREMAL_EMAIL_ADDRESS != FN_RESULT
  )


# IDs where every active GOREMAL row equals the bounced email --
# there's no alternate to fall back to, so no fix is possible.
no_alternate_ids <- setdiff(
  unique(active_emails$SPRIDEN_ID),
  unique(alternate_emails$SPRIDEN_ID)
)


if (length(no_alternate_ids) > 0) {
  message(
    glue::glue(
      "WARNING: {length(no_alternate_ids)} ID(s) have no active email different from the bounced one: {paste(no_alternate_ids, collapse = ', ')}"
    )
  )
} else {
  message("No warnings: every bounced ID has an alternate active email.")
}


missed_emails <- alternate_emails |>
  slice_max(
    activity_dt,
    n = 1,
    by = SPRIDEN_ID,
    with_ties = FALSE
  ) |>
  transmute(
    FACULTY_ID = str_pad(
      str_remove(as.character(SPRIDEN_ID), "^@"),
      width = 8,
      pad = "0"
    ),
    corrected_email = GOREMAL_EMAIL_ADDRESS,
    fn_result = FN_RESULT
  )


missed_emails

# --------------------------------------------------
# APPLY CORRECTIONS
# --------------------------------------------------
#
# course_data.csv's FACULTY_ID may have a leading "@"
# (stripped here only for matching, not saved back).

course_data_joined <- course_data |>
  mutate(
    join_id = str_pad(
      str_remove(FACULTY_ID, "^@"),
      width = 8,
      pad = "0"
    )
  ) |>
  left_join(
    missed_emails,
    by = c("join_id" = "FACULTY_ID")
  ) |>
  mutate(
    FACULTY_EMAIL = coalesce(
      corrected_email,
      FACULTY_EMAIL
    )
  )


# --------------------------------------------------
# SUMMARY
# --------------------------------------------------

n_applied <- sum(!is.na(course_data_joined$corrected_email))

message(
  glue::glue(
    "{nrow(missed_emails)} corrected email(s) available; applied to {n_applied} row(s) in course_data.csv."
  )
)

course_data_edited <- course_data_joined |>
  select(
    -join_id,
    -corrected_email,
    -fn_result
  )


# --------------------------------------------------
# WRITE EDITED FILE
# --------------------------------------------------

write_csv(
  course_data_edited,
  "data/course_data_edited.csv"
)

message("Saved: data/course_data_edited.csv")
