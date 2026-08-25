Directions

Aight, Vinnie (or someone else, i guess). Looks like we are doing this again.


In the new copied folder, delete these files:
- "output" folder should be empty of PDFs
- "data" folder should be empty
- Delete all log files (e.g., "email_send_log.csv") in main folder.

1) Run 1.0faculty_course_success.sql. Save it as "data/course_data.csv". CHANGE TERMS.
2) Run 1.1dropped_students.sql. Save it as "data/dropped_students.csv". CHANGE TERMS.
0) Leave 2faculty_functions.R alone. DON"T TOUCH IT.
3) Run 3render_all_faculty.R. (Can skip 2faculty_functions.R bc it's referenced in faculty_template.qmd)
     -Change academic year in 'Loop and Render' section.
     -Results are in "output" folder.
     -Check file name under "faculty_ids". The file should be "data/course_data.csv".
     -Check output file names are the correct pattern under "out_name."
     -Optionally render a subset.
4) Run 4sendemail.R to automate sending in batches.
     -Add tenant id.
     -Change academic year.
     -Check the email on send_from(). The file should be "data/course_data.csv"
     -Check email SUBJECT and BODY.
     -Under "pdf_index", make sure the pattern for file names is correct. This  should be the same as Step 3 "out_name".
     -Check "log_file." If new run, then delete log_file, or it'll think it was already sent.
5) After sending, check ie-noreply@fullcoll.edu inbox to see if there are any emails that bounced.
     -Get the list of Banner IDs for those faculty. Change Banner IDs in code.
     -Run 5.troubleshoot_faculty_emails.sql to get list of emails.
     -Save as "data/missed_emails.csv"
6) Run 6replace-emails.R. This is to create a new doc "data/course_data_edited.csv"
7) Go to 7sendmail-subset.R. Feel free to test first.
     -Change academic year.
     -Change Banner IDs.
     -Add tenant id.
     -It should state # of reports available to send, and it should send.
     -Sending is being logged into "email_send_log_TEST.csv"

NOTE: clientID and clientSecret needs to be updated every year. Reach out to ACT for that.
Keep it secret, keep it safe.
