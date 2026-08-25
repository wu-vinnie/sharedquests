-- Diagnostic: compare fz_get_best_email() output against raw GOREMAL rows
-- Fill in the faculty/staff ID(s) that produced wrong/stale emails


-- add in the banner IDs below
-- this is for Ffaculty/staff whose emails got returned
-- Go off based on "active" goremal_status_ind = 'A' rows, and then check goremal_activity_date

-- Diagnostic: compare fz_get_best_email() output against raw GOREMAL rows
-- Fill in the faculty/staff ID(s) that produced wrong/stale emails

SELECT
  sp.spriden_id,
  e.goremal_pidm,
  fz_get_best_email(e.goremal_pidm) AS fn_result,
  e.goremal_emal_code,
  e.goremal_email_address,
  e.goremal_preferred_ind,
  e.goremal_status_ind,
  e.goremal_activity_date
FROM goremal e
JOIN spriden sp
  ON sp.spriden_pidm = e.goremal_pidm
  AND sp.spriden_change_ind IS NULL
WHERE sp.spriden_id IN (

-- MANUALLY LIST OUT BANNER IDS W/ @ SYMBOL, E.G.,
-- '@01234567',
-- '@99999999'

)
ORDER BY sp.spriden_id, e.goremal_status_ind, e.goremal_activity_date DESC;


