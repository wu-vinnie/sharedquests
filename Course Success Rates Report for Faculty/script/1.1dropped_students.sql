--Combined: Dropped Students, included if have drop reasons
-- Save file as dropped_students.csv

WITH 

json_data AS (
  SELECT
    gzrjsnr_pidm,
    gzrjsnr_term_code,
    gzrjsnr_key,
    gzrjsnr_activity_date,
    utl_raw.cast_to_varchar2(dbms_lob.substr(gzrjsnr_json)) AS question_text
  FROM GZRJSNR
  WHERE gzrjsnr_code = 'drop'

  ------------------------------------
  --EDIT TERM CODES HERE (in multiple places below)
  ------------------------------------
    AND gzrjsnr_term_code IN (
      --2023-24
   --  '202230', '202310', '202320', 
      --2024-25
 --    '202330', '202410', '202420',
      --2025-26
   --  '202430','202510', '202520'
   --2026-27
      '202530', '202610', '202620'
    )
),

faculty AS (
  SELECT
    a.sirasgn_term_code   AS term_code,
    a.sirasgn_crn         AS crn,
    a.sirasgn_pidm        AS faculty_pidm,
    i.spriden_id          AS faculty_id,
    i.spriden_last_name,
    i.spriden_first_name,
    m.goremal_email_address AS faculty_email
  FROM sirasgn a
  JOIN spriden i
    ON i.spriden_pidm = a.sirasgn_pidm
   AND i.spriden_change_ind IS NULL
  LEFT JOIN (
    SELECT 
      goremal_pidm,
      goremal_email_address,
      ROW_NUMBER() OVER (
        PARTITION BY goremal_pidm 
        ORDER BY 
          CASE WHEN goremal_preferred_ind = 'Y' THEN 1 ELSE 2 END,
          CASE WHEN goremal_status_ind = 'A' THEN 1 ELSE 2 END,
          goremal_activity_date DESC
      ) AS rn
    FROM goremal
    WHERE goremal_status_ind = 'A'
  ) m ON m.goremal_pidm = a.sirasgn_pidm AND m.rn = 1
),

-- ===================================================================
-- REPLACEMENT FOR SV_STUDENT_CURRENT_COURSES

-- ===================================================================
student_courses AS (
  SELECT
    sp.spriden_id                               AS id,
    r.sfrstcr_pidm                              AS pidm,
    sp.spriden_first_name                       AS name_first,
    sp.spriden_last_name                        AS name_last_30,
    r.sfrstcr_term_code                         AS term_code,
    r.sfrstcr_crn                               AS crn,
    r.sfrstcr_ptrm_code                         AS part_of_term,
    s.ssbsect_camp_code                         AS college_code,
    s.ssbsect_subj_code                         AS subject_code,
    s.ssbsect_crse_numb                         AS course_number,
    NVL(s.ssbsect_crse_title, crs.scbcrse_title) AS course_title,
    r.sfrstcr_grde_code                         AS course_grade,
    s.ssbsect_ptrm_start_date                   AS course_start_date
  FROM sfrstcr r
  JOIN stvrsts rs
    ON rs.stvrsts_code = r.sfrstcr_rsts_code
  JOIN ssbsect s
    ON s.ssbsect_term_code = r.sfrstcr_term_code
   AND s.ssbsect_crn       = r.sfrstcr_crn
  JOIN scbcrse crs
    ON crs.scbcrse_subj_code = s.ssbsect_subj_code
   AND crs.scbcrse_crse_numb = s.ssbsect_crse_numb
   AND crs.scbcrse_eff_term  = (
         SELECT MAX(c2.scbcrse_eff_term)
         FROM scbcrse c2
         WHERE c2.scbcrse_subj_code = s.ssbsect_subj_code
           AND c2.scbcrse_crse_numb = s.ssbsect_crse_numb
           AND c2.scbcrse_eff_term <= s.ssbsect_term_code
       )
  JOIN spriden sp
    ON sp.spriden_pidm = r.sfrstcr_pidm
   AND sp.spriden_change_ind IS NULL
  WHERE r.sfrstcr_term_code IN ('202530', '202610', '202620')
)

SELECT 
f.faculty_id,
f.spriden_last_name || ', ' || f.spriden_first_name AS faculty_name,
f.faculty_email,
c.term_code,
c.id as STUD_ID, 
c.pidm as STUD_PIDM, 
c.NAME_FIRST as STUD_FIRST_NAME,
c.NAME_LAST_30 as STUD_LAST_NAME,
c.college_code, 
c.part_of_term,
c.crn, 
c.subject_code ||' '|| c.course_number as COURSE,  
c.course_title,

-- COURSE OUTCOMES
c.course_grade, 
CASE 
  WHEN c.course_grade NOT IN ('W', 'EW') THEN 1 
  ELSE 0 
END AS retained,

-- Approved in 2016 by Faculty Senate but implemented in 2020
-- Drop Reason Categories (will be 0 if no survey response)
CASE WHEN j.question_text LIKE '%work_schedule%' THEN 1 ELSE 0 END AS work,
CASE WHEN j.question_text LIKE '%financial%' THEN 1 ELSE 0 END AS financial,
CASE WHEN j.question_text LIKE '%costs%' THEN 1 ELSE 0 END AS costs,
CASE WHEN j.question_text LIKE '%not_enough_financial_aid%' THEN 1 ELSE 0 END AS not_enough_financial_aid,
CASE WHEN j.question_text LIKE '%difficult_to_manage_coursework%' THEN 1 ELSE 0 END AS difficult_to_manage_coursework,
CASE WHEN j.question_text LIKE '%moving_away%' THEN 1 ELSE 0 END AS moving_away,
CASE WHEN j.question_text LIKE '%adequate_childcare%' THEN 1 ELSE 0 END AS adequate_childcare,
CASE WHEN j.question_text LIKE '%transportation%' THEN 1 ELSE 0 END AS transportation,
CASE WHEN j.question_text LIKE '%unforeseen_emergency%' THEN 1 ELSE 0 END AS unforeseen_emergency,
CASE WHEN j.question_text LIKE '%low_grade%' THEN 1 ELSE 0 END AS low_grade,
CASE WHEN j.question_text LIKE '%more_academic_support%' THEN 1 ELSE 0 END AS more_academic_support,
CASE WHEN j.question_text LIKE '%class_is_not_required%' THEN 1 ELSE 0 END AS class_is_not_required,
CASE WHEN j.question_text LIKE '%not_a_good_fit%' THEN 1 ELSE 0 END AS not_a_good_fit,

-- Free Text Comments (will be NULL if no survey response)
CASE 
  WHEN j.question_text LIKE '%share_why%'
  THEN JSON_VALUE(j.question_text, '$.share_why')
  ELSE NULL
END AS share_why,

CASE 
  WHEN j.question_text LIKE '%what_help%'
  THEN JSON_VALUE(j.question_text, '$.what_help') 
  ELSE NULL
END AS what_help,

j.gzrjsnr_activity_date AS survey_date,

CASE
  WHEN w.registration_code LIKE 'R%' THEN NULL
  ELSE w.registration_status_date
END AS "course_dropped_date",

c.course_start_date

-- START FROM DROPPED STUDENTS
FROM student_courses c

-- Add drop survey data (will be NULL if student didn't complete survey)
LEFT JOIN json_data j
  ON j.gzrjsnr_pidm = c.pidm
 AND j.gzrjsnr_term_code = c.term_code
 AND j.gzrjsnr_key = c.crn

LEFT JOIN faculty f
  ON f.term_code = c.term_code
 AND f.crn = c.crn

LEFT JOIN spbpers s
  ON s.spbpers_pidm = c.pidm

LEFT JOIN GTVDICD t
  ON t.gtvdicd_coll_code = c.college_code

LEFT JOIN (
  SELECT 
    term_code,
    crn,
    pidm,
    course_number,
    registration_code,
    registration_status_date,
    ROW_NUMBER() OVER (
      PARTITION BY term_code, crn, pidm 
      ORDER BY registration_status_date DESC
    ) AS rn
  FROM SZWSCE2
) w
  ON w.term_code = c.term_code
 AND w.crn = c.crn
 AND w.pidm = c.pidm
 AND w.rn = 1

-- FILTERS
WHERE SUBSTR(c.college_code, 1, 1) = '2'

------------------------------------
--EDIT TERM CODES HERE 
------------------------------------
  AND c.term_code IN ('202530', '202610', '202620')
  AND w.course_number NOT IN ('N01 F')
  AND w.registration_code NOT IN ('CN')
  AND (
    c.course_grade IS NULL
    OR c.course_grade IN ('W', 'EW')
  )
  AND (c.course_grade IS NULL OR c.course_grade <> 'UD')
  AND f.faculty_id IS NOT NULL
  
  AND ( --drop date is after start date
    CASE
      WHEN w.registration_code LIKE 'R%' THEN NULL
      ELSE w.registration_status_date
    END
  ) >= c.course_start_date;