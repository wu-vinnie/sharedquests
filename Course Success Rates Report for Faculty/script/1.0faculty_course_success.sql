-- Faculty Course Success Report
-- Note: This query is designed to extract faculty course success metrics
-- Edit the term_code filter in the WHERE clause as needed (bottom)
-- Save file as course_data.csv

--this is the new query

WITH faculty AS (
  SELECT
    a.sirasgn_term_code            AS term_code,
    a.sirasgn_crn                  AS crn,
    a.sirasgn_pidm                 AS faculty_pidm,
    i.spriden_id                   AS faculty_id,
    i.spriden_last_name,
    i.spriden_first_name,
    fz_get_best_email(a.sirasgn_pidm) AS faculty_email
  FROM sirasgn a
  JOIN spriden i
    ON  i.spriden_pidm       = a.sirasgn_pidm
    AND i.spriden_change_ind IS NULL
)

SELECT
  f.faculty_id,
  f.spriden_last_name || ', ' || f.spriden_first_name AS faculty_name,
  f.faculty_email,
  f.crn,
  f.term_code,

  i.spriden_id                                        AS stud_id,
  shrtckn_pidm                                        AS stud_pidm,
  shrtckn_camp_code                                   AS college_code,
  shrtckn_subj_code || ' ' || shrtckn_crse_numb       AS course,
  shrtckn_crse_title                                  AS course_title,

  -- COURSE OUTCOMES
  shrtckg_grde_code_final                             AS final_grade,
  CASE
    WHEN shrtckg_grde_code_final NOT IN ('W', 'EW') THEN 1
    ELSE 0
  END AS retained,
  CASE
    WHEN shrtckg_grde_code_final IN ('A', 'B', 'C', 'CR', 'P', 'IP', 'INB', 'INC') THEN 1
    ELSE 0
  END AS success,

  -- ETHNICITY
  sv_mis_sb_local.f_get_ethnicity(shrtckn_pidm, 'SB_LOCAL',
    s.spbpers_ethn_cde)                               AS mis_ethnicity_code,
  
  fz_get_student_ipeds_ethnicity(shrtckn_pidm)        AS multi_ethnicity_group,

  CASE 
        WHEN (fz_get_student_ipeds_ethnicity(c.pidm)) = 'B' THEN 'Y'                                                                     
        WHEN (SUBSTR(fz_get_mis_multi_ethnicity(c.pidm), 15,1) )= 'Y' THEN 'Y'
        ELSE 'N'
    END AS "Black_Expanded",


  DECODE(fz_get_student_ipeds_ethnicity(shrtckn_pidm),
    'N', 'American Indian or Alaska Native',
    'A', 'Asian',
    'F', 'Filipino',
    'B', 'Black or African American',
    'H', 'Hispanic/Latino',
    'P', 'Native Hawaiian or Other Pacific Islander',
    'W', 'White',
    'T', 'Two or more races',
    'X', 'Race/ethnicity unknown')                    AS ipeds_ethnicity,

  NVL(CASE WHEN race_subquery.North_African_Middle_Eastern = 'Y' THEN 1 END, 0) AS ethn_mena,
  NVL(CASE WHEN sv_mis_sb_local.f_get_ethnicity(shrtckn_pidm, 'SB_LOCAL', s.spbpers_ethn_cde) = 'NNNNNNNNNNNNNNNNNNNNN'
            OR (SUBSTR(sv_mis_sb_local.f_get_ethnicity(shrtckn_pidm, 'SB_LOCAL', s.spbpers_ethn_cde), 1, 1) = 'X'
                AND SUBSTR(sv_mis_sb_local.f_get_ethnicity(shrtckn_pidm, 'SB_LOCAL', s.spbpers_ethn_cde), 2, 20) = 'NNNNNNNNNNNNNNNNNNNN')
       THEN 1 END, 0)                                 AS ethn_unknown,

  -- GENDER IDENTITY
  CASE WHEN s.spbpers_gndr_code LIKE '%B%' THEN '1' ELSE '0' END AS gender_nb,
  CASE WHEN s.spbpers_gndr_code LIKE '%1%' THEN '1' ELSE '0' END AS gender_trans,
  CASE WHEN s.spbpers_gndr_code IN ('0', '2') AND s.spbpers_sex = 'M' THEN '1'
       WHEN s.spbpers_gndr_code IS NULL        AND s.spbpers_sex = 'M' THEN '1'
       ELSE '0' END                                   AS gender_m,
  CASE WHEN s.spbpers_gndr_code IN ('0', '2') AND s.spbpers_sex = 'F' THEN '1'
       WHEN s.spbpers_gndr_code IS NULL        AND s.spbpers_sex = 'F' THEN '1'
       ELSE '0' END                                   AS gender_f,
  CASE WHEN s.spbpers_gndr_code IS NULL AND s.spbpers_sex IN ('X', 'N') THEN '1'
       WHEN s.spbpers_gndr_code IS NULL AND s.spbpers_sex IS NULL       THEN '1'
       WHEN s.spbpers_gndr_code = '0'   AND s.spbpers_sex = 'N'        THEN '1'
       WHEN s.spbpers_gndr_code = '0'   AND s.spbpers_sex IS NULL      THEN '1'
       ELSE '0' END                                   AS gender_unknown,

  -- FIRST GENERATION STATUS
  (
    SELECT
      CASE
        WHEN g1 BETWEEN '1' AND '7' AND g2 BETWEEN '1' AND '7' THEN
          CASE WHEN GREATEST(g1, g2) < '4' THEN 1 ELSE 0 END
        WHEN g1 BETWEEN '1' AND '7' THEN
          CASE WHEN g1 < '4' THEN 1 ELSE 0 END
        WHEN g2 BETWEEN '1' AND '7' THEN
          CASE WHEN g2 < '4' THEN 1 ELSE 0 END
        WHEN g2 = 'Y' THEN 1
        ELSE NULL
      END AS first_gen_calc
    FROM (
      SELECT
        svbsgpd_guard_1_gedl_code AS g1,
        svbsgpd_guard_2_gedl_code AS g2,
        ROW_NUMBER() OVER (
          PARTITION BY svbsgpd_pidm
          ORDER BY svbsgpd_activity_date DESC
        ) AS rn
      FROM svbsgpd
      WHERE svbsgpd_pidm = shrtckn_pidm
    )
    WHERE rn = 1
  ) AS first_gen

FROM shrtckn

JOIN shrtckg
  ON  shrtckg.rowid = f_get_shrtckg_rowid(shrtckn_pidm, shrtckn_term_code, shrtckn_seq_no, 1)

JOIN spriden i
  ON  i.spriden_pidm       = shrtckn_pidm
  AND i.spriden_change_ind IS NULL

LEFT JOIN faculty f
  ON  f.term_code = shrtckn_term_code
  AND f.crn       = shrtckn_crn

LEFT JOIN spbpers s
  ON  s.spbpers_pidm = shrtckn_pidm

LEFT JOIN (
  SELECT
    g.gorprac_pidm,
    MAX(CASE WHEN e.gorrace_rrac_code = 'M' THEN 'Y' ELSE 'N' END) AS North_African_Middle_Eastern
  FROM gorprac g
  LEFT JOIN gorrace e ON g.gorprac_race_cde = e.gorrace_race_cde
  GROUP BY g.gorprac_pidm
) race_subquery
  ON race_subquery.gorprac_pidm = shrtckn_pidm

WHERE SUBSTR(shrtckn_camp_code, 1, 1) = '2'
------------------------------------
--EDIT TERM CODES HERE-------------
  AND shrtckn_term_code IN ('202530', '202610', '202620')
  AND shrtckg_grde_code_final IS NOT NULL   -- made it to census
  AND shrtckg_grde_code_final <> 'UD';