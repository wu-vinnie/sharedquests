select 
        locale_name as locale_name
        , metric_id as metric_id
        , data_element_name as metric_description
        , academic_year_string as academic_year_string 
        , primary_disagg as primary_disagg
        , primary_subgroup as primary_disagg_subgroup
        , secondary_subgroup as gender_disagg_subgroup

        , case 
        when overall_secondary = 'Y'
            then case
                when "DI Overall to Primary-Overall" = 1 then 'Y'
                when "DI Overall to Primary-Overall" = 0 then 'N'
                end 
        when overall_primary = 'N' and overall_secondary = 'N'
            then case
                when "DI Overall to Primary-Overall" = 1 then 'PY'
                when "DI Overall to Primary-Overall" = 0 then 'PN'
                end 
        else 'N/A'
        end as primary_DI_observed_y

        , ceil(
        case 
        when overall_secondary = 'Y'
            and "DI Overall to Primary-Overall" = 1
        then "Full Equity Overall to Primary-Overall"
        end
        )
        as primary_full_equity_number

        , ceil(
        case 
        when overall_secondary = 'Y'
            and "DI Overall to Primary-Overall" = 1
        then "Min Equity Overall to Primary-Overall"
        end
        )
        as primary_eliminate_substantive_DI_number

        , case 
        when overall_secondary = 'N'
            and "DI Overall to Primary-Overall" = 1
            then case
                when "DI Primary-Overall to Primary-Sub" = 1 then 'Y'
                when "DI Primary-Overall to Primary-Sub" = 0 then 'N'
                end 
        when overall_secondary ='N'
            and "DI Overall to Primary-Overall" = 0
            then case
                when "DI Overall to Primary-Sub" = 1 then 'Y'
                when "DI Overall to Primary-Sub" = 0 then 'N'
                end 
        else 'N/A'
        end as gender_intersectional_DI_observed_y

        , ceil(
        case 
        when overall_secondary = 'N'
            and "DI Overall to Primary-Overall" = 1
            and "DI Primary-Overall to Primary-Sub" = 1
            and "DI Overall to Primary-Sub" = 1
        then "Full Equity Overall Ex Primary-Overall to Primary-Sub"

        when overall_secondary = 'N'
            and "DI Overall to Primary-Overall" = 0
            and "DI Overall to Primary-Sub" = 1
        then "Full Equity Overall to Primary-Sub"

        end 
        )
        as gender_intersectional_full_equity_number              

        , ceil(
        case 
        when overall_secondary = 'N'
            and "DI Overall to Primary-Overall" = 1
            and "DI Primary-Overall to Primary-Sub" = 1
            and "DI Overall to Primary-Sub" = 1
        then "Min Equity Overall Ex Primary-Overall to Primary-Sub"

        when overall_secondary = 'N'
            and "DI Overall to Primary-Overall" = 0
            and "DI Overall to Primary-Sub" = 1
        then "Min Equity Overall to Primary-Sub"

        end 
        )
        as gender_eliminate_substantive_DI_number 

        , r_value as subgroup_value
        , r_denom as subgroup_denom
        , round(r_perc, 3) as subgroup_outcome_rate

        , round(MOE, 3) as MoE

        , round(
        case 
        when overall_secondary = 'Y'
            then "PPG-1 Diff Overall to Primary-Overall"
        end 
        , 3)
        as primary_ppg   

        , round(
        case 
        when overall_secondary = 'Y'
            then "PPG-1 Diff Overall to Primary-Overall" + MoE
        end 
        , 3)
        as primary_ppg_eliminate_substantive_DI 

        , round(
        case 
        when overall_secondary = 'N'
            and "DI Overall to Primary-Overall" = 0
        then "PPG-1 Diff Overall to Primary-Sub"
        end
        , 3)
        as gender_ppg_pn

        , round(
        case 
        when overall_secondary = 'N'
            and "DI Overall to Primary-Overall" = 0
        then "PPG-1 Diff Overall to Primary-Sub" + MoE
        end
        , 3)
        as gender_ppg_eliminate_substantive_DI_pn

        , round(
        case 
        when overall_secondary = 'N'
            and "DI Overall to Primary-Overall" = 1
            then "PPG-1 Diff Primary-Overall to Primary-Sub"
        end
        , 3)
        as gender_ppg_py

        , round(
        case 
        when overall_secondary = 'N'
            and "DI Overall to Primary-Overall" = 1
            then "PPG-1 Diff Primary-Overall to Primary-Sub" + MoE
        end
        , 3)
        as gender_ppg_eliminate_substantive_DI_py
                        
        from (
        select 
        *
        , case 
            when subgroup1 = 1 then null
            when 
                "PPG-1 Diff Overall to Primary-Overall" < 0 
                and ABS("PPG-1 Diff Overall to Primary-Overall") > MOE_prime
                then 1
            else 0
            end as "DI Overall to Primary-Overall"

        , case 
            when subgroup1 = 1 then null
            when 
                "PPG-1 Diff Primary-Overall to Primary-Sub" < 0 
                and ABS("PPG-1 Diff Primary-Overall to Primary-Sub") > MOE 
                then 1
            else 0
            end as "DI Primary-Overall to Primary-Sub"

        , case 
            when subgroup1 = 1 then null
            when 
                "PPG-1 Diff Overall to Primary-Sub" < 0 
                and ABS("PPG-1 Diff Overall to Primary-Sub") > MOE 
                then 1
            else 0
            end as "DI Overall to Primary-Sub"

        , case 
            when subgroup1 = 1 then null
            when 
                "PPG-1 Diff Overall Ex Primary-Overall to Primary-Sub" < 0 
                --MOE not used in full equity calculation
                --this field is only used for full equity calculation
                --and ABS("PPG-1 Diff Overall Ex Primary-Overall to Primary-Sub") > MOE
                then 1
            else 0
            end as "DI Overall Ex Primary-Overall to Primary-Sub"

        , ("PPG-1 Ratio Overall to Primary-Overall" - MOE_prime) * primary_denom - primary_value as "Min Equity Overall to Primary-Overall"
        , ("PPG-1 Ratio Overall to Primary-Overall" * primary_denom) - primary_value as "Full Equity Overall to Primary-Overall"
        , ("PPG-1 Ratio Primary-Overall to Primary-Sub" - MOE) * r_denom - r_value as "Min Equity Primary-Overall to Primary-Sub"
        , ("PPG-1 Ratio Primary-Overall to Primary-Sub" * r_denom) - r_value as "Full Equity Primary-Overall to Primary-Sub"

        , ("PPG-1 Ratio Overall to Primary-Sub" - MOE) * r_denom - r_value as "Min Equity Overall to Primary-Sub"
        , ("PPG-1 Ratio Overall to Primary-Sub" * r_denom) - r_value as "Full Equity Overall to Primary-Sub"
        , ("PPG-1 Ratio Overall to Primary-Overall" - MOE) * r_denom - r_value as "Min Equity Overall Ex Primary-Overall to Primary-Sub"
        , ("PPG-1 Ratio Overall to Primary-Overall" * r_denom) - r_value as "Full Equity Overall Ex Primary-Overall to Primary-Sub"


        from (
            select 

                l.locale_type
                , l.locale_name
                , l.locale_id
                , r.metric_id
                , case
                    when r.metric_id = '300' then 'Successful Enrollment in the First Year'
                    when r.metric_id = '453' then 'Persisted First Primary Term to Subsequent Primary Term'
                    when r.metric_id = '501' then 'Completed Both Transfer-Level Math and English within the District in the First Year'
                    when r.metric_id = '619' then 'Attained the Vision for Success Definition of Completion within Three Years'
                    when r.metric_id = '620' then 'Transferred to a Four-Year Institution within Three Years'
                    end as data_element_name
                , y.year_id as academic_year_string
                , s1.disagg_id
                , s1.disagg_label as primary_disagg
                , r.subgroup1
                , s1.label as primary_subgroup
                , r.subgroup2
                , s2.label as secondary_subgroup
                , r.value as r_value
                , r.denom as r_denom
                , r.perc as r_perc
                , case 
                    when r.value = r.denom then 0.99
                    when r.value = 0 then 0.01
                    --else r.perc
                    end as r_perc_alt

                , case when s1.disagg_id = 1 and s2.disagg_id = 1 then 'Y' else 'N' end as overall_primary
                , case when s1.disagg_id != 1 and s2.disagg_id = 1 then 'Y' else 'N' end as overall_secondary

                , overall.value as overall_value
                , overall.denom as overall_denom
                , overall.perc as overall_perc
                , case 
                    when overall.value = overall.denom then 0.99
                    when overall.value = 0 then 0.01
                    --else overall.perc
                    end as overall_perc_alt

                , pr.value as primary_value
                , pr.denom as primary_denom
                , pr.perc as primary_perc
                , case 
                    when pr.value = pr.denom then 0.99
                    when pr.value = 0 then 0.01
                    --else pr.perc
                    end as pr_perc_alt

                , case 
                    when r.subgroup1 = 1 then null
                    when s1.disagg_id = 1 and s2.disagg_id = 1 then null
                    when overall.denom = pr.denom then 0
                    else (overall.value - pr.value)/(overall.denom - pr.denom)
                    end as "PPG-1 Ratio Overall to Primary-Overall"

                , case 
                    when r.subgroup1 = 1 then null
                    when s1.disagg_id = 1 or s2.disagg_id = 1 then null
                    when pr.denom = r.denom then 0 
                    else (pr.value - r.value)/(pr.denom - r.denom)
                    end as "PPG-1 Ratio Primary-Overall to Primary-Sub"

                , case 
                    when r.subgroup1 = 1 then null
                    when overall.denom = r.denom then 0 
                    else (overall.value - r.value)/(overall.denom - r.denom)
                    end as "PPG-1 Ratio Overall to Primary-Sub"

                , case 
                    when r.subgroup1 = 1 then null
                    when s1.disagg_id = 1 and s2.disagg_id = 1 then null
                    when overall.denom = pr.denom then 0
                    else pr.perc
                        - round((overall.value - pr.value)/(overall.denom - pr.denom), 10)
                    end as "PPG-1 Diff Overall to Primary-Overall"

                , case 
                    when r.subgroup1 = 1 then null
                    when s1.disagg_id = 1 or s2.disagg_id = 1 then null
                    when pr.denom = r.denom then 0
                    else r.perc
                        - round((pr.value - r.value)/(pr.denom - r.denom), 10)
                    end as "PPG-1 Diff Primary-Overall to Primary-Sub"

                , case 
                    when r.subgroup1 = 1 then null
                    when overall.denom = r.denom then 0
                    else r.perc
                        - round((overall.value - r.value)/(overall.denom - r.denom), 10)
                    end as "PPG-1 Diff Overall to Primary-Sub"

                , case 
                    when r.subgroup1 = 1 then null
                    when overall.denom = r.denom then 0
                    else r.perc
                        - round((overall.value - pr.value)/(overall.denom - pr.denom), 10)
                    end as "PPG-1 Diff Overall Ex Primary-Overall to Primary-Sub"

                , case 
                    when r.subgroup1 = 1 then null
                    when r.value = r.denom then case --outcome rate 100%
                        when 1.96*SQRT((0.99)*(1-0.99)/r.denom) < 0.02 then 0.02
                        else 1.96*SQRT((0.99)*(1-0.99)/r.denom)
                        end
                    when r.value = 0 then case --outcome rate 0%
                        when 1.96*SQRT((0.01)*(1-0.01)/r.denom) < 0.02 then 0.02
                        else 1.96*SQRT((0.01)*(1-0.01)/r.denom)
                        end
                    when 1.96*SQRT((r.perc)*(1-r.perc)/r.denom) < 0.02 then 0.02
                    else 1.96*SQRT((r.perc)*(1-r.perc)/r.denom)
                    end as MOE

                , case 
                    when r.subgroup1 = 1 then null
                    when pr.value = pr.denom then case --outcome rate 100%
                        when 1.96*SQRT((0.99)*(1-0.99)/pr.denom) < 0.02 then 0.02
                        else 1.96*SQRT((0.99)*(1-0.99)/pr.denom)
                        end
                    when pr.value = 0 then case --outcome rate 0%
                        when 1.96*SQRT((0.01)*(1-0.01)/pr.denom) < 0.02 then 0.02
                        else 1.96*SQRT((0.01)*(1-0.01)/pr.denom)
                        end
                    when 1.96*SQRT((pr.perc)*(1-pr.perc)/pr.denom) < 0.02 then 0.02
                    else 1.96*SQRT((pr.perc)*(1-pr.perc)/pr.denom)
                    end as MOE_prime

            from analytics_dev.results.results_masked r

            --necessary reference values
            join analytics_dev.ref_tx.stnd_ref__term_id y on r.year_term_id = y.term_id
            join analytics_dev.metrics.locale_id l on r.locale_id = l.locale_id
            join analytics_dev.metrics.disagg_id s1 on r.subgroup1 = s1.subgroup_id
            join analytics_dev.metrics.disagg_id s2 on r.subgroup2 = s2.subgroup_id

            --get the max complete year for the metric.  only necessary for the summary file
            --contains all possible metric/journey combinations for the entire SSM dashboard
            --along with reference data, which includes "max year"
            join analytics_dev.metrics.metric_journeys bd
            on r.metric_id = bd.metric_id
            and r.journey_id = bd.journey_id
            and r.subjourney_id = bd.subjourney_id

            --get the primary overall / overall record for common selection
            join analytics_dev.results.results_masked overall
            on r.year_term_id = overall.year_term_id
            and r.locale_id = overall.locale_id
            and r.metric_id = overall.metric_id
            and r.journey_id = overall.journey_id
            and r.subjourney_id = overall.subjourney_id
            and r.program_id = overall.program_id
            and overall.subgroup1 = 1
            and overall.subgroup2 = 1

            --get the primary subroup / overall record for common selection
            join analytics_dev.results.results_masked pr
            on r.year_term_id = pr.year_term_id
            and r.locale_id = pr.locale_id
            and r.metric_id = pr.metric_id
            and r.journey_id = pr.journey_id
            and r.subjourney_id = pr.subjourney_id
            and r.program_id = pr.program_id
            and r.subgroup1 = pr.subgroup1
            and pr.subgroup2 = 1

            where 
                1=1
                and --get only the record for the most recent available on this metric, and the 
                y.ANNL_ID >=240+REPORT_YEAR_OFFSET-20
                    
                and y.ANNL_ID <= 240+REPORT_YEAR_OFFSET
                and ( --get only the disagg X disagg records we want
                    (s1.disagg_id in (1, 3) and s2.disagg_id = 1) 
                    or (s1.disagg_id >= 4 and s1.disagg_id not in (9, 19)) 
                ) 
                --get only the relevent metrics for SEP
                and r.journey_id = 'C'
                and r.program_id = 1
                and s1.code != 'AMV'
                and s2.code != 'AMV'
                and r.denom >0
                and r.perc between 0 and 1
                and pr.denom >0
                and pr.perc between 0 and 1
                --get the 3 year cohort data for certain metrics
                and (
                        (r.metric_id in ('619', '620') and r.subjourney_id = '3')
                        OR (r.metric_id in ('300', '453', '501') and r.subjourney_id = '1')
                )
                --allows user to programmatically cycle over locales
                and l.locale_type != 'Institution'
                and r.locale_id = 139 
        ) first_pass
        ) second_pass
        order by 
        metric_id, academic_year_string, locale_name, disagg_id, subgroup1, subgroup2
        
        