--single cohort
select count(distinct patient_id)
from fact_table
where code in (codes for condition A) 
	and date >= 2014-01-01 and date <= 2023-12-31


--combination of both cohorts
with group_A as (
	select patient_id, date
	from fact_table
	where code in (codes for condition A) 
		and date >= 2014-01-01 and date <= 2023-12-31
)
, group_B as (
	select patient_id, date
	from fact_table
	where code in (codes for condition B) 
		and date >= 2014-01-01 and date <= 2023-12-31
)
select count(distinct patient_id)
from group_A a
join group_B b on a.patient_id = b.patient_id 
where b.date >= a.date and b.date <= DATE_ADD(month, 1, a.date)
--where b.date >= a.date and b.date <= DATE_ADD(year, 2, a.date)