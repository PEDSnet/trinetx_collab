-- Denominator: all patients with a visit, per month
select 	'denominator' as descr,
		LAST_DAY(start_date) as date,
		count(distinct patient_id) as patients	
from SCHEMA.encounter e
where start_date <= '2023-12-31' and start_date >= '2014-01-01'
group by descr, date

--Care setting queries
select 	'inpatients' as descr,
		LAST_DAY(start_date) as date,
		count(distinct patient_id) as patients	
from SCHEMA.encounter e
where start_date <= '2023-12-31' and start_date >= '2014-01-01' and encounter_type = 'Inpatient'
group by descr, date

--Care setting + domain queries
select 	'inpatient meds' as descr,
		LAST_DAY(date) as date,
		count(distinct patient_id) as patients	
from SCHEMA.medication med
join (
	select distinct encounter_id from SCHEMA.encounter
	where encounter_type = 'Inpatient'
) e on med.encounter_id = e.encounter_id
where date <= '2023-12-31' and start_date >= '2014-01-01'
group by descr, date

--Diagnoses
select 	'diag anxiety & depression' as descr,
		LAST_DAY(date) as date,
		count(distinct patient_id) as patients	
from SCHEMA.diagnosis d
where date <= '2023-12-31' and date >= '2014-01-01' and code like 'ICD10CM:F41%'
group by descr, date1