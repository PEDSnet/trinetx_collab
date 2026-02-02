--denominator
select count(distinct patient_id)
from diagnoses
where date >= 2014-01-01 and date <= 2023-12-31

--numerator example
select count(distinct patient_id)
from diagnoses d
join hierarchy_reference h on d.code = h.code
where h.parent_code = 'A00-B99'
	d.date >= 2014-01-01 and d.date <= 2023-12-31
	
--numerators
select 	parent_code,
		count(distinct patient_id)
from diagnoses d
join hierarchy_reference h on d.code = h.code
where d.date >= 2014-01-01 and d.date <= 2023-12-31
group by parent_code -- A00-B99, C00-C99, etc.