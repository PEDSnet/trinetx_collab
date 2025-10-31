--To improve computational efficiency, the calculation is structured in 2 main steps:

--1. Initial Count: Patients are grouped based on their presence in each domain, counting those with data in at least one of the specified domains.
with included as (
	SELECT
		  count(dx.patient_id) as dx
		, count(dx.patient_id || med.patient_id) as dxmed
		, count(dx.patient_id || med.patient_id || prc.patient_id) as dxmedprc
		, count(dx.patient_id || med.patient_id || prc.patient_id || lab.patient_id) as dxmedprclab
		, count(dx.patient_id || med.patient_id || lab.patient_id) as dxmedlab
		, count(dx.patient_id || prc.patient_id) as dxprc
		, count(dx.patient_id || prc.patient_id || lab.patient_id) as dxprclab
		, count(dx.patient_id || lab.patient_id) as dxlab
		, count(med.patient_id) as med
		, count(med.patient_id || prc.patient_id) as medprc
		, count(med.patient_id || prc.patient_id || lab.patient_id) as medprclab
		, count(med.patient_id || lab.patient_id) as medlab
		, count(prc.patient_id) as prc
		, count(prc.patient_id || lab.patient_id) as prclab
		, count(lab.patient_id) as lab
	FROM SCHEMA.patient p
		LEFT JOIN (SELECT DISTINCT patient_id FROM SCHEMA.diagnosis where date >= '2014-01-01' and date < '2024-01-01') dx on dx.patient_id = p.patient_id
		LEFT JOIN (SELECT DISTINCT patient_id FROM SCHEMA.medication where date >= '2014-01-01' and date < '2024-01-01') med on med.patient_id = p.patient_id
		LEFT JOIN (SELECT DISTINCT patient_id FROM SCHEMA.procedure where date >= '2014-01-01' and date < '2024-01-01') prc on prc.patient_id = p.patient_id
		LEFT JOIN (SELECT DISTINCT patient_id FROM SCHEMA.laboratory where date >= '2014-01-01' and date < '2024-01-01') lab on lab.patient_id = p.patient_id
),

--2. Overlap Adjustment: Using mathematical expressions, the results are refined to isolate patients belonging strictly to each combination of domains.
excluded as (
	SELECT 	DXMEDPRCLAB,
			(DXMEDLAB - DXMEDPRCLAB) AS DXMEDLAB,
			(DXMEDPRC - DXMEDPRCLAB) AS DXMEDPRC,
			(DXPRCLAB - DXMEDPRCLAB) AS DXPRCLAB,
			(MEDPRCLAB - DXMEDPRCLAB) AS MEDPRCLAB,
			(DXLAB + DXMEDPRCLAB - DXMEDLAB - DXPRCLAB) AS DXLAB,
			(DXMED + DXMEDPRCLAB - DXMEDLAB - DXMEDPRC) AS DXMED,
			(DXPRC + DXMEDPRCLAB - DXPRCLAB - DXMEDPRC) AS DXPRC,
			(MEDLAB + DXMEDPRCLAB - DXMEDLAB - MEDPRCLAB) AS MEDLAB,
			(MEDPRC + DXMEDPRCLAB - DXMEDPRC - MEDPRCLAB) AS MEDPRC,
			(PRCLAB + DXMEDPRCLAB - DXPRCLAB - MEDPRCLAB) AS PRCLAB,
			(DX - DXMED - DXPRC + DXMEDPRC - DXLAB - DXMEDPRCLAB + DXMEDLAB + DXPRCLAB) AS DX,
			(LAB - MEDLAB - PRCLAB + MEDPRCLAB - DXLAB - DXMEDPRCLAB + DXMEDLAB + DXPRCLAB) AS LAB,
			(MED - MEDLAB - DXMED + DXMEDLAB - MEDPRC - DXMEDPRCLAB + DXMEDPRC + MEDPRCLAB) AS MED,
			(PRC - PRCLAB - DXPRC + DXPRCLAB - MEDPRC - DXMEDPRCLAB + DXMEDPRC + MEDPRCLAB) AS PRC
	FROM included
)
-- Finally, a column with the total number of patients is added, to be used as the denominator when calculating the percentages.
-- This can be done after the query if preferred.
SELECT *, DXMEDPRCLAB+DXMEDLAB+DXMEDPRC+DXPRCLAB+MEDPRCLAB+DXLAB+DXMED+DXPRC+MEDLAB+MEDPRC+PRCLAB+DX+LAB+MED+PRC as TOTAL from excluded