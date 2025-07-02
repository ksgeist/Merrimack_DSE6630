## Using a LEFT JOIN 
dbExecute(con, 
          "CREATE TABLE merged2 AS
  SELECT 
    m.pt_key,
    m.ins_key,
    m.age_years,
    m.city,
    m.state,
    m.zipcode,
    m.disability_status, 
    m.employmt_status,
    m.flu_innoc,
    m.tet_innoc,
    m.covid_innoc,
    m.bmi_category,
    m.smoking_status,
    s.public_ins
  FROM merged AS m
  INNER JOIN 
  patient_insurance AS s
  ON m.pt_key = s.pt_key AND m.ins_key = s.ins_key")

## Now remove any duplicates:

## Drop the original
dbExecute(con, "DROP TABLE merged")

## Remove the duplicates from the newly merged file and make it the new merged
dbExecute(con, 
          "CREATE TABLE merged 
  AS SELECT DISTINCT *
  FROM merged2")

## Drop merged2 to tidy up
dbExecute(con, "DROP TABLE merged2")
