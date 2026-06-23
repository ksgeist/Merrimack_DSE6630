# Step 1
dbExecute(con, "CREATE TABLE temp AS
  SELECT pt_key, flu_innoc, tet_innoc, covid_innoc, bmi_category, smoking_status
  FROM patient_clinical")

# Step 2
dbExecute(con, "INSERT INTO temp (pt_key, flu_innoc, tet_innoc, covid_innoc, bmi_category, smoking_status)
  SELECT pt_key, flu_innoc, tet_innoc, covid_innoc, bmi_category, smoking_status
  FROM patient_clinical")

# Step 3
dbExecute(con, "DROP TABLE patient_clinical")

# Step 4
dbExecute(con, "ALTER TABLE temp RENAME TO patient_clinical")