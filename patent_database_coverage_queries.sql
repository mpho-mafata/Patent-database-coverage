-- Update the PTMT tables
UPDATE patstat.mpho."PTMT_grant_type"
SET "Utility_Patents" = REPLACE("Utility_Patents", ' ', ''),
    "Design_Patents"  = REPLACE("Design_Patents", ' ', ''),
    "Plant_Patents"   = REPLACE("Plant_Patents", ' ', ''),
    "Reissue_Patents" = REPLACE("Reissue_Patents", ' ', ''),
    "Total_Patents"   = REPLACE("Total_Patents", ' ', '')
;

UPDATE patstat.reference."PTMT_grant_type"
SET "Utility_Patents" = REPLACE("Utility_Patents", ' ', ''),
    "Design_Patents"  = REPLACE("Design_Patents", ' ', ''),
    "Plant_Patents"   = REPLACE("Plant_Patents", ' ', ''),
    "Reissue_Patents" = REPLACE("Reissue_Patents", ' ', ''),
    "Total_Patents"   = REPLACE("Total_Patents", ' ', '')
;

UPDATE patstat.mpho."PTMT_applications_type"
SET "Utility_Patents" = REPLACE("Utility_Patents", ' ', ''),
    "Design_Patents"  = REPLACE("Design_Patents", ' ', ''),
    "Plant_Patents"   = REPLACE("Plant_Patents", ' ', ''),
    "Total_Patents"   = REPLACE("Total_Patents", ' ', '')
;

UPDATE patstat.reference."PTMT_applications_type"
SET "Utility_Patents" = REPLACE("Utility_Patents", ' ', ''),
    "Design_Patents"  = REPLACE("Design_Patents", ' ', ''),
    "Plant_Patents"   = REPLACE("Plant_Patents", ' ', ''),
    "Total_Patents"   = REPLACE("Total_Patents", ' ', '')
;

--PATSTAT COVERAGE INVESTIGATION
--patent type coverage applications and grants
DROP MATERIALIZED VIEW IF EXISTS mpho.patstat_us_patent_type_appln_coverage;
CREATE MATERIALIZED VIEW mpho.patstat_us_patent_type_appln_coverage AS
SELECT a.appln_filing_year        AS appln_year,
       a.appln_kind               AS ip_type,
       count(DISTINCT a.appln_nr) AS n_applns
FROM patstat."2022_fall".tls201_appln AS a
WHERE upper(a.appln_auth) = 'US'
GROUP BY appln_year, ip_type
ORDER BY appln_year, ip_type
;

DROP MATERIALIZED VIEW IF EXISTS mpho.patstat_us_patent_type_grants_coverage;
CREATE MATERIALIZED VIEW mpho.patstat_us_patent_type_grants_coverage AS
SELECT date_part('year', p.publn_date) AS grant_year,
       p.publn_kind                    AS ip_type,
       count(DISTINCT p.publn_nr)      AS n_grants
FROM patstat."2022_fall".tls201_appln AS a
         JOIN patstat."2022_fall".tls211_pat_publn AS p ON p.appln_id = a.appln_id
WHERE upper(a.appln_auth) = 'US'
  AND upper(a.granted) = 'Y'
  AND upper(p.publn_first_grant) = 'Y'
GROUP BY grant_year, ip_type
ORDER BY grant_year, ip_type
;

--geographic coverage
DROP MATERIALIZED VIEW IF EXISTS mpho.patstat_us_geographic_coverage CASCADE;
CREATE MATERIALIZED VIEW mpho.patstat_us_geographic_coverage AS
SELECT publn_year               AS grant_year,
       person_ctry_code,
       publn_kind               AS ip_type,
       --Publication numbers may repeat, so we count them once per number
       COUNT(DISTINCT publn_nr) AS n_grants
FROM (SELECT person_ctry_code,
             --We use kind rather than IPR-type, because IPR-type does not have plant patents
             publn_kind,
             --We assume that grants are published (WIPO ST6 number)
             publn_nr,
             --We assume that the publication date is a close approximation of the grant date
             -- WIPO ST9 date
             DATE_PART('year', publn_date) AS publn_year
      FROM patstat."2022_fall".tls201_appln t201al
               JOIN patstat."2022_fall".tls211_pat_publn t211ppl ON t201al.appln_id = t211ppl.appln_id
          --USPTO documents only (ST3 office)
               JOIN patstat."2022_fall".tls227_pers_publn t227ppl ON t211ppl.pat_publn_id = t227ppl.pat_publn_id
          AND invt_seq_nr = 1 --We use the first-named inventor
               JOIN patstat."2022_fall".tls206_person t206p ON t227ppl.person_id = t206p.person_id
      WHERE appln_auth = 'US'
        AND granted = 'Y' --Only count for granted patents
        AND publn_first_grant = 'Y' --Avoid counting re-issues
     ) AS publn_list
GROUP BY publn_year,
         person_ctry_code,
         publn_kind
ORDER BY publn_year, person_ctry_code ASC
;

--patent ownership coverage
DROP MATERIALIZED VIEW IF EXISTS mpho.patstat_us_owner_coverage CASCADE;
CREATE MATERIALIZED VIEW mpho.patstat_us_owner_coverage AS
SELECT publn_year               AS grant_year,
       publn_kind               AS ip_type,
       person_name,
       psn_sector,
       COUNT(DISTINCT publn_nr) AS n_grants
FROM (SELECT person_name,
             psn_sector,
             publn_nr,
             publn_kind,
             DATE_PART('year', publn_date) AS publn_year
      FROM patstat."2022_fall".tls201_appln t201al
               JOIN patstat."2022_fall".tls211_pat_publn t211ppl ON t201al.appln_id = t211ppl.appln_id
               JOIN patstat."2022_fall".tls227_pers_publn t227ppl ON t211ppl.pat_publn_id = t227ppl.pat_publn_id
          AND applt_seq_nr = 1
               JOIN patstat."2022_fall".tls206_person t206p ON t227ppl.person_id = t206p.person_id
      WHERE appln_auth = 'US'
        AND granted = 'Y'
        AND publn_first_grant = 'Y') publn_list
GROUP BY publn_year,
         publn_kind,
         person_name,
         psn_sector
ORDER BY publn_year ASC, COUNT(DISTINCT publn_nr) DESC;
;

-- PATSTAT owners using legal events table
DROP MATERIALIZED VIEW IF EXISTS mpho.patstat_us_owner_legal;
CREATE MATERIALIZED VIEW mpho.patstat_us_owner_legal AS
SELECT date_part('year', p.publn_date) AS year,
       p.publn_kind                    AS ip_type,
       l.party_new                     AS owner,
       count(DISTINCT p.publn_nr)      AS n_grants
FROM patstat."2022_fall".tls201_appln AS a
         JOIN patstat."2022_fall".tls211_pat_publn AS p ON p.appln_id = a.appln_id
         JOIN patstat."2022_fall".tls231_inpadoc_legal_event AS l on l.appln_id = a.appln_id
WHERE upper(a.appln_auth) = 'US'
  AND upper(a.granted) = 'Y'
  AND upper(p.publn_first_grant) = 'Y'
  AND upper(l.party_type) = 'OWN'
  AND l.party_seq_nr = 1
GROUP BY year, owner, ip_type
;

--technology coverage at top level
DROP MATERIALIZED VIEW IF EXISTS mpho.patstat_us_technology_coverage CASCADE;
CREATE MATERIALIZED VIEW mpho.patstat_us_technology_coverage AS
SELECT date_part('year', p.publn_date)     AS grant_year,
       p.publn_kind                        AS ip_type,
       substring(b.cpc_class_symbol, 1, 1) AS cpc_class,
       count(DISTINCT p.publn_nr)          AS n_grants
FROM patstat."2022_fall".tls201_appln AS a
         JOIN patstat."2022_fall".tls224_appln_cpc AS b ON b.appln_id = a.appln_id
         JOIN patstat."2022_fall".tls211_pat_publn AS p ON p.appln_id = a.appln_id
WHERE upper(a.appln_auth) = 'US'
  AND upper(a.granted) = 'Y'
  AND upper(p.publn_first_grant) = 'Y'
GROUP BY grant_year, substring(b.cpc_class_symbol, 1, 1), ip_type
ORDER BY grant_year DESC, substring(b.cpc_class_symbol, 1, 1), ip_type, n_grants DESC
;

-- OPTIONAL QUERY
-- granular technology classifications
DROP MATERIALIZED VIEW IF EXISTS mpho.patstat_us_technology_coverage_granular CASCADE;
CREATE MATERIALIZED VIEW mpho.patstat_us_technology_coverage_granular AS
SELECT date_part('year', p.publn_date) AS grant_year,
       p.publn_kind                    AS ip_type,
       b.cpc_class_symbol              AS cpc_class,
       count(DISTINCT p.publn_nr)      AS n_grants
FROM patstat."2022_fall".tls201_appln AS a
         JOIN patstat."2022_fall".tls224_appln_cpc AS b ON b.appln_id = a.appln_id
         JOIN patstat."2022_fall".tls211_pat_publn AS p ON p.appln_id = a.appln_id
WHERE upper(a.appln_auth) = 'US'
  AND upper(a.granted) = 'Y'
  AND upper(p.publn_first_grant) = 'Y'
GROUP BY grant_year, b.cpc_class_symbol, ip_type
ORDER BY grant_year DESC, b.cpc_class_symbol, ip_type, n_grants DESC
;

-- OPTIONAL QUERY
-- granular technology classifications with reference annotations
DROP MATERIALIZED VIEW IF EXISTS mpho.patstat_us_technology_coverage_granular_annotated CASCADE;
CREATE MATERIALIZED VIEW mpho.patstat_us_technology_coverage_granular_annotatated AS
SELECT date_part('year', p.publn_date) AS grant_year,
       p.publn_kind                    AS ip_type,
       b.cpc_class_symbol              AS cpc_class,
       cl.level_1                      AS technology_class,
       count(DISTINCT p.publn_nr)      AS n_grants
FROM patstat."2022_fall".tls201_appln AS a
         JOIN patstat."2022_fall".tls224_appln_cpc AS b ON b.appln_id = a.appln_id
         JOIN patstat."2022_fall".tls211_pat_publn AS p ON p.appln_id = a.appln_id
         JOIN patstat.mpho.cpc_classification AS cl ON cl.classification_code = REPLACE(b.cpc_class_symbol, ' ', '')
WHERE upper(a.appln_auth) = 'US'
  AND upper(a.granted) = 'Y'
  AND upper(p.publn_first_grant) = 'Y'
GROUP BY grant_year, b.cpc_class_symbol, ip_type, technology_class
ORDER BY grant_year DESC, b.cpc_class_symbol, ip_type, n_grants DESC
;

--PATENTSVIEW COVERAGE INVESTIGATION
--create keys for tables with no keys
ALTER TABLE patstat.patentsview_current.g_gov_interest_contracts
    ADD COLUMN crest_id SERIAL PRIMARY KEY

ALTER TABLE patstat.patentsview_current.g_location_disambiguated
    ADD COLUMN crest_id SERIAL PRIMARY KEY

ALTER TABLE patstat.patentsview_current.g_gov_interest_org
    ADD COLUMN crest_id SERIAL PRIMARY KEY

ALTER TABLE patstat.patentsview_current.g_pct_data
    ADD COLUMN crest_id SERIAL PRIMARY KEY

ALTER TABLE patstat.patentsview_current.g_foreign_priority
    ADD COLUMN crest_id SERIAL PRIMARY KEY

--patent type coverage applications and grants
DROP MATERIALIZED VIEW IF EXISTS mpho.patentsview_patent_type_applications_coverage;
CREATE MATERIALIZED VIEW mpho.patentsview_patent_type_applications_coverage AS
SELECT date_part('year', a.filing_date) AS appln_year,
       b.ip_type                        AS appln_type,
       count(DISTINCT a.application_id) AS n_applns
FROM patstat.patentsview_current.g_application AS a
         JOIN patstat.reference.patentsview_application_types AS b
              ON a.patent_application_type = b.patent_application_type
GROUP BY appln_year, appln_type
ORDER BY appln_year ASC, ip_type
;

DROP MATERIALIZED VIEW IF EXISTS mpho.patentsview_patent_type_grants_coverage;
CREATE MATERIALIZED VIEW mpho.patentsview_patent_type_grants_coverage AS
SELECT date_part('year', a.patent_date) AS year,
       a.wipo_kind                      AS wipo_kind,
       count(distinct a.patent_id)      AS n_patents
FROM patstat.patentsview_current.g_patent AS a
GROUP BY year, wipo_kind
;

--geographic coverage
DROP MATERIALIZED VIEW IF EXISTS mpho.patentsview_geographic_coverage CASCADE;
CREATE MATERIALIZED VIEW mpho.patentsview_geographic_coverage AS
WITH first_inventors AS (SELECT *
                         FROM patentsview_current.g_inventor_not_disambiguated
                         WHERE inventor_sequence = 0)
SELECT DATE_PART('year', a.patent_date) AS grant_year,
       CASE
           WHEN c.raw_country = '' AND c.raw_state != '' THEN 'US'
           ELSE c.raw_country
           END                          AS person_ctry_code,
       a.wipo_kind                      AS ip_wipo,
       c.raw_state,
       c.raw_city,
       COUNT(DISTINCT a.patent_id)      AS n_grants
FROM first_inventors b
         JOIN patentsview_current.g_patent AS a ON a.patent_id = b.patent_id
         JOIN patentsview_current.g_location_not_disambiguated AS c ON c.rawlocation_id = b.rawlocation_id
GROUP BY grant_year, ip_wipo, person_ctry_code, c.raw_state,
         c.raw_city
;

--ownership coverage
DROP MATERIALIZED VIEW IF EXISTS mpho.patentsview_assignee_coverage CASCADE;
CREATE MATERIALIZED VIEW mpho.patentsview_assignee_coverage AS
SELECT date_part('year', a.patent_date)                                                       AS grant_year,
       concat(b.raw_assignee_individual_name_first, ' ', b.raw_assignee_individual_name_last) AS person_name,
       b.raw_assignee_organization                                                            AS organization,
       a.wipo_kind                                                                            AS ip_type,
       count(DISTINCT a.patent_id)                                                            AS n_grants
FROM patstat.patentsview_current.g_patent AS a
         JOIN patstat.patentsview_current.g_assignee_not_disambiguated AS b ON b.patent_id = a.patent_id
WHERE b.assignee_sequence = 0
GROUP BY grant_year, person_name, organization, ip_type
;

-- technology coverage
DROP MATERIALIZED VIEW IF EXISTS mpho.patentsview_technology_coverage CASCADE;
CREATE MATERIALIZED VIEW mpho.patentsview_technology_coverage AS
SELECT date_part('year', a.patent_date) AS grant_year,
       a.wipo_kind                      AS ip_type,
       b.cpc_section                    AS level_1,
       b.cpc_class                      AS level_2,
       b.cpc_subclass                   AS level_3,
       b.cpc_group                      AS level_4,
       count(DISTINCT a.patent_id)      AS n_patents
FROM patstat.patentsview_current.g_patent AS a
         JOIN patstat.patentsview_current.g_cpc_current AS b ON b.patent_id = a.patent_id
GROUP BY grant_year,
         ip_type,
         level_1,
         level_2,
         level_3,
         level_4
;

--additional technology annotations
DROP MATERIALIZED VIEW IF EXISTS mpho.patentsview_technology_annotated;
CREATE MATERIALIZED VIEW mpho.patentsview_technology_annotated AS
SELECT a.grant_year, a.ip_type, a.n_patents, b.*
FROM patstat.mpho.patentsview_technology_coverage AS a
         JOIN patstat.reference.cpc_classification AS b ON b.classification_code = a.level_4
;




