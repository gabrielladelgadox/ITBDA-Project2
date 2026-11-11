-- Student: Gabriella Delgado
-- Student Number: EDUV4815041
-- Project 2
-- Question 4
-- Question 4.3

-- Create the relational table

CREATE TABLE country_data (
    country TEXT PRIMARY KEY,
    capital TEXT,
    population BIGINT,
    area_km2 NUMERIC
);

-- Check the imported records
SELECT COUNT(*) AS total_records
FROM country_data;

-- Display the imported data
SELECT *
FROM country_data
LIMIT 10;

-- Investigation 1
-- Largest Countries and Capitals by Area

SELECT
    country,
    capital,
    area_km2,
    population
FROM country_data
WHERE area_km2 > 0
ORDER BY area_km2 DESC
LIMIT 10;


-- Investigation 2
-- Highest Population Density

SELECT
    country,
    population,
    area_km2,
    ROUND(population / area_km2, 2) AS population_density
FROM country_data
WHERE population >= 100000
  AND area_km2 > 0
ORDER BY population_density DESC
LIMIT 10;