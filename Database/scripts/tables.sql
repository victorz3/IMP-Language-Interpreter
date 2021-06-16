-- Table program stores our programs' data
CREATE TABLE program (
       number INT PRIMARY KEY,
       result TEXT,
       length INT, 
       steps INT
);

-- Table universal_dist stores the universal distribution
-- calculated value for each string
CREATE TABLE universal_dist (
       string TEXT PRIMARY KEY,
       value REAL
);
