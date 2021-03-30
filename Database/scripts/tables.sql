-- Table program stores our programs' data
CREATE TABLE program (
       number BIGINT PRIMARY KEY,
       length INT, 
       result TEXT
);

-- Table universal_dist stores the universal distribution
-- calculated value for each string
CREATE TABLE universal_dist (
       string TEXT,
       value DECIMAL
);
	
