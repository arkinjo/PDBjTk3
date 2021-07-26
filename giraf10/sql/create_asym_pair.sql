CREATE TABLE asym_pair_sum (
	pdbcode1 INTEGER NOT NULL
	, label_asym_id1 TEXT NOT NULL
	, pdbcode2 INTEGER NOT NULL
	, label_asym_id2 TEXT NOT NULL
	, score FLOAT NOT NULL
	, npair INTEGER NOT NULL
	, type TEXT NOT NULL
        , ali TEXT[][]
	, PRIMARY KEY(pdbcode1,label_asym_id1,pdbcode2,label_asym_id2,type)
	);


CREATE TABLE asym_pair_sum_nonpolymer () INHERITS(asym_pair_sum);
CREATE TABLE asym_pair_sum_ppi () INHERITS(asym_pair_sum);
CREATE TABLE asym_pair_sum_dnarna () INHERITS(asym_pair_sum);

CREATE TABLE asym_pair_sum_nonpolymer_ppi () INHERITS(asym_pair_sum);
CREATE TABLE asym_pair_sum_nonpolymer_ppi_dnarna () INHERITS(asym_pair_sum);

ALTER TABLE asym_pair_sum_nonpolymer ADD
        PRIMARY KEY (pdbcode1, label_asym_id1, pdbcode2, label_asym_id2);
ALTER TABLE asym_pair_sum_ppi ADD
        PRIMARY KEY (pdbcode1, label_asym_id1, pdbcode2, label_asym_id2); 
ALTER TABLE asym_pair_sum_dnarna ADD
        PRIMARY KEY (pdbcode1, label_asym_id1, pdbcode2, label_asym_id2);

ALTER TABLE asym_pair_sum_nonpolymer_ppi ADD
        PRIMARY KEY (pdbcode1, label_asym_id1, pdbcode2, label_asym_id2);

ALTER TABLE asym_pair_sum_nonpolymer_ppi_dnarna ADD
        PRIMARY KEY (pdbcode1, label_asym_id1, pdbcode2, label_asym_id2);

