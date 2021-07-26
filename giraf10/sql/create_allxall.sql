
CREATE TABLE all_vs_all (
        if_id1 TEXT NOT NULL
        , if_id2 TEXT NOT NULL
        , irscore FLOAT NOT NULL
        , giscore FLOAT NOT NULL
        , crms FLOAT NOT NULL
        , drms FLOAT NOT NULL
	, tanimoto FLOAT NOT NULL
        , seqid FLOAT NOT NULL
        , nali INT NOT NULL
        , ngi INT NOT NULL
        );

CREATE TABLE all_vs_all_ppi (
        FOREIGN KEY (if_id1) 
                REFERENCES Interfaces_ppi(if_id)
                ON DELETE CASCADE
        , FOREIGN KEY (if_id2) 
                REFERENCES Interfaces_ppi(if_id)
                ON DELETE CASCADE
        ) INHERITS (all_vs_all);

CREATE TABLE all_vs_all_dnarna (
        FOREIGN KEY (if_id1) 
                REFERENCES Interfaces_dnarna(if_id)
                ON DELETE CASCADE
        , FOREIGN KEY (if_id2) 
                REFERENCES Interfaces_dnarna(if_id)
                ON DELETE CASCADE
        ) INHERITS (all_vs_all);


CREATE TABLE all_vs_all_small () INHERITS (all_vs_all);


CREATE TABLE all_vs_all_peptide (
        FOREIGN KEY (if_id1) 
                REFERENCES Interfaces_peptide(if_id)
                ON DELETE CASCADE
        , FOREIGN KEY (if_id2) 
                REFERENCES Interfaces_peptide(if_id)
                ON DELETE CASCADE
        ) INHERITS (all_vs_all);

CREATE TABLE all_vs_all_opoly (
        FOREIGN KEY (if_id1) 
                REFERENCES Interfaces_opoly(if_id)
                ON DELETE CASCADE
        , FOREIGN KEY (if_id2) 
                REFERENCES Interfaces_opoly(if_id)
                ON DELETE CASCADE
        ) INHERITS (all_vs_all);

CREATE TABLE all_vs_all_nonpolymer (
        FOREIGN KEY (if_id1) 
                REFERENCES Interfaces_nonpolymer(if_id)
                ON DELETE CASCADE
        , FOREIGN KEY (if_id2) 
                REFERENCES Interfaces_nonpolymer(if_id)
                ON DELETE CASCADE
        ) INHERITS (all_vs_all);

