CREATE OR REPLACE FUNCTION hash_pdbcode(TEXT)
RETURNS INT AS
$body$
DECLARE
  b BYTEA;
  hpdbcode INT;
BEGIN
  b := md5($1)::BYTEA;
  hpdbcode := 
    (get_byte(b,0) * 13 
   + get_byte(b,3) * 5 
   + get_byte(b,5) * 7 
   + get_byte(b,7) * 11) ;
  RETURN hpdbcode;
END;
$body$ LANGUAGE plpgsql;

CREATE TYPE interface_type AS ENUM ('nonpolymer', 'ppi', 'dnarna', 'peptide', 'opoly', 'fold', 'small', 'query');

CREATE TABLE __current_pdbid (
        pdbid TEXT constraint current_pdbid_pkey PRIMARY KEY
        );
CREATE TABLE __updated_pdbid (
        pdbid TEXT constraint updated_pdbid_pkey PRIMARY KEY
        );

/* UniProt-related tables */
CREATE TABLE Uniprot (
        uniprot_acc TEXT PRIMARY KEY
        , uniprot_id TEXT NOT NULL
        , fullname TEXT NOT NULL
        , gene TEXT
        , species TEXT
        );

CREATE TABLE Uniprot_sec (
        sec_ac TEXT
        , uniprot_acc TEXT
        );

-- pdb->uniprot mapping provided by UniProt
CREATE TABLE pdbtosp (
        pdbid TEXT
        , uniprot_acc TEXT
        );

CREATE TABLE Keywords (
        kwid TEXT PRIMARY KEY
        , kw_acc TEXT NOT NULL
        , ca TEXT NOT NULL
        , hi TEXT []
        );

CREATE TABLE Uniprot_kw (
        uniprot_acc TEXT NOT NULL
        , kwid TEXT NOT NULL
        , PRIMARY KEY (uniprot_acc, kwid)
        , CONSTRAINT kw_fkey FOREIGN KEY (kwid) REFERENCES Keywords(kwid) ON DELETE CASCADE
        , CONSTRAINT up_fkey FOREIGN KEY (uniprot_acc) REFERENCES Uniprot(uniprot_acc) ON DELETE CASCADE
        );

/* Motif-related tables */

CREATE TABLE Cmotif (
        cmotif_id TEXT PRIMARY KEY
        , kw TEXT []
        );

CREATE TABLE Emotif (
        emotif_id TEXT PRIMARY KEY
        , type interface_type NOT NULL 
        , kw TEXT []
        , cmotif_id TEXT 
--        , CONSTRAINT cmotif_fkey FOREIGN KEY (cmotif_id) REFERENCES Cmotif(cmotif_id) ON DELETE CASCADE
        );
        
/* PDB-related tables */
CREATE TABLE Structs (
        pdbid TEXT CONSTRAINT structs_pkey PRIMARY KEY
        , title TEXT NOT NULL
        , descriptor TEXT NOT NULL
        , atom_record XML NOT NULL
        , reldate DATE
        , mtime DOUBLE PRECISION -- last modification time
        , updated BOOLEAN default true
        );

CREATE TABLE Entities (
        pdbid TEXT NOT NULL REFERENCES Structs(pdbid) ON DELETE CASCADE
        , entity_id TEXT NOT NULL
        , type TEXT NOT NULL
        , comp_id TEXT NOT NULL
        , description TEXT NOT NULL
        , uniprot_acc TEXT
        , uniprot_mod BOOLEAN DEFAULT false
        , CONSTRAINT entities_pkey PRIMARY KEY (pdbid, entity_id)
--        , CONSTRAINT uniprot_fkey FOREIGN KEY (uniprot_acc) REFERENCES Uniprot(uniprot_acc)
        );

CREATE TABLE Assemblies (
        pdbid TEXT NOT NULL REFERENCES Structs(pdbid) ON DELETE CASCADE
        , assembly_id TEXT NOT NULL 
        , details TEXT
        , method_details TEXT
        , oligomeric_count INTEGER
        , CONSTRAINT assemblies_pkey PRIMARY KEY(pdbid,assembly_id)
        );

CREATE TABLE Asyms (
        pdbid TEXT NOT NULL
        , assembly_id TEXT NOT NULL
        , label_asym_id TEXT NOT NULL
        , auth_asym_id TEXT NOT NULL
        , entity_id TEXT NOT NULL
        , cmotif_id TEXT
        , CONSTRAINT asyms_pkey PRIMARY KEY (pdbid, assembly_id, label_asym_id)
        , CONSTRAINT asyms_fkey1 FOREIGN KEY (pdbid, assembly_id)
                REFERENCES Assemblies(pdbid, assembly_id) ON DELETE CASCADE
        , CONSTRAINT asyms_fkey2 FOREIGN KEY (pdbid, entity_id) 
                REFERENCES Entities(pdbid,entity_id) ON DELETE CASCADE
--        , CONSTRAINT cmotif_fkey FOREIGN KEY (cmotif_id) REFERENCES Cmotif(cmotif_id) ON DELETE SET NULL
        );

CREATE TABLE Interfaces (
        if_id TEXT CONSTRAINT interfaces_pkey PRIMARY KEY
        --- PK should be like: 'pdbid:assembly_id:label_asym_id:ligand_asym_id'
        , pdbid TEXT NOT NULL
        , assembly_id TEXT NOT NULL
        , label_asym_id TEXT NOT NULL
        , ligand_asym_id TEXT NOT NULL
        , CONSTRAINT interfaces_fkey1 
                FOREIGN KEY (pdbid,assembly_id,label_asym_id) 
                REFERENCES Asyms(pdbid,assembly_id,label_asym_id) 
                ON DELETE CASCADE
        , CONSTRAINT interfaces_fkey2 
                FOREIGN KEY (pdbid,assembly_id,ligand_asym_id) 
                REFERENCES Asyms(pdbid,assembly_id,label_asym_id) 
                ON DELETE CASCADE
        , type interface_type  
--        , atoms BYTEA NOT NULL
        , atoms TEXT NOT NULL
        , atom_site_ids TEXT[] NOT NULL
        , natoms INTEGER NOT NULL DEFAULT 0
        , nlatoms INTEGER NOT NULL DEFAULT 0
        , prob DOUBLE PRECISION NOT NULL DEFAULT 1.0
        , emotif_id TEXT 
--        , CONSTRAINT emotif_fkey FOREIGN KEY (emotif_id) REFERENCES Emotif(emotif_id) ON DELTE SET NULL
        );

/* nucleic acid ligands */
CREATE TABLE Interfaces_dnarna (
        PRIMARY KEY (if_id)
        , type interface_type DEFAULT 'dnarna'
        , FOREIGN KEY (pdbid,assembly_id,label_asym_id) 
                REFERENCES Asyms(pdbid,assembly_id,label_asym_id) 
                ON DELETE CASCADE
        , FOREIGN KEY (pdbid,assembly_id,ligand_asym_id) 
                REFERENCES Asyms(pdbid,assembly_id,label_asym_id) 
                ON DELETE CASCADE
        ) INHERITS (Interfaces);

/* small molecule = nonpolymer + peptide + opoly */
CREATE TABLE Interfaces_small (
        PRIMARY KEY (if_id)
        , FOREIGN KEY (pdbid,assembly_id,label_asym_id) 
                REFERENCES Asyms(pdbid,assembly_id,label_asym_id) 
                ON DELETE CASCADE
        , FOREIGN KEY (pdbid,assembly_id,ligand_asym_id) 
                REFERENCES Asyms(pdbid,assembly_id,label_asym_id) 
                ON DELETE CASCADE
        ) INHERITS (Interfaces);


/* non-polymer ligands */
CREATE TABLE Interfaces_nonpolymer (
        PRIMARY KEY (if_id)
        , type interface_type DEFAULT 'nonpolymer'
        , FOREIGN KEY (pdbid,assembly_id,label_asym_id) 
                REFERENCES Asyms(pdbid,assembly_id,label_asym_id) 
                ON DELETE CASCADE
        , FOREIGN KEY (pdbid,assembly_id,ligand_asym_id) 
                REFERENCES Asyms(pdbid,assembly_id,label_asym_id) 
                ON DELETE CASCADE
        ) INHERITS (Interfaces_small);

/* peptide ligands */
CREATE TABLE Interfaces_peptide (
        PRIMARY KEY (if_id)
        , type interface_type DEFAULT 'peptide'
        , FOREIGN KEY (pdbid,assembly_id,label_asym_id) 
                REFERENCES Asyms(pdbid,assembly_id,label_asym_id) 
                ON DELETE CASCADE
        , FOREIGN KEY (pdbid,assembly_id,ligand_asym_id) 
                REFERENCES Asyms(pdbid,assembly_id,label_asym_id) 
                ON DELETE CASCADE
        ) INHERITS (Interfaces_small);

/* other polymer ligands */
CREATE TABLE Interfaces_opoly (
        PRIMARY KEY (if_id)
        , type interface_type DEFAULT 'opoly'
        , FOREIGN KEY (pdbid,assembly_id,label_asym_id) 
                REFERENCES Asyms(pdbid,assembly_id,label_asym_id) 
                ON DELETE CASCADE
        , FOREIGN KEY (pdbid,assembly_id,ligand_asym_id) 
                REFERENCES Asyms(pdbid,assembly_id,label_asym_id) 
                ON DELETE CASCADE
        ) INHERITS (Interfaces_small);

/* pairwise protein-protein interactions */
CREATE TABLE Interfaces_ppi (
        PRIMARY KEY (if_id)
        , type interface_type DEFAULT 'ppi'
        , FOREIGN KEY (pdbid,assembly_id,label_asym_id) 
                REFERENCES Asyms(pdbid,assembly_id,label_asym_id) 
                ON DELETE CASCADE
        , FOREIGN KEY (pdbid,assembly_id,ligand_asym_id) 
                REFERENCES Asyms(pdbid,assembly_id,label_asym_id) 
                ON DELETE CASCADE
        ) INHERITS (Interfaces);


/* fold comparison */
CREATE TABLE Interfaces_fold (
        PRIMARY KEY (if_id)
        , type interface_type DEFAULT 'fold'
        , FOREIGN KEY (pdbid,assembly_id,label_asym_id) 
                REFERENCES Asyms(pdbid,assembly_id,label_asym_id) 
                ON DELETE CASCADE
        ) INHERITS (Interfaces);

CREATE TABLE Refsets_base (
        type interface_type
        , if_id TEXT NOT NULL
        , rs_id INTEGER NOT NULL
        -- origin, and x,y,z axes of local coordinate systems.
--        , frame BYTEA NOT NULL
        , frame text NOT NULL
        , ft_0 REAL NOT NULL
        , ft_1 REAL NOT NULL
        , ft_2 REAL NOT NULL
        , ft_3 REAL NOT NULL
        , ft_4 REAL NOT NULL
        , ft_5 REAL NOT NULL
        , ft_6 REAL NOT NULL
        , ft_7 REAL NOT NULL
        , ft_8 REAL NOT NULL
        , ft_9 REAL NOT NULL
        , ft_10 REAL NOT NULL
        , ft_11 REAL NOT NULL
        , ft_12 REAL NOT NULL
        , ft_13 REAL NOT NULL
        , ft_14 REAL NOT NULL
        , ft_15 REAL NOT NULL
        , ft_16 REAL NOT NULL
        , ft_17 REAL NOT NULL
        , ft_18 REAL NOT NULL
        , ft_19 REAL NOT NULL
        , ft_20 REAL NOT NULL
        , ft_21 REAL NOT NULL
        , ft_22 REAL NOT NULL
        , ft_23 REAL NOT NULL
        , ft_24 REAL NOT NULL
        , ft_25 REAL NOT NULL
        , ft_26 REAL NOT NULL
        , ft_27 REAL NOT NULL
        , ft_28 REAL NOT NULL
        , ft_29 REAL NOT NULL
        , ft_30 REAL NOT NULL
        , ft_31 REAL NOT NULL
        , ft_32 REAL NOT NULL
        , ft_33 REAL NOT NULL
        , ft_34 REAL NOT NULL
        , ft_35 REAL NOT NULL
        , ft_36 REAL NOT NULL
        , ft_37 REAL NOT NULL
        , ft_38 REAL NOT NULL
        , ft_39 REAL NOT NULL
        , ft_40 REAL NOT NULL
        , ft_41 REAL NOT NULL
        , ft_42 REAL NOT NULL
        , ft_43 REAL NOT NULL
        );

CREATE TABLE Refaco (
        lattice BIGINT[]
        , natoms INT
        , PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces(if_id) ON DELETE CASCADE
        ) INHERITS (Refsets_base);

CREATE TABLE Refaco_dnarna (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_dnarna(if_id) ON DELETE CASCADE
        ) INHERITS (Refaco);

CREATE TABLE Refaco_small (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_nonpolymer(if_id) ON DELETE CASCADE
        ) INHERITS (Refaco);

CREATE TABLE Refaco_nonpolymer (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_nonpolymer(if_id) ON DELETE CASCADE
        ) INHERITS (Refaco_small);

CREATE TABLE Refaco_peptide (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_peptide(if_id) ON DELETE CASCADE
        ) INHERITS (Refaco_small);


CREATE TABLE Refaco_opoly (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_opoly(if_id) ON DELETE CASCADE
        ) INHERITS (Refaco_small);

CREATE TABLE Refaco_ppi (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_ppi(if_id) ON DELETE CASCADE
        ) INHERITS (Refaco);

CREATE TABLE Refaco_fold (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_ppi(if_id) ON DELETE CASCADE
        ) INHERITS (Refaco);

DROP VIEW IF EXISTS Interfaces_summary;
CREATE VIEW Interfaces_summary(pdbid, assembly_id, if_id, type, title, descriptor
        , label_asym_id, auth_asym_id, entity_id, description
        , l_label_asym_id, l_auth_asym_id, l_entity_id, l_description, comp_id) 
AS
SELECT s.pdbid, o.assembly_id, o.if_id, o.type
    , s.title, s.descriptor
    , a1.label_asym_id
    , a1.auth_asym_id
    , e1.entity_id
    , e1.description
    , a2.label_asym_id
    , a2.auth_asym_id
    , e2.entity_id
    , e2.description
    , e2.comp_id
FROM Interfaces o 
JOIN Asyms a1 
        ON a1.pdbid = o.pdbid 
        AND a1.assembly_id = o.assembly_id
        AND a1.label_asym_id = o.label_asym_id
JOIN Entities e1 ON e1.pdbid = a1.pdbid
        AND e1.entity_id = a1.entity_id
JOIN Asyms a2 
        ON a2.pdbid = o.pdbid
        AND a2.assembly_id = o.assembly_id
        AND a2.label_asym_id = o.ligand_asym_id
JOIN Entities e2 ON e2.pdbid = a2.pdbid AND e2.entity_id = a2.entity_id 
JOIN Structs s ON o.pdbid = s.pdbid 
;

/* for determining the prob column of Interfaces. */
CREATE TABLE prob_base (
        if_id TEXT NOT NULL
        , p FLOAT NOT NULL
        , c DOUBLE PRECISION NOT NULL
        , n DOUBLE PRECISION NOT NULL
        );
