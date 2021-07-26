CREATE SCHEMA uniprot;

CREATE TABLE uniprot.UniProt (
       acc TEXT PRIMARY KEY
       , name TEXT NOT NULL
       , modified DATE NOT NULL
       , created DATE NOT NULL
       , doc XML NOT NULL
);

CREATE TABLE uniprot.PDBref (
       acc TEXT NOT NULL 
       , doc XML NOT NULL
       , PDBID TEXT
       , CHAINS TEXT[]
       , keywords TEXT[]
);

CREATE TABLE uniprot.Keywords (
        id TEXT PRIMARY KEY
        , acc TEXT NOT NULL
        , ca TEXT NOT NULL
        , hi TEXT []
        );
