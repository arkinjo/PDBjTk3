-- DROP TABLE IF EXISTS blast_pair CASCADE;

CREATE TABLE blast_pair(
        pdbcode1 INTEGER NOT NULL
        , entity_id1 TEXT NOT NULL
        , pdbcode2 INTEGER NOT NULL
        , entity_id2 TEXT NOT NULL
        , pidentity FLOAT NOT NULL
        , alen INTEGER NOT NULL
        , mismatch INTEGER NOT NULL
        , gap_open INTEGER NOT NULL
        , qstart INTEGER NOT NULL
        , qend INTEGER NOT NULL
        , sstart INTEGER NOT NULL
        , send INTEGER NOT NULL
        , evalue DOUBLE PRECISION NOT NULL
        , score DOUBLE PRECISION NOT NULL
        );

CREATE TABLE blast_cluster (
        sclust INTEGER NOT NULL
        , cclust INTEGER NOT NULL
        , pdbcode INTEGER NOT NULL
        , entity_id TEXT NOT NULL
        , nedge INTEGER NOT NULL
        );

CREATE INDEX blast_cluster_cid ON blast_cluster(sclust,cclust);
CREATE INDEX blast_cluster_ent ON blast_cluster(pdbcode,entity_id);

CREATE TABLE blast_cluster_asym (
        sclust INTEGER NOT NULL
        , cclust INTEGER NOT NULL
        , pdbcode INTEGER NOT NULL
        , label_asym_id TEXT NOT NULL
        , nedge INTEGER NOT NULL
        );

CREATE INDEX blast_cluster_asym_cid ON blast_cluster_asym(sclust,cclust);
CREATE INDEX blast_cluster_asym_ent ON blast_cluster_asym(pdbcode,label_asym_id);

CREATE TABLE blast_cluster_asym_10 (LIKE blast_cluster_asym);
CREATE INDEX blast_cluster_asym_10_cid ON blast_cluster_asym_10(sclust,cclust);
CREATE INDEX blast_cluster_asym_10_ent ON blast_cluster_asym_10(pdbcode,label_asym_id);

CREATE TABLE blast_cluster_asym_15 (LIKE blast_cluster_asym);
CREATE INDEX blast_cluster_asym_15_cid ON blast_cluster_asym_15(sclust,cclust);
CREATE INDEX blast_cluster_asym_15_ent ON blast_cluster_asym_15(pdbcode,label_asym_id);

CREATE TABLE blast_cluster_asym_20 (LIKE blast_cluster_asym);
CREATE INDEX blast_cluster_asym_20_cid ON blast_cluster_asym_20(sclust,cclust);
CREATE INDEX blast_cluster_asym_20_ent ON blast_cluster_asym_20(pdbcode,label_asym_id);

CREATE TABLE blast_cluster_asym_25 (LIKE blast_cluster_asym);
CREATE INDEX blast_cluster_asym_25_cid ON blast_cluster_asym_25(sclust,cclust);
CREATE INDEX blast_cluster_asym_25_ent ON blast_cluster_asym_25(pdbcode,label_asym_id);

CREATE TABLE blast_cluster_asym_30 (LIKE blast_cluster_asym);
CREATE INDEX blast_cluster_asym_30_cid ON blast_cluster_asym_30(sclust,cclust);
CREATE INDEX blast_cluster_asym_30_ent ON blast_cluster_asym_30(pdbcode,label_asym_id);

CREATE TABLE blast_cluster_asym_35 (LIKE blast_cluster_asym);
CREATE INDEX blast_cluster_asym_35_cid ON blast_cluster_asym_35(sclust,cclust);
CREATE INDEX blast_cluster_asym_35_ent ON blast_cluster_asym_35(pdbcode,label_asym_id);

CREATE TABLE blast_cluster_asym_40 (LIKE blast_cluster_asym);
CREATE INDEX blast_cluster_asym_40_cid ON blast_cluster_asym_40(sclust,cclust);
CREATE INDEX blast_cluster_asym_40_ent ON blast_cluster_asym_40(pdbcode,label_asym_id);

CREATE TABLE blast_cluster_asym_45 (LIKE blast_cluster_asym);
CREATE INDEX blast_cluster_asym_45_cid ON blast_cluster_asym_45(sclust,cclust);
CREATE INDEX blast_cluster_asym_45_ent ON blast_cluster_asym_45(pdbcode,label_asym_id);

CREATE TABLE blast_cluster_asym_50 (LIKE blast_cluster_asym);
CREATE INDEX blast_cluster_asym_50_cid ON blast_cluster_asym_50(sclust,cclust);
CREATE INDEX blast_cluster_asym_50_ent ON blast_cluster_asym_50(pdbcode,label_asym_id);

CREATE TABLE blast_cluster_asym_55 (LIKE blast_cluster_asym);
CREATE INDEX blast_cluster_asym_55_cid ON blast_cluster_asym_55(sclust,cclust);
CREATE INDEX blast_cluster_asym_55_ent ON blast_cluster_asym_55(pdbcode,label_asym_id);

CREATE TABLE blast_cluster_asym_60 (LIKE blast_cluster_asym);
CREATE INDEX blast_cluster_asym_60_cid ON blast_cluster_asym_60(sclust,cclust);
CREATE INDEX blast_cluster_asym_60_ent ON blast_cluster_asym_60(pdbcode,label_asym_id);

CREATE TABLE blast_cluster_asym_65 (LIKE blast_cluster_asym);
CREATE INDEX blast_cluster_asym_65_cid ON blast_cluster_asym_65(sclust,cclust);
CREATE INDEX blast_cluster_asym_65_ent ON blast_cluster_asym_65(pdbcode,label_asym_id);

CREATE TABLE blast_cluster_asym_70 (LIKE blast_cluster_asym);
CREATE INDEX blast_cluster_asym_70_cid ON blast_cluster_asym_70(sclust,cclust);
CREATE INDEX blast_cluster_asym_70_ent ON blast_cluster_asym_70(pdbcode,label_asym_id);

CREATE TABLE blast_cluster_asym_75 (LIKE blast_cluster_asym);
CREATE INDEX blast_cluster_asym_75_cid ON blast_cluster_asym_75(sclust,cclust);
CREATE INDEX blast_cluster_asym_75_ent ON blast_cluster_asym_75(pdbcode,label_asym_id);

CREATE TABLE blast_cluster_asym_80 (LIKE blast_cluster_asym);
CREATE INDEX blast_cluster_asym_80_cid ON blast_cluster_asym_80(sclust,cclust);
CREATE INDEX blast_cluster_asym_80_ent ON blast_cluster_asym_80(pdbcode,label_asym_id);

CREATE TABLE blast_cluster_asym_85 (LIKE blast_cluster_asym);
CREATE INDEX blast_cluster_asym_85_cid ON blast_cluster_asym_85(sclust,cclust);
CREATE INDEX blast_cluster_asym_85_ent ON blast_cluster_asym_85(pdbcode,label_asym_id);

CREATE TABLE blast_cluster_asym_90 (LIKE blast_cluster_asym);
CREATE INDEX blast_cluster_asym_90_cid ON blast_cluster_asym_90(sclust,cclust);
CREATE INDEX blast_cluster_asym_90_ent ON blast_cluster_asym_90(pdbcode,label_asym_id);

CREATE TABLE blast_cluster_asym_95 (LIKE blast_cluster_asym);
CREATE INDEX blast_cluster_asym_95_cid ON blast_cluster_asym_95(sclust,cclust);
CREATE INDEX blast_cluster_asym_95_ent ON blast_cluster_asym_95(pdbcode,label_asym_id);

CREATE TABLE blast_cluster_asym_100 (LIKE blast_cluster_asym);
CREATE INDEX blast_cluster_asym_100_cid ON blast_cluster_asym_100(sclust,cclust);
CREATE INDEX blast_cluster_asym_100_ent ON blast_cluster_asym_100(pdbcode,label_asym_id);
