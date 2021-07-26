\timing
DROP TABLE IF EXISTS ientity;

CREATE TEMP TABLE ientity (
        pdbcode INTEGER NOT NULL
        , entity_id TEXT NOT NULL
        , PRIMARY KEY (pdbcode,entity_id));

INSERT INTO ientity
SELECT a.pdbcode , a.entity_id
FROM objects_nonpolymer m
JOIN asyms a ON a.pdbcode = m.pdbcode AND a.label_asym_id = m.label_asym_id
UNION
SELECT a.pdbcode , a.entity_id
FROM objects_ppi m
JOIN asyms a ON a.pdbcode = m.pdbcode AND a.label_asym_id = m.label_asym_id
UNION
SELECT a.pdbcode , a.entity_id
FROM objects_dnarna m
JOIN asyms a ON a.pdbcode = m.pdbcode AND a.label_asym_id = m.label_asym_id
;

CREATE INDEX blast_pair_p1 ON blast_pair(pdbcode1, entity_id1);
CREATE INDEX blast_pair_p2 ON blast_pair(pdbcode2, entity_id2);
CREATE INDEX blast_pair_pp ON blast_pair(pdbcode1, entity_id1, pdbcode2, entity_id2);
CREATE INDEX blast_pair_score ON blast_pair(score,qstart,sstart);

DELETE FROM blast_pair b
WHERE NOT EXISTS(
    SELECT * FROM ientity i
    WHERE b.pdbcode1 = i.pdbcode AND b.entity_id1 = i.entity_id)
OR NOT EXISTS(
    SELECT * FROM ientity i
    WHERE b.pdbcode2 = i.pdbcode AND b.entity_id2 = i.entity_id)
;

DROP TABLE IF EXISTS blast_pair_sum;

CREATE TABLE blast_pair_sum (
        pdbcode1 INTEGER NOT NULL
        , entity_id1 TEXT NOT NULL
        , pdbcode2 INTEGER NOT NULL
        , entity_id2 TEXT NOT NULL
        , pidentity FLOAT NOT NULL
        , alen INTEGER NOT NULL
        , score DOUBLE PRECISION NOT NULL
        , cnt BIGINT NOT NULL
        , PRIMARY KEY (pdbcode1, entity_id1, pdbcode2, entity_id2)
        );

INSERT INTO blast_pair_sum
SELECT pdbcode1, entity_id1, pdbcode2, entity_id2
    , SUM(alen * pidentity) / SUM(alen) AS pidentity
    , SUM(alen) AS alen
    , SUM(score) AS score
    , COUNT(*) AS cnt
FROM blast_pair p1
WHERE p1.score = (SELECT MAX(score)
    FROM blast_pair p2
    WHERE p1.pdbcode1 = p2.pdbcode1 AND p1.entity_id1 = p2.entity_id1 
      AND p2.pdbcode2 = p1.pdbcode2 AND p2.entity_id2 = p1.entity_id2
      AND ((p2.qstart BETWEEN p1.qstart AND p1.qend)
       OR  (p2.qend BETWEEN p1.qstart AND p1.qend)
       OR  (p2.sstart BETWEEN p1.sstart AND p1.send)
       OR  (p2.send BETWEEN p1.sstart AND p1.send)))
AND p1.qstart = (SELECT MIN(qstart)
    FROM blast_pair p2
    WHERE p1.pdbcode1 = p2.pdbcode1 AND p1.entity_id1 = p2.entity_id1
      AND p2.pdbcode2 = p1.pdbcode2 AND p2.entity_id2 = p1.entity_id2
      AND ((p2.qstart BETWEEN p1.qstart AND p1.qend)
       OR  (p2.qend BETWEEN p1.qstart AND p1.qend)
       OR  (p2.sstart BETWEEN p1.sstart AND p1.send)
       OR  (p2.send BETWEEN p1.sstart AND p1.send))
      AND p2.score = p1.score)
AND p1.sstart = (SELECT MIN(sstart)
    FROM blast_pair p2
    WHERE p1.pdbcode1 = p2.pdbcode1 AND p1.entity_id1 = p2.entity_id1
      AND p2.pdbcode2 = p1.pdbcode2 AND p2.entity_id2 = p1.entity_id2
      AND ((p2.qstart BETWEEN p1.qstart AND p1.qend)
       OR  (p2.qend BETWEEN p1.qstart AND p1.qend)
       OR  (p2.sstart BETWEEN p1.sstart AND p1.send)
       OR  (p2.send BETWEEN p1.sstart AND p1.send))
      AND p2.score = p1.score
      AND p2.qstart = p1.qstart)
GROUP BY pdbcode1,entity_id1,pdbcode2,entity_id2
--ORDER BY score DESC, cnt DESC
;

\echo deleting from blast_pair_sum

DELETE FROM blast_pair_sum b
WHERE EXISTS(SELECT * FROM blast_pair_sum
    WHERE pdbcode1 = b.pdbcode2 AND entity_id1 = b.entity_id2
    AND   pdbcode2 = b.pdbcode1 AND entity_id2 = b.entity_id1
    AND   score > b.score)
;

DROP TABLE IF EXISTS blast_pair_sum_asym; 

CREATE TABLE blast_pair_sum_asym (
        pdbcode1 INTEGER NOT NULL
        , label_asym_id1 TEXT NOT NULL
        , pdbcode2 INTEGER NOT NULL
        , label_asym_id2 TEXT NOT NULL
        , score DOUBLE PRECISION NOT NULL
        , cnt INTEGER NOT NULL
        , alen INTEGER NOT NULL
        , pidentity DOUBLE PRECISION NOT NULL
        , PRIMARY KEY (pdbcode1, label_asym_id1, pdbcode2, label_asym_id2)
        );

INSERT INTO blast_pair_sum_asym
SELECT pdbcode1, a1.label_asym_id as label_asym_id1
    , pdbcode2, a2.label_asym_id as label_asym_id2
    , score
    , cnt
    , alen
    , pidentity
FROM blast_pair_sum b
JOIN asyms a1 ON a1.pdbcode = b.pdbcode1 AND a1.entity_id = b.entity_id1
JOIN asyms a2 ON a2.pdbcode = b.pdbcode2 AND a2.entity_id = b.entity_id2
UNION
SELECT pdbcode2 as pdbcode1, a2.label_asym_id as label_asym_id1
    , pdbcode1 as pdbcode2, a1.label_asym_id as label_asym_id2
    , score
    , cnt
    , alen
    , pidentity
FROM blast_pair_sum b
JOIN asyms a1 ON a1.pdbcode = b.pdbcode1 AND a1.entity_id = b.entity_id1
JOIN asyms a2 ON a2.pdbcode = b.pdbcode2 AND a2.entity_id = b.entity_id2
;

DELETE FROM blast_pair_sum_asym b
WHERE EXISTS(SELECT *
    FROM objects_peptide p
    WHERE 
       (p.pdbcode = b.pdbcode1 AND b.label_asym_id1 = ANY(p.ligand_asym_ids))
    OR (p.pdbcode = b.pdbcode2 AND b.label_asym_id2 = ANY(p.ligand_asym_ids))
        )
;

CREATE INDEX blast_pair_sum_asym_pid ON blast_pair_sum_asym(pidentity);
CREATE INDEX blast_pair_sum_asym_1 ON blast_pair_sum_asym(pdbcode1, label_asym_id1);
CREATE INDEX blast_pair_sum_asym_2 ON blast_pair_sum_asym(pdbcode2, label_asym_id2);
