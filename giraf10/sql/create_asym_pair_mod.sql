DROP TABLE asym_pair_sum_mod;

CREATE TABLE asym_pair_sum_mod (
	PRIMARY KEY(pdbcode1,label_asym_id1,pdbcode2,label_asym_id2)
	) INHERITS (asym_pair_sum);

INSERT INTO asym_pair_sum_mod(pdbcode1, label_asym_id1
    , pdbcode2, label_asym_id2 
    , score, npair, type)
SELECT pdbcode1, label_asym_id1, pdbcode2, label_asym_id2 
    , SUM(score), SUM(npair), 'all+seq'
FROM (
SELECT 
    CASE WHEN pdbcode1 < pdbcode2 THEN pdbcode1
    WHEN pdbcode1 > pdbcode2 THEN pdbcode2
    WHEN label_asym_id1 <= label_asym_id2 THEN pdbcode1
    ELSE pdbcode2 END
    ,
    CASE WHEN pdbcode1 < pdbcode2 THEN label_asym_id1
    WHEN pdbcode1 > pdbcode2 THEN label_asym_id2
    WHEN label_asym_id1 <= label_asym_id2 THEN label_asym_id1
    ELSE label_asym_id2 END
    ,
    CASE WHEN pdbcode1 < pdbcode2 THEN pdbcode2
    WHEN pdbcode1 > pdbcode2 THEN pdbcode1
    WHEN label_asym_id1 <= label_asym_id2 THEN pdbcode2
    ELSE pdbcode1 END
    , 
    CASE WHEN pdbcode1 < pdbcode2 THEN label_asym_id2
    WHEN pdbcode1 > pdbcode2 THEN label_asym_id1
    WHEN label_asym_id1 <= label_asym_id2 THEN label_asym_id2
    ELSE label_asym_id1 END
    , score, npair
FROM asym_pair_sum_nonpolymer_ppi_dnarna 
WHERE (pdbcode1 <> pdbcode2 OR label_asym_id1 <> label_asym_id2)
UNION ALL 
SELECT 
    CASE WHEN pdbcode1 < pdbcode2 THEN pdbcode1
    WHEN pdbcode1 > pdbcode2 THEN pdbcode2
    WHEN label_asym_id1 <= label_asym_id2 THEN pdbcode1
    ELSE pdbcode2 END
    ,
    CASE WHEN pdbcode1 < pdbcode2 THEN label_asym_id1
    WHEN pdbcode1 > pdbcode2 THEN label_asym_id2
    WHEN label_asym_id1 <= label_asym_id2 THEN label_asym_id1
    ELSE label_asym_id2 END
    ,
    CASE WHEN pdbcode1 < pdbcode2 THEN pdbcode2
    WHEN pdbcode1 > pdbcode2 THEN pdbcode1
    WHEN label_asym_id1 <= label_asym_id2 THEN pdbcode2
    ELSE pdbcode1 END
    , 
    CASE WHEN pdbcode1 < pdbcode2 THEN label_asym_id2
    WHEN pdbcode1 > pdbcode2 THEN label_asym_id1
    WHEN label_asym_id1 <= label_asym_id2 THEN label_asym_id2
    ELSE label_asym_id1 END
    , score , 0
FROM blast_pair_sum_asym
WHERE pidentity > 99.0
AND (pdbcode1 <> pdbcode2 OR label_asym_id1 <> label_asym_id2)
) AS t(pdbcode1, label_asym_id1, pdbcode2, label_asym_id2 , score, npair)
GROUP BY pdbcode1, label_asym_id1, pdbcode2, label_asym_id2
;





