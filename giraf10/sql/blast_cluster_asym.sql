INSERT INTO blast_cluster_asym
SELECT b.sclust, b.cclust, a.pdbcode, a.label_asym_id, b.nedge
FROM blast_cluster b
JOIN asyms a ON a.pdbcode = b.pdbcode AND a.entity_id = b.entity_id
;

DELETE FROM blast_cluster_asym b
WHERE NOT EXISTS(SELECT *
    FROM objects o
    WHERE o.pdbcode = b.pdbcode AND o.label_asym_id = b.label_asym_id
    AND o.type in ('nonpolymer', 'ppi', 'dnarna'))
OR EXISTS(SELECT *
    FROM objects_peptide o
    WHERE o.pdbcode = b.pdbcode AND ARRAY[b.label_asym_id] <@ o.ligand_asym_ids)
;

DELETE FROM blast_cluster_asym_40 b
WHERE NOT EXISTS(SELECT *
    FROM objects o
    WHERE o.pdbcode = b.pdbcode AND o.label_asym_id = b.label_asym_id
    AND o.type in ('nonpolymer', 'ppi', 'dnarna'))
OR EXISTS(SELECT *
    FROM objects_peptide o
    WHERE o.pdbcode = b.pdbcode AND ARRAY[b.label_asym_id] <@ o.ligand_asym_ids)
;

DELETE FROM blast_cluster_asym_90 b
WHERE NOT EXISTS(SELECT *
    FROM objects o
    WHERE o.pdbcode = b.pdbcode AND o.label_asym_id = b.label_asym_id
    AND o.type in ('nonpolymer', 'ppi', 'dnarna'))
OR EXISTS(SELECT *
    FROM objects_peptide o
    WHERE o.pdbcode = b.pdbcode AND ARRAY[b.label_asym_id] <@ o.ligand_asym_ids)
;


SELECT * FROM blast_cluster_asym_90 b
WHERE /*NOT EXISTS(SELECT *
    FROM objects o
    WHERE o.pdbcode = b.pdbcode AND o.label_asym_id = b.label_asym_id
    AND o.type in ('nonpolymer', 'ppi', 'dnarna'))
OR */EXISTS(SELECT *
    FROM objects_peptide o
    WHERE o.pdbcode = b.pdbcode AND ARRAY[b.label_asym_id] <@ o.ligand_asym_ids)
;


/*
SELECT count(*)
FROM asyms a
WHERE EXISTS(SELECT *
    FROM objects o
    WHERE o.pdbcode = a.pdbcode AND o.label_asym_id = a.label_asym_id
    AND o.type in ('nonpolymer', 'ppi', 'dnarna'))
AND NOT EXISTS(SELECT *
    FROM objects_peptide o
    WHERE o.pdbcode = a.pdbcode AND ARRAY[a.label_asym_id] <@ o.ligand_asym_ids)
;

*/