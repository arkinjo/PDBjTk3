CREATE TABLE kcombu_pairs (
       comp_id1 TEXT NOT NULL
       , comp_id2 TEXT NOT NULL
       , tanimoto  FLOAT NOT NULL
       , PRIMARY KEY (comp_id1,comp_id2)
       );


SELECT x.*, k.*
FROM all_vs_all_nonpolymer x
JOIN interfaces_nonpolymer i1 ON i1.if_id = x.if_id1
JOIN interfaces_nonpolymer i2 ON i2.if_id = x.if_id2
JOIN asyms a1 ON a1.pdbid = i1.pdbid AND a1.assembly_id = i1.assembly_id
     AND a1.label_asym_id = i1.ligand_asym_id
JOIN asyms a2 ON a2.pdbid = i2.pdbid AND a2.assembly_id = i2.assembly_id
     AND a2.label_asym_id = i2.ligand_asym_id
JOIN entities e1 ON e1.pdbid = a1.pdbid AND e1.entity_id = a1.entity_id
JOIN entities e2 ON e2.pdbid = a2.pdbid AND e2.entity_id = a2.entity_id
JOIN kcombu_pairs k 
  ON (k.comp_id1 = e1.comp_id AND k.comp_id2 = e2.comp_id)
  OR (k.comp_id1 = e2.comp_id AND k.comp_id2 = e1.comp_id)