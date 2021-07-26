-- EXPLAIN
/*
CREATE  TABLE asym_pair_sum_nonpolymer_max1() INHERITS (asym_pair_sum);

INSERT INTO asym_pair_sum_nonpolymer_max1
SELECT o1.pdbcode, o1.label_asym_id, o2.pdbcode, o2.label_asym_id, MAX(irscore),
    1, 'nonpolymer_max1'
FROM all_vs_all_nonpolymer x
JOIN objects_nonpolymer o1 ON o1.pdbcode = x.pdbcode1 AND o1.obj_id = x.obj_id1
JOIN objects_nonpolymer o2 ON o2.pdbcode = x.pdbcode2 AND o2.obj_id = x.obj_id2
WHERE (x.pdbcode1 <> x.pdbcode2 OR x.obj_id1 <> x.obj_id2)
  AND x.irscore > 15.0
GROUP BY o1.pdbcode, o1.label_asym_id, o2.pdbcode, o2.label_asym_id
HAVING (o1.pdbcode <> o2.pdbcode OR o1.label_asym_id <> o2.label_asym_id)
;

CREATE  TABLE asym_pair_sum_ppi_max1() INHERITS (asym_pair_sum);
*/

INSERT INTO asym_pair_sum_ppi_max1
SELECT o1.pdbcode, o1.label_asym_id, o2.pdbcode, o2.label_asym_id, MAX(irscore),
    1, 'ppi_max1'
FROM all_vs_all_ppi x
JOIN objects_ppi o1 ON o1.pdbcode = x.pdbcode1 AND o1.obj_id = x.obj_id1
JOIN objects_ppi o2 ON o2.pdbcode = x.pdbcode2 AND o2.obj_id = x.obj_id2
WHERE (x.pdbcode1 <> x.pdbcode2 OR x.obj_id1 <> x.obj_id2)
  AND x.irscore > 15.0
GROUP BY o1.pdbcode, o1.label_asym_id, o2.pdbcode, o2.label_asym_id
HAVING (o1.pdbcode <> o2.pdbcode OR o1.label_asym_id <> o2.label_asym_id)
;

