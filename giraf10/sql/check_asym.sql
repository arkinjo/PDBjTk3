SELECT x.*
FROM asym_pair_sum ax 
JOIN objects_nonpolymer o1 
        ON o1.pdbcode = ax.pdbcode1 AND o1.label_asym_id = ax.label_asym_id1
JOIN objects_nonpolymer o2 
        ON o2.pdbcode = ax.pdbcode2 AND o2.label_asym_id = ax.label_asym_id2
JOIN all_vs_all_nonpolymer x
        ON x.pdbcode1 = o1.pdbcode AND x.obj_id1 = o1.obj_id
        AND x.pdbcode2 = o2.pdbcode AND x.obj_id2 = o2.obj_id
WHERE ax.score = 0
AND o1.pdbcode = 48340 AND o1.label_asym_id = 'B'
AND o2.pdbcode = 51464 AND o2.label_asym_id = 'C'
;