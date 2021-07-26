COPY(
SELECT clust_id, COUNT(DISTINCT c.if_id), ARRAY_AGG(DISTINCT comp_id), COUNT(DISTINCT comp_id)
FROM clusters_nonpolymer c
JOIN interfaces_nonpolymer i ON c.if_id = i.if_id
JOIN asyms a ON a.pdbid = i.pdbid 
            AND a.assembly_id = i.assembly_id
            AND a.label_asym_id = i.ligand_asym_id
JOIN entities e ON e.pdbid = a.pdbid AND e.entity_id = a.entity_id
GROUP BY clust_id
HAVING COUNT(DISTINCT c.if_id) >= 10
ORDER BY COUNT(DISTINCT c.if_id) DESC
) TO STDOUT;