/*
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
JOIN Structs s ON o.pdbid = s.pdbid 
JOIN Asyms a1 
        ON a1.pdbid = s.pdbid 
        AND a1.assembly_id = o.assembly_id
        AND a1.label_asym_id = o.label_asym_id
JOIN Entities e1 ON e1.pdbid = a1.pdbid
        AND e1.entity_id = a1.entity_id
JOIN Asyms a2 
        ON a2.pdbid = s.pdbid
        AND a2.assembly_id = o.assembly_id
        AND a2.label_asym_id = o.ligand_asym_id
JOIN Entities e2 ON e2.pdbid = a2.pdbid AND e2.entity_id = a2.entity_id 
WHERE o.if_id = '101M:1:A-1:C-1'
;
*/

DROP TABLE IF EXISTS interfaces_summary_mat;
CREATE TABLE interfaces_summary_mat AS
SELECT * FROM interfaces_summary;
CREATE INDEX interfaces_summary_mat_key ON interfaces_summary_mat(if_id);

