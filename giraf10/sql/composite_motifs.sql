DROP TABLE IF EXISTS asyms_composite_motifs;
CREATE TABLE asyms_composite_motifs (pdbid, assembly_id, label_asym_id, cmotif)
AS
WITH perchain AS (
SELECT i.pdbid, i.assembly_id, i.label_asym_id, 'N' || c.clust_id AS clust_id
FROM interfaces i
JOIN clusters_nonpolymer c ON c.if_id = i.if_id
     UNION ALL
SELECT i.pdbid, i.assembly_id, i.label_asym_id, 'P' || c.clust_id AS clust_id
FROM interfaces i
JOIN clusters_ppi c ON c.if_id = i.if_id
     UNION ALL
SELECT i.pdbid, i.assembly_id, i.label_asym_id, 'A' || c.clust_id AS clust_id
FROM interfaces i
JOIN clusters_dnarna c ON c.if_id = i.if_id
)
SELECT pdbid, assembly_id, label_asym_id, array_order(ARRAY_AGG(clust_id))
FROM perchain
GROUP BY pdbid, assembly_id, label_asym_id
;

CREATE INDEX asyms_composite_motifs_id ON asyms_composite_motifs(pdbid, assembly_id, label_asym_id);
CREATE INDEX asyms_composite_motifs_cm ON asyms_composite_motifs USING GIN(cmotif);

DELETE FROM asyms_composite_motifs a
WHERE 10 > (SELECT COUNT(*)
            FROM asyms_composite_motifs b
            WHERE b.cmotif = a.cmotif)
;