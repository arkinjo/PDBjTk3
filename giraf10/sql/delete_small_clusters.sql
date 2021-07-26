CREATE INDEX clusters_nonpolymer_i ON clusters_nonpolymer(itype, clust_id);
CREATE INDEX clusters_ppi_i ON clusters_ppi(itype, clust_id);
CREATE INDEX clusters_dnarna_i ON clusters_dnarna(itype, clust_id);

CREATE TEMP TABLE _small_clusters AS
SELECT itype, clust_id
FROM clusters
GROUP BY itype, clust_id
HAVING COUNT(*) < 10
;

CREATE INDEX _small_clusters_i ON _small_clusters(itype,clust_id);
       
-- EXPLAIN
DELETE FROM clusters c
WHERE EXISTS(SELECT *
              FROM _small_clusters d
              WHERE d.itype = c.itype AND d.clust_id = c.clust_id)
;