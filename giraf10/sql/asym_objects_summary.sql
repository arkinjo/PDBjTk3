DROP TABLE IF EXISTS asym_objects_summary;
CREATE TABLE asym_objects_summary (
        pdbid TEXT NOT NULL
        , assembly_id TEXT NOT NULL
        , label_asym_id TEXT NOT NULL
        , auth_asym_id TEXT NOT NULL
        , title TEXT 
        , descriptor TEXT
        , entity_id TEXT NOT NULL
        , description TEXT 
        , comp_id TEXT
        , l_description TEXT
        , ligand_asym_ids TEXT
        , PRIMARY KEY (pdbid, assembly_id, label_asym_id)
        );

INSERT INTO asym_objects_summary
SELECT s.pdbid, a.assembly_id, a.label_asym_id, a.auth_asym_id
    , s.title, s.descriptor
    , e.entity_id, e.description
    , (SELECT array_to_string(array_agg(e2.comp_id), ' ')
        FROM entities e2
        JOIN asyms a2 
                ON a2.pdbid = e2.pdbid 
                AND a2.entity_id = e2.entity_id
        JOIN interfaces o2 
                ON o2.pdbid = a.pdbid 
                AND o2.assembly_id = a.assembly_id
                AND o2.label_asym_id = a.label_asym_id
                AND a2.label_asym_id = o2.ligand_asym_id
                AND o2.type IN ('nonpolymer','ppi','dnarna')
        WHERE e2.pdbid = s.pdbid) AS comp_id
    , (SELECT array_to_string(array_agg(e2.description), ' ')
        FROM entities e2
        JOIN asyms a2 
                ON a2.pdbid = e2.pdbid AND a2.entity_id = e2.entity_id
        JOIN interfaces o2 
                ON o2.pdbid = a.pdbid 
                AND o2.assembly_id = a.assembly_id
                AND o2.label_asym_id = a.label_asym_id
                AND a2.label_asym_id = o2.ligand_asym_id
                AND o2.type IN ('nonpolymer','ppi','dnarna')
        WHERE e2.pdbid = s.pdbid) AS l_description
    , (SELECT array_to_string(array_agg(a2.label_asym_id), ',')
        FROM entities e2
        JOIN asyms a2 
                ON a2.pdbid = e2.pdbid AND a2.entity_id = e2.entity_id
        JOIN interfaces o2 
                ON o2.pdbid = a.pdbid 
                AND o2.assembly_id = a.assembly_id
                AND o2.label_asym_id = a.label_asym_id
                AND a2.label_asym_id = o2.ligand_asym_id
                AND o2.type IN ('nonpolymer','ppi','dnarna')
        WHERE e2.pdbid = s.pdbid) AS ligand_asym_ids
FROM structs s
JOIN entities e ON e.pdbid = s.pdbid
JOIN asyms a ON a.pdbid = e.pdbid AND a.entity_id = e.entity_id
WHERE EXISTS(SELECT *
    FROM interfaces o
    WHERE o.pdbid = a.pdbid and o.label_asym_id = a.label_asym_id
    AND o.type IN ('nonpolymer', 'ppi', 'dnarna'))
;

CREATE TABLE cluster_summary (
        clust_id TEXT NOT NULL
        , itype TEXT NOT NULL
        , nmember INTEGER NOT NULL
        , labels TEXT[]
        );

CREATE TABLE blast_cluster_summary (PRIMARY KEY(clust_id)) 
        INHERITS(cluster_summary);

CREATE TABLE motif_cluster_summary (PRIMARY KEY(clust_id)) 
        INHERITS(cluster_summary);

CREATE TABLE cluster_summary_nonpolymer(PRIMARY KEY(clust_id))
	INHERITS(cluster_summary);
CREATE TABLE cluster_summary_ppi(PRIMARY KEY(clust_id))
	INHERITS(cluster_summary);
CREATE TABLE cluster_summary_dnarna(PRIMARY KEY(clust_id))
	INHERITS(cluster_summary);

CREATE TABLE cmotif_summary (
       cmotif TEXT[] PRIMARY KEY
       , nmember INTEGER NOT NULL
       , labels TEXT[]
       );
