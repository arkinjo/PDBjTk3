DROP TABLE IF EXISTS cmotif_chain;

CREATE TABLE cmotif_chain (
       pdbid TEXT NOT NULL
       , assembly_id TEXT NOT NULL
       , label_asym_id TEXT NOT NULL
       , auth_asym_id TEXT NOT NULL
       , entity_id TEXT NOT NULL
       , description TEXT NOT NULL
       , acc TEXT
       , cmotif TEXT[] NOT NULL
       , keywords TEXT[]
       , kw_cc TEXT[]
       , kw_mf TEXT[]
       , kw_bp TEXT[]
       , kw_ligand TEXT[]
       , kw_disease TEXT[]
       , PRIMARY KEY(pdbid, assembly_id, label_asym_id, acc)
       );

INSERT INTO cmotif_chain(pdbid,assembly_id,label_asym_id,auth_asym_id,entity_id,description,acc,cmotif,keywords)
SELECT c.pdbid, c.assembly_id, c.label_asym_id, a.auth_asym_id
       , a.entity_id, a.description
       , r.acc , c.cmotif, r.keywords
FROM asyms_composite_motifs c
JOIN asym_objects_summary a 
     ON a.pdbid = c.pdbid AND a.assembly_id = c.assembly_id 
     AND a.label_asym_id = c.label_asym_id
JOIN uniprot.PDBref r 
     ON  r.pdbid = a.pdbid
     AND substring(a.auth_asym_id from '#"%#"-%' for '#') = ANY(r.chains)
;

CREATE INDEX cmotif_chain_kw ON cmotif_chain USING GIN(keywords);

CREATE INDEX uniprot_keywords_acc ON uniprot.keywords(acc);

CREATE TEMP TABLE __ca_compotif(pdbid, assembly_id, label_asym_id, acc, ca, kw) AS
SELECT c.pdbid, c.assembly_id, c.label_asym_id
      , c.acc, k.ca, k.id
FROM cmotif_chain c
JOIN uniprot.keywords k ON k.acc = ANY(c.keywords)
;

CREATE INDEX __ca_ind ON __ca_compotif(pdbid, assembly_id, label_asym_id, acc, ca);

UPDATE cmotif_chain c
SET kw_cc = (SELECT ARRAY_AGG(kw)
             FROM __ca_compotif a
             WHERE a.pdbid = c.pdbid AND a.assembly_id = c.assembly_id
             AND   a.label_asym_id = c.label_asym_id
             AND   a.acc = c.acc
             AND   a.ca = 'Cellular component')
   ,kw_bp = (SELECT ARRAY_AGG(kw)
             FROM __ca_compotif a
             WHERE a.pdbid = c.pdbid AND a.assembly_id = c.assembly_id
             AND   a.label_asym_id = c.label_asym_id
             AND   a.acc = c.acc
             AND   a.ca = 'Biological process')
   ,kw_mf = (SELECT ARRAY_AGG(kw)
             FROM __ca_compotif a
             WHERE a.pdbid = c.pdbid AND a.assembly_id = c.assembly_id
             AND   a.label_asym_id = c.label_asym_id
             AND   a.acc = c.acc
             AND   a.ca = 'Molecular function')
   ,kw_ligand = (SELECT ARRAY_AGG(kw)
             FROM __ca_compotif a
             WHERE a.pdbid = c.pdbid AND a.assembly_id = c.assembly_id
             AND   a.label_asym_id = c.label_asym_id
             AND   a.acc = c.acc
             AND   a.ca = 'Ligand')
   ,kw_disease = (SELECT ARRAY_AGG(kw)
             FROM __ca_compotif a
             WHERE a.pdbid = c.pdbid AND a.assembly_id = c.assembly_id
             AND   a.label_asym_id = c.label_asym_id
             AND   a.acc = c.acc
             AND   a.ca = 'Disease')
;
