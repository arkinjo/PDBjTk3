CREATE INDEX uniprot_pdbref_pdbid ON uniprot.pdbref(pdbid);

SELECT a.pdbid, a.label_asym_id, r.acc, r.keywords
FROM asyms a
JOIN uniprot.PDBref r 
     ON  r.pdbid = a.pdbid
     AND substring(a.auth_asym_id from '#"%#"-%' for '#') = ANY(r.chains)
;


