CREATE INDEX uniprot_sec_sec ON Uniprot_sec (sec_ac);
CREATE INDEX interfaces_nonpolymer_pdbid ON interfaces_nonpolymer(pdbid);
CREATE INDEX interfaces_ppi_pdbid ON interfaces_ppi(pdbid);
CREATE INDEX interfaces_dnarna_pdbid ON interfaces_dnarna(pdbid);
CREATE INDEX entities_id ON entities(pdbid, entity_id);
CREATE INDEX asyms_id ON asyms(pdbid, assembly_id, label_asym_id);
