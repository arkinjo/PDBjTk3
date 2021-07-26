CREATE TABLE motif_cluster (
	sclust INTEGER NOT NULL
	, cclust INTEGER NOT NULL
	, pdbcode INTEGER NOT NULL
	, label_asym_id TEXT NOT NULL
	, nedges INTEGER NOT NULL
	, type TEXT NOT NULL
	);

CREATE TABLE motif_cluster_nonpolymer() INHERITS(motif_cluster);
CREATE TABLE motif_cluster_ppi() INHERITS(motif_cluster);
CREATE TABLE motif_cluster_dnarna() INHERITS(motif_cluster);

CREATE TABLE motif_cluster_nonpolymer_ppi() INHERITS(motif_cluster);
CREATE TABLE motif_cluster_nonpolymer_ppi_dnarna() INHERITS(motif_cluster);
CREATE TABLE motif_cluster_all_seq() INHERITS(motif_cluster);

CREATE INDEX motif_cluster_nonpolymer_ppi_dnarna_cid 
        ON motif_cluster_nonpolymer_ppi_dnarna(sclust,cclust);

CREATE INDEX motif_cluster_nonpolymer_ppi_dnarna_p
        ON motif_cluster_nonpolymer_ppi_dnarna(pdbcode,label_asym_id);

CREATE INDEX motif_cluster_all_seqcid 
        ON motif_cluster_all_seq(sclust,cclust);
CREATE INDEX motif_cluster_all_seq_p
        ON motif_cluster_all_seq(pdbcode,label_asym_id);
