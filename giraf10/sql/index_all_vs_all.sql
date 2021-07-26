CREATE INDEX all_vs_all_nonpolymer_score ON all_vs_all_nonpolymer(irscore);
CREATE INDEX all_vs_all_dnarna_score ON all_vs_all_dnarna(irscore);
CREATE INDEX all_vs_all_ppi_score ON all_vs_all_ppi(irscore);

CREATE INDEX all_vs_all_nonpolymer_tani ON all_vs_all_nonpolymer(irscore,tanimoto);
CREATE INDEX all_vs_all_dnarna_tani ON all_vs_all_dnarna(irscore,tanimoto);
CREATE INDEX all_vs_all_ppi_tani ON all_vs_all_ppi(irscore,tanimoto);

CREATE INDEX all_vs_all_nonpolymer_pair ON all_vs_all_nonpolymer(if_id1,if_id2);
CREATE INDEX all_vs_all_dnarna_pair ON all_vs_all_dnarna(if_id1,if_id2);
CREATE INDEX all_vs_all_ppi_pair ON all_vs_all_ppi(if_id1,if_id2);
