
CREATE INDEX blast_pair_sum_asym_1 ON blast_pair_sum_asym(pdbcode1, label_asym_id1);
CREATE INDEX blast_pair_sum_asym_2 ON blast_pair_sum_asym(pdbcode2, label_asym_id2);
/*
CREATE INDEX blast_pair_q ON blast_pair(pdbcode1, entity_id1);
CREATE INDEX blast_pair_s ON blast_pair(pdbcode2, entity_id2);
CREATE INDEX blast_pair_pair ON blast_pair(pdbcode1,entity_id1,pdbcode2, entity_id2);
CREATE INDEX blast_pair_score ON blast_pair(score);
CREATE INDEX blast_pair_range ON blast_pair(qstart,qend,sstart,send);
*/
