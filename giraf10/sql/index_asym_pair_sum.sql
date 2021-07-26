-- pdbcode1 | label_asym_id1 | pdbcode2 | label_asym_id2 |   score   | npair |    type    
CREATE INDEX asym_pair_sum_score ON asym_pair_sum(score);
CREATE INDEX asym_pair_sum_type ON asym_pair_sum(type);
