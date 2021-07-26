UPDATE Entities e
SET uniprot_acc = 
    (SELECT u.uniprot_acc
     FROM uniprot_sec u 
     WHERE e.uniprot_acc = u.sec_ac
     LIMIT 1)
, uniprot_mod = true
WHERE EXISTS
    (SELECT *
     FROM uniprot_sec u 
     WHERE e.uniprot_acc = u.sec_ac)
;