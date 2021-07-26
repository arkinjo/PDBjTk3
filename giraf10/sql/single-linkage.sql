explain
SELECT if_id1
FROM all_vs_all_nonpolymer 
WHERE irscore >= 15
GROUP BY if_id1
ORDER BY COUNT(DISTINCT if_id2)
LIMIT 1;


WITH RECURSIVE _iclust(if_id) AS (
     SELECT if_id1 AS if_id
     FROM  (SELECT if_id1
            FROM all_vs_all_nonpolymer
            WHERE irscore >= 15
            LIMIT 1) AS t
          UNION
     SELECT if_id2 AS if_id
     FROM all_vs_all_nonpolymer
     JOIN _iclust ON if_id = if_id1
     WHERE irscore >= 15
), _iclust2 (if_id) AS (
   SELECT if_id
   FROM _iclust
      UNION
   SELECT if_id1 AS if_id
   FROM all_vs_all_nonpolymer
   JOIN _iclust2 ON if_id = if_id2
   WHERE irscore >= 15
)

SELECT if_id
FROM _iclust
;

