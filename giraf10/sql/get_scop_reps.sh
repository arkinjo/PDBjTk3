for c in a b c d f g; do
    psql --quiet giraf9 <<EOF
COPY(
SELECT min(pdb || ' ' || chainID) || ' ' || fold
FROM (
SELECT DISTINCT lower(pdb) AS pdb
, (regexp_split_to_array(region[1], ':'))[1] AS chainID
, array_to_string((regexp_split_to_array(sccs,E'\\\\.'))[1:2],'.') AS fold
FROM scop_px
WHERE sccs LIKE '${c}.%'
AND EXISTS(SELECT * FROM Structs s WHERE s.pdbid = upper(pdb))
) AS t
GROUP BY fold
ORDER BY RANDOM()
LIMIT 20
) TO STDOUT;
EOF
done
