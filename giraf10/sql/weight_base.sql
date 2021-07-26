CREATE INDEX prob_base_if_id ON prob_base(if_id);

UPDATE interfaces i
SET prob = (
    SELECT (1.0 - SUM(c) / 120.0)
    FROM prob_base w
    WHERE w.if_id = i.if_id)
WHERE EXISTS(SELECT *
        FROM prob_base w
        WHERE w.if_id = i.if_id)
;
