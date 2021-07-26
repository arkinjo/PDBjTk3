#!/bin/zsh

n=${1:-"10"}
n1=`expr $n - 1`
rm -f create_ce_tables.sql
rm -f create_ce_indexes.sql

printf "SET default_tablespace to 'work3';\n\n\n" > create_ce_indexes.sql

for itype in small ppi nonpolymer dnarna peptide opoly fold; do
    for i in {0..${n1}}; do
	if [ "${itype}" = "nonpolymer" ] || [ "${itype}" = "peptide" ] || [ "${itype}" = "opoly" ]; then
	    parents="Refaco_small${i},Refaco_${itype}"
	else
	    parents="Refaco_${itype}"
	fi
	if [ "${itype}" = "small" ]; then
	    check=""
	else
	    check=", CHECK (hash_pdbcode(if_id) % ${n} = ${i})"
	fi
	cat<<EOF >>create_ce_tables.sql
/* table and rule: $itype -- ${i} / ${n} */
CREATE TABLE Refaco_${itype}${i} (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_${itype}(if_id) ON DELETE CASCADE
        ${check}

    ) 
    INHERITS (${parents});

EOF
	cat<<EOF>>create_ce_indexes.sql
/* index: $itype -- ${i} / $n */
CREATE INDEX Refaco_${itype}${i}_geom ON Refaco_${itype}${i} (
	ft_0,ft_1,ft_2,ft_3,ft_4,ft_5,ft_6,ft_7,ft_8,ft_9
	,ft_10,ft_11,ft_12,ft_13,ft_14,ft_15,ft_16,ft_17,ft_18,ft_19
	,ft_20,ft_21,ft_22,ft_23,ft_24,ft_25,ft_26,ft_27,ft_28,ft_29
	,ft_30,ft_31,ft_32,ft_33,ft_34,ft_35,ft_36,ft_37,ft_38,ft_39
	,ft_40,ft_41,ft_42,ft_43
    );

EOF

    done
# trigger
    if [ "${itype}" != "small" ]; then
	cat<<EOF>>create_ce_tables.sql
/* Trigger function for Refaco_${itype} */
CREATE OR REPLACE FUNCTION refaco_${itype}_insert_trigger()
RETURNS TRIGGER AS \$$
DECLARE
    m INTEGER := hash_pdbcode(NEW.if_id) % ${n};
BEGIN
EOF
	for i in {0..${n1}}; do
	    if [ $i -eq 0 ]; then
		head="IF"
	    else
		head="ELSIF"
	    fi
	    cat<<EOF>>create_ce_tables.sql
    $head (m = $i) THEN
        INSERT INTO Refaco_${itype}${i} VALUES (NEW.*);
EOF
	done
	cat<<EOF>>create_ce_tables.sql
    ELSE
        RAISE EXCEPTION 'pdbcode out of range. Fix refaco_${itype}_insert_trigger() function';
    END IF;
    RETURN NULL;
END;
\$$
LANGUAGE plpgsql;
/* Trigger itself: Refaco_${itype} */
DROP TRIGGER IF EXISTS insert_refaco_${itype}_trigger ON refaco_${itype};
CREATE TRIGGER insert_refaco_${itype}_trigger
    BEFORE INSERT ON refaco_${itype}
    FOR EACH ROW EXECUTE PROCEDURE refaco_${itype}_insert_trigger();

EOF
    fi
done
