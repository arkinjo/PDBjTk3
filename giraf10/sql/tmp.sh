for i in {0..9}; do
    psql giraf8 <<EOF
ALTER TABLE Refaco_small${i} DROP CONSTRAINT refaco_small${i}_if_id_check;
ALTER TABLE Refaco_nonpolymer${i} INHERIT Refaco_small${i};
ALTER TABLE Refaco_peptide${i} INHERIT Refaco_small${i};
ALTER TABLE Refaco_opoly${i} INHERIT Refaco_small${i};
EOF
done