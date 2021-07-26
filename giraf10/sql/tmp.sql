UPDATE refaco_nonpolymer r
SET natoms = (SELECT natoms FROM interfaces_nonpolymer i WHERE i.if_id = r.if_id);