CREATE OR REPLACE FUNCTION hash_pdbcode(TEXT)
RETURNS INT AS
$body$
DECLARE
  b BYTEA;
  hpdbcode INT;
BEGIN
  b := md5($1)::BYTEA;
  hpdbcode := 
    (get_byte(b,0) * 13 
   + get_byte(b,3) * 5 
   + get_byte(b,5) * 7 
   + get_byte(b,7) * 11) ;
  RETURN hpdbcode;
END;
$body$ LANGUAGE plpgsql;
