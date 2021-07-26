CREATE OR REPLACE FUNCTION bitcount(i integer) RETURNS integer AS $$ 
DECLARE n integer; 
DECLARE amount integer; 
  BEGIN 
    amount := 0; 
    FOR n IN 1..16 LOOP 
      amount := amount + ((i >> (n-1)) & 1); 
    END LOOP; 
    RETURN amount; 
  END 
$$ LANGUAGE plpgsql; 
