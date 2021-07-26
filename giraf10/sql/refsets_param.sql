DROP TABLE IF EXISTS Refsets_param CASCADE;
TRUNCATE Refsets_param;

/* parameters for GI search */
CREATE TABLE Refsets_param(
        LIKE Refsets_base
        , dmax DOUBLE PRECISION
        , sc DOUBLE PRECISION
        , sn DOUBLE PRECISION
        );

CREATE TABLE Refsets_param_sd(
        LIKE Refsets_base
        );

CREATE TABLE Refsets_param_ave(
        LIKE Refsets_base
        );

TRUNCATE Refsets_param_sd;
TRUNCATE Refsets_param_ave;

INSERT INTO refsets_param_sd
SELECT type, '', 0, ''::BYTEA
    , stddev(ft_0) , stddev(ft_1) , stddev(ft_2) 
    , stddev(ft_3) , stddev(ft_4) , stddev(ft_5) 
    , stddev(ft_6) , stddev(ft_7) , stddev(ft_8) 
    , stddev(ft_9) , stddev(ft_10) , stddev(ft_11) 
    , stddev(ft_12) , stddev(ft_13) , stddev(ft_14)
    , stddev(ft_15) , stddev(ft_16) , stddev(ft_17) 
    , stddev(ft_18) , stddev(ft_19) , stddev(ft_20)
    , stddev(ft_21) , stddev(ft_22) , stddev(ft_23) 
    , stddev(ft_24) , stddev(ft_25) , stddev(ft_26) 
    , stddev(ft_27) , stddev(ft_28) , stddev(ft_29)
    , stddev(ft_30) , stddev(ft_31) , stddev(ft_32) 
    , stddev(ft_33) , stddev(ft_34) , stddev(ft_35) 
    , stddev(ft_36) , stddev(ft_37) , stddev(ft_38) 
    , stddev(ft_39) , stddev(ft_40) , stddev(ft_41)
    , stddev(ft_42) , stddev(ft_43)
FROM refaco r
GROUP BY type;

INSERT INTO refsets_param_sd
SELECT 'small','', 0, ''::BYTEA
    , stddev(ft_0) , stddev(ft_1) , stddev(ft_2) 
    , stddev(ft_3) , stddev(ft_4) , stddev(ft_5) 
    , stddev(ft_6) , stddev(ft_7) , stddev(ft_8)
    , stddev(ft_9) , stddev(ft_10) , stddev(ft_11) 
    , stddev(ft_12) , stddev(ft_13) , stddev(ft_14)
    , stddev(ft_15) , stddev(ft_16) , stddev(ft_17)
    , stddev(ft_18) , stddev(ft_19) , stddev(ft_20)
    , stddev(ft_21) , stddev(ft_22) , stddev(ft_23)
    , stddev(ft_24) , stddev(ft_25) , stddev(ft_26)
    , stddev(ft_27) , stddev(ft_28) , stddev(ft_29)
    , stddev(ft_30) , stddev(ft_31) , stddev(ft_32) 
    , stddev(ft_33) , stddev(ft_34) , stddev(ft_35)
    , stddev(ft_36) , stddev(ft_37) , stddev(ft_38) 
    , stddev(ft_39) , stddev(ft_40) , stddev(ft_41)
    , stddev(ft_42) , stddev(ft_43)
FROM refaco_small;


INSERT INTO refsets_param_ave
SELECT r.type, '', 0, ''::BYTEA
    , avg(ft_0) , avg(ft_1) , avg(ft_2) , avg(ft_3) , avg(ft_4)
    , avg(ft_5) , avg(ft_6) , avg(ft_7) , avg(ft_8) , avg(ft_9)
    , avg(ft_10) , avg(ft_11) , avg(ft_12) , avg(ft_13) , avg(ft_14)
    , avg(ft_15) , avg(ft_16) , avg(ft_17) , avg(ft_18) , avg(ft_19)
    , avg(ft_20) , avg(ft_21) , avg(ft_22) , avg(ft_23) , avg(ft_24)
    , avg(ft_25) , avg(ft_26) , avg(ft_27) , avg(ft_28) , avg(ft_29)
    , avg(ft_30) , avg(ft_31) , avg(ft_32) , avg(ft_33) , avg(ft_34)
    , avg(ft_35) , avg(ft_36) , avg(ft_37) , avg(ft_38) , avg(ft_39)
    , avg(ft_40) , avg(ft_41) , avg(ft_42) , avg(ft_43)
FROM refaco r
GROUP BY type;

INSERT INTO refsets_param_ave
SELECT 'small', '', 0, ''::BYTEA
    , avg(ft_0) , avg(ft_1) , avg(ft_2) , avg(ft_3) , avg(ft_4)
    , avg(ft_5) , avg(ft_6) , avg(ft_7) , avg(ft_8) , avg(ft_9)
    , avg(ft_10) , avg(ft_11) , avg(ft_12) , avg(ft_13) , avg(ft_14)
    , avg(ft_15) , avg(ft_16) , avg(ft_17) , avg(ft_18) , avg(ft_19)
    , avg(ft_20) , avg(ft_21) , avg(ft_22) , avg(ft_23) , avg(ft_24)
    , avg(ft_25) , avg(ft_26) , avg(ft_27) , avg(ft_28) , avg(ft_29)
    , avg(ft_30) , avg(ft_31) , avg(ft_32) , avg(ft_33) , avg(ft_34)
    , avg(ft_35) , avg(ft_36) , avg(ft_37) , avg(ft_38) , avg(ft_39)
    , avg(ft_40) , avg(ft_41) , avg(ft_42) , avg(ft_43)
FROM refaco_small;


CREATE OR REPLACE FUNCTION update_refsets_param
        (IN _type interface_type , IN sc DOUBLE PRECISION
        ,IN sn DOUBLE PRECISION)
        RETURNS VOID AS 
$body$
BEGIN
 DELETE FROM Refsets_param WHERE type = _type::interface_type;
 INSERT INTO Refsets_param
 SELECT type, if_id, 0, ''::BYTEA
    , sc*ft_0 , sc*ft_1 , sc*ft_2 
    , sc*ft_3 , sc*ft_4 , sc*ft_5 
    , sc*ft_6 , sc*ft_7 , sc*ft_8 
    , sc*ft_9 , sc*ft_10 , sc*ft_11 
    , GREATEST(1,sn*ft_12) , GREATEST(1,sn*ft_13) , GREATEST(1,sn*ft_14)
    , GREATEST(1,sn*ft_15) , GREATEST(1,sn*ft_16) , GREATEST(1,sn*ft_17) 
    , GREATEST(1,sn*ft_18) , GREATEST(1,sn*ft_19)
    , GREATEST(1,sn*ft_20) , GREATEST(1,sn*ft_21) , GREATEST(1,sn*ft_22) 
    , GREATEST(1,sn*ft_23) , GREATEST(1,sn*ft_24)
    , GREATEST(1,sn*ft_25) , GREATEST(1,sn*ft_26) , GREATEST(1,sn*ft_27) 
    , GREATEST(1,sn*ft_28) , GREATEST(1,sn*ft_29)
    , GREATEST(1,sn*ft_30) , GREATEST(1,sn*ft_31) , GREATEST(1,sn*ft_32) 
    , GREATEST(1,sn*ft_33) , GREATEST(1,sn*ft_34)
    , GREATEST(1,sn*ft_35) , GREATEST(1,sn*ft_36) , GREATEST(1,sn*ft_37) 
    , GREATEST(1,sn*ft_38) , GREATEST(1,sn*ft_39)
    , GREATEST(1,sn*ft_40) , GREATEST(1,sn*ft_41) , GREATEST(1,sn*ft_42) 
    , GREATEST(1,sn*ft_43)
    , 0, sc, sn
 FROM refsets_param_sd WHERE type = _type;
END;
$body$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION update_refsets_param_fold(IN dmax DOUBLE PRECISION, IN sc DOUBLE PRECISION, IN sn DOUBLE PRECISION)
        RETURNS VOID AS 
$body$
BEGIN
 DELETE FROM Refsets_param WHERE if_id = 'fold';
 INSERT INTO Refsets_param
 SELECT type, if_id, 0, ''::BYTEA
    , LEAST(dmax,sc*ft_0) , LEAST(dmax,sc*ft_1) , LEAST(dmax,sc*ft_2)
    , LEAST(dmax,sc*ft_3) , LEAST(dmax,sc*ft_4) , LEAST(dmax,sc*ft_5) 
    , LEAST(dmax,sc*ft_6) , LEAST(dmax,sc*ft_7) , LEAST(dmax,sc*ft_8) 
    , LEAST(dmax,sc*ft_9) , LEAST(dmax,sc*ft_10) , LEAST(dmax,sc*ft_11)
    , LEAST(dmax,sc*ft_12) , LEAST(dmax,sc*ft_13) , LEAST(dmax,sc*ft_14)
    , LEAST(dmax,sc*ft_15) , LEAST(dmax,sc*ft_16) , LEAST(dmax,sc*ft_17)
    , LEAST(dmax,sc*ft_18) , LEAST(dmax,sc*ft_19) , LEAST(dmax,sc*ft_20)
    , LEAST(dmax,sc*ft_21) , LEAST(dmax,sc*ft_22) , LEAST(dmax,sc*ft_23) 
    , sn*ft_24
    , sn*ft_25 , sn*ft_26 , sn*ft_27 , sn*ft_28 , sn*ft_29
    , sn*ft_30 , sn*ft_31 , sn*ft_32 , sn*ft_33 , sn*ft_34
    , sn*ft_35 , sn*ft_36 , sn*ft_37 , sn*ft_38 , sn*ft_39
    , sn*ft_40 , sn*ft_41 , sn*ft_42 , sn*ft_43
    , dmax, sc, sn
 FROM refsets_param_sd WHERE type = 'fold';
END;
$body$ LANGUAGE plpgsql;

select * from update_refsets_param('nonpolymer',1.0, 1.2);
select * from update_refsets_param('peptide',   1.0, 1.2);
select * from update_refsets_param('opoly',     1.0, 1.2);
select * from update_refsets_param('small',     1.0, 1.2);

select * from update_refsets_param('ppi',       1.0, 1.05);
select * from update_refsets_param('dnarna',    1.0, 1.05);


select * from update_refsets_param_fold(3.0,1.0, 1.5);
