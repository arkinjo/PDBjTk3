/* table and rule: small -- 0 / 10 */
CREATE TABLE Refaco_small0 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_small(if_id) ON DELETE CASCADE
        

    ) 
    INHERITS (Refaco_small);

/* table and rule: small -- 1 / 10 */
CREATE TABLE Refaco_small1 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_small(if_id) ON DELETE CASCADE
        

    ) 
    INHERITS (Refaco_small);

/* table and rule: small -- 2 / 10 */
CREATE TABLE Refaco_small2 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_small(if_id) ON DELETE CASCADE
        

    ) 
    INHERITS (Refaco_small);

/* table and rule: small -- 3 / 10 */
CREATE TABLE Refaco_small3 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_small(if_id) ON DELETE CASCADE
        

    ) 
    INHERITS (Refaco_small);

/* table and rule: small -- 4 / 10 */
CREATE TABLE Refaco_small4 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_small(if_id) ON DELETE CASCADE
        

    ) 
    INHERITS (Refaco_small);

/* table and rule: small -- 5 / 10 */
CREATE TABLE Refaco_small5 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_small(if_id) ON DELETE CASCADE
        

    ) 
    INHERITS (Refaco_small);

/* table and rule: small -- 6 / 10 */
CREATE TABLE Refaco_small6 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_small(if_id) ON DELETE CASCADE
        

    ) 
    INHERITS (Refaco_small);

/* table and rule: small -- 7 / 10 */
CREATE TABLE Refaco_small7 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_small(if_id) ON DELETE CASCADE
        

    ) 
    INHERITS (Refaco_small);

/* table and rule: small -- 8 / 10 */
CREATE TABLE Refaco_small8 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_small(if_id) ON DELETE CASCADE
        

    ) 
    INHERITS (Refaco_small);

/* table and rule: small -- 9 / 10 */
CREATE TABLE Refaco_small9 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_small(if_id) ON DELETE CASCADE
        

    ) 
    INHERITS (Refaco_small);

/* table and rule: ppi -- 0 / 10 */
CREATE TABLE Refaco_ppi0 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_ppi(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 0)

    ) 
    INHERITS (Refaco_ppi);

/* table and rule: ppi -- 1 / 10 */
CREATE TABLE Refaco_ppi1 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_ppi(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 1)

    ) 
    INHERITS (Refaco_ppi);

/* table and rule: ppi -- 2 / 10 */
CREATE TABLE Refaco_ppi2 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_ppi(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 2)

    ) 
    INHERITS (Refaco_ppi);

/* table and rule: ppi -- 3 / 10 */
CREATE TABLE Refaco_ppi3 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_ppi(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 3)

    ) 
    INHERITS (Refaco_ppi);

/* table and rule: ppi -- 4 / 10 */
CREATE TABLE Refaco_ppi4 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_ppi(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 4)

    ) 
    INHERITS (Refaco_ppi);

/* table and rule: ppi -- 5 / 10 */
CREATE TABLE Refaco_ppi5 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_ppi(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 5)

    ) 
    INHERITS (Refaco_ppi);

/* table and rule: ppi -- 6 / 10 */
CREATE TABLE Refaco_ppi6 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_ppi(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 6)

    ) 
    INHERITS (Refaco_ppi);

/* table and rule: ppi -- 7 / 10 */
CREATE TABLE Refaco_ppi7 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_ppi(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 7)

    ) 
    INHERITS (Refaco_ppi);

/* table and rule: ppi -- 8 / 10 */
CREATE TABLE Refaco_ppi8 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_ppi(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 8)

    ) 
    INHERITS (Refaco_ppi);

/* table and rule: ppi -- 9 / 10 */
CREATE TABLE Refaco_ppi9 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_ppi(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 9)

    ) 
    INHERITS (Refaco_ppi);

/* Trigger function for Refaco_ppi */
CREATE OR REPLACE FUNCTION refaco_ppi_insert_trigger()
RETURNS TRIGGER AS $$
DECLARE
    m INTEGER := hash_pdbcode(NEW.if_id) % 10;
BEGIN
    IF (m = 0) THEN
        INSERT INTO Refaco_ppi0 VALUES (NEW.*);
    ELSIF (m = 1) THEN
        INSERT INTO Refaco_ppi1 VALUES (NEW.*);
    ELSIF (m = 2) THEN
        INSERT INTO Refaco_ppi2 VALUES (NEW.*);
    ELSIF (m = 3) THEN
        INSERT INTO Refaco_ppi3 VALUES (NEW.*);
    ELSIF (m = 4) THEN
        INSERT INTO Refaco_ppi4 VALUES (NEW.*);
    ELSIF (m = 5) THEN
        INSERT INTO Refaco_ppi5 VALUES (NEW.*);
    ELSIF (m = 6) THEN
        INSERT INTO Refaco_ppi6 VALUES (NEW.*);
    ELSIF (m = 7) THEN
        INSERT INTO Refaco_ppi7 VALUES (NEW.*);
    ELSIF (m = 8) THEN
        INSERT INTO Refaco_ppi8 VALUES (NEW.*);
    ELSIF (m = 9) THEN
        INSERT INTO Refaco_ppi9 VALUES (NEW.*);
    ELSE
        RAISE EXCEPTION 'pdbcode out of range. Fix refaco_ppi_insert_trigger() function';
    END IF;
    RETURN NULL;
END;
$$
LANGUAGE plpgsql;
/* Trigger itself: Refaco_ppi */
DROP TRIGGER IF EXISTS insert_refaco_ppi_trigger ON refaco_ppi;
CREATE TRIGGER insert_refaco_ppi_trigger
    BEFORE INSERT ON refaco_ppi
    FOR EACH ROW EXECUTE PROCEDURE refaco_ppi_insert_trigger();

/* table and rule: nonpolymer -- 0 / 10 */
CREATE TABLE Refaco_nonpolymer0 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_nonpolymer(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 0)

    ) 
    INHERITS (Refaco_small0,Refaco_nonpolymer);

/* table and rule: nonpolymer -- 1 / 10 */
CREATE TABLE Refaco_nonpolymer1 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_nonpolymer(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 1)

    ) 
    INHERITS (Refaco_small1,Refaco_nonpolymer);

/* table and rule: nonpolymer -- 2 / 10 */
CREATE TABLE Refaco_nonpolymer2 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_nonpolymer(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 2)

    ) 
    INHERITS (Refaco_small2,Refaco_nonpolymer);

/* table and rule: nonpolymer -- 3 / 10 */
CREATE TABLE Refaco_nonpolymer3 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_nonpolymer(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 3)

    ) 
    INHERITS (Refaco_small3,Refaco_nonpolymer);

/* table and rule: nonpolymer -- 4 / 10 */
CREATE TABLE Refaco_nonpolymer4 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_nonpolymer(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 4)

    ) 
    INHERITS (Refaco_small4,Refaco_nonpolymer);

/* table and rule: nonpolymer -- 5 / 10 */
CREATE TABLE Refaco_nonpolymer5 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_nonpolymer(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 5)

    ) 
    INHERITS (Refaco_small5,Refaco_nonpolymer);

/* table and rule: nonpolymer -- 6 / 10 */
CREATE TABLE Refaco_nonpolymer6 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_nonpolymer(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 6)

    ) 
    INHERITS (Refaco_small6,Refaco_nonpolymer);

/* table and rule: nonpolymer -- 7 / 10 */
CREATE TABLE Refaco_nonpolymer7 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_nonpolymer(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 7)

    ) 
    INHERITS (Refaco_small7,Refaco_nonpolymer);

/* table and rule: nonpolymer -- 8 / 10 */
CREATE TABLE Refaco_nonpolymer8 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_nonpolymer(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 8)

    ) 
    INHERITS (Refaco_small8,Refaco_nonpolymer);

/* table and rule: nonpolymer -- 9 / 10 */
CREATE TABLE Refaco_nonpolymer9 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_nonpolymer(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 9)

    ) 
    INHERITS (Refaco_small9,Refaco_nonpolymer);

/* Trigger function for Refaco_nonpolymer */
CREATE OR REPLACE FUNCTION refaco_nonpolymer_insert_trigger()
RETURNS TRIGGER AS $$
DECLARE
    m INTEGER := hash_pdbcode(NEW.if_id) % 10;
BEGIN
    IF (m = 0) THEN
        INSERT INTO Refaco_nonpolymer0 VALUES (NEW.*);
    ELSIF (m = 1) THEN
        INSERT INTO Refaco_nonpolymer1 VALUES (NEW.*);
    ELSIF (m = 2) THEN
        INSERT INTO Refaco_nonpolymer2 VALUES (NEW.*);
    ELSIF (m = 3) THEN
        INSERT INTO Refaco_nonpolymer3 VALUES (NEW.*);
    ELSIF (m = 4) THEN
        INSERT INTO Refaco_nonpolymer4 VALUES (NEW.*);
    ELSIF (m = 5) THEN
        INSERT INTO Refaco_nonpolymer5 VALUES (NEW.*);
    ELSIF (m = 6) THEN
        INSERT INTO Refaco_nonpolymer6 VALUES (NEW.*);
    ELSIF (m = 7) THEN
        INSERT INTO Refaco_nonpolymer7 VALUES (NEW.*);
    ELSIF (m = 8) THEN
        INSERT INTO Refaco_nonpolymer8 VALUES (NEW.*);
    ELSIF (m = 9) THEN
        INSERT INTO Refaco_nonpolymer9 VALUES (NEW.*);
    ELSE
        RAISE EXCEPTION 'pdbcode out of range. Fix refaco_nonpolymer_insert_trigger() function';
    END IF;
    RETURN NULL;
END;
$$
LANGUAGE plpgsql;
/* Trigger itself: Refaco_nonpolymer */
DROP TRIGGER IF EXISTS insert_refaco_nonpolymer_trigger ON refaco_nonpolymer;
CREATE TRIGGER insert_refaco_nonpolymer_trigger
    BEFORE INSERT ON refaco_nonpolymer
    FOR EACH ROW EXECUTE PROCEDURE refaco_nonpolymer_insert_trigger();

/* table and rule: dnarna -- 0 / 10 */
CREATE TABLE Refaco_dnarna0 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_dnarna(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 0)

    ) 
    INHERITS (Refaco_dnarna);

/* table and rule: dnarna -- 1 / 10 */
CREATE TABLE Refaco_dnarna1 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_dnarna(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 1)

    ) 
    INHERITS (Refaco_dnarna);

/* table and rule: dnarna -- 2 / 10 */
CREATE TABLE Refaco_dnarna2 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_dnarna(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 2)

    ) 
    INHERITS (Refaco_dnarna);

/* table and rule: dnarna -- 3 / 10 */
CREATE TABLE Refaco_dnarna3 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_dnarna(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 3)

    ) 
    INHERITS (Refaco_dnarna);

/* table and rule: dnarna -- 4 / 10 */
CREATE TABLE Refaco_dnarna4 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_dnarna(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 4)

    ) 
    INHERITS (Refaco_dnarna);

/* table and rule: dnarna -- 5 / 10 */
CREATE TABLE Refaco_dnarna5 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_dnarna(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 5)

    ) 
    INHERITS (Refaco_dnarna);

/* table and rule: dnarna -- 6 / 10 */
CREATE TABLE Refaco_dnarna6 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_dnarna(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 6)

    ) 
    INHERITS (Refaco_dnarna);

/* table and rule: dnarna -- 7 / 10 */
CREATE TABLE Refaco_dnarna7 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_dnarna(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 7)

    ) 
    INHERITS (Refaco_dnarna);

/* table and rule: dnarna -- 8 / 10 */
CREATE TABLE Refaco_dnarna8 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_dnarna(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 8)

    ) 
    INHERITS (Refaco_dnarna);

/* table and rule: dnarna -- 9 / 10 */
CREATE TABLE Refaco_dnarna9 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_dnarna(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 9)

    ) 
    INHERITS (Refaco_dnarna);

/* Trigger function for Refaco_dnarna */
CREATE OR REPLACE FUNCTION refaco_dnarna_insert_trigger()
RETURNS TRIGGER AS $$
DECLARE
    m INTEGER := hash_pdbcode(NEW.if_id) % 10;
BEGIN
    IF (m = 0) THEN
        INSERT INTO Refaco_dnarna0 VALUES (NEW.*);
    ELSIF (m = 1) THEN
        INSERT INTO Refaco_dnarna1 VALUES (NEW.*);
    ELSIF (m = 2) THEN
        INSERT INTO Refaco_dnarna2 VALUES (NEW.*);
    ELSIF (m = 3) THEN
        INSERT INTO Refaco_dnarna3 VALUES (NEW.*);
    ELSIF (m = 4) THEN
        INSERT INTO Refaco_dnarna4 VALUES (NEW.*);
    ELSIF (m = 5) THEN
        INSERT INTO Refaco_dnarna5 VALUES (NEW.*);
    ELSIF (m = 6) THEN
        INSERT INTO Refaco_dnarna6 VALUES (NEW.*);
    ELSIF (m = 7) THEN
        INSERT INTO Refaco_dnarna7 VALUES (NEW.*);
    ELSIF (m = 8) THEN
        INSERT INTO Refaco_dnarna8 VALUES (NEW.*);
    ELSIF (m = 9) THEN
        INSERT INTO Refaco_dnarna9 VALUES (NEW.*);
    ELSE
        RAISE EXCEPTION 'pdbcode out of range. Fix refaco_dnarna_insert_trigger() function';
    END IF;
    RETURN NULL;
END;
$$
LANGUAGE plpgsql;
/* Trigger itself: Refaco_dnarna */
DROP TRIGGER IF EXISTS insert_refaco_dnarna_trigger ON refaco_dnarna;
CREATE TRIGGER insert_refaco_dnarna_trigger
    BEFORE INSERT ON refaco_dnarna
    FOR EACH ROW EXECUTE PROCEDURE refaco_dnarna_insert_trigger();

/* table and rule: peptide -- 0 / 10 */
CREATE TABLE Refaco_peptide0 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_peptide(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 0)

    ) 
    INHERITS (Refaco_small0,Refaco_peptide);

/* table and rule: peptide -- 1 / 10 */
CREATE TABLE Refaco_peptide1 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_peptide(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 1)

    ) 
    INHERITS (Refaco_small1,Refaco_peptide);

/* table and rule: peptide -- 2 / 10 */
CREATE TABLE Refaco_peptide2 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_peptide(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 2)

    ) 
    INHERITS (Refaco_small2,Refaco_peptide);

/* table and rule: peptide -- 3 / 10 */
CREATE TABLE Refaco_peptide3 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_peptide(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 3)

    ) 
    INHERITS (Refaco_small3,Refaco_peptide);

/* table and rule: peptide -- 4 / 10 */
CREATE TABLE Refaco_peptide4 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_peptide(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 4)

    ) 
    INHERITS (Refaco_small4,Refaco_peptide);

/* table and rule: peptide -- 5 / 10 */
CREATE TABLE Refaco_peptide5 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_peptide(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 5)

    ) 
    INHERITS (Refaco_small5,Refaco_peptide);

/* table and rule: peptide -- 6 / 10 */
CREATE TABLE Refaco_peptide6 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_peptide(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 6)

    ) 
    INHERITS (Refaco_small6,Refaco_peptide);

/* table and rule: peptide -- 7 / 10 */
CREATE TABLE Refaco_peptide7 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_peptide(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 7)

    ) 
    INHERITS (Refaco_small7,Refaco_peptide);

/* table and rule: peptide -- 8 / 10 */
CREATE TABLE Refaco_peptide8 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_peptide(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 8)

    ) 
    INHERITS (Refaco_small8,Refaco_peptide);

/* table and rule: peptide -- 9 / 10 */
CREATE TABLE Refaco_peptide9 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_peptide(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 9)

    ) 
    INHERITS (Refaco_small9,Refaco_peptide);

/* Trigger function for Refaco_peptide */
CREATE OR REPLACE FUNCTION refaco_peptide_insert_trigger()
RETURNS TRIGGER AS $$
DECLARE
    m INTEGER := hash_pdbcode(NEW.if_id) % 10;
BEGIN
    IF (m = 0) THEN
        INSERT INTO Refaco_peptide0 VALUES (NEW.*);
    ELSIF (m = 1) THEN
        INSERT INTO Refaco_peptide1 VALUES (NEW.*);
    ELSIF (m = 2) THEN
        INSERT INTO Refaco_peptide2 VALUES (NEW.*);
    ELSIF (m = 3) THEN
        INSERT INTO Refaco_peptide3 VALUES (NEW.*);
    ELSIF (m = 4) THEN
        INSERT INTO Refaco_peptide4 VALUES (NEW.*);
    ELSIF (m = 5) THEN
        INSERT INTO Refaco_peptide5 VALUES (NEW.*);
    ELSIF (m = 6) THEN
        INSERT INTO Refaco_peptide6 VALUES (NEW.*);
    ELSIF (m = 7) THEN
        INSERT INTO Refaco_peptide7 VALUES (NEW.*);
    ELSIF (m = 8) THEN
        INSERT INTO Refaco_peptide8 VALUES (NEW.*);
    ELSIF (m = 9) THEN
        INSERT INTO Refaco_peptide9 VALUES (NEW.*);
    ELSE
        RAISE EXCEPTION 'pdbcode out of range. Fix refaco_peptide_insert_trigger() function';
    END IF;
    RETURN NULL;
END;
$$
LANGUAGE plpgsql;
/* Trigger itself: Refaco_peptide */
DROP TRIGGER IF EXISTS insert_refaco_peptide_trigger ON refaco_peptide;
CREATE TRIGGER insert_refaco_peptide_trigger
    BEFORE INSERT ON refaco_peptide
    FOR EACH ROW EXECUTE PROCEDURE refaco_peptide_insert_trigger();

/* table and rule: opoly -- 0 / 10 */
CREATE TABLE Refaco_opoly0 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_opoly(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 0)

    ) 
    INHERITS (Refaco_small0,Refaco_opoly);

/* table and rule: opoly -- 1 / 10 */
CREATE TABLE Refaco_opoly1 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_opoly(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 1)

    ) 
    INHERITS (Refaco_small1,Refaco_opoly);

/* table and rule: opoly -- 2 / 10 */
CREATE TABLE Refaco_opoly2 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_opoly(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 2)

    ) 
    INHERITS (Refaco_small2,Refaco_opoly);

/* table and rule: opoly -- 3 / 10 */
CREATE TABLE Refaco_opoly3 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_opoly(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 3)

    ) 
    INHERITS (Refaco_small3,Refaco_opoly);

/* table and rule: opoly -- 4 / 10 */
CREATE TABLE Refaco_opoly4 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_opoly(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 4)

    ) 
    INHERITS (Refaco_small4,Refaco_opoly);

/* table and rule: opoly -- 5 / 10 */
CREATE TABLE Refaco_opoly5 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_opoly(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 5)

    ) 
    INHERITS (Refaco_small5,Refaco_opoly);

/* table and rule: opoly -- 6 / 10 */
CREATE TABLE Refaco_opoly6 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_opoly(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 6)

    ) 
    INHERITS (Refaco_small6,Refaco_opoly);

/* table and rule: opoly -- 7 / 10 */
CREATE TABLE Refaco_opoly7 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_opoly(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 7)

    ) 
    INHERITS (Refaco_small7,Refaco_opoly);

/* table and rule: opoly -- 8 / 10 */
CREATE TABLE Refaco_opoly8 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_opoly(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 8)

    ) 
    INHERITS (Refaco_small8,Refaco_opoly);

/* table and rule: opoly -- 9 / 10 */
CREATE TABLE Refaco_opoly9 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_opoly(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 9)

    ) 
    INHERITS (Refaco_small9,Refaco_opoly);

/* Trigger function for Refaco_opoly */
CREATE OR REPLACE FUNCTION refaco_opoly_insert_trigger()
RETURNS TRIGGER AS $$
DECLARE
    m INTEGER := hash_pdbcode(NEW.if_id) % 10;
BEGIN
    IF (m = 0) THEN
        INSERT INTO Refaco_opoly0 VALUES (NEW.*);
    ELSIF (m = 1) THEN
        INSERT INTO Refaco_opoly1 VALUES (NEW.*);
    ELSIF (m = 2) THEN
        INSERT INTO Refaco_opoly2 VALUES (NEW.*);
    ELSIF (m = 3) THEN
        INSERT INTO Refaco_opoly3 VALUES (NEW.*);
    ELSIF (m = 4) THEN
        INSERT INTO Refaco_opoly4 VALUES (NEW.*);
    ELSIF (m = 5) THEN
        INSERT INTO Refaco_opoly5 VALUES (NEW.*);
    ELSIF (m = 6) THEN
        INSERT INTO Refaco_opoly6 VALUES (NEW.*);
    ELSIF (m = 7) THEN
        INSERT INTO Refaco_opoly7 VALUES (NEW.*);
    ELSIF (m = 8) THEN
        INSERT INTO Refaco_opoly8 VALUES (NEW.*);
    ELSIF (m = 9) THEN
        INSERT INTO Refaco_opoly9 VALUES (NEW.*);
    ELSE
        RAISE EXCEPTION 'pdbcode out of range. Fix refaco_opoly_insert_trigger() function';
    END IF;
    RETURN NULL;
END;
$$
LANGUAGE plpgsql;
/* Trigger itself: Refaco_opoly */
DROP TRIGGER IF EXISTS insert_refaco_opoly_trigger ON refaco_opoly;
CREATE TRIGGER insert_refaco_opoly_trigger
    BEFORE INSERT ON refaco_opoly
    FOR EACH ROW EXECUTE PROCEDURE refaco_opoly_insert_trigger();

/* table and rule: fold -- 0 / 10 */
CREATE TABLE Refaco_fold0 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_fold(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 0)

    ) 
    INHERITS (Refaco_fold);

/* table and rule: fold -- 1 / 10 */
CREATE TABLE Refaco_fold1 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_fold(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 1)

    ) 
    INHERITS (Refaco_fold);

/* table and rule: fold -- 2 / 10 */
CREATE TABLE Refaco_fold2 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_fold(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 2)

    ) 
    INHERITS (Refaco_fold);

/* table and rule: fold -- 3 / 10 */
CREATE TABLE Refaco_fold3 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_fold(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 3)

    ) 
    INHERITS (Refaco_fold);

/* table and rule: fold -- 4 / 10 */
CREATE TABLE Refaco_fold4 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_fold(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 4)

    ) 
    INHERITS (Refaco_fold);

/* table and rule: fold -- 5 / 10 */
CREATE TABLE Refaco_fold5 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_fold(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 5)

    ) 
    INHERITS (Refaco_fold);

/* table and rule: fold -- 6 / 10 */
CREATE TABLE Refaco_fold6 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_fold(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 6)

    ) 
    INHERITS (Refaco_fold);

/* table and rule: fold -- 7 / 10 */
CREATE TABLE Refaco_fold7 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_fold(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 7)

    ) 
    INHERITS (Refaco_fold);

/* table and rule: fold -- 8 / 10 */
CREATE TABLE Refaco_fold8 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_fold(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 8)

    ) 
    INHERITS (Refaco_fold);

/* table and rule: fold -- 9 / 10 */
CREATE TABLE Refaco_fold9 (
        PRIMARY KEY (if_id,rs_id)
        , FOREIGN KEY (if_id) 
                REFERENCES Interfaces_fold(if_id) ON DELETE CASCADE
        , CHECK (hash_pdbcode(if_id) % 10 = 9)

    ) 
    INHERITS (Refaco_fold);

/* Trigger function for Refaco_fold */
CREATE OR REPLACE FUNCTION refaco_fold_insert_trigger()
RETURNS TRIGGER AS $$
DECLARE
    m INTEGER := hash_pdbcode(NEW.if_id) % 10;
BEGIN
    IF (m = 0) THEN
        INSERT INTO Refaco_fold0 VALUES (NEW.*);
    ELSIF (m = 1) THEN
        INSERT INTO Refaco_fold1 VALUES (NEW.*);
    ELSIF (m = 2) THEN
        INSERT INTO Refaco_fold2 VALUES (NEW.*);
    ELSIF (m = 3) THEN
        INSERT INTO Refaco_fold3 VALUES (NEW.*);
    ELSIF (m = 4) THEN
        INSERT INTO Refaco_fold4 VALUES (NEW.*);
    ELSIF (m = 5) THEN
        INSERT INTO Refaco_fold5 VALUES (NEW.*);
    ELSIF (m = 6) THEN
        INSERT INTO Refaco_fold6 VALUES (NEW.*);
    ELSIF (m = 7) THEN
        INSERT INTO Refaco_fold7 VALUES (NEW.*);
    ELSIF (m = 8) THEN
        INSERT INTO Refaco_fold8 VALUES (NEW.*);
    ELSIF (m = 9) THEN
        INSERT INTO Refaco_fold9 VALUES (NEW.*);
    ELSE
        RAISE EXCEPTION 'pdbcode out of range. Fix refaco_fold_insert_trigger() function';
    END IF;
    RETURN NULL;
END;
$$
LANGUAGE plpgsql;
/* Trigger itself: Refaco_fold */
DROP TRIGGER IF EXISTS insert_refaco_fold_trigger ON refaco_fold;
CREATE TRIGGER insert_refaco_fold_trigger
    BEFORE INSERT ON refaco_fold
    FOR EACH ROW EXECUTE PROCEDURE refaco_fold_insert_trigger();

