CREATE TABLE Clusters (
        clust_id TEXT NOT NULL
        , if_id TEXT NOT NULL
        , nedge INTEGER NOT NULL
        , itype TEXT NOT NULL
        );

CREATE TABLE Clusters_nonpolymer (
        itype TEXT DEFAULT 'nonpolymer'
        , FOREIGN KEY (if_id) REFERENCES Interfaces_nonpolymer(if_id) ON DELETE CASCADE
        ) INHERITS(Clusters);

CREATE TABLE Clusters_ppi (
        itype TEXT DEFAULT 'ppi'
        , FOREIGN KEY (if_id) REFERENCES Interfaces_ppi(if_id) ON DELETE CASCADE
        ) INHERITS(Clusters);


CREATE TABLE Clusters_dnarna (
        itype TEXT DEFAULT 'dnarna'
        , FOREIGN KEY (if_id) REFERENCES Interfaces_dnarna(if_id) ON DELETE CASCADE
        ) INHERITS(Clusters);
