CREATE TABLE dict(
    k VARCHAR(20) PRIMARY KEY,
    v TEXT
);
CREATE TABLE sen(
    word VARCHAR(20),
    offset INT,
    data TEXT,
    PRIMARY KEY (word, offset)
);
CREATE TABLE wenyan(
    k VARCHAR(10),
    v TEXT
);
CREATE TABLE user(
    id INT PRIMARY KEY,
    pwd CHAR(32)
);
CREATE TABLE birthday(
    id INT PRIMARY KEY,
    name VARCHAR(5),
    year INT,
    month INT,
    day INT
);
CREATE TABLE notice(
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    target INT,
    html TEXT
);
CREATE TABLE issue(
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    author INT,
    content VARCHAR(64)
);