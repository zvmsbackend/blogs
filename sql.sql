CREATE TABLE IF NOT EXISTS dict(
    k VARCHAR(20) PRIMARY KEY,
    v TEXT
);
CREATE TABLE IF NOT EXISTS sen(
    word VARCHAR(20),
    offset INT,
    data TEXT,
    PRIMARY KEY (word, offset)
);
CREATE TABLE IF NOT EXISTS wenyan(
    k VARCHAR(10),
    v TEXT
);
CREATE TABLE IF NOT EXISTS user(
    id INT PRIMARY KEY,
    name VARCHAR(5),
    pwd CHAR(32)
);
CREATE TABLE IF NOT EXISTS birthday(
    id INT PRIMARY KEY,
    name VARCHAR(5),
    year INT,
    month INT,
    day INT
);
CREATE TABLE IF NOT EXISTS notice(
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    target INT,
    html TEXT
);
CREATE TABLE IF NOT EXISTS issue(
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    author INT,
    content VARCHAR(64),
    time DATETIME
);
CREATE TABLE IF NOT EXISTS muyu(
    id INT PRIMARY KEY,
    count INT,
    anonymous SMALLINT
);
CREATE TABLE IF NOT EXISTS aichat(
    id INT PRIMARY KEY,
    user INT,
    human SMALLINT,
    content TEXT,
    time DATETIME
);
CREATE TABLE IF NOT EXISTS ai(
    user INT PRIMARY KEY,
    session TEXT
);