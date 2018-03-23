

CREATE USER 'testuser'@'localhost' IDENTIFIED BY  'cs';
GRANT ALL ON *.* TO 'testuser'@'localhost';
FLUSH PRIVILEGES ;

CREATE DATABASE IF NOT EXISTS `tests`;
USE `tests`;


CREATE TABLE IF NOT EXISTS t1(
    id INT NOT NULL auto_increment PRIMARY KEY,
    n1  int ,
    n2  FLOAT ,
    n3  DOUBLE ,
    n4  BOOLEAN default 1 ,
    n5  ENUM ('x','y','q'),
    v1  varchar (255) ,
    v2  char(2) ,
    dt1 DATE,
    dt2 TIME,
    dt3 DATETIME
    );




