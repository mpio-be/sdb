# db and user test
CREATE DATABASE tests;
CREATE USER 'testuser'@'localhost' IDENTIFIED BY 'cs';
GRANT ALL PRIVILEGES  ON tests.* TO 'testuser'@'localhost' ; FLUSH PRIVILEGES;


# tests and examples
  require(devtools)
  run_examples()
  test()





