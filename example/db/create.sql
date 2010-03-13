DROP TABLE IF EXISTS `customers`;
CREATE TABLE `customers` (
  `_id` INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
  `category_id` INT(11) NOT NULL,
  `firstname` VARCHAR(45) NOT NULL,
  `initials` VARCHAR(45) NULL,
  `lastname` VARCHAR(45) NOT NULL,
  `date_of_birth` DATE NOT NULL,
  `updated_at` DATETIME NULL
)
;

DROP TABLE IF EXISTS `categories`;
CREATE TABLE `categories` (
  `_id` INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
  `name` VARCHAR(45) NULL,
  `discount` VARCHAR(45) NULL
)
;
