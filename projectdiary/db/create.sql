DROP TABLE IF EXISTS `areas`;
CREATE TABLE `areas` (
  `_id` INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
  `project_id` INT(11) NOT NULL,
  `description` VARCHAR(64) NOT NULL,
  `comment` TEXT NULL,
  `created_at` DATETIME NULL
)
;

DROP TABLE IF EXISTS `packages`;
CREATE TABLE `packages` (
  `_id` INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
  `area_id` INT(11) NOT NULL,
  `description` VARCHAR(64) NOT NULL,
  `comment` TEXT NULL,
  `work_min` FLOAT NOT NULL,
  `work_max` FLOAT NOT NULL,
  `created_at` DATETIME NULL
)
;

DROP TABLE IF EXISTS `projects`;
CREATE TABLE `projects` (
  `_id` INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
  `prefix` VARCHAR(10) NOT NULL,
  `description` VARCHAR(64) NULL,
  `start_at` DATE NOT NULL,
  `end_at` DATE NOT NULL
)
;

DROP TABLE IF EXISTS `steps`;
CREATE TABLE `steps` (
  `_id` INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
  `package_id` INT(11) NOT NULL,
  `user_id` INT(11) NULL,
  `description` VARCHAR(64) NULL,
  `comment` TEXT NULL,
  `duration` FLOAT NOT NULL,
  `completiondegree` INT(11) NOT NULL,
  `created_at` DATETIME NULL
)
;

DROP TABLE IF EXISTS `users`;
CREATE TABLE `users` (
  `_id` INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
  `name` VARCHAR(64) NOT NULL,
  `hashed_password` VARCHAR(255) NOT NULL,
  `admin` TINYINT(1) NULL DEFAULT '0'
)
;

DROP TABLE IF EXISTS `workers`;
CREATE TABLE `workers` (
  `_id` INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
  `user_id` INT(11) NOT NULL,
  `project_id` INT(11) NOT NULL
)
;

DROP TABLE IF EXISTS `session`;
CREATE TABLE `session` (
  `_id` INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
  `expiry` DATETIME  NULL,
  `data` TEXT NOT NULL
)
;

