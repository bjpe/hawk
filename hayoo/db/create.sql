DROP TABLE IF EXISTS `user`;
CREATE TABLE `user` (
  `_uid` INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
  `username` VARCHAR(100) NOT NULL UNIQUE,
  `password` VARCHAR(100) NOT NULL,
  `email` VARCHAR(100) NULL
);

DROP TABLE IF EXISTS `searchConfig`;
CREATE TABLE `searchConfig` (
  `_uid` INTEGER NOT NULL PRIMARY KEY,
  `useCase` BOOLEAN NOT NULL,
  `useFuzzy` BOOLEAN NOT NULL,
  `allowPackages` BOOLEAN NOT NULL,
  `optimizeQuery` BOOLEAN NOT NULL,
  `wordLimit` INTEGER NOT NULL
);

DROP TABLE IF EXISTS `fuzzyConfig`;
CREATE TABLE `fuzzyConfig` (
  `_uid` INTEGER NOT NULL PRIMARY KEY,
  `replace` BOOLEAN NOT NULL,
  `swapChars` BOOLEAN NOT NULL,
  `replacements` VARCHAR(100) NOT NULL,
  `maxFuzzyness` FLOAT NOT NULL
);

DROP TABLE IF EXISTS `packages`;
CREATE TABLE `packages` (
  `_pid` INTEGER NOT NULL PRIMARY KEY,
  `name` VARCHAR(100) NOT NULL UNIQUE
);

DROP TABLE IF EXISTS `packageConfig`;
CREATE TABLE `packageConfig` (
  `_pid` INTEGER NOT NULL,
  `_uid` INTEGER NOT NULL,
  `allowed` BOOLEAN NOT NULL,
  `ranking` INTEGER NOT NULL
);

