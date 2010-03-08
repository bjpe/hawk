DROP TABLE IF EXISTS `user`;
CREATE TABLE `user` (
  `_uid` INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
  `username` VARCHAR(100) NOT NULL UNIQUE,
  `password` VARCHAR(100) NOT NULL,
  `email` VARCHAR(100) NULL,
  `caseSensitive` BOOLEAN,
  `optimizeQuery` BOOLEAN NOT NULL,
  `wordLimit` INTEGER NOT NULL,
  `replace` BOOLEAN NOT NULL,
  `swapChars` BOOLEAN NOT NULL,
  `replacements` VARCHAR(100),
  `maxFuzzy` FLOAT NOT NULL,
  `modules` TEXT,
  `packages` TEXT
);

