DROP TABLE IF EXISTS `user`;
CREATE TABLE `user` (
  `_uid` INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
  `username` VARCHAR(100) NOT NULL UNIQUE,
  `password` VARCHAR(100) NOT NULL,
  `email` VARCHAR(100) NULL,
  `useCase` BOOLEAN,
  `optimizeQuery` BOOLEAN NOT NULL,
  `wordLimit` INTEGER NOT NULL,
  `f_replace` BOOLEAN NOT NULL,
  `f_swapChars` BOOLEAN NOT NULL,
  `f_replacements` VARCHAR(100),
  `f_max` FLOAT NOT NULL,
  `modules` TEXT,
  `packages` TEXT
);

