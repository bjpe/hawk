DROP TABLE IF EXISTS `guestbook`;
CREATE TABLE `guestbook` (
  `_id` INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
  `name` VARCHAR(100) NOT NULL,
  `message` TEXT NOT NULL,
  `createdAt` DATETIME NOT NULL
);
