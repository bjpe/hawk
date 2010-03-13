-- Categories
INSERT INTO `categories`(`_id`, `name`, `discount`)
VALUES (1, 'key accounts', '3');

INSERT INTO `categories`(`_id`, `name`, `discount`)
VALUES (2, 'international customers', '1');

-- Customers
INSERT INTO `customers`(`_id`, `category_id`, `firstname`, `initials`, `lastname`, `date_of_birth`, `updated_at`)
VALUES (1, 1, 'Hans', NULL, 'Müller', '1967-06-11', '2007-01-10 22:09:24');

INSERT INTO `customers`(`_id`, `category_id`, `firstname`, `initials`, `lastname`, `date_of_birth`, `updated_at`)
VALUES (2, 1, 'Fritz', NULL, 'Maier', '1967-06-11', '2007-01-10 22:09:24');

INSERT INTO `customers`(`_id`, `category_id`, `firstname`, `initials`, `lastname`, `date_of_birth`, `updated_at`)
VALUES (3, 2, 'Kiichiro', NULL, 'Toyoda', '1967-06-11', '2007-01-10 22:09:24');

INSERT INTO `customers`(`_id`, `category_id`, `firstname`, `initials`, `lastname`, `date_of_birth`, `updated_at`)
VALUES (4, 2, 'Katsuaki', NULL, 'Watanabe', '1967-06-11', '2007-01-10 22:09:24');

INSERT INTO `customers`(`_id`, `category_id`, `firstname`, `initials`, `lastname`, `date_of_birth`, `updated_at`)
VALUES (5, 1, 'Lieschen', NULL, 'Müller', '1967-06-11', '2007-01-10 22:09:24');

