-- Projects
INSERT INTO `projects`(`_id`, `prefix`, `description`, `start_at`, `end_at`)
VALUES (1, 'swp 1', 'Projektdiary with Hawk', '2006-10-01', '2007-01-31');

INSERT INTO `projects`(`_id`, `prefix`, `description`, `start_at`, `end_at`)
VALUES (2, 'swp 2', 'Hacks with Perl', '2006-10-01', '2007-01-31');

-- Areas
INSERT INTO `areas`(`_id`, `project_id`, `description`, `comment`, `created_at`)
VALUES (1, 1, 'concept', NULL, '2007-01-10 22:09:24');

INSERT INTO `areas`(`_id`, `project_id`, `description`, `comment`, `created_at`)
VALUES (2, 1, 'database blueprint', NULL, '2007-01-10 22:09:25');

INSERT INTO `areas`(`_id`, `project_id`, `description`, `comment`, `created_at`)
VALUES (3, 1, 'implementation', NULL, '2007-01-10 22:09:25');

INSERT INTO `areas`(`_id`, `project_id`, `description`, `comment`, `created_at`)
VALUES (4, 1, 'tests', NULL, '2007-01-10 22:09:25');

INSERT INTO `areas`(`_id`, `project_id`, `description`, `comment`, `created_at`)
VALUES (5, 1, 'documentation', NULL, '2007-01-10 22:09:25');


-- Packages
INSERT INTO `packages`(`_id`, `area_id`, `description`, `comment`, `work_min`, `work_max`, `created_at`)
VALUES (1, 1, 'blueprint', NULL, 5, 10, '2007-01-10 22:09:24');

INSERT INTO `packages`(`_id`, `area_id`, `description`, `comment`, `work_min`, `work_max`, `created_at`)
VALUES (2, 1, 'fixing', '', 6, 10, '2007-01-10 22:09:25');


-- Steps
INSERT INTO `steps`(`_id`, `package_id`, `user_id`, `description`, `comment`, `duration`, `completiondegree`, `created_at`)
VALUES (1, 1, 3, 'brainstorming', NULL, 6, 50, '2007-01-10 22:09:25');

INSERT INTO `steps`(`_id`, `package_id`, `user_id`, `description`, `comment`, `duration`, `completiondegree`, `created_at`)
VALUES (2, 1, 3, 'improvement', NULL, 3, 100, '2007-01-10 22:09:25');

INSERT INTO `steps`(`_id`, `package_id`, `user_id`, `description`, `comment`, `duration`, `completiondegree`, `created_at`)
VALUES (3, 2, 4, 'create graph', NULL, 1, 100, '2007-01-10 22:09:25');

INSERT INTO `steps`(`_id`, `package_id`, `user_id`, `description`, `comment`, `duration`, `completiondegree`, `created_at`)
VALUES (4, 2, 3, 'locate errors', NULL, 1, 10, '2007-01-10 22:09:25');

INSERT INTO `steps`(`_id`, `package_id`, `user_id`, `description`, `comment`, `duration`, `completiondegree`, `created_at`)
VALUES (5, 2, 4, 'finish graph', NULL, 3, 100, '2007-01-10 22:09:25');


-- Users
INSERT INTO `users`(`_id`, `name`, `hashed_password`, `admin`)
VALUES (1, 'root', 'dc76e9f0c0006e8f919e0c515c66dbba3982f785', 1);

INSERT INTO `users`(`_id`, `name`, `hashed_password`, `admin`)
VALUES (2, 'admin', 'd033e22ae348aeb5660fc2140aec35850c4da997', 1);

INSERT INTO `users`(`_id`, `name`, `hashed_password`, `admin`)VALUES (3, 'alice', '522b276a356bdf39013dfabea2cd43e141ecc9e8', 0);

INSERT INTO `users`(`_id`, `name`, `hashed_password`, `admin`)
VALUES (4, 'bob', '48181acd22b3edaebc8a447868a7df7ce629920a', 0);


-- Workers
INSERT INTO `workers`(`_id`, `user_id`, `project_id`)
VALUES (1, 3, 1);

INSERT INTO `workers`(`_id`, `user_id`, `project_id`)
VALUES (2, 4, 1);

INSERT INTO `workers`(`_id`, `user_id`, `project_id`)
VALUES (3, 3, 2);
