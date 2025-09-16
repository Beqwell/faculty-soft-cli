-- db/schema.sql
-- Engine/charset
SET NAMES utf8mb4;
SET time_zone = '+00:00';

CREATE TABLE IF NOT EXISTS authors (
  id          INT AUTO_INCREMENT PRIMARY KEY,
  full_name   VARCHAR(200) NOT NULL,
  email       VARCHAR(200) NULL,
  created_at  TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS users (
  id          INT AUTO_INCREMENT PRIMARY KEY,
  username    VARCHAR(100) NOT NULL UNIQUE,
  full_name   VARCHAR(200) NOT NULL,
  department  VARCHAR(200) NULL,
  created_at  TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS licenses (
  id          INT AUTO_INCREMENT PRIMARY KEY,
  name        VARCHAR(120) NOT NULL,
  terms_text  TEXT NULL,
  valid_until DATE NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS storage_locations (
  id          INT AUTO_INCREMENT PRIMARY KEY,
  location    VARCHAR(255) NOT NULL,
  notes       VARCHAR(255) NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS software_kinds (
  id          INT AUTO_INCREMENT PRIMARY KEY,
  kind_name   VARCHAR(120) NOT NULL UNIQUE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS softwares (
  id             INT AUTO_INCREMENT PRIMARY KEY,
  title          VARCHAR(200) NOT NULL,
  annotation     TEXT NULL,
  kind_id        INT NULL,
  version_label  VARCHAR(80) NOT NULL,
  license_id     INT NULL,
  storage_id     INT NULL,
  created_at     TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  CONSTRAINT fk_software_kind    FOREIGN KEY (kind_id)   REFERENCES software_kinds(id) ON DELETE SET NULL,
  CONSTRAINT fk_software_license FOREIGN KEY (license_id) REFERENCES licenses(id)      ON DELETE SET NULL,
  CONSTRAINT fk_software_storage FOREIGN KEY (storage_id) REFERENCES storage_locations(id) ON DELETE SET NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS software_authors (
  software_id INT NOT NULL,
  author_id   INT NOT NULL,
  PRIMARY KEY (software_id, author_id),
  CONSTRAINT fk_sa_sw FOREIGN KEY (software_id) REFERENCES softwares(id) ON DELETE CASCADE,
  CONSTRAINT fk_sa_au FOREIGN KEY (author_id)   REFERENCES authors(id)   ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE IF NOT EXISTS usage_events (
  id           INT AUTO_INCREMENT PRIMARY KEY,
  software_id  INT NOT NULL,
  user_id      INT NULL,
  event_type   ENUM('install','run','uninstall') NOT NULL,
  event_time   TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  CONSTRAINT fk_use_sw FOREIGN KEY (software_id) REFERENCES softwares(id) ON DELETE CASCADE,
  CONSTRAINT fk_use_us FOREIGN KEY (user_id)    REFERENCES users(id)     ON DELETE SET NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

INSERT IGNORE INTO software_kinds (id, kind_name) VALUES
  (1,'IDE'), (2,'DBMS'), (3,'Library'), (4,'Tool');

INSERT IGNORE INTO licenses (id, name, terms_text, valid_until) VALUES
  (1,'MIT','Permissive use','2099-12-31'),
  (2,'Campus','Use inside faculty only',NULL);

INSERT IGNORE INTO storage_locations (id, location, notes) VALUES
  (1,'\\\\faculty-nas\\soft\\','NAS share'),
  (2,'https://downloads.example.com/','HTTP mirror');
