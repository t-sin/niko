CREATE TABLE "users" (
    "id" INTEGER PRIMARY KEY AUTOINCREMENT,
    "github_id" STRING NOT NULL,
    "slack_id" STRING NOT NULL,
    "created_at" TIMESTAMP,
    "updated_at" TIMESTAMP
);

CREATE TABLE IF NOT EXISTS "schema_migrations" (
    "version" VARCHAR(255) PRIMARY KEY
);
INSERT INTO schema_migrations (version) VALUES ('20180503131337');
