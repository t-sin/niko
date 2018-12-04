CREATE TABLE "user" (
    "id" BIGSERIAL NOT NULL PRIMARY KEY,
    "github_id" VARCHAR(256) NOT NULL,
    "github_name" VARCHAR(256) NOT NULL,
    "slack_id" VARCHAR(256) NOT NULL,
    "slack_name" VARCHAR(256) NOT NULL,
    "created_at" TIMESTAMPTZ,
    "updated_at" TIMESTAMPTZ
);
