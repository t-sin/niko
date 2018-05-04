ALTER TABLE "users" RENAME TO "users873";
CREATE TABLE "users" (
    "id" INTEGER PRIMARY KEY AUTOINCREMENT,
    "github_id" STRING NOT NULL,
    "github_name" STRING NOT NULL,
    "slack_id" STRING NOT NULL,
    "slack_name" STRING NOT NULL,
    "created_at" TIMESTAMP,
    "updated_at" TIMESTAMP
);
INSERT INTO "users" ("created_at", "github_id", "id", "slack_id", "updated_at") SELECT "created_at", "github_id", "id", "slack_id", "updated_at" FROM "users873";
